{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Edabo.CmdLine.Commands where

import           Control.Exception          (handle)
import           Control.Monad              (liftM, void)
import           Control.Monad.Extra        (ifM)
import           Data.Aeson                 (encode, toJSON)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                (mconcat)
import           Data.Time                  (UTCTime (..), getCurrentTime)
import           Data.UUID                  (UUID, toString)
import           Data.UUID.V4               (nextRandom)
import           Edabo.CmdLine.Types        (AddToPlaylistOptions (..),
                                             CommandResult (..),
                                             DeletePlaylistOptions (..),
                                             EditPlaylistOptions (..),
                                             LoadOptions (..), PathOptions (..),
                                             SaveOptions (..),
                                             UploadOptions (..))
import           Edabo.Helpers              (checkPlaylistForCompletion,
                                             playlistActor)
import           Edabo.MPD                  (clearMPDPlaylist, getCurrentTrack,
                                             getTracksFromPlaylist,
                                             loadMPDPlaylist)
import           Edabo.Types                (Playlist (..), Track,
                                             plDescription, plName, plTracks,
                                             recordingID, releaseTrackID)
import           Edabo.Utils                (edaboExtension, filterErrors,
                                             interactWithPlaylist,
                                             makePlaylistFileName,
                                             readPlaylistByName, userdir,
                                             writePlaylist)
import           Network.HTTP.Client        (HttpException)
import           Network.MPD                (Metadata (MUSICBRAINZ_RELEASETRACKID, MUSICBRAINZ_TRACKID),
                                             Response)
import           Network.Wreq               (partLBS, post)
import           Safe                       (tailDef)
import           System.Directory           (doesFileExist,
                                             getDirectoryContents, removeFile)
import           System.FilePath            (takeExtension)

addToPlaylist :: AddToPlaylistOptions -> IO CommandResult
addToPlaylist AddToPlaylistOptions {..} = do
  currentplaylist <- if atpOptAll
                        then getTracksFromPlaylist
                        else currentTrackAsList
  case currentplaylist of
   (Left e) -> return e
   (Right currenttracks) -> liftM
                            mconcat
                            (mapM (\name -> add name currenttracks)
                             atpOptPlaylistNames)
  where
    add :: String -> [Track] -> IO CommandResult
    add name tracks = interactWithPlaylist name atpOptCreate (update tracks)
                      ("Updated " ++ name)
    currentTrackAsList :: IO (Either CommandResult [Track])
    currentTrackAsList = do
      t <- getCurrentTrack
      either (return . Left) (\track -> return $ Right [track]) t
    update :: [Track] -- ^ The new tracks
           -> Playlist -- ^ The playlist that's about to be updated
           -> Either CommandResult Playlist
    update newtracks playlist@Playlist{plTracks = currentTracks} =
      Right $ playlist {plTracks = currentTracks ++ checkPlaylistForCompletion currentTracks newtracks}

deletePlaylist :: DeletePlaylistOptions -> IO CommandResult
deletePlaylist DeletePlaylistOptions {optPlaylistToDeleteName = plname} = do
  plfilename <- makePlaylistFileName plname
  ifM (doesFileExist plfilename)
      (remove plfilename)
      (return $ PlaylistDoesNotExist plname)
  where remove :: FilePath -> IO CommandResult
        remove filename = removeFile filename >> return (Success filename)

edit :: EditPlaylistOptions -> IO CommandResult
edit EditPlaylistOptions { epDescription = Nothing } = return $ Success "nothing to update"
edit EditPlaylistOptions {..} = interactWithPlaylist
                                epName
                                False
                                (\p -> Right p {plDescription = epDescription})
                                ("Updated " ++ epName)

list :: IO CommandResult
list = do
  now <- getCurrentTime
  newuuid <- nextRandom
  playlistActor (\tracks ->
                  Success
                $ B.unpack
                $ encodePretty
                $ Playlist "current" (Just "the current playlist") now tracks newuuid
                )

listPlaylists :: IO CommandResult
listPlaylists = userdir
            >>= getDirectoryContents
            >>= mapM makeDescription . filterPlaylistFiles
            >>= \ds -> case filterErrors ds of
                         [] -> return $ MultipleResults ds
                         errors -> return $ OtherError $ unlines $ map show errors
  where filterPlaylistFiles :: [FilePath] -> [FilePath]
        filterPlaylistFiles = filter ((== edaboExtension) . tailDef "" . takeExtension)
        makeDescription :: FilePath -> IO CommandResult
        makeDescription filename = readPlaylistByName filename
                                   >>= \x -> case x  of
                                               Left err -> return
                                                          $ DecodingFailed
                                                          filename err
                                               Right pl -> return $ Success $ printableDescription pl
        printableDescription :: Playlist -> String
        printableDescription pl = unwords [ plName pl
                                          , "-"
                                          , fromMaybe "no description" (plDescription pl)
                                          , "("
                                          , ( show . length . plTracks  ) pl
                                          , "tracks)"
                                          ]

load :: LoadOptions -> IO CommandResult
load LoadOptions {..} = do
   cleared <- if optClear then clearMPDPlaylist else return (return ())
   case cleared of
     (Left l)-> return $ OtherError $ show l
     Right _ -> readPlaylistByName optPlaylist >>= (\f -> case f of
                    Left err       -> return $ DecodingFailed optPlaylist err
                    Right playlist -> do
                      let pltracks = plTracks playlist
                      void $ loadPlaylistIgnoringResults pltracks
                      playlistActor $ \loadedTracks -> completionCase loadedTracks pltracks reportNotFounds
                )
   where loadPlaylistIgnoringResults :: [Track] -> IO ()
         loadPlaylistIgnoringResults pl = do
                         -- first, try to load all tracks via their release track id
                         void $ doLoad pl MUSICBRAINZ_RELEASETRACKID releaseTrackID
                         -- then try to load the missing ones via their normal recording/track id
                         getTracksFromPlaylist >>= \tracks -> case tracks of
                                 Left _ -> return ()
                                 Right tracks -> void $ completionCase tracks pl (\missings -> doLoad missings MUSICBRAINZ_TRACKID (Just . recordingID))
         doLoad :: [Track] -> Metadata -> (Track -> Maybe UUID) -> IO [Response ()]
         doLoad trs meta uuidgetter = sequence $ loadMPDPlaylist trs meta uuidgetter
         completionCase :: [Track] -> [Track] -> ([Track] -> t) -> t
         completionCase current expected f = f $ checkPlaylistForCompletion current expected
         reportNotFounds :: [Track] -> CommandResult
         reportNotFounds xs = case xs of
                                [] -> Success "Loaded all tracks"
                                (_:_) -> MissingTracks xs

path :: PathOptions -> IO CommandResult
path PathOptions {..} = do
  plpath <- makePlaylistFileName pName
  exists <- doesFileExist plpath
  return (if exists
             then Success plpath
             else PlaylistDoesNotExist pName)

save :: SaveOptions -> IO CommandResult
save SaveOptions {..} = do
  plpath <- makePlaylistFileName optPlaylistName
  now <- getCurrentTime
  exists <- doesFileExist plpath
  let writer = write now
  if exists
     then if optOverWrite
             -- By using interactWithPlaylist, the playlist gets overwritten,
             -- but keeps its UUID.
             then getTracksFromPlaylist
                     >>= either return (\tracks -> interactWithPlaylist
                                                   optPlaylistName
                                                   False
                                                   (Right . overwriteTracks tracks)
                                                   ("Wrote " ++ optPlaylistName))
             else return (NotOverwritingPlaylist optPlaylistName)
     else writer
  where overwriteTracks :: [Track] -> Playlist -> Playlist
        overwriteTracks tracks playlist = playlist {plTracks = tracks}
        write :: UTCTime -> IO CommandResult
        write time = getTracksFromPlaylist
                        >>= either return
                                   (\tracks -> writefile time tracks
                                            >> return (Success ("Wrote " ++ optPlaylistName)))
        writefile :: UTCTime -> [Track] -> IO ()
        writefile time tracks = do
          newuuid <- nextRandom
          writePlaylist $ Playlist optPlaylistName optDescription time tracks newuuid

upload :: UploadOptions -> IO CommandResult
upload UploadOptions {..} = do
  readPlaylist <- readPlaylistByName upName
  case readPlaylist of
   Left err -> return $ DecodingFailed upName err
   Right pl -> doUpload pl
  where doUpload playlist =
          handle handleHTTPException $ do
            -- Wreq throws exceptions for almost everything, so just ignore the
            -- response for now.
            _ <- post ("http://localhost:5000/playlist/" ++
                     toString (plUUID playlist))
                 -- Decoding the playlist was possible, therefore encoding it
                 -- again *should* be safe.
                 [partLBS "playlist" (encode (toJSON playlist))]
            return $ Success "upload successful"
        handleHTTPException :: HttpException -> IO CommandResult
        handleHTTPException e = return $ HttpError e
