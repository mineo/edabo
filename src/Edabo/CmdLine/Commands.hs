module Edabo.CmdLine.Commands where

import           Control.Monad              (void)
import           Data.Aeson.Encode          (encode)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Either                (partitionEithers)
import           Data.Maybe                 (fromMaybe)
import           Data.Time                  (UTCTime (..), getCurrentTime)
import           Data.UUID                  (UUID)
import           Edabo.CmdLine.Types        (CommandResult,
                                             DeletePlaylistOptions (..),
                                             LoadOptions (..), SaveOptions (..),
                                             optPretty)
import           Edabo.Helpers              (checkPlaylistForCompletion,
                                             playlistActor)
import           Edabo.MPD                  (clearMPDPlaylist,
                                             getTracksFromPlaylist,
                                             loadMPDPlaylist)
import           Edabo.Types                (Playlist (Playlist), Track,
                                             description, name, recordingID,
                                             releaseTrackID, tracks)
import           Edabo.Utils                (edaboExtension,
                                             makePlaylistFileName, readPlaylist,
                                             userdir, writePlaylist)
import           Network.MPD                (Metadata (MUSICBRAINZ_RELEASETRACKID, MUSICBRAINZ_TRACKID),
                                             Response)
import           Safe                       (tailDef)
import           System.Directory           (doesFileExist,
                                             getDirectoryContents, removeFile)
import           System.FilePath            (takeExtension)


deletePlaylist :: DeletePlaylistOptions -> IO CommandResult
deletePlaylist DeletePlaylistOptions {optPlaylistToDeleteName = plname} =
  makePlaylistFileName plname
  >>= \plfilename -> doesFileExist plfilename
  >>= \doesit -> if doesit
                 then remove plfilename
                 else return $ Left $ plfilename ++ " doesn't exist"
  where remove :: FilePath -> IO CommandResult
        remove filename = removeFile filename >> return (Right filename)

list :: IO CommandResult
list = do
  now <- getCurrentTime
  playlistActor (Right
                . B.unpack
                . encodePretty
                . Playlist "current" (Just "the current playlist") now
                )

listPlaylists :: IO CommandResult
listPlaylists = userdir
            >>= getDirectoryContents
            >>= mapM makeDescription . filterPlaylistFiles
            >>= \ds -> case partitionEithers ds of
                         ([], rs) -> return $ Right $ unlines rs
                         (ls, _) -> return $ Left $ unlines ls
  where filterPlaylistFiles :: [FilePath] -> [FilePath]
        filterPlaylistFiles = filter ((== edaboExtension) . tailDef "" . takeExtension)
        makeDescription :: FilePath -> IO CommandResult
        makeDescription filename = makePlaylistFileName filename
                                    >>= readPlaylist
                                    >>= \x -> case x  of
                                                Nothing -> return $ Left $ filename ++ "can't be loaded"
                                                Just pl -> return $ Right $ printableDescription pl
        printableDescription :: Playlist -> String
        printableDescription pl = unwords [ name pl
                                          , "-"
                                          , fromMaybe "no description" (description pl)
                                          , "("
                                          , ( show . length . tracks  ) pl
                                          , "tracks)"
                                          ]


save :: SaveOptions -> IO CommandResult
save SaveOptions {optPretty = pretty
                  , optOverWrite = overwrite
                  , optPlaylistName = plname
                  , optDescription = desc} = do
  plpath <- makePlaylistFileName plname
  now <- getCurrentTime
  exists <- doesFileExist plpath
  let writer = write plpath now
  if exists
     then if overwrite
             then writer
             else return (Left $ unwords ["Not saving because the playlist"
                                         , plname
                                         , "exists."])
     else writer
  where write :: FilePath -> UTCTime -> IO CommandResult
        write path time = getTracksFromPlaylist
                        >>= either (return . Left )
                                   (\tracks -> writefile path time tracks
                                            >> return (Right ("Wrote " ++ plname)))
        writefile :: FilePath -> UTCTime -> [Track] -> IO ()
        writefile path time tracks = writePlaylist pretty $ Playlist plname desc time tracks

load :: LoadOptions -> IO CommandResult
load LoadOptions {optClear = clear
                 , optPlaylist = plname} = do
   cleared <- if clear then clearMPDPlaylist else return (return ())
   plpath <- makePlaylistFileName plname
   case cleared of
     (Left l)-> return $ Left $ show l
     Right _ -> readPlaylist plpath >>= (\f -> case f of
                  Nothing       -> return $ Left "Couldn't load it"
                  Just playlist -> do
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
         completionCase :: (Monad m) => [Track] -> [Track] -> ([Track] -> m a) -> m a
         completionCase current expected f = f $ checkPlaylistForCompletion current expected
         reportNotFounds :: [Track] -> CommandResult
         reportNotFounds xs = case xs of
                                [] -> Right "Loaded all tracks"
                                (_:_) -> Left $ unlines (map show xs) ++ "were not found"
