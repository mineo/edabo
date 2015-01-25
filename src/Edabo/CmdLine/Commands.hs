{-# LANGUAGE RecordWildCards #-}
module Edabo.CmdLine.Commands where

import           Control.Monad              (void, liftM)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Either                (partitionEithers)
import           Data.Maybe                 (fromMaybe)
import           Data.Time                  (UTCTime (..), getCurrentTime)
import           Data.UUID                  (UUID)
import           Edabo.CmdLine.Types        (AddToPlaylistOptions (..),
                                             CommandResult,
                                             DeletePlaylistOptions (..),
                                             EditPlaylistOptions (..),
                                             LoadOptions (..), SaveOptions (..),
                                             CommandError (..))
import           Edabo.Helpers              (checkPlaylistForCompletion,
                                             playlistActor)
import           Edabo.MPD                  (clearMPDPlaylist, getCurrentTrack,
                                             getTracksFromPlaylist,
                                             loadMPDPlaylist)
import           Edabo.Types                (Playlist (Playlist), Track,
                                             plDescription, plName, plTracks,
                                             recordingID, releaseTrackID)
import           Edabo.Utils                (edaboExtension,
                                             makePlaylistFileName,
                                             readPlaylistByName, userdir,
                                             writePlaylist)
import           Network.MPD                (Metadata (MUSICBRAINZ_RELEASETRACKID, MUSICBRAINZ_TRACKID),
                                             Response)
import           Safe                       (tailDef)
import           System.Directory           (doesFileExist,
                                             getDirectoryContents, removeFile)
import           System.FilePath            (takeExtension)
import Control.Monad.Extra (ifM)

addToPlaylist :: AddToPlaylistOptions -> IO CommandResult
addToPlaylist AddToPlaylistOptions {..} = do
  currentplaylist <- if atpOptAll
                        then getTracksFromPlaylist
                        else currentTrackAsList
  either (return . Left) addTracks currentplaylist
  where addTracks :: [Track] -> IO CommandResult
        addTracks tracks = do
            pl <- loadPlaylist atpOptCreate atpOptPlaylistName
            either (return . Left . OtherError) (update tracks) pl
        currentTrackAsList :: IO (Either CommandError [Track])
        currentTrackAsList = do
          t <- getCurrentTrack
          either (return . Left) (\track -> return $ Right [track]) t
        -- | Load the playlist we want to manipulate from a local file
        loadPlaylist :: Bool -- ^ Whether the file should be created if necessary
                     -> FilePath -- ^ The name of the playlist
                     -> IO (Either String Playlist)
        loadPlaylist create name = do
          filename <- makePlaylistFileName name
          ifM (doesFileExist filename)
              (liftM readPlaylistCase (readPlaylistByName name))
              (createOrMsg name create)
        -- | Convert from 'Maybe Playlist' to 'Either String Playlist'
        readPlaylistCase :: Maybe Playlist -> Either String Playlist
        readPlaylistCase Nothing = Left "Could not read the playlist"
        readPlaylistCase (Just pl) = Right pl
        -- | Either create a new playlist file or return a message stating that
        --   the playlist doesn't exist.
        createOrMsg :: FilePath -- ^ The playlists name
                    -> Bool -- ^ Whether to create it
                    -> IO (Either String Playlist)
        createOrMsg name False = return $ Left $ name ++ " does not exist"
        createOrMsg name True  = do
          time <- getCurrentTime
          return $ Right $ Playlist name Nothing  time []
        -- | Update the tracklist and save it
        update :: [Track] -- ^ The new tracks
               -> Playlist -- ^ The playlist that's about to be updated
               -> IO CommandResult
        update newtracks playlist@Playlist{plTracks = currentTracks} = do
          let newpl = playlist {plTracks = currentTracks ++ checkPlaylistForCompletion currentTracks newtracks}
          void $ writePlaylist newpl
          return $ Right ("Updated " ++ atpOptPlaylistName)

deletePlaylist :: DeletePlaylistOptions -> IO CommandResult
deletePlaylist DeletePlaylistOptions {optPlaylistToDeleteName = plname} = do
  plfilename <- makePlaylistFileName plname
  ifM (doesFileExist plfilename)
      (remove plfilename)
      (return $ Left $ PlaylistDoesNotExist plname)
  where remove :: FilePath -> IO CommandResult
        remove filename = removeFile filename >> return (Right filename)

edit :: EditPlaylistOptions -> IO CommandResult
edit EditPlaylistOptions { epDescription = Nothing } = return $ Right "nothing to update"
edit EditPlaylistOptions {..} = do
  pl <- readPlaylistByName epName
  case pl of
    Nothing -> return $ Left $ PlaylistDoesNotExist epName
    (Just playlist) -> doWrite playlist >> return (Right $ "Updated " ++ epName)
  where doWrite p = writePlaylist $ p { plDescription = epDescription }

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
                         (ls, _) -> return $ Left $ OtherError $ unlines $ map show ls
  where filterPlaylistFiles :: [FilePath] -> [FilePath]
        filterPlaylistFiles = filter ((== edaboExtension) . tailDef "" . takeExtension)
        makeDescription :: FilePath -> IO CommandResult
        makeDescription filename = readPlaylistByName filename
                                   >>= \x -> case x  of
                                               Nothing -> return
                                                          $ Left
                                                          $ OtherError
                                                          $ filename ++ "can't be loaded"
                                               Just pl -> return $ Right $ printableDescription pl
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
     (Left l)-> return $ Left $ OtherError $ show l
     Right _ -> readPlaylistByName optPlaylist >>= (\f -> case f of
                    Nothing       -> return $ Left $ PlaylistDoesNotExist optPlaylist
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
                                (_:_) -> Left $ MissingTracks xs

save :: SaveOptions -> IO CommandResult
save SaveOptions {..} = do
  plpath <- makePlaylistFileName optPlaylistName
  now <- getCurrentTime
  exists <- doesFileExist plpath
  let writer = write now
  if exists
     then if optOverWrite
             then writer
             else return (Left $ NotOverwritingPlaylist optPlaylistName)
     else writer
  where write :: UTCTime -> IO CommandResult
        write time = getTracksFromPlaylist
                        >>= either (return . Left)
                                   (\tracks -> writefile time tracks
                                            >> return (Right ("Wrote " ++ optPlaylistName)))
        writefile :: UTCTime -> [Track] -> IO ()
        writefile time tracks = writePlaylist $ Playlist optPlaylistName optDescription time tracks
