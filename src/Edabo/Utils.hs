{-# LANGUAGE RecordWildCards #-}
module Edabo.Utils where

import           Control.Applicative            ((<$>))
import           Control.Monad                  (liftM)
import           Control.Monad.Extra            (ifM)
import           Data.Aeson                     (eitherDecode)
import           Data.Aeson.Encode              (encode)
import qualified Data.ByteString.Lazy           as B
import           Data.List                      (intercalate)
import           Data.Time                      (getCurrentTime)
import           Data.UUID.V4                   (nextRandom)
import           Edabo.CmdLine.Types            (CommandResult (..))
import           Edabo.Types                    (Playlist (..), Track (..))
import           System.Directory               (createDirectoryIfMissing,
                                                 doesFileExist)
import           System.Environment.XDG.BaseDir (getUserDataFile)
import           System.Exit                    (exitFailure, exitSuccess)
import           System.FilePath                (combine, hasExtension, (<.>))

-- | The file extension of playlists files.
edaboExtension :: String
edaboExtension = "edabo"

-- | The directory in which playlists will be saved.
userdir :: IO FilePath
userdir  = getUserDataFile "edabo" "playlists"

-- | Make sure the 'userdir' exists.
ensureUserDir :: IO ()
ensureUserDir = userdir >>= createDirectoryIfMissing True

-- | Exit with the correct exit code - 0 is the argument is a 'Success', non-0
-- otherwise.
exit :: CommandResult -> IO a
exit (Success _)= exitSuccess
exit (MultipleResults res) = case filterErrors res of
                             [] -> exitSuccess
                             _ -> exitFailure
exit _ = exitFailure

-- | Returns all non-'Success' values from a list of 'CommandResult' values.
filterErrors :: [CommandResult] -> [CommandResult]
filterErrors = filter isError
  where isError (Success _) = False
        isError (MultipleResults res) = any isError res
        isError _ = True

interactWithPlaylist :: String -- ^ The playlists name
                     -> Bool -- ^ Whether to create the playlist
                     -> (Playlist -> Either CommandResult Playlist) -- ^ A function that changes the playlist
                     -> String -- ^ A message for the user upon successful execution
                     -> IO CommandResult
interactWithPlaylist name create f message = do
  filename <- makePlaylistFileName name
  playlist <- ifM (doesFileExist filename)
              readExistingPlaylist
              (return $ Left $ PlaylistDoesNotExist name)
  case playlist of
   (Right pl) -> modifyPlaylist pl
   (Left e@(PlaylistDoesNotExist _)) -> if not create
                                      then return e
                                      else createNewPlaylist >>= modifyPlaylist
   (Left e@_) -> return e
  where modifyPlaylist :: Playlist -> IO CommandResult
        modifyPlaylist pl = case f pl of
          (Left e) -> return e
          (Right newplaylist) -> writePlaylist newplaylist >> return (Success message)
        createNewPlaylist :: IO Playlist
        createNewPlaylist = do
          time <- getCurrentTime
          newuuid <- nextRandom
          return $ Playlist name Nothing time [] newuuid
        readExistingPlaylist :: IO (Either CommandResult Playlist)
        readExistingPlaylist = liftM (\res -> case res of
                                       (Left err) -> Left (DecodingFailed name err)

                                       (Right pl) -> Right pl)
                               (readPlaylistByName name)

-- | Builds the absolute filename of a playlist from its name.
makePlaylistFileName :: FilePath -- ^ The playlists name
                     -> IO FilePath -- ^ The absolute path.
makePlaylistFileName plname = let filename = if hasExtension plname then plname
                                             else plname <.> edaboExtension
                              in flip combine filename <$> userdir

-- | Print CommandResults in a human readable format.
printResult :: CommandResult -> IO ()
printResult (PlaylistDoesNotExist name) = putStrLn ("The playlist "
                                                ++ name
                                                ++ " does not exist")
printResult (MissingMetadata metas song) = putStrLn ("The song "
                                                ++ show song
                                                ++ " is missing the following\
                                                   \ metadata: "
                                                ++ intercalate ", " (map show metas)
                                                )
printResult (MissingTracks tracks) = putStrLn ("The following tracks are missing:"
                                          ++ "\n"
                                          ++ unlines (map
                                                      trackLinks
                                                      tracks))
  where trackLinks :: Track -> String
        trackLinks Track{..} = unlines ["https://musicbrainz.org/recording/"
                                        ++ show recordingID
                                       , case releaseTrackID of
                                       (Just rtid)->
                                         "https://musicbrainz.org/track/"
                                         ++ show rtid
                                       Nothing -> ""
                                       ]
printResult (MPDFailure e) = putStrLn ("The following error occured while\
                                   \ communicating with MPD: "
                                    ++ show e)
printResult NoCurrentSong = putStrLn "No song is in MPDs playlist at the moment"
printResult (NotOverwritingPlaylist name) = putStrLn ("Did not overwrite " ++ name)
printResult (OtherError e) = putStrLn e
printResult (MultipleResults errors) = mapM_ printResult errors
printResult (DecodingFailed name reason) = putStrLn ("Decoding the JSON content of "
                                            ++ name
                                            ++ " failed for the following reason: "
                                            ++ reason)
printResult (Success msg) = putStrLn msg
printResult (HttpError exc) = print exc

-- | Open a playlists by its absolute path and try to decode it into a
-- 'Playlist' object
readPlaylist :: FilePath -> IO (Either String Playlist)
readPlaylist filename = liftM eitherDecode (B.readFile filename)

-- | Like 'readPlaylist', only that the the playlist is opened by only its name,
-- not absolute path
readPlaylistByName :: FilePath -- ^ The playlists name
                   -> IO (Either String Playlist)
readPlaylistByName name = makePlaylistFileName name >>= readPlaylist

-- | Write a 'Playlist' to a file. The filename will be deduced from the playlists
--   name
writePlaylist :: Playlist -> IO ()
writePlaylist pl@Playlist{..} = do
  now <- getCurrentTime
  plpath <- makePlaylistFileName plName
  B.writeFile plpath $ encode pl {plTimestamp = now}
