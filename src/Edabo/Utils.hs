{-# LANGUAGE RecordWildCards #-}
module Edabo.Utils where

import           Control.Applicative            ((<$>))
import           Control.Monad                  (liftM)
import           Control.Monad.Extra            (ifM)
import           Data.Aeson                     (decode)
import           Data.Aeson.Encode              (encode)
import qualified Data.ByteString.Lazy           as B
import           Data.Time                      (getCurrentTime)
import           Edabo.CmdLine.Types            (CommandError (..),
                                                 CommandResult)
import           Edabo.Types                    (Playlist (..))
import           System.Directory               (createDirectoryIfMissing,
                                                 doesFileExist)
import           System.Environment.XDG.BaseDir (getUserDataFile)
import           System.FilePath                (combine, hasExtension, (<.>))

edaboExtension :: String
edaboExtension = "edabo"

userdir :: IO FilePath
userdir  = getUserDataFile "edabo" "playlists"

ensureUserDir :: IO ()
ensureUserDir = userdir >>= createDirectoryIfMissing True

interactWithPlaylist :: String -- ^ The playlists name
                     -> Bool -- ^ Whether to create the playlist
                     -> (Playlist -> Either CommandError Playlist) -- ^ A function that changes the playlist
                     -> String -- ^ A message for the user upon successful execution
                     -> IO CommandResult
interactWithPlaylist name create f message = do
  filename <- makePlaylistFileName name
  playlist <- ifM (doesFileExist filename)
              readExistingPlaylist
              (return $ Left $ PlaylistDoesNotExist name)
  case playlist of
   (Right pl) -> modifyPlaylist pl
   (Left e) -> if not create
                          then return $ Left e
                          else createNewPlaylist >>= modifyPlaylist
  where modifyPlaylist :: Playlist -> IO CommandResult
        modifyPlaylist pl = case f pl of
          (Left e) -> return (Left e)
          (Right newplaylist) -> writePlaylist newplaylist >> return (Right message)
        createNewPlaylist :: IO Playlist
        createNewPlaylist = do
          time <- getCurrentTime
          return $ Playlist name Nothing time []
        readExistingPlaylist :: IO (Either CommandError Playlist)
        readExistingPlaylist = liftM (maybe
                                      (Left (OtherError ("Couldn't read " ++ name)))
                                      Right)
                               (readPlaylistByName name)

makePlaylistFileName :: FilePath -> IO FilePath
makePlaylistFileName plname = let filename = if hasExtension plname then plname
                                             else plname <.> edaboExtension
                              in flip combine filename <$> userdir

readPlaylist :: FilePath -> IO (Maybe Playlist)
readPlaylist filename = liftM decode (B.readFile filename)

readPlaylistByName :: FilePath -> IO (Maybe Playlist)
readPlaylistByName name = makePlaylistFileName name >>= readPlaylist

-- | Write a playlist to a file. The filename will be deduced from the playlists
--   name
writePlaylist :: Playlist -> IO ()
writePlaylist pl@Playlist{..} = do
  plpath <- makePlaylistFileName plName
  B.writeFile plpath $ encode pl
