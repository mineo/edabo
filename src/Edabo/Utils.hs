{-# LANGUAGE RecordWildCards #-}
module Edabo.Utils where

import           Control.Applicative            ((<$>))
import           Data.Aeson                     (decode)
import           Data.Aeson.Encode              (encode)
import qualified Data.ByteString.Lazy           as B
import           Edabo.Types                    (Playlist (..))
import           System.Directory               (createDirectoryIfMissing)
import           System.Environment.XDG.BaseDir (getUserDataFile)
import           System.FilePath                (combine, hasExtension, (<.>))

edaboExtension :: String
edaboExtension = "edabo"

userdir :: IO FilePath
userdir  = getUserDataFile "edabo" "playlists"

ensureUserDir :: IO ()
ensureUserDir = userdir >>= createDirectoryIfMissing True

makePlaylistFileName :: FilePath -> IO FilePath
makePlaylistFileName plname = let filename = if hasExtension plname then plname
                                             else plname <.> edaboExtension
                              in flip combine filename <$> userdir

readPlaylist :: FilePath -> IO (Maybe Playlist)
readPlaylist filename = B.readFile filename >>= (return . decode)

readPlaylistByName :: FilePath -> IO (Maybe Playlist)
readPlaylistByName name = makePlaylistFileName name >>= readPlaylist

-- | Write a playlist to a file. The filename will be deduced from the playlists
--   name
writePlaylist :: Playlist -> IO ()
writePlaylist pl@Playlist{..} = do
  plpath <- makePlaylistFileName plName
  B.writeFile plpath $ encode pl
