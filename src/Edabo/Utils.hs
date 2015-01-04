{-# LANGUAGE RecordWildCards #-}
module Edabo.Utils where

import           Control.Applicative            ((<$>))
import           Data.Aeson                     (decode)
import           Data.Aeson.Encode              (encode)
import           Data.Aeson.Encode.Pretty       (encodePretty)
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

-- | Write a playlist to a file. The filename will be deduced from the playlists
--   name
writePlaylist :: Bool -> Playlist -> IO ()
writePlaylist makePretty pl@Playlist{..} = do
  let encoder = if makePretty
                   then encodePretty
                   else encode
  plpath <- makePlaylistFileName plName
  B.writeFile plpath $ encoder pl
