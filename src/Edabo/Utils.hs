module Edabo.Utils where

import           Control.Applicative            ((<$>))
import           Data.Aeson                     (decode)
import qualified Data.ByteString.Lazy.Char8     as B
import           Edabo.Types                    (Playlist)
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
readPlaylist filename = readFile filename >>= (return . decode . B.pack)
