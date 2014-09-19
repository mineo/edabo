module Edabo.Utils where

import           Control.Applicative            ((<$>))
import           System.Directory               (createDirectoryIfMissing)
import           System.Environment.XDG.BaseDir (getUserDataFile)
import           System.FilePath                (combine)

userdir :: IO FilePath
userdir  = getUserDataFile "edabo" "playlists"

ensureUserDir :: IO ()
ensureUserDir = userdir >>= createDirectoryIfMissing True

makePlaylistFileName :: FilePath -> IO FilePath
makePlaylistFileName plname = flip combine plname <$> userdir
