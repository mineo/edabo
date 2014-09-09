module Edabo.Utils where

import           System.Directory               (createDirectoryIfMissing)
import           System.Environment.XDG.BaseDir (getUserDataFile)

userdir :: IO FilePath
userdir  = getUserDataFile "edabo" "playlists"

ensureUserDir :: IO ()
ensureUserDir = userdir >>= createDirectoryIfMissing True
