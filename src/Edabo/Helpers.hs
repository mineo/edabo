{-# LANGUAGE RecordWildCards #-}
module Edabo.Helpers where

import           Data.Aeson.Encode          (encode)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as B
import           Edabo.CmdLine.Types        (CommandResult)
import           Edabo.MPD                  (getTracksFromPlaylist)
import           Edabo.Types                (Playlist (..), Track (..))
import           Edabo.Utils                (makePlaylistFileName)

-- | Returns a list of 'Track's recordings that are in 'expected' but not in
-- 'current'
checkPlaylistForCompletion :: [Track] -> [Track] -> [Track]
checkPlaylistForCompletion current expected =
   let got_ids = map recordingID current
   in filter (\track -> recordingID track `notElem` got_ids) expected

-- | The 'playlistActor' function tries to get the tracklist and, in case that
--   didn't work (a Left was returned), returns the Left or (in case a
--   Right was returned) applys a function to it.
playlistActor :: ([Track] -> CommandResult) -> IO CommandResult
playlistActor f = getTracksFromPlaylist >>= either (return . Left) (return . f)

-- | Write a playlist to a file. The filename will be deduced from the playlists
--   name
writePlaylist :: Bool -> Playlist -> IO ()
writePlaylist makePretty pl@Playlist{..} = do
  let encoder = if makePretty
                   then encodePretty
                   else encode
  plpath <- makePlaylistFileName name
  writeFile plpath $ B.unpack $ encoder pl
