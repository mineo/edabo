module Edabo.Helpers where

import           Edabo.MPD   (getTracksFromPlaylist)
import           Edabo.Types (Track (..))

-- | Returns a list of 'Track's recordings that are in 'expected' but not in
-- 'current'
checkPlaylistForCompletion :: [Track] -> [Track] -> [Track]
checkPlaylistForCompletion current expected =
   let got_ids = map recordingID current
   in filter (\track -> recordingID track `notElem` got_ids) expected

-- | The 'playlistActor' function tries to get the tracklist and, in case that
--   didn't work (a Left was returned), prints the error message or (in case a
--   Right was returned)applys a function to it.
playlistActor :: ([Track] -> IO ()) -- ^ A function to apply to a list of tracks
              -> IO ()
playlistActor f = getTracksFromPlaylist >>= either putStrLn f
