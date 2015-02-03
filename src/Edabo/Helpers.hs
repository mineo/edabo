module Edabo.Helpers where

import           Edabo.CmdLine.Types        (CommandError(..))
import           Edabo.MPD                  (getTracksFromPlaylist)
import           Edabo.Types                (Track (..))

-- | Returns a list of 'Track's recordings that are in 'expected' but not in
-- 'current'
checkPlaylistForCompletion :: [Track] -- ^ The current tracks
                           -> [Track] -- ^ The expected tracks
                           -> [Track] -- ^ expected - current
checkPlaylistForCompletion current expected =
   let got_ids = map recordingID current
   in filter (\track -> recordingID track `notElem` got_ids) expected

-- | The 'playlistActor' function tries to get the tracklist and, in case that
--   didn't work (a Left was returned), returns the Left or (in case a
--   Right was returned) applys a function to it.
playlistActor :: ([Track] -> Either CommandError b) -> IO (Either CommandError b)
playlistActor f = getTracksFromPlaylist >>= either (return . Left) (return . f)
