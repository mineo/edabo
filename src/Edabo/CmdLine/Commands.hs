module Edabo.CmdLine.Commands where

import           Data.Aeson.Encode          (encode)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as B
import           Edabo.CmdLine.Types        (SaveOptions (..), optPretty)
import           Edabo.MPD                  (getTracksFromPlaylist)
import           Edabo.Types                (Playlist (Playlist), Track)

list :: IO ()
list = playlistActor (B.putStrLn
                     . encodePretty
                     . Playlist "current" (Just "the current playlist")
                     )

save :: SaveOptions -> IO ()
save SaveOptions {optPretty = pretty
                  , optPlaylistName = plname
                  , optDescription = desc} =
  playlistActor (B.putStrLn
                . encoder
                . Playlist plname desc)
  where encoder = if pretty then encodePretty else encode

-- | The 'playlistActor' function tries to get the tracklist and (in case that
-- | didn't work - a Left was returned) prints the error message or applys a function
-- | to it.
playlistActor :: ([Track] -> IO ()) -- ^ A function to apply to a list of tracks
              -> IO ()
playlistActor f = do
  tl <- getTracksFromPlaylist
  either putStrLn f tl
