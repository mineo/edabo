module Edabo.CmdLine.Commands where

import           Data.Aeson.Encode          (encode)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as B
import           Edabo.CmdLine.Types        (SaveOptions (..), optPretty)
import           Edabo.MPD                  (getTracksFromPlaylist)
import           Edabo.Types                (Playlist (Playlist), Track)
import           Edabo.Utils                (userdir)
import           System.FilePath            (combine)

list :: IO ()
list = playlistActor (B.putStrLn
                     . encodePretty
                     . Playlist "current" (Just "the current playlist")
                     )

save :: SaveOptions -> IO ()
save SaveOptions {optPretty = pretty
                  , optPlaylistName = plname
                  , optDescription = desc} = do
  plpath <- fmap (`combine` plname) userdir
  playlistActor (writeFile plpath
                . B.unpack
                . encoder
                . Playlist plname desc)
  where encoder = if pretty then encodePretty else encode

-- | The 'playlistActor' function tries to get the tracklist and, in case that
--   didn't work (a Left was returned), prints the error message or (in case a
--   Right was returned)applys a function to it.
playlistActor :: ([Track] -> IO ()) -- ^ A function to apply to a list of tracks
              -> IO ()
playlistActor f = getTracksFromPlaylist >>= either putStrLn f
