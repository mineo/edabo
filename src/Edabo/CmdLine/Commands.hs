module Edabo.CmdLine.Commands where

import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as B
import           Edabo.CmdLine.Types        (SaveOptions(..), optPretty)
import           Edabo.MPD                  (getTracksFromPlaylist)
import           Edabo.Types                (Playlist (Playlist))
import Data.Aeson.Encode (encode)

list :: IO ()
list = do
  tl <- getTracksFromPlaylist
  case tl of
    Left e -> putStrLn e
    Right tracks -> B.putStrLn
                    $ encodePretty
                    $ Playlist "current" Nothing tracks

save :: SaveOptions -> IO ()
save SaveOptions {optPretty = pretty, optPlaylistName = plname} = do
  tl <- getTracksFromPlaylist
  case tl of
    Left e -> putStrLn e
    Right tracks -> B.putStrLn
                    $ encoder
                    $ Playlist plname Nothing tracks
  where encoder = if pretty then encodePretty else encode
