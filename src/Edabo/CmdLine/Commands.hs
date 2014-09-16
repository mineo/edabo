module Edabo.CmdLine.Commands where

import           Data.Aeson                 (decode)
import           Data.Aeson.Encode          (encode)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Time                  (getCurrentTime)
import           Edabo.CmdLine.Types        (LoadOptions (..), SaveOptions (..),
                                             optPretty)
import           Edabo.MPD                  (clearMPDPlaylist,
                                             getTracksFromPlaylist,
                                             loadMPDPlaylist)
import           Edabo.Types                (Playlist (Playlist), Track)
import           Edabo.Utils                (makePlaylistFileName)
import           System.Directory           (doesFileExist)

list :: IO ()
list = do
  now <- getCurrentTime
  playlistActor (B.putStrLn
                       . encodePretty
                       . Playlist "current" (Just "the current playlist") now
                       )

save :: SaveOptions -> IO ()
save SaveOptions {optPretty = pretty
                  , optOverWrite = overwrite
                  , optPlaylistName = plname
                  , optDescription = desc} = do
  plpath <- makePlaylistFileName plname
  now <- getCurrentTime
  exists <- doesFileExist plpath
  let writer = write plpath now
  if exists
     then if overwrite
             then sequence_ [putStrLn $ unwords ["Overwriting", plname, "."]
                             , writer]
             else putStrLn $ unwords ["Not saving because the playlist"
                                      , plname
                                      , "exists."]
     else writer
  where encoder = if pretty then encodePretty else encode
        write path time = playlistActor (writeFile path
                            . B.unpack
                            . encoder
                            . Playlist plname desc time)

load :: LoadOptions -> IO ()
load LoadOptions {optClear = clear
                 , optPlaylist = plname} = do
   cleared <- if clear then clearMPDPlaylist else return (return ())
   case cleared of
     Left e -> print e
     Right _ -> readPlaylist >>= (\f -> case f of
                    Nothing       -> putStrLn "Couldn't load it"
                    Just playlist -> do
                      _ <- loadPlaylistIgnoringResults playlist
                      return ()
                )
   where readPlaylist :: IO (Maybe Playlist)
         readPlaylist = readFile plname >>= (return . decode . B.pack)
         loadPlaylistIgnoringResults :: Playlist -> IO ()
         loadPlaylistIgnoringResults pl = do
                        _ <- sequence $ loadMPDPlaylist pl
                        return ()

-- | The 'playlistActor' function tries to get the tracklist and, in case that
--   didn't work (a Left was returned), prints the error message or (in case a
--   Right was returned)applys a function to it.
playlistActor :: ([Track] -> IO ()) -- ^ A function to apply to a list of tracks
              -> IO ()
playlistActor f = getTracksFromPlaylist >>= either putStrLn f
