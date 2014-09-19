module Edabo.CmdLine.Commands where

import           Data.Aeson.Encode          (encode)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.List                  ((\\))
import           Data.Maybe                 (fromMaybe)
import           Data.Time                  (getCurrentTime)
import           Data.UUID                  (UUID, toString)
import           Edabo.CmdLine.Types        (LoadOptions (..), SaveOptions (..),
                                             optPretty)
import           Edabo.MPD                  (clearMPDPlaylist,
                                             getTracksFromPlaylist,
                                             loadMPDPlaylist)
import           Edabo.Types                (Playlist (Playlist), Track,
                                             description, name, recordingID,
                                             tracks)
import           Edabo.Utils                (edaboExtension,
                                             makePlaylistFileName, readPlaylist,
                                             userdir)
import           Safe                       (tailDef)
import           System.Directory           (doesFileExist,
                                             getDirectoryContents)
import           System.FilePath            (takeExtension)

list :: IO ()
list = do
  now <- getCurrentTime
  playlistActor (B.putStrLn
                       . encodePretty
                       . Playlist "current" (Just "the current playlist") now
                       )

listPlaylists :: IO ()
listPlaylists = userdir
            >>= getDirectoryContents
            >>= mapM_ printDescription . filterPlaylistFiles
  where filterPlaylistFiles :: [FilePath] -> [FilePath]
        filterPlaylistFiles = filter ((== edaboExtension) . tailDef "" . takeExtension)
        printDescription :: FilePath -> IO ()
        printDescription filename = makePlaylistFileName filename
                                >>= readPlaylist
                                >>= putStrLn . maybe (filename ++ "can't be loaded") printableDescription
        printableDescription :: Playlist -> String
        printableDescription pl = name pl ++ " - " ++ fromMaybe "no description" (description pl)

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
     Right _ -> readPlaylist plname >>= (\f -> case f of
                    Nothing       -> putStrLn "Couldn't load it"
                    Just playlist -> do
                      _ <- loadPlaylistIgnoringResults playlist
                      playlistActor $ checkCompletion $ tracks playlist
                      return ()
                )
   where loadPlaylistIgnoringResults :: Playlist -> IO ()
         loadPlaylistIgnoringResults pl = do
                         _ <- sequence $ loadMPDPlaylist pl
                         return ()
         checkCompletion :: [Track] -> [Track] -> IO ()
         checkCompletion current expected = do
           let notFounds = checkPlaylistForCompletion current expected
           case notFounds of
                [] -> putStrLn "Loaded all tracks"
                xs -> putStrLn $ unwords (map toString xs) ++ "were not found"

-- | Returns a list of 'UUID's for all recordings that are in 'expected' but
--   not in 'current'
checkPlaylistForCompletion :: [Track] -> [Track] -> [UUID]
checkPlaylistForCompletion current expected =
   let wanted_ids = map recordingID expected
       got_ids    = map recordingID current
   in wanted_ids \\ got_ids

-- | The 'playlistActor' function tries to get the tracklist and, in case that
--   didn't work (a Left was returned), prints the error message or (in case a
--   Right was returned)applys a function to it.
playlistActor :: ([Track] -> IO ()) -- ^ A function to apply to a list of tracks
              -> IO ()
playlistActor f = getTracksFromPlaylist >>= either putStrLn f
