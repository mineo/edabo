module Edabo.MPD where

import Data.Either (lefts, rights)
import Edabo.Types (Track, makeTrack)
import Network.MPD (withMPD,
                    Response,
                    Song(..),
                    sgGetTag,
                    Metadata(MUSICBRAINZ_TRACKID),
                    toString)
import Network.MPD.Commands.Extensions (getPlaylist)

getMPDPlaylist :: IO (Response [Song])
getMPDPlaylist = withMPD getPlaylist

getTrackFromSong :: Song
                 -> Either String Track
getTrackFromSong song@(Song {sgIndex = Just pos}) = case sgGetTag MUSICBRAINZ_TRACKID song of
                             Just trackids -> Right $ makeTrack $ toString $ head trackids
                             Nothing       -> Left  $ unwords ["Song"
                                                              , show pos
                                                              , "has no recording id"
                                                              ]
getTrackFromSong Song {sgFilePath = path, sgIndex = Nothing} =
                 Left $ unwords [toString path
                                , "has no recording id"]


getTracksFromSongs :: [Song]
                   -> [Either String Track]
getTracksFromSongs = map getTrackFromSong

getTracksFromPlaylist :: IO (Either String [Track])
getTracksFromPlaylist = do
  playlist <- getMPDPlaylist
  let res = case playlist of
              Left e -> Left $ show e
              Right songs -> trackListOrError $ getTracksFromSongs songs
  return res
  where trackListOrError tl = case length tllefts of
                                0 -> Right $ rights tl
                                _ -> Left  $ unlines tllefts
                              where tllefts = lefts tl
