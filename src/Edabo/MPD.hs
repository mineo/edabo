module Edabo.MPD where

import Data.Either (lefts, rights)
import Data.Maybe (fromJust)
import Edabo.Types (Track, makeTrack)
import Network.MPD (withMPD,
                    Response,
                    Song(..),
                    sgGetTag,
                    Metadata(MUSICBRAINZ_ALBUMID, MUSICBRAINZ_TRACKID),
                    toString)
import Network.MPD.Commands.Extensions (getPlaylist)

getMPDPlaylist :: IO (Response [Song])
getMPDPlaylist = withMPD getPlaylist

getTrackFromSong :: Song
                 -> Either String Track
getTrackFromSong song@(Song {sgIndex = Just pos}) =
                      let recordingid = sgGetTag MUSICBRAINZ_TRACKID song
                          releaseid   = sgGetTag MUSICBRAINZ_ALBUMID song
                      in case recordingid of
                             Just trackids -> Right $ makeTrack tid rlid
                                                where tid = toString $ head trackids
                                                      rlid = buildOptional releaseid
                                                      buildOptional value = 
                                                            case value of
                                                                 Nothing -> Nothing
                                                                 Just [] -> Nothing
                                                                 _ -> Just $ toString $ head $ fromJust releaseid
                             Nothing       -> Left  $ unwords ["Song"
                                                              , show pos
                                                              , "has no recording id"
                                                              ]
getTrackFromSong Song {sgFilePath = path, sgIndex = Nothing} =
                 Left $ unwords [toString path
                                , "has no position in the playlist - weird!"]


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
