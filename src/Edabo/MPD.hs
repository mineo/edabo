module MPD where

import Network.MPD (withMPD,
                    Response,
                    Song(..),
                    sgGetTag,
                    Metadata(MUSICBRAINZ_TRACKID),
                    toString)
import Network.MPD.Commands.Extensions (getPlaylist)
import Types (Track, makeTrack)

getPlayList :: IO (Response [Song])
getPlayList = withMPD getPlaylist

getTrackFromSong :: Song
                 -> Either String Track
getTrackFromSong song@(Song {sgIndex = Just pos}) = case sgGetTag MUSICBRAINZ_TRACKID song of
                             Just trackids -> Right $ makeTrack $ toString $ head trackids
                             Nothing       -> Left $ unwords ["Song"
                                                             , show pos
                                                             , "has no recording id"
                                                             ]
getTrackFromSong Song {sgFilePath = path, sgIndex = Nothing} =
                 Left $ unwords [toString path
                                , "has no recording id"]


getTracksFromSongs :: [Song]
                   -> [Either String Track]
getTracksFromSongs = map getTrackFromSong
