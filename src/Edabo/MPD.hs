{-# LANGUAGE OverloadedStrings #-}
module Edabo.MPD where

import           Data.Either                     (lefts, rights)
import           Data.Maybe                      (catMaybes, fromJust)
import           Data.String                     (fromString)
import qualified Data.UUID                       (toString)
import qualified Data.UUID as UUID
import           Edabo.Types                     (Track (..),  makeTrack)
import           Network.MPD                     (Metadata (MUSICBRAINZ_ALBUMID,
                                                            MUSICBRAINZ_TRACKID,
                                                            MUSICBRAINZ_RELEASETRACKID),
                                                  Response, Song (..), clear,
                                                  findAdd, sgGetTag, toString,
                                                  withMPD, (=?))
import           Network.MPD.Commands.Extensions (getPlaylist)

clearMPDPlaylist :: IO (Response ())
clearMPDPlaylist = withMPD clear

getMPDPlaylist :: IO (Response [Song])
getMPDPlaylist = withMPD getPlaylist

getTrackFromSong :: Song
                 -> Either String Track
getTrackFromSong song@(Song {sgIndex = Just pos}) =
                      let recordingid    = sgGetTag MUSICBRAINZ_TRACKID song
                          releaseid      = sgGetTag MUSICBRAINZ_ALBUMID song
                          releasetrackid = sgGetTag MUSICBRAINZ_RELEASETRACKID song
                      in case recordingid of
                             Just trackids -> Right $ makeTrack tid rlid rltid
                                                where tid = toString $ head trackids
                                                      rlid = buildOptional releaseid
                                                      rltid = buildOptional releasetrackid
                                                      buildOptional value =
                                                            case value of
                                                                 Nothing -> Nothing
                                                                 Just [] -> Nothing
                                                                 Just v -> Just $ toString $ head v
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

loadMPDPlaylist :: [Track] -> Metadata -> (Track -> Maybe UUID.UUID) -> [IO (Response ())]
loadMPDPlaylist pltracks meta uuidgetter = map loadsong $ catMaybes $map uuidgetter pltracks
  where loadsong :: UUID.UUID -> IO (Response ())
        loadsong uuid = withMPD $ findAdd $ meta =? fromString (Data.UUID.toString uuid)
