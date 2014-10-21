{-# LANGUAGE OverloadedStrings #-}
module Edabo.MPD where

import           Data.Either                     (lefts, rights)
import           Data.Maybe                      (mapMaybe)
import           Data.String                     (fromString)
import           Data.UUID                       (UUID)
import qualified Data.UUID                       (toString)
import qualified Data.UUID                       as UUID
import           Edabo.Types                     (Track (..), makeTrack)
import           Network.MPD                     (Metadata (MUSICBRAINZ_ALBUMID, MUSICBRAINZ_TRACKID, MUSICBRAINZ_RELEASETRACKID, Title, Artist),
                                                  Response, Song (..), add,
                                                  clear, find, sgGetTag,
                                                  toString, withMPD, (=?))
import           Network.MPD.Commands.Extensions (getPlaylist)
import           Safe                            (headMay, headDef)

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
                                                              , "("
                                                              , show title
                                                              , "by"
                                                              , show artist
                                                              , ")"
                                                              , "has no recording id"
                                                              ]
                                              where title = metahelper "unknown title" Title
                                                    artist = metahelper "unknown artist" Artist
                                                    metahelper defaultvalue meta = maybe defaultvalue
                                                                                         (headDef defaultvalue . map toString)
                                                                                         (sgGetTag meta song)

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
loadMPDPlaylist pltracks meta uuidgetter = map loadsong $ mapMaybe uuidgetter pltracks
  where loadsong :: UUID -> IO (Response ())
        loadsong uuid = do
          withMPD $ find $ meta =? fromString (Data.UUID.toString uuid)
          >>= either (return . Left) addFirst
        addFirst songs = case headMay songs of
                           Just s -> withMPD $ add $ sgFilePath s
                           Nothing -> return $ return ()
