{-# LANGUAGE OverloadedStrings #-}
module Edabo.MPD where

import           Data.Either                     (partitionEithers)
import           Data.Maybe                      (mapMaybe)
import           Data.String                     (fromString)
import           Data.UUID                       (UUID)
import qualified Data.UUID                       (toString)
import qualified Data.UUID                       as UUID
import           Edabo.CmdLine.Types             (CommandResult (..))
import           Edabo.Types                     (Track (..), makeTrack)
import           Network.MPD                     (Metadata (MUSICBRAINZ_ALBUMID, MUSICBRAINZ_TRACKID, MUSICBRAINZ_RELEASETRACKID),
                                                  Response, Song (..), add,
                                                  clear, currentSong, find,
                                                  sgGetTag, toString, withMPD,
                                                  (=?))
import           Network.MPD.Commands.Extensions (getPlaylist)
import           Safe                            (headMay)

-- | Clears the current playlist.
clearMPDPlaylist :: IO (Response ())
clearMPDPlaylist = withMPD clear

-- | Returns a list of songs in the current playlist.
getMPDPlaylist :: IO (Response [Song])
getMPDPlaylist = withMPD getPlaylist

-- | Tries to convert from 'Song' to 'Track'.
getTrackFromSong :: Song
                 -> Either CommandResult Track
getTrackFromSong song@(Song {sgIndex = Just _}) =
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
                             Nothing       -> Left  $ MissingMetadata [MUSICBRAINZ_TRACKID] song

getTrackFromSong song@Song {sgIndex = Nothing} =
                 Left $ InvalidInfo "The song has no position in the playlist" song

-- | Gets the current 'Song', converted to a 'Track'.
getCurrentTrack :: IO (Either CommandResult Track)
getCurrentTrack = do
  response <- withMPD currentSong
  case response of
    (Left e) -> return $ Left $ MPDFailure e
    (Right maybeSong) -> case maybeSong of
                           Nothing -> return $ Left NoCurrentSong
                           (Just song) -> return $ getTrackFromSong song

-- | Tries to convert multiple 'Song's to 'Track's.
getTracksFromSongs :: [Song]
                   -> [Either CommandResult Track]
getTracksFromSongs = map getTrackFromSong


-- | Gets the 'Tracks' that are currently in the playlist.
getTracksFromPlaylist :: IO (Either CommandResult [Track])
getTracksFromPlaylist = do
  playlist <- getMPDPlaylist
  let res = case playlist of
              Left e -> Left $ MPDFailure e
              Right songs -> trackListOrError $ getTracksFromSongs songs
  return res
  where trackListOrError :: [Either CommandResult Track]
                         -> Either CommandResult [Track]
        trackListOrError tl = case partitionEithers tl of
                                (lefts@(_:_), _) -> Left $ MultipleResults lefts
                                (_, rights) -> Right rights

-- | Loads a list of 'Track's into the current playlist.
loadMPDPlaylist :: [Track] -- ^ The list of tracks
                -> Metadata -- ^ The 'Metadata' used to look up each track in
                   -- MPDs database
                -> (Track -> Maybe UUID.UUID) -- ^ The function used to get a
                                              -- 'UUID.UUID' value corresponding
                                              -- to 'Metadata'
                -> [IO (Response ())]
loadMPDPlaylist pltracks meta uuidgetter = map loadsong $ mapMaybe uuidgetter pltracks
  where loadsong :: UUID -> IO (Response ())
        loadsong uuid = do
          withMPD $ find $ meta =? fromString (Data.UUID.toString uuid)
          >>= either (return . Left) addFirst
        addFirst songs = case headMay songs of
                           Just s -> withMPD $ add $ sgFilePath s
                           Nothing -> return $ return ()
