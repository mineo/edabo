{-# LANGUAGE OverloadedStrings #-}
module Edabo.Types where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (mzero)
import           Data.Aeson          (FromJSON, ToJSON, Value (..), object,
                                      parseJSON, toJSON, (.:), (.:?), (.=))
import           Data.Maybe          (fromJust)
import           Data.Time           (UTCTime (..))
import           Data.UUID           (UUID, fromString)
import           Data.UUID.Aeson     ()

data Track = Track
  { recordingID    :: UUID
  , releaseID      :: Maybe UUID
  , releaseTrackID :: Maybe UUID
  } deriving (Show, Eq)

instance FromJSON Track where
  parseJSON (Object v) = Track
                         <$> v .:  "recordingid"
                         <*> v .:? "releaseid"
                         <*> v .:? "releasetrackid"

  parseJSON _ = mzero

instance ToJSON Track where
  toJSON (Track recordingid releaseid releasetrackid) =
    object ["recordingid" .= recordingid, "releaseid" .= releaseid
           ,"releasetrackid" .= releasetrackid]

-- | The 'makeTrack' function makes a 'Track' object out of a bunch of MBIDs.
makeTrack :: String       -- ^ The recording id
          -> Maybe String -- ^ The release id
          -> Maybe String -- ^ The release track id
          -> Track        -- ^ The returned track
makeTrack recordingid releaseid releasetrackid = Track
                                                 (stringToUUID recordingid)
                                                 (stringToMaybeUUID releaseid)
                                                 (stringToMaybeUUID releasetrackid)
                                                 where stringToUUID = fromJust . fromString
                                                       stringToMaybeUUID Nothing = Nothing
                                                       stringToMaybeUUID (Just rid) = fromString rid

data Playlist = Playlist
  { name        :: String
  , description :: Maybe String
  , timestamp   :: UTCTime
  , tracks      :: [Track]
  }

instance FromJSON Playlist where
  parseJSON (Object v) = Playlist
                         <$> v .:  "name"
                         <*> v .:? "description"
                         <*> v .:  "timestamp"
                         <*> v .:  "tracklist"
  parseJSON _ = mzero

instance ToJSON Playlist where
  toJSON (Playlist _name desc _timestamp _tracks) =
    object ["name" .= _name, "description" .= desc, "timestamp" .= _timestamp,
            "tracklist" .= _tracks]
