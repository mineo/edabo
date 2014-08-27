{-# LANGUAGE OverloadedStrings #-}
module Edabo.Types where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON, (.:), (.:?), (.=), Value(..), object)
import Data.Maybe (fromJust)
import Data.UUID (UUID, fromString)
import Data.UUID.Aeson ()

data Track = Track { recordingID :: UUID,
                     releaseID   :: Maybe UUID
                   } deriving (Show, Eq)

instance FromJSON Track where
    parseJSON (Object v) = Track <$>
                           v .:  "recordingid" <*>
                           v .:? "releaseid"

    parseJSON _ = mzero

instance ToJSON Track where
    toJSON (Track recordingid releaseid) =
      object ["recordingid" .= recordingid, "releaseid" .= releaseid]

-- | The 'makeTrack' function makes a 'Track' object out of a bunch of MBIDs.
makeTrack :: String       -- ^ The recording id
          -> Maybe String -- ^ The release id
          -> Track        -- ^ The returned track
makeTrack recordingid releaseid = Track
                                  (stringToUUID recordingid)
                                  (stringToMaybeUUID releaseid)
                                  where stringToUUID = fromJust . fromString
                                        stringToMaybeUUID Nothing = Nothing
                                        stringToMaybeUUID (Just rid) = fromString rid
