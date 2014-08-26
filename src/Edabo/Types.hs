{-# LANGUAGE OverloadedStrings #-}
module Edabo.Types where

import Control.Applicative ((<$>))
import Control.Monad (mzero)
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON, (.:), (.=), Value(..), object)
import Data.Maybe (fromJust)
import Data.UUID (UUID, fromString)
import Data.UUID.Aeson ()

data Track = Track { recordingID :: UUID
                   } deriving (Show, Eq)

instance FromJSON Track where
    parseJSON (Object v) = Track <$>
                           v .: "recordingid"

    parseJSON _ = mzero

instance ToJSON Track where
    toJSON (Track recordingid ) =
      object ["recordingid" .= recordingid]

-- | The 'makeTrack' function makes a 'Track' object out of a bunch of MBIDs.
makeTrack :: String -- ^ The recording id
          -> Track  -- ^ The returned track
makeTrack recordingid = Track
                        (stringToUUID recordingid)
                        where stringToUUID = fromJust . fromString 
