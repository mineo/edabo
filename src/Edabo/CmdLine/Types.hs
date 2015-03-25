{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Edabo.CmdLine.Types where

import           Data.Monoid         (Monoid (..))
import           Edabo.Types         (Track)
import           Network.HTTP.Client (HttpException)
import           Network.MPD         (MPDError, Metadata, Song)

data Options = Options
  { optVerbose :: Bool
  , optCommand :: Command }

data SaveOptions = SaveOptions
  { optOverWrite    :: Bool
  , optDescription  :: Maybe String
  , optPlaylistName :: String}

data LoadOptions = LoadOptions
  { optClear    :: Bool
  , optPlaylist :: String
  }

data DeletePlaylistOptions = DeletePlaylistOptions
  { optPlaylistToDeleteName :: String
  }

data AddToPlaylistOptions = AddToPlaylistOptions
  { atpOptPlaylistNames :: [String]
  , atpOptAll           :: Bool
  , atpOptCreate        :: Bool
  }

data EditPlaylistOptions = EditPlaylistOptions
  { epName        :: String
  , epDescription :: Maybe String
  }

data PathOptions = PathOptions
  { pName :: String
  }

data UploadOptions = UploadOptions
  { upName :: String
  }

data Command
  = List
  | ListPlaylists
  | Save SaveOptions
  | Load LoadOptions
  | DeletePlaylist DeletePlaylistOptions
  | AddToPlaylist AddToPlaylistOptions
  | EditPlaylist EditPlaylistOptions
  | PlaylistPath PathOptions
  | Upload UploadOptions

data CommandResult
  = PlaylistDoesNotExist String
  | InvalidInfo String Song
  | HttpError HttpException
  | MissingMetadata [Metadata] Song
  | MissingTracks [Track]
  | MPDFailure MPDError
  | NoCurrentSong
  | NotOverwritingPlaylist String
  | OtherError String
  | MultipleResults [CommandResult]
  | DecodingFailed String
  | Success String
    deriving (Show)

instance Monoid CommandResult where
  mempty = Success ""
  mappend (Success "") a = a
  mappend a (Success "") = a
  mappend (MultipleResults a) (MultipleResults b) = MultipleResults (a ++ b)
  mappend (MultipleResults a) b = MultipleResults (a ++ [b])
  mappend a (MultipleResults b) = MultipleResults (a : b)
  mappend a b = MultipleResults [a, b]
