{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Edabo.CmdLine.Types where

import           Data.Monoid (Monoid (..))
import           Edabo.Types (Track)
import           Network.MPD (MPDError, Metadata, Song)

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

data Command
  = List
  | ListPlaylists
  | Save SaveOptions
  | Load LoadOptions
  | DeletePlaylist DeletePlaylistOptions
  | AddToPlaylist AddToPlaylistOptions
  | EditPlaylist EditPlaylistOptions
  | PlaylistPath PathOptions

data CommandError
  = PlaylistDoesNotExist String
  | InvalidInfo String Song
  | MissingMetadata [Metadata] Song
  | MissingTracks [Track]
  | MPDFailure MPDError
  | NoCurrentSong
  | NotOverwritingPlaylist String
  | OtherError String
  | MultipleErrors [CommandError]
  | DecodingFailed String
    deriving (Show, Eq)
type CommandResult = Either CommandError String

instance Monoid CommandResult where
  mempty = Right ""
  mappend (Right "") other = other
  mappend other (Right "") = other
  mappend (Right s1) (Right s2) = Right (s1 ++ "\n" ++ s2)
  mappend (Right s) (Left e) = Left (MultipleErrors [OtherError s, e])
  mappend (Left e) (Right s) = Left (MultipleErrors [e, OtherError s])
  mappend (Left (MultipleErrors es1)) (Left (MultipleErrors es2)) =
    Left (MultipleErrors (es1 ++ es2))
  mappend (Left (MultipleErrors es1)) (Left e) =
    Left (MultipleErrors (es1 ++ [e]))
  mappend (Left e) (Left (MultipleErrors es1)) =
    Left (MultipleErrors (es1 ++ [e]))
  mappend (Left e1) (Left e2) = Left (MultipleErrors [e1, e2])
