module Edabo.CmdLine.Types where

import           Edabo.Types (Track)
import           Network.MPD (Metadata, MPDError, Song)

data Options = Options
  { optVerbose :: Bool
  , optCommand :: Command }

data SaveOptions = SaveOptions
  {  optOverWrite   :: Bool
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
  { atpOptPlaylistName :: String
  , atpOptAll          :: Bool
  , atpOptCreate       :: Bool
  }

data EditPlaylistOptions = EditPlaylistOptions
  { epName        :: String
  , epDescription :: Maybe String
 }

data Command
  = List
  | ListPlaylists
  | Save SaveOptions
  | Load LoadOptions
  | DeletePlaylist DeletePlaylistOptions
  | AddToPlaylist AddToPlaylistOptions
  | EditPlaylist EditPlaylistOptions

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
