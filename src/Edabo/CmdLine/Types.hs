module Edabo.CmdLine.Types where

data Options = Options
  { optVerbose :: Bool
  , optCommand :: Command }

data SaveOptions = SaveOptions
  {  optOverWrite    :: Bool
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
  }

data Command
  = List
  | ListPlaylists
  | Save SaveOptions
  | Load LoadOptions
  | DeletePlaylist DeletePlaylistOptions
  | AddToPlaylist AddToPlaylistOptions

type CommandResult = Either String String
