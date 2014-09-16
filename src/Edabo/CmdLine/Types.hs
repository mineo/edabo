module Edabo.CmdLine.Types where

data Options = Options
  { optVerbose :: Bool
  , optCommand :: Command }

data SaveOptions = SaveOptions
  { optPretty       :: Bool
  , optOverWrite    :: Bool
  , optDescription  :: Maybe String
  , optPlaylistName :: String}

data LoadOptions = LoadOptions
  { optClear    :: Bool
  , optPlaylist :: String
  }

data Command
  = List
  | Save SaveOptions
  | Load LoadOptions
