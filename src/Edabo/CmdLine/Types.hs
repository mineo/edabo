module Edabo.CmdLine.Types where

data Options = Options
  { optVerbose :: Bool
  , optCommand :: Command }

data SaveOptions = SaveOptions
  { optPretty       :: Bool
  , optPlaylistName :: String}

data Command
  = List
  | Save SaveOptions

