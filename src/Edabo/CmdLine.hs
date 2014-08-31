module Edabo.CmdLine where

import           Control.Applicative        ((<$>), (<*>))
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Monoid                ((<>))
import           Edabo.MPD                  (getTracksFromPlaylist)
import           Edabo.Types                (Playlist (Playlist))
import           Options.Applicative        (Parser, command, execParser,
                                             fullDesc, header, help, helper,
                                             info, long, progDesc, pure, short,
                                             subparser, switch)

data Options = Options
  { optVerbose :: Bool
  , optCommand :: Command }

data Command
  = List

list :: IO ()
list = do
  tl <- getTracksFromPlaylist
  case tl of
    Left e -> putStrLn e
    Right tracks -> B.putStrLn
                    $ encodePretty
                    $ Playlist "current" Nothing tracks

parseList :: Parser Command
parseList = pure List

subCommandParser :: Parser Command
subCommandParser = subparser
           (command "list" (info parseList (progDesc "print the playlist, \
                                                     \JSON-style"))
           )

globalParser :: Parser Options
globalParser = Options
               <$> switch (short 'v'
                           <> long "verbose")
               <*> subCommandParser

handleArgs :: IO ()
handleArgs = execParser opts >>= run
  where opts = info (helper <*> globalParser)
          ( fullDesc
          <> progDesc "playlist saver & loader"
          <> header "edabo"
          )

run :: Options -> IO ()
run Options {optCommand = cmd} =
  case cmd of
    List -> list
    _    -> undefined
