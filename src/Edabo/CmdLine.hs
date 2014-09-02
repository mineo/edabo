module Edabo.CmdLine where

import           Control.Applicative    ((<$>), (<*>))
import           Data.Monoid            ((<>))
import           Edabo.CmdLine.Commands (list, save)
import           Edabo.CmdLine.Types    (Command (..), Options (..),
                                         SaveOptions (..))
import           Options.Applicative    (Parser, argument, command, execParser,
                                         fullDesc, header, help, helper, info,
                                         long, metavar, progDesc, pure, short,
                                         str, subparser, switch)

parseList :: Parser Command
parseList = pure List

parseSave :: Parser Command
parseSave = Save
            <$> (SaveOptions
                 <$> switch
                 ( long "pretty"
                 <> help "use encodePretty to encode the JSON data - this will\
                         \make it more readable for humans :)"
                 )
                 <*> argument str
                 ( metavar "NAME"
                 <> help "the playlists name"
                 )
            )

subCommandParser :: Parser Command
subCommandParser = subparser
           (command "list" (info (withHelper parseList) (progDesc "print the playlist, \
                                                                           \JSON-style"))
           <> command "save" (info (withHelper parseSave) (progDesc "save the playlist"))
           )
           where withHelper f = helper <*> f

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
    Save options -> save options
    _    -> undefined
