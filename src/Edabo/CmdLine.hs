module Edabo.CmdLine where

import           Control.Applicative    ((<$>), (<*>))
import           Data.Monoid            ((<>))
import           Edabo.CmdLine.Commands (deletePlaylist, list, listPlaylists,
                                         load, save)
import           Edabo.CmdLine.Types    (Command (..),
                                         DeletePlaylistOptions (..),
                                         LoadOptions (..), Options (..),
                                         SaveOptions (..))
import           Options.Applicative    (Parser, argument, command, execParser,
                                         fullDesc, header, help, helper, info,
                                         long, metavar, optional, progDesc,
                                         pure, short, str, strOption, subparser,
                                         switch)

parseDeletePlaylist :: Parser Command
parseDeletePlaylist = DeletePlaylist
                    <$> (DeletePlaylistOptions
                        <$> argument str
                        ( metavar "NAME"
                        <> help "the name of the playlist to delete"
                        ))

parseList :: Parser Command
parseList = pure List

parseListPlaylists :: Parser Command
parseListPlaylists = pure ListPlaylists

parseLoad :: Parser Command
parseLoad = Load
            <$> (LoadOptions
                <$> switch
                ( long "clear"
                <> short 'c'
                <> help "clear the playlist before loading the new one")
                <*> argument str
                ( metavar "NAME"
                <> help "the playlists name"
                )
            )

parseSave :: Parser Command
parseSave = Save
            <$> (SaveOptions
                 <$> switch
                 (  long "pretty"
                 <> short 'p'
                 <> help "use encodePretty to encode the JSON data - this will\
                         \ make it more readable for humans :)"
                 )
                 <*> switch
                 (  long "overwrite"
                 <> short 'o'
                 <> help "overwrite the playlist if it already exists"
                 )
                 <*> optional (strOption
                 (  long "description"
                 <> short 'd'
                 <> help "the playlist description"
                 <> metavar "DESCRIPTION"))
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
           <> command "load" (info (withHelper parseLoad) (progDesc "load a playlist"))
           <> command "listplaylists" (info (withHelper parseListPlaylists) (progDesc "list all available playlists"))
           <> command "delete" (info (withHelper parseDeletePlaylist) (progDesc "delete a playlist"))
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
run Options {optCommand = cmd} = runCmd >>= either putStrLn putStrLn
  where runCmd = case cmd of
                   List -> list
                   ListPlaylists -> listPlaylists
                   Save options -> save options
                   Load options -> load options
                   DeletePlaylist options -> deletePlaylist options
