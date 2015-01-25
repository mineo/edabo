module Edabo.CmdLine where

import           Control.Applicative    ((<$>), (<*>))
import           Data.Monoid            ((<>))
import           Edabo.CmdLine.Commands (addToPlaylist, deletePlaylist, list,
                                         listPlaylists, load, save, edit)
import           Edabo.CmdLine.Types    (AddToPlaylistOptions (..),
                                         Command (..),
                                         DeletePlaylistOptions (..),
                                         EditPlaylistOptions (..),
                                         LoadOptions (..), Options (..),
                                         SaveOptions (..))
import           Options.Applicative    (Parser, argument, command, execParser,
                                         fullDesc, header, help, helper, info,
                                         long, metavar, optional, progDesc,
                                         pure, short, str, strOption, subparser,
                                         switch)

parseAddToPlaylist :: Parser Command
parseAddToPlaylist = AddToPlaylist
                     <$> (AddToPlaylistOptions
                         <$> argument str
                         (metavar "NAME"
                         <> help "the name of the playlist to add songs to"
                         )
                         <*> switch
                          ( long "all"
                          <> short 'a'
                          <> help "add all tracks from the current playlist"
                         )
                         <*> switch
                          ( long "create"
                          <> short 'c'
                          <> help "if the playlist doesn't exist, create it"))

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

parseEditPlaylist :: Parser Command
parseEditPlaylist = EditPlaylist
                    <$> (EditPlaylistOptions
                        <$> argument str
                        ( metavar "NAME"
                        <> help "the playlists name")
                        <*> optional (strOption
                        (   long "description"
                        <>  short 'd'
                        <>  help "the new description"
                        <> metavar "DESCRIPTION"
                        ))
                        )

subCommandParser :: Parser Command
subCommandParser = subparser
           (command "listplaylist" (info (withHelper parseList)
                                    (progDesc "print the playlist, JSON-style"))
           <> command "save" (info (withHelper parseSave) (progDesc "save the playlist"))
           <> command "load" (info (withHelper parseLoad) (progDesc "load a playlist"))
           <> command "list" (info (withHelper parseListPlaylists)
                              (progDesc "list all available playlists"))
           <> command "delete" (info (withHelper parseDeletePlaylist) (progDesc "delete a playlist"))
           <> command "add" (info (withHelper parseAddToPlaylist) (progDesc "add tracks to an existing playlist"))
           <> command "edit" (info (withHelper parseEditPlaylist) (progDesc "edit some information about a playlist"))
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
run Options {optCommand = cmd} = runCmd >>= either print putStrLn
  where runCmd = case cmd of
                   List -> list
                   ListPlaylists -> listPlaylists
                   Save options -> save options
                   Load options -> load options
                   DeletePlaylist options -> deletePlaylist options
                   AddToPlaylist options -> addToPlaylist options
                   EditPlaylist options -> edit options
