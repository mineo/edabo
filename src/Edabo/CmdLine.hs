module Edabo.CmdLine where

import           Control.Applicative    ((<$>), (<*>), many)
import           Data.Monoid            ((<>))
import           Edabo.CmdLine.Commands (addToPlaylist, deletePlaylist, edit,
                                         list, listPlaylists, load, path, save,
                                         upload)
import           Edabo.CmdLine.Types    (AddToPlaylistOptions (..),
                                         Command (..),
                                         DeletePlaylistOptions (..),
                                         EditPlaylistOptions (..),
                                         LoadOptions (..), Options (..),
                                         PathOptions (..), SaveOptions (..),
                                         UploadOptions (..))
import           Edabo.Utils            (exit, printResult)
import           Options.Applicative    (Parser, argument, command, execParser,
                                         fullDesc, header, help, helper, info,
                                         long, metavar, optional, progDesc,
                                         pure, short, str, strOption, subparser,
                                         switch)

parseAddToPlaylist :: Parser Command
parseAddToPlaylist = AddToPlaylist
                     <$> (AddToPlaylistOptions
                         <$> many (argument str
                                   (metavar "NAME"
                                    <> help "the name of the playlist to add songs to"
                                   )
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

parsePath :: Parser Command
parsePath = PlaylistPath
            <$> (PathOptions
                 <$> argument str
                 ( metavar "NAME"
                 <> help "the playlists name")
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

parseUpload :: Parser Command
parseUpload = Upload
              <$> (UploadOptions
                  <$> argument str
                  ( metavar "NAME"
                  <> help "the playlists name"))

subCommandParser :: Parser Command
subCommandParser = subparser
           (makeCommand "listplaylist" parseList "print the playlist, JSON-style"
           <> makeCommand "save" parseSave "save the playlist"
           <> makeCommand "load" parseLoad "load a playlist"
           <> makeCommand "list" parseListPlaylists "list all available playlists"
           <> makeCommand "delete" parseDeletePlaylist "delete a playlist"
           <> makeCommand "add" parseAddToPlaylist "add tracks to an existing playlist"
           <> makeCommand "edit" parseEditPlaylist "edit some information about a playlist"
           <> makeCommand "path" parsePath "show the full path to a playlist file"
           <> makeCommand "upload" parseUpload "upload a playlist"
           )
           where withHelper f = helper <*> f
                 makeCommand name f desc = command name (info (withHelper f)
                                                         (progDesc desc))

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
run Options {optCommand = cmd} = runCmd >>= quit
  where runCmd = case cmd of
                   List -> list
                   ListPlaylists -> listPlaylists
                   Save options -> save options
                   Load options -> load options
                   DeletePlaylist options -> deletePlaylist options
                   AddToPlaylist options -> addToPlaylist options
                   EditPlaylist options -> edit options
                   PlaylistPath options -> path options
                   Upload options -> upload options
        quit e = do
          printResult e
          exit e
