-- | Main module
--
module Main
    ( main
    ) where

import Data.Maybe (fromMaybe)
import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Applicative ((<$>))
import System.Directory (getHomeDirectory, doesFileExist)
import System.FilePath ((</>))
import System.Environment (getArgs, getProgName)
import Text.Printf

import Snaptic
import Snaptic.Parse
import Snaptic.Http

-- | Main function
--
main :: IO ()
main = do
    homeDirectory <- getHomeDirectory
    let configFile = homeDirectory </> ".snaptic-cli.json"

    exists <- doesFileExist configFile
    if exists
        then do
            config <- parseSnapticConfiguration <$> readFile configFile
            case config of
                Nothing -> putStrLn $ "Could not parse " ++ configFile
                Just c -> runReaderT commands c

        -- Write a sample configuration file.
        else do
            writeFile configFile $ unlines
                [ "{"
                , "    \"user_name\": \"Your username here\","
                , "    \"password\": \"Your password here\""
                , "}"
                ]
            putStrLn $ "Config file created: " ++ configFile
            putStrLn "Please edit it and set your credentials."

-- | Spawns the different commands.
--
commands :: Snaptic ()
commands = do
    args <- liftIO getArgs
    progName <- liftIO getProgName
    case args of
        [] -> liftIO $ putStrLn $ "Usage: " ++ progName ++ " command"
        (c : xs) ->
            let command = fromMaybe help $ lookup c
                    [ ("help", help)
                    , ("list", list)
                    , ("add", add)
                    , ("delete", delete)
                    , ("get", get)
                    ]
            in command xs

-- | Put some help.
--
help :: [String] -> Snaptic ()
help _ = liftIO $ do
    progName <- getProgName
    putStr $ unlines
        [ "USAGE"
        , ""
        , "  " ++ progName ++ " command"
        , ""
        , "COMMANDS"
        , ""
        , "  help          Display this help message"
        , "  list          List all notes"
        , "  add <text>    Add a note"
        , "  delete <id>   Delete a note"
        , "  get <id>      Show a single note"
        , ""
        ]

-- | List all notes
--
list :: [String] -> Snaptic ()
list _ = withNotes $ \n -> forM_ (zip n [1 ..]) showNote
  where
    showNote :: (Note, Int) -> Snaptic ()
    showNote (note, i) =
        liftIO $ putStr $ printf "%3d. " i ++ indentTail (noteText note)
    indentTail t = let l = lines t
                   in unlines $ take 1 l ++ map ("     " ++) (drop 1 l)

-- | Add a note
--
add :: [String] -> Snaptic ()
add xs = addNote $ unwords xs

-- | Delete a note
--
delete :: [String] -> Snaptic ()
delete [id'] = deleteNote (read id')
delete _ = liftIO $ putStrLn "Give one number to delete a note"

-- | Get (show) a note
--
get :: [String] -> Snaptic ()
get [id'] = flip withNote (read id') $ liftIO . putStrLn . noteText
get _ = liftIO $ putStrLn "Give one number to get a note"
