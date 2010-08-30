-- | Get sample data through HTTP
--
module Snaptic.Http
    ( withNotes
    , withNote
    , addNote
    , deleteNote
    ) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ask)
import Text.Printf
import System.Cmd (system)

import Network.Curl

import Snaptic
import Snaptic.Parse

-- | Perform a curl action
--
withCurl :: String -> [CurlOption] -> Snaptic String
withCurl uri opts = do
    config <- ask
    let userPwd = snapticUserName config ++ ":" ++ snapticPassword config
    (_, body) <- liftIO $ curlGetString uri (CurlUserPwd userPwd : opts)
    return body

-- | Perform a curl and parse action
--
withParse :: String               -- ^ URL to get
          -> (String -> Maybe a)  -- ^ Parse function
          -> (a -> Snaptic ())    -- ^ Action to take
          -> Snaptic ()
withParse uri parse action = do
    body <- withCurl uri []
    case parse body of
        Nothing -> liftIO $ putStrLn "Could not parse."
        Just x  -> action x

-- | Get all notes
--
withNotes :: ([Note] -> Snaptic ()) -> Snaptic ()
withNotes = withParse "https://api.snaptic.com/v1/notes" parseNotes

-- | Perform an action on one note, specified by 1-based index
--
withNote :: (Note -> Snaptic ()) -> Int -> Snaptic ()
withNote f i = withNotes $ \notes -> case drop (i - 1) notes of
    (note : _) -> f note
    _ -> liftIO $ putStrLn $ "Note " ++ show i ++ " was not found."

-- | Add a note
--
addNote :: String -> Snaptic ()
addNote text = do
    _ <- withCurl "https://api.snaptic.com/v1/notes"
                  [CurlPost True, CurlPostFields ["text=" ++ text]]
    return ()

-- | Delete a note
--
deleteNote :: Int -> Snaptic ()
deleteNote = withNote $ \note -> do
    config <- ask 
    _ <- liftIO $ system $ printf cmdTemplate (snapticUserName config)
                                              (snapticPassword config)
                                              (noteId note)
    return ()
  where
    -- We use the command line curl utility here because haskell-curl
    -- does not support DELETE for some reason.
    cmdTemplate = "curl -X DELETE -u %s:%s \
                  \https://api.snaptic.com/v1/notes/%d >/dev/null 2>&1"
