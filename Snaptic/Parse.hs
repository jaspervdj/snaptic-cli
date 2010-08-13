-- | Parse JSON into snap data structures
--
module Snaptic.Parse
    ( parseSnapticConfiguration
    , parseNotes
    ) where

import Data.Maybe (mapMaybe)
import Control.Monad (liftM, ap)
import Data.Ord (comparing)
import Data.List (sortBy)

import Text.JSON

import Snaptic (SnapticConfiguration (..), Note (..))

-- | Parse the main snaptic configuration
--
parseSnapticConfiguration :: String -> Maybe SnapticConfiguration
parseSnapticConfiguration body = do
    result <- resultToMaybe (decode body)
    case result of
        JSObject object -> liftM SnapticConfiguration
            (getField "user_name" object)
                `ap` getField "password" object
        _ -> Nothing

-- | Parse a notes list
--
parseNotes :: String -> Maybe [Note]
parseNotes body = do
    result <- resultToMaybe (decode body)
    field <- case result of
        JSObject object -> getField "notes" object
        _               -> Nothing
    list <- case field of
        JSArray l -> return l
        _         -> Nothing
    return $ sortBy (comparing noteId) $ mapMaybe parseNote list
  where
    parseNote value = case value of
        JSObject object -> liftM Note (getField "text" object)
                `ap` getField "id" object
        _ -> Nothing

-- | Auxiliary function: turn a 'Result' into a 'Maybe'.
--
resultToMaybe :: Result a -> Maybe a
resultToMaybe (Ok x) = Just x
resultToMaybe (Error _) = Nothing

-- | Get a field from a JSON object into the 'Maybe' monad.
--
getField :: JSON a => String -> JSObject JSValue -> Maybe a
getField key = resultToMaybe . valFromObj key
