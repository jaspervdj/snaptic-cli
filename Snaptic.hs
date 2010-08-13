-- | Snaptic data structures
--
module Snaptic
    ( Snaptic
    , SnapticConfiguration (..)
    , Note (..)
    ) where

import Control.Monad.Reader (ReaderT)

-- | Monad stack
--
type Snaptic = ReaderT SnapticConfiguration IO

-- | Global configuration
--
data SnapticConfiguration = SnapticConfiguration
    { snapticUserName :: String
    , snapticPassword :: String
    } deriving (Show, Eq, Ord)

-- | A note.
--
data Note = Note
    { noteText :: String
    , noteId   :: Int
    } deriving (Show, Eq, Ord)
