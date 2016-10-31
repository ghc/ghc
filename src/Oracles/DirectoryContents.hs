{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}
module Oracles.DirectoryContents (
    directoryContents, directoryContentsOracle, Match (..)
    ) where

import System.Directory.Extra
import GHC.Generics

import Base

newtype DirectoryContents = DirectoryContents (Match, FilePath)
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)

data Match = Test FilePattern | Not Match | And [Match] | Or [Match]
    deriving (Generic, Eq, Show, Typeable)

matches :: Match -> FilePath -> Bool
matches (Test p) f = p ?== f
matches (Not  m) f = not $ matches m f
matches (And ms) f = all (`matches` f) ms
matches (Or  ms) f = any (`matches` f) ms

-- | Given a 'Match' expression and a directory, recursively traverse it and all
-- its subdirectories to find and return all matching contents.
directoryContents :: Match -> FilePath -> Action [FilePath]
directoryContents expr dir = askOracle $ DirectoryContents (expr, dir)

directoryContentsOracle :: Rules ()
directoryContentsOracle = void $
    addOracle $ \(DirectoryContents (expr, dir)) -> liftIO $ map unifyPath .
        filter (matches expr) <$> listFilesInside (return . matches expr) dir

instance Binary Match
instance Hashable Match
instance NFData Match
