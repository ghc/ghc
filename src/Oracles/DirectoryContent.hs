{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}
module Oracles.DirectoryContent (
    directoryContent, directoryContentOracle, Match (..)
    ) where

import System.Directory.Extra
import GHC.Generics

import Base

newtype DirectoryContent = DirectoryContent (Match, FilePath)
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)

data Match = Test FilePattern | Not Match | And [Match] | Or [Match]
    deriving (Generic, Eq, Show, Typeable)

matches :: Match -> FilePath -> Bool
matches (Test p) f = p ?== f
matches (Not  m) f = not $ matches m f
matches (And ms) f = all (`matches` f) ms
matches (Or  ms) f = any (`matches` f) ms

-- | Get the directory content recursively.
directoryContent :: Match -> FilePath -> Action [FilePath]
directoryContent expr dir = askOracle $ DirectoryContent (expr, dir)

directoryContentOracle :: Rules ()
directoryContentOracle = void $
    addOracle $ \(DirectoryContent (expr, dir)) -> liftIO $
        filter (matches expr) <$> listFilesInside (return . matches expr) dir

instance Binary Match
instance Hashable Match
instance NFData Match
