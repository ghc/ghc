{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}
module Oracles.DirectoryContent (
    getDirectoryContent, directoryContentOracle, Match(..)
    ) where

import Base
import GHC.Generics
import System.Directory.Extra

newtype DirectoryContent = DirectoryContent (Match, FilePath)
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)

data Match = Test FilePattern | Not (Match) | And [Match] | Or [Match]
    deriving (Generic, Eq, Show, Typeable)
instance Binary Match
instance Hashable Match
instance NFData Match

matches :: Match -> FilePath -> Bool
matches (Test m) f = m ?== f
matches (Not m) f = not $ matches m f
matches (And [])     _               = True
matches (And (m:ms)) f | matches m f = matches (And ms) f
                       | otherwise   = False
matches (Or [])     _               = False
matches (Or (m:ms)) f | matches m f = True
                      | otherwise   = matches (Or ms) f

-- | Get the directory content recursively.
getDirectoryContent :: Match -> FilePath -> Action [FilePath]
getDirectoryContent expr dir =
    askOracle $ DirectoryContent (expr, dir)

directoryContentOracle :: Rules ()
directoryContentOracle = void $ addOracle oracle
  where
    oracle :: DirectoryContent -> Action [FilePath]
    oracle (DirectoryContent (expr, dir)) =
        liftIO $ filter (matches expr) <$> listFilesInside (return . matches expr) dir
