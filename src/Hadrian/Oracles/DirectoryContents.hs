{-# LANGUAGE TypeFamilies #-}
module Hadrian.Oracles.DirectoryContents (
    directoryContents, copyDirectoryContents, directoryContentsOracle, copyDirectoryContentsUntracked,
    Match (..), matches, matchAll
    ) where

import Control.Monad
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import GHC.Generics

import Hadrian.Utilities

import qualified System.Directory.Extra as IO

data Match = Test FilePattern | Not Match | And [Match] | Or [Match]
    deriving (Generic, Eq, Show, Typeable)

instance Binary   Match
instance Hashable Match
instance NFData   Match

-- | A 'Match' expression that always evaluates to 'True' (i.e. always matches).
matchAll :: Match
matchAll = And []

-- | Check if a file name matches a given 'Match' expression.
matches :: Match -> FilePath -> Bool
matches (Test p) f = p ?== f
matches (Not  m) f = not $ matches m f
matches (And ms) f = all (`matches` f) ms
matches (Or  ms) f = any (`matches` f) ms

-- | Given a 'Match' expression and a directory, recursively traverse it and all
-- its subdirectories to find and return all matching contents.
directoryContents :: Match -> FilePath -> Action [FilePath]
directoryContents expr dir = askOracle $ DirectoryContents (expr, dir)

-- | Copy the contents of the source directory that matches a given 'Match'
-- expression into the target directory. The copied contents is tracked.
copyDirectoryContents :: Match -> FilePath -> FilePath -> Action ()
copyDirectoryContents expr source target = do
    putProgressInfo =<< renderAction "Copy directory contents" source target
    let cp file = copyFile file $ target -/- makeRelative source file
    mapM_ cp =<< directoryContents expr source

-- | Copy the contents of the source directory that matches a given 'Match'
-- expression into the target directory. The copied contents is untracked.
copyDirectoryContentsUntracked :: Match -> FilePath -> FilePath -> Action ()
copyDirectoryContentsUntracked expr source target = do
    putProgressInfo =<< renderAction "Copy directory contents (untracked)" source target
    let cp file = copyFileUntracked file $ target -/- makeRelative source file
    mapM_ cp =<< directoryContents expr source

newtype DirectoryContents = DirectoryContents (Match, FilePath)
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
type instance RuleResult DirectoryContents = [FilePath]

-- | This oracle answers 'directoryContents' queries and tracks the results.
directoryContentsOracle :: Rules ()
directoryContentsOracle = void $
    addOracle $ \(DirectoryContents (expr, dir)) -> liftIO $ map unifyPath .
        filter (matches expr) <$> IO.listFilesInside (return . matches expr) dir
