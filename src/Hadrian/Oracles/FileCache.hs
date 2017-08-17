{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Oracles.FileCache
-- Copyright  : (c) Andrey Mokhov 2014-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Build and read text file caches. File caches can be used to cache expensive
-- computations whose results are not expected to change between builds. One
-- example is parsing package @.cabal@ files to determine all inter-package
-- dependencies. Use "Hadrian.Oracles.KeyValue" to read and track individual
-- lines in text file caches.
-----------------------------------------------------------------------------
module Hadrian.Oracles.FileCache (readFileCache, fileCacheRules) where

import Control.Monad
import Development.Shake
import Development.Shake.Classes

import Hadrian.Utilities

newtype FileCache = FileCache FilePath
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
type instance RuleResult FileCache = String

-- | Read a text file, caching and tracking the result. To read and track
-- individual lines of the file, see "Hadrian.Oracles.KeyValue".
readFileCache :: FilePath -> Action String
readFileCache = askOracle . FileCache

-- | This oracle builds text files using supplied generators and caches access
-- to them to efficiently answer 'readFileCache' queries, tracking the results.
-- The argument is a list of pairs @(pattern, generator)@, where @pattern@
-- describes the files that can be built using the corresponding @generator@
-- action. The latter takes a specific file path to be generated as the input.
fileCacheRules :: [(FilePattern, FilePath -> Action String)] -> Rules ()
fileCacheRules patternGenerators = do
    -- Generate file contents
    forM_ patternGenerators $ \(pattern, generate) ->
        pattern %> \file -> do
            contents <- generate file
            writeFileChanged file contents
            putSuccess $ "| Successfully generated " ++ file
    -- Cache file reading
    cache <- newCache $ \file -> do
        need [file]
        putLoud $ "Reading " ++ file ++ "..."
        liftIO $ readFile file
    void $ addOracle $ \(FileCache file) -> cache file
