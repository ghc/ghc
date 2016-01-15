{-# OPTIONS_GHC -fno-warn-dodgy-imports #-} -- for Development.Shake.parallel

module Base (
    -- * General utilities
    module Control.Applicative,
    module Control.Monad.Extra,
    module Data.Function,
    module Data.List.Extra,
    module Data.Maybe,
    module Data.Monoid,
    MonadTrans(lift),

    -- * Shake
    module Development.Shake,
    module Development.Shake.Classes,
    module Development.Shake.FilePath,

    -- * Paths
    shakeFilesPath, configPath, sourcePath, programInplacePath,
    bootPackageConstraints, packageDependencies,

    -- * Output
    putColoured, putOracle, putBuild, putSuccess, putError,

    -- * Miscellaneous utilities
    bimap, minusOrd, intersectOrd, replaceEq, quote, replaceSeparators,
    decodeModule, encodeModule, unifyPath, (-/-), versionToInt,
    removeFileIfExists, removeDirectoryIfExists
    ) where

import Control.Applicative
import Control.Monad.Extra
import Control.Monad.Reader
import Data.Function
import Data.List.Extra
import Data.Maybe
import Data.Monoid
import Development.Shake hiding (parallel, unit, (*>), Normal)
import Development.Shake.Classes
import Development.Shake.FilePath
import System.Console.ANSI
import qualified System.Directory as IO
import System.IO

-- TODO: reexport Stage, etc.?

-- Build system files and paths
shakePath :: FilePath
shakePath = "shake-build"

shakeFilesPath :: FilePath
shakeFilesPath = shakePath -/- ".db"

configPath :: FilePath
configPath = shakePath -/- "cfg"

-- | Path to source files of the build system, e.g. this file is located at
-- sourcePath -/- "Base.hs". We use this to `need` some of the source files.
sourcePath :: FilePath
sourcePath = shakePath -/- "src"

-- TODO: move to buildRootPath, see #113
programInplacePath :: FilePath
programInplacePath = "inplace/bin"

bootPackageConstraints :: FilePath
bootPackageConstraints = shakeFilesPath -/- "boot-package-constraints"

packageDependencies :: FilePath
packageDependencies = shakeFilesPath -/- "package-dependencies"

-- Utility functions
-- | Find and replace all occurrences of a value in a list
replaceEq :: Eq a => a -> a -> [a] -> [a]
replaceEq from = replaceWhen (== from)

-- | Find and replace all occurrences of path separators in a String with a Char
replaceSeparators :: Char -> String -> String
replaceSeparators = replaceWhen isPathSeparator

replaceWhen :: (a -> Bool) -> a -> [a] -> [a]
replaceWhen p to = map (\from -> if p from then to else from)

-- | Add quotes to a String
quote :: String -> String
quote s = "\"" ++ s ++ "\""

-- | Given a version string such as "2.16.2" produce an integer equivalent
versionToInt :: String -> Int
versionToInt s = major * 1000 + minor * 10 + patch
  where
    [major, minor, patch] = map read . words $ replaceEq '.' ' ' s

-- | Given a module name extract the directory and file name, e.g.:
--
-- > decodeModule "Data.Functor.Identity" = ("Data/Functor/", "Identity")
decodeModule :: String -> (FilePath, String)
decodeModule = splitFileName . replaceEq '.' '/'

-- | Given the directory and file name find the corresponding module name, e.g.:
--
-- > encodeModule "Data/Functor/" "Identity.hs" = "Data.Functor.Identity"
encodeModule :: FilePath -> String -> String
encodeModule dir file = replaceEq '/' '.' $ dir -/- takeBaseName file

-- | Normalise a path and convert all path separators to @/@, even on Windows.
unifyPath :: FilePath -> FilePath
unifyPath = toStandard . normaliseEx

-- | Combine paths using '</>' and apply 'unifyPath' to the result
(-/-) :: FilePath -> FilePath -> FilePath
a -/- b = unifyPath $ a </> b

infixr 6 -/-

-- | A more colourful version of Shake's putNormal
putColoured :: Color -> String -> Action ()
putColoured colour msg = do
    liftIO $ setSGR [SetColor Foreground Vivid colour]
    putNormal msg
    liftIO $ setSGR []
    liftIO $ hFlush stdout

-- | Make oracle output more distinguishable
putOracle :: String -> Action ()
putOracle = putColoured Blue

-- | Make build output more distinguishable
putBuild :: String -> Action ()
putBuild = putColoured White

-- | A more colourful version of success message
putSuccess :: String -> Action ()
putSuccess = putColoured Green

-- | A more colourful version of error message
putError :: String -> Action a
putError msg = do
    putColoured Red msg
    error $ "GHC build system error: " ++ msg

-- Explicit definition to avoid dependency on Data.Bifunctor
-- | Bifunctor bimap.
bimap :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
bimap f g (x, y) = (f x, g y)

-- Explicit definition to avoid dependency on Data.List.Ordered
-- | Difference of two ordered lists.
minusOrd :: Ord a => [a] -> [a] -> [a]
minusOrd [] _  = []
minusOrd xs [] = xs
minusOrd (x:xs) (y:ys) = case compare x y of
    LT -> x : minusOrd xs (y:ys)
    EQ ->     minusOrd xs ys
    GT ->     minusOrd (x:xs) ys

-- Explicit definition to avoid dependency on Data.List.Ordered
-- | Intersection of two ordered lists by a predicate.
intersectOrd :: (a -> b -> Ordering) -> [a] -> [b] -> [a]
intersectOrd cmp = loop
  where
    loop [] _ = []
    loop _ [] = []
    loop (x:xs) (y:ys) = case cmp x y of
         LT ->     loop xs (y:ys)
         EQ -> x : loop xs ys
         GT ->     loop (x:xs) ys

-- | Remove a file that doesn't necessarily exist
removeFileIfExists :: FilePath -> Action ()
removeFileIfExists f = liftIO . whenM (IO.doesFileExist f) $ IO.removeFile f

-- | Remove a directory that doesn't necessarily exist
removeDirectoryIfExists :: FilePath -> Action ()
removeDirectoryIfExists d =
    liftIO . whenM (IO.doesDirectoryExist d) $ IO.removeDirectoryRecursive d
