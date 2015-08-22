module Base (
    module Control.Applicative,
    module Control.Monad.Extra,
    module Data.Char,
    module Data.Function,
    module Data.List,
    module Data.Maybe,
    module Data.Monoid,
    module Development.Shake,
    module Development.Shake.Classes,
    module Development.Shake.Config,
    module Development.Shake.FilePath,
    module Development.Shake.Util,
    module System.Console.ANSI,
    shakeFilesPath, configPath, bootPackageConstraints, packageDependencies,
    replaceEq, replaceSeparators, decodeModule,
    unifyPath, (-/-), chunksOfSize,
    putColoured, putOracle, putBuild, putSuccess, putError,
    bimap, minusOrd, intersectOrd,
    removeFileIfExists
    ) where

import Control.Applicative
import Control.Monad.Extra
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid
import Development.Shake hiding (unit, (*>))
import Development.Shake.Classes
import Development.Shake.Config
import Development.Shake.FilePath
import Development.Shake.Util
import System.Console.ANSI
import qualified System.Directory as IO
import System.IO

-- Build system files and paths
shakeFilesPath :: FilePath
shakeFilesPath = "_build/"

configPath :: FilePath
configPath = "shake/cfg/"

bootPackageConstraints :: FilePath
bootPackageConstraints = shakeFilesPath ++ "boot-package-constraints"

packageDependencies :: FilePath
packageDependencies = shakeFilesPath ++ "package-dependencies"

-- Utility functions
-- Find and replace all occurrences of a value in a list
replaceEq :: Eq a => a -> a -> [a] -> [a]
replaceEq from = replaceIf (== from)

-- Find and replace all occurrences of path separators in a String with a Char
replaceSeparators :: Char -> String -> String
replaceSeparators = replaceIf isPathSeparator

replaceIf :: (a -> Bool) -> a -> [a] -> [a]
replaceIf p to = map (\from -> if p from then to else from)

-- Given a module name extract the directory and file names, e.g.:
-- decodeModule "Data.Functor.Identity" = ("Data/Functor/", "Identity")
decodeModule :: String -> (FilePath, String)
decodeModule = splitFileName . replaceEq '.' '/'

-- Normalise a path and convert all path separators to /, even on Windows.
unifyPath :: FilePath -> FilePath
unifyPath = toStandard . normaliseEx

-- Combine paths using </> and apply unifyPath to the result
(-/-) :: FilePath -> FilePath -> FilePath
a -/- b = unifyPath $ a </> b

infixr 6 -/-

-- (chunksOfSize size strings) splits a given list of strings into chunks not
-- exceeding the given 'size'.
chunksOfSize :: Int -> [String] -> [[String]]
chunksOfSize _    [] = []
chunksOfSize size strings = reverse chunk : chunksOfSize size rest
  where
    (chunk, rest) = go [] 0 strings
    go res _         []     = (res, [])
    go res chunkSize (s:ss) =
        if newSize > size then (res, s:ss) else go (s:res) newSize ss
      where
        newSize = chunkSize + length s

-- A more colourful version of Shake's putNormal
putColoured :: Color -> String -> Action ()
putColoured colour msg = do
    liftIO $ setSGR [SetColor Foreground Vivid colour]
    putNormal msg
    liftIO $ setSGR []
    liftIO $ hFlush stdout

-- Make oracle output more distinguishable
putOracle :: String -> Action ()
putOracle = putColoured Blue

-- Make build output more distinguishable
putBuild :: String -> Action ()
putBuild = putColoured White

-- A more colourful version of success message
putSuccess :: String -> Action ()
putSuccess = putColoured Green

-- A more colourful version of error message
putError :: String -> Action a
putError msg = do
    putColoured Red msg
    error $ "GHC build system error: " ++ msg

-- Depending on Data.Bifunctor only for this function seems an overkill
bimap :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
bimap f g (x, y) = (f x, g y)

-- Depending on Data.List.Ordered only for these two functions seems an overkill
minusOrd :: Ord a => [a] -> [a] -> [a]
minusOrd [] _  = []
minusOrd xs [] = xs
minusOrd (x:xs) (y:ys) = case compare x y of
    LT -> x : minusOrd xs (y:ys)
    EQ ->     minusOrd xs ys
    GT ->     minusOrd (x:xs) ys

intersectOrd :: (a -> b -> Ordering) -> [a] -> [b] -> [a]
intersectOrd cmp = loop
  where
    loop [] _ = []
    loop _ [] = []
    loop (x:xs) (y:ys) = case cmp x y of
         LT ->     loop xs (y:ys)
         EQ -> x : loop xs ys
         GT ->     loop (x:xs) ys

-- Convenient helper function for removing a file that doesn't necessarily exist
removeFileIfExists :: FilePath -> Action ()
removeFileIfExists f = liftIO . whenM (IO.doesFileExist f) $ IO.removeFile f
