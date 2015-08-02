module Util (
    module Data.Char,
    module System.Console.ANSI,
    replaceIf, replaceEq, replaceSeparators,
    unifyPath, (-/-),
    chunksOfSize,
    putColoured, putOracle, putBuild, redError, redError_,
    bimap, minusOrd, intersectOrd
    ) where

import Base
import Data.Char
import Control.Monad
import System.IO
import System.Console.ANSI

replaceIf :: (a -> Bool) -> a -> [a] -> [a]
replaceIf p to = map (\from -> if p from then to else from)

replaceEq :: Eq a => a -> a -> [a] -> [a]
replaceEq from = replaceIf (== from)

replaceSeparators :: Char -> String -> String
replaceSeparators = replaceIf isPathSeparator

-- Normalise a path and convert all path separators to /, even on Windows.
unifyPath :: FilePath -> FilePath
unifyPath = toStandard . normaliseEx

-- Combine paths using </> and apply unifyPath to the result
(-/-) :: FilePath -> FilePath -> FilePath
a -/- b = unifyPath $ a </> b

infixr 6 -/-

-- (chunksOfSize size ss) splits a list of strings 'ss' into chunks not
-- exceeding the given 'size'.
chunksOfSize :: Int -> [String] -> [[String]]
chunksOfSize _    [] = []
chunksOfSize size ss = reverse chunk : chunksOfSize size rest
  where
    (chunk, rest) = go [] 0 ss
    go chunk _         []     = (chunk, [])
    go chunk chunkSize (s:ss) = let newSize = chunkSize + length s
                                    (newChunk, rest) = go (s:chunk) newSize ss
                                in
                                if newSize > size
                                then (chunk   , s:ss)
                                else (newChunk, rest)

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

-- A more colourful version of error
redError :: String -> Action a
redError msg = do
    putColoured Red msg
    error $ "GHC build system error: " ++ msg

redError_ :: String -> Action ()
redError_ = void . redError

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
