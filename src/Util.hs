module Util (
    module Data.Char,
    module System.Console.ANSI,
    replaceIf, replaceEq, replaceSeparators,
    chunksOfSize,
    putColoured, redError
    ) where

import Base
import Data.Char
import System.Console.ANSI
import System.IO

replaceIf :: (a -> Bool) -> a -> [a] -> [a]
replaceIf p to = map (\from -> if p from then to else from)

replaceEq :: Eq a => a -> a -> [a] -> [a]
replaceEq from = replaceIf (== from)

replaceSeparators :: Char -> String -> String
replaceSeparators = replaceIf isPathSeparator

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
putColoured :: ColorIntensity -> Color -> String -> Action ()
putColoured intensity colour msg = do
    liftIO $ setSGR [SetColor Foreground intensity colour]
    putNormal msg
    liftIO $ setSGR []
    liftIO $ hFlush stdout

-- A more colourful version of error
redError :: String -> Action a
redError msg = do
    putColoured Vivid Red msg
    return $ error $ "GHC build system error: " ++ msg
