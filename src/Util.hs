module Util (
    module Data.Char,
    module System.Console.ANSI,
    replaceIf, replaceEq, replaceSeparators,
    unifyPath,
    chunksOfSize,
    putColoured, redError, redError_
    ) where

import Base
import Data.Char
import System.Console.ANSI
import System.IO
import Control.Monad

replaceIf :: (a -> Bool) -> a -> [a] -> [a]
replaceIf p to = map (\from -> if p from then to else from)

replaceEq :: Eq a => a -> a -> [a] -> [a]
replaceEq from = replaceIf (== from)

replaceSeparators :: Char -> String -> String
replaceSeparators = replaceIf isPathSeparator

unifyPath :: FilePath -> FilePath
unifyPath = toStandard . normaliseEx

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

-- A more colourful version of error
redError :: String -> Action a
redError msg = do
    putColoured Red msg
    error $ "GHC build system error: " ++ msg

redError_ :: String -> Action ()
redError_ = void . redError
