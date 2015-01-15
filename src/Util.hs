module Util (
    module Data.Char,
    replaceIf, replaceEq, replaceSeparators,
    chunksOfSize
    ) where

import Base
import Data.Char

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
