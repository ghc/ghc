module Util (
    module Data.Char,
    replaceIf, replaceEq, replaceSeparators
    ) where

import Base
import Data.Char

replaceIf :: (a -> Bool) -> a -> [a] -> [a]
replaceIf p to = map (\from -> if p from then to else from) 

replaceEq :: Eq a => a -> a -> [a] -> [a]
replaceEq from = replaceIf (== from)

replaceSeparators :: Char -> String -> String
replaceSeparators = replaceIf isPathSeparator

