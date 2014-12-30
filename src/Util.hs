module Util (
    module Data.Char,
    isSlash,
    replaceIf, replaceEq
    ) where

import Data.Char

isSlash :: Char -> Bool
isSlash = (`elem` ['/', '\\']) 

replaceIf :: (a -> Bool) -> a -> [a] -> [a]
replaceIf p to = map (\from -> if p from then to else from) 

replaceEq :: Eq a => a -> a -> [a] -> [a]
replaceEq from = replaceIf (== from)
