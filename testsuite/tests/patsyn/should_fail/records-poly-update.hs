{-# LANGUAGE PatternSynonyms #-}
module Main where

pattern ReqNoProv :: Show a => a -> Maybe a
pattern ReqNoProv{j} = Just j

data A = A deriving Show

p1 = Just True

p6 = p1 {j = A}

main = print p6
