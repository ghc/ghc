{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-overlapping-patterns #-}

module PMC002 where

f :: [a] -> Bool
f []              = True
f x  | (_:_) <- x = False -- exhaustive
