{-# LANGUAGE NoIncomplete #-}
{-# OPTIONS_GHC -fdefer-incomplete-uni-patterns #-}

module A where

-- should warn about non-exhaustive pattern match
w :: String -> String
w x = let (_:_) = x in "1"
