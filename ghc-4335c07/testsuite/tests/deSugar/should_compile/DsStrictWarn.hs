{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# LANGUAGE Strict #-}
module DsStrictWarn where

-- should warn about non-exhaustive pattern match
w :: String -> String
w x = let (_:_) = x in "1"
