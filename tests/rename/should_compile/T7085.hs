{-# OPTIONS_GHC -fwarn-unused-binds #-}

module ShouldFail where

foo :: Int -> Int
foo n = n + 1
   where
     Nothing = Just n
