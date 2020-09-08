{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import GHC.Classes

instance IP "x" Int where
  ip = 21

baz :: (?x :: Int) => Int
baz = ?x

main :: IO ()
main  = let ?x = 42
        in print baz
