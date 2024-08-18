module T24625 where

import GHC.IO.Exception
import GHC.Exts

data Foo = Foo !Int !Int String

true :: Bool
true = True
{-# NOINLINE true #-}

function :: Int -> Int -> String -> Int
function !a !b c = case assertError true (Foo a b c) of
  Foo a b c -> a + b
