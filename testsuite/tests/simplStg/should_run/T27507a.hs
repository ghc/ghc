{-# LANGUAGE BangPatterns #-}
-- Ensure the Box is returned boxed rather than unwrapped by CPR.
-- Ensure fun_exit is not floated to top, but works either way.
{-# OPTIONS_GHC -fno-cpr-anal -fno-full-laziness #-}

module Main where

import GHC.Exts (noinline)
import GHC.Exts.Heap (getClosureData, info, tipe)

data Box a = Box !a

foo :: Bool -> Int -> (Int, Box Int)
foo b !x = case x of
  x' -> let fun_exit x_f i = (i, Box x_f)
            {-# NOINLINE fun_exit #-}
        in if b then fun_exit x' 0 else fun_exit x' 1
{-# NOINLINE foo #-}

main :: IO ()
main =
  case foo True 42 of
    (_, bx) -> do
      c <- noinline getClosureData bx
      noinline print (tipe (info c))
