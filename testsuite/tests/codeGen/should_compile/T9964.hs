{-# LANGUAGE UnboxedTuples #-}
module T9964 where

import GHC.Base

crash :: IO ()
crash = IO (\s ->
  let
    {-# NOINLINE s' #-}
    s' = s
  in (# s', () #))
