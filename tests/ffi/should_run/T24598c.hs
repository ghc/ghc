{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE MagicHash #-}

-- | Test that `foreign import prim` imports handle `State#` in arguments correctly.
module Main where

import GHC.IO
import GHC.Exts

foreign import prim "hello"
  hello# :: State# RealWorld -> State# RealWorld

main :: IO ()
main = hello

hello :: IO ()
hello = IO $ \s ->
  case hello# s of s' -> (# s', () #)

