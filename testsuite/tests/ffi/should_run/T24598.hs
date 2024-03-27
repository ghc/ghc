{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE MagicHash #-}

-- | Test that `foreign import prim` imports handle `State#` in results correctly.
module Main where

import GHC.IO
import GHC.Int
import GHC.Exts

foreign import prim "hello"
  hello# :: State# RealWorld -> (# State# RealWorld, Int# #)

main :: IO ()
main = hello >>= print

hello :: IO Int
hello = IO $ \s -> case hello# s of (# s', n# #) -> (# s', I# n# #)
