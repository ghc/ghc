{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE MagicHash #-}

-- | Test that `foreign import prim` imports handle `State#` in arguments correctly.
module Main where

import GHC.IO
import GHC.Int
import GHC.Exts

foreign import prim "hello"
  hello# :: Int# -> State# RealWorld -> (# State# RealWorld, Int# #)

main :: IO ()
main = hello 21 >>= print

hello :: Int -> IO Int
hello (I# n#) = IO $ \s ->
  case hello# n# s of (# s', n# #) -> (# s', I# n# #)

