{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE MagicHash #-}

-- | Test that `foreign import prim` imports handle `State#` in arguments correctly.
module Main where

-- import GHC.IO
import GHC.Exts
import GHC.Int

foreign import prim "a_number_cmm"
  cmm_number :: (# #) -> Int#

foreign import ccall "a_number_c"
  c_number :: (# #) -> Int64#

main :: IO ()
main = do
  print $ I# (cmm_number (# #))
  print $ I64# (c_number (# #))
