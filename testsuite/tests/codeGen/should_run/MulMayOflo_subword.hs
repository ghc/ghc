{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Main (main) where

import GHC.Exts
import GHC.Word

foreign import prim "runMulMayOfloSubword8zh"
  runMulMayOfloSubword8# :: Word# -> Word# -> Word#
foreign import prim "runMulMayOfloSubword16zh"
  runMulMayOfloSubword16# :: Word# -> Word# -> Word#
foreign import prim "runMulMayOfloSubword32zh"
  runMulMayOfloSubword32# :: Word# -> Word# -> Word#

main :: IO ()
main = do
  -- The low 8/16/32 bits are 0xc8/0xff38/0xffffffc8.  A live local that has
  -- been sign-extended in place by %mulmayoflo would come back with all upper
  -- bits set instead of zero-extended.
  putStrLn ("W8: " ++ show (W# (runMulMayOfloSubword8# 0xc8## 1##)))
  putStrLn ("W16: " ++ show (W# (runMulMayOfloSubword16# 0xff38## 1##)))
  putStrLn ("W32: " ++ show (W# (runMulMayOfloSubword32# 0xffffffc8## 1##)))
