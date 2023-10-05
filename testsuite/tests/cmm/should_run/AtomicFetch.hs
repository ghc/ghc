{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- This is not a test of atomic semantics,
-- just checking that GHC can parse %fetch_fooXX

import GHC.Exts
import GHC.Int
import GHC.ST

foreign import prim "cmm_foo8" cmm_foo8
  :: MutableByteArray# s -> State# s -> (# State# s, Int8# #)

foreign import prim "cmm_foo16" cmm_foo16
  :: MutableByteArray# s -> State# s -> (# State# s, Int16# #)

foreign import prim "cmm_foo32" cmm_foo32
  :: MutableByteArray# s -> State# s -> (# State# s, Int32# #)

foreign import prim "cmm_foo64" cmm_foo64
  :: MutableByteArray# s -> State# s -> (# State# s, Int64# #)

go8 :: Int8
go8 = runST $ ST $ \s0 ->
  case newByteArray# 8# s0 of
    (# s1, mba #) -> case cmm_foo8 mba s1 of
        (# s2, n' #) -> (# s2, I8# n' #)

go16 :: Int16
go16 = runST $ ST $ \s0 ->
  case newByteArray# 8# s0 of
    (# s1, mba #) -> case cmm_foo16 mba s1 of
        (# s2, n' #) -> (# s2, I16# n' #)

go32 :: Int32
go32 = runST $ ST $ \s0 ->
  case newByteArray# 8# s0 of
    (# s1, mba #) -> case cmm_foo32 mba s1 of
        (# s2, n' #) -> (# s2, I32# n' #)

go64 :: Int64
go64 = runST $ ST $ \s0 ->
  case newByteArray# 8# s0 of
    (# s1, mba #) -> case cmm_foo64 mba s1 of
        (# s2, n' #) -> (# s2, I64# n' #)

main = do
  print go8
  print go16
  print go32
  print go64
