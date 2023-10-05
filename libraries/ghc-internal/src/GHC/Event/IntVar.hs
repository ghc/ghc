{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP, MagicHash, NoImplicitPrelude, UnboxedTuples #-}

module GHC.Event.IntVar
    ( IntVar
    , newIntVar
    , readIntVar
    , writeIntVar
    ) where

import GHC.Base
import GHC.Bits

data IntVar = IntVar (MutableByteArray# RealWorld)

newIntVar :: Int -> IO IntVar
newIntVar n = do
  let !(I# size) = finiteBitSize (0 :: Int) `unsafeShiftR` 3
  iv <- IO $ \s ->
    case newByteArray# size s of
      (# s', mba #) -> (# s', IntVar mba #)
  writeIntVar iv n
  return iv

readIntVar :: IntVar -> IO Int
readIntVar (IntVar mba) = IO $ \s ->
  case readIntArray# mba 0# s of
    (# s', n #) -> (# s', I# n #)

writeIntVar :: IntVar -> Int -> IO ()
writeIntVar (IntVar mba) (I# n) = IO $ \s ->
  case writeIntArray# mba 0# n s of
    s' -> (# s', () #)
