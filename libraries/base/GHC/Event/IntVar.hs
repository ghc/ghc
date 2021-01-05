{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns, CPP, GeneralizedNewtypeDeriving, MagicHash,
  NoImplicitPrelude, UnboxedTuples #-}

module GHC.Event.IntVar
    ( IntVar
    , newIntVar
    , readIntVar
    , writeIntVar
    ) where

import GHC.Base

data IntVar = IntVar (MutableByteArray# RealWorld)

newIntVar :: Int -> IO IntVar
newIntVar n = do
  iv <- IO $ \s ->
    case newByteArray# 1# s of
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
