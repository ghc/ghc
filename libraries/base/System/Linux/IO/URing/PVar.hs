{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module System.Linux.IO.URing.PVar
  ( PVar
  , newPVar
  , Prim(..)
  ) where

-- import GHC.Exts
import GHC.Base
import GHC.IO ()
import GHC.Word

data PVar a = PVar (MutableByteArray# RealWorld)

class Prim a where
  sizeOf :: a -> Int
  readPVar :: PVar a -> IO a
  writePVar :: PVar a -> a -> IO ()

instance Prim Word32 where
  sizeOf _ = 4
  readPVar (PVar mba) = IO $ \s ->
    case readWord32Array# mba 0# s of (# s', r #) -> (# s', W32# (narrowWord32# r) #)
  writePVar (PVar mba) (W32# x) = IO $ \s ->
    case writeWord32Array# mba 0# (extendWord32# x) s of s' -> (# s', () #)

newPVar :: forall a. Prim a => a -> IO (PVar a)
newPVar x = do
    pvar <- create
    writePVar pvar x
    return pvar
  where
    create =
      IO $ \s -> case newByteArray# size s of
        (# s', mba #) -> (# s', PVar mba #)
    !(I# size) = sizeOf (undefined :: a)

