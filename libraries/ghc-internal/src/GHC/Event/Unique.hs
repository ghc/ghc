{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns, CPP, GeneralizedNewtypeDeriving, MagicHash,
  NoImplicitPrelude, UnboxedTuples #-}

module GHC.Event.Unique
    (
      UniqueSource
    , Unique(..)
    , newSource
    , newUnique
    ) where

import GHC.Base
import GHC.Num(Num)
import GHC.Show(Show(..))

#include "MachDeps.h"

data UniqueSource = US (MutableByteArray# RealWorld)

newtype Unique = Unique { asInt :: Int }
    deriving ( Eq  -- ^ @since 4.4.0.0
             , Ord -- ^ @since 4.4.0.0
             , Num -- ^ @since 4.4.0.0
             )

-- | @since 4.3.1.0
instance Show Unique where
    show = show . asInt

newSource :: IO UniqueSource
newSource = IO $ \s ->
  case newByteArray# size s of
    (# s', mba #) -> (# s', US mba #)
  where
    !(I# size) = SIZEOF_HSINT

newUnique :: UniqueSource -> IO Unique
newUnique (US mba) = IO $ \s ->
  case fetchAddIntArray# mba 0# 1# s of
    (# s', a #) -> (# s', Unique (I# a) #)
{-# INLINE newUnique #-}
