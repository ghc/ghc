{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns, CPP, GeneralizedNewtypeDeriving, MagicHash,
  NoImplicitPrelude, UnboxedTuples #-}

module GHC.Internal.Event.Unique
    (
      UniqueSource
    , Unique(..)
    , newSource
    , newUnique
    ) where

import GHC.Internal.Base
import GHC.Internal.Num(Num)
import GHC.Internal.Show(Show(..))

#include "MachDeps.h"

data UniqueSource = US (MutableByteArray# RealWorld)

newtype Unique = Unique { asInt :: Int }
    deriving ( Eq  -- ^ @since base-4.4.0.0
             , Ord -- ^ @since base-4.4.0.0
             , Num -- ^ @since base-4.4.0.0
             )

-- | @since base-4.3.1.0
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
