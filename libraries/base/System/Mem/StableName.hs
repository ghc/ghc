{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
{-# LANGUAGE MagicHash #-}
#if !defined(__PARALLEL_HASKELL__)
{-# LANGUAGE UnboxedTuples #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Mem.StableName
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Stable names are a way of performing fast (O(1)), not-quite-exact
-- comparison between objects.
-- 
-- Stable names solve the following problem: suppose you want to build
-- a hash table with Haskell objects as keys, but you want to use
-- pointer equality for comparison; maybe because the keys are large
-- and hashing would be slow, or perhaps because the keys are infinite
-- in size.  We can\'t build a hash table using the address of the
-- object as the key, because objects get moved around by the garbage
-- collector, meaning a re-hash would be necessary after every garbage
-- collection.
--
-------------------------------------------------------------------------------

module System.Mem.StableName (
  -- * Stable Names
  StableName,
  makeStableName,
  hashStableName,
  eqStableName
  ) where

import Prelude

import Data.Typeable

import GHC.IO           ( IO(..) )
import GHC.Base		( Int(..), StableName#, makeStableName#
			, eqStableName#, stableNameToInt# )

-----------------------------------------------------------------------------
-- Stable Names

{-|
  An abstract name for an object, that supports equality and hashing.

  Stable names have the following property:

  * If @sn1 :: StableName@ and @sn2 :: StableName@ and @sn1 == sn2@
   then @sn1@ and @sn2@ were created by calls to @makeStableName@ on 
   the same object.

  The reverse is not necessarily true: if two stable names are not
  equal, then the objects they name may still be equal.  Note in particular
  that `mkStableName` may return a different `StableName` after an
  object is evaluated.

  Stable Names are similar to Stable Pointers ("Foreign.StablePtr"),
  but differ in the following ways:

  * There is no @freeStableName@ operation, unlike "Foreign.StablePtr"s.
    Stable names are reclaimed by the runtime system when they are no
    longer needed.

  * There is no @deRefStableName@ operation.  You can\'t get back from
    a stable name to the original Haskell object.  The reason for
    this is that the existence of a stable name for an object does not
    guarantee the existence of the object itself; it can still be garbage
    collected.
-}

data StableName a = StableName (StableName# a)
                    deriving Typeable

-- | Makes a 'StableName' for an arbitrary object.  The object passed as
-- the first argument is not evaluated by 'makeStableName'.
makeStableName  :: a -> IO (StableName a)
#if defined(__PARALLEL_HASKELL__)
makeStableName a = 
  error "makeStableName not implemented in parallel Haskell"
#else
makeStableName a = IO $ \ s ->
    case makeStableName# a s of (# s', sn #) -> (# s', StableName sn #)
#endif

-- | Convert a 'StableName' to an 'Int'.  The 'Int' returned is not
-- necessarily unique; several 'StableName's may map to the same 'Int'
-- (in practice however, the chances of this are small, so the result
-- of 'hashStableName' makes a good hash key).
hashStableName :: StableName a -> Int
#if defined(__PARALLEL_HASKELL__)
hashStableName (StableName sn) = 
  error "hashStableName not implemented in parallel Haskell"
#else
hashStableName (StableName sn) = I# (stableNameToInt# sn)
#endif

instance Eq (StableName a) where 
#if defined(__PARALLEL_HASKELL__)
    (StableName sn1) == (StableName sn2) = 
      error "eqStableName not implemented in parallel Haskell"
#else
    (StableName sn1) == (StableName sn2) = 
       case eqStableName# sn1 sn2 of
	 0# -> False
	 _  -> True
#endif

-- | Equality on 'StableName' that does not require that the types of
-- the arguments match.
--
-- /Since: 4.7.0.0/
eqStableName :: StableName a -> StableName b -> Bool
eqStableName (StableName sn1) (StableName sn2) =
       case eqStableName# sn1 sn2 of
	 0# -> False
	 _  -> True
  -- Requested by Emil Axelsson on glasgow-haskell-users, who wants to
  -- use it for implementing observable sharing.

