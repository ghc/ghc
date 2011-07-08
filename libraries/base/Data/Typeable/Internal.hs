-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Typeable.Internal
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2011
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- The representations of the types TyCon and TypeRep, and the
-- function mkTyCon which is used by derived instances of Typeable to
-- construct a TyCon.
--
-----------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude,
             MagicHash #-}
module Data.Typeable.Internal (
    TypeRep(..),
    TyCon(..),
    mkTyCon
  ) where

import GHC.Base
import GHC.Word
import GHC.Fingerprint.Type

-- | A concrete representation of a (monomorphic) type.  'TypeRep'
-- supports reasonably efficient equality.
data TypeRep = TypeRep {-# UNPACK #-} !Fingerprint TyCon [TypeRep]

-- Compare keys for equality
instance Eq TypeRep where
  (TypeRep k1 _ _) == (TypeRep k2 _ _) = k1 == k2

-- | An abstract representation of a type constructor.  'TyCon' objects can
-- be built using 'mkTyCon'.
data TyCon = TyCon {
   tyConHash    :: {-# UNPACK #-} !Fingerprint,
   tyConPackage :: String,
   tyConModule  :: String,
   tyConName    :: String
 }

instance Eq TyCon where
  (TyCon t1 _ _ _) == (TyCon t2 _ _ _) = t1 == t2

#include "MachDeps.h"

-- mkTyCon is an internal function to make it easier for GHC to
-- generate derived instances.  GHC precomputes the MD5 hash for the
-- TyCon and passes it as two separate 64-bit values to mkTyCon.  The
-- TyCon for a derived Typeable instance will end up being statically
-- allocated.

#if WORD_SIZE_IN_BITS < 64
mkTyCon :: Word64# -> Word64# -> String -> String -> String -> TyCon
#else
mkTyCon :: Word#   -> Word#   -> String -> String -> String -> TyCon
#endif
mkTyCon high# low# pkg modl name
  = TyCon (Fingerprint (W64# high#) (W64# low#)) pkg modl name
