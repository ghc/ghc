-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Type.Reflection.Unsafe
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2015
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- The representations of the types 'TyCon' and 'TypeRep', and the function
-- 'mkTyCon' which is used by derived instances of 'Typeable' to construct
-- 'TyCon's.
--
-- Be warned, these functions can be used to construct ill-kinded
-- type representations.
--
-----------------------------------------------------------------------------
{-# LANGUAGE PolyKinds, DataKinds, ScopedTypeVariables, TypeApplications, Unsafe #-}

module GHC.Internal.Type.Reflection.Unsafe (
      -- * Type representations
      TypeRep, mkTrApp, mkTyCon, typeRepFingerprint, someTypeRepFingerprint
      -- * Kind representations
    , KindRep(..), TypeLitSort(..)
      -- * Type constructors
    , TyCon, mkTrCon, tyConKindRep, tyConKindArgs, tyConFingerprint
  ) where

import GHC.Internal.Types -- For known-occ names
import GHC.Internal.Prim (FUN)
import GHC.Internal.Data.Typeable.Internal hiding (mkTrApp)
import qualified GHC.Internal.Data.Typeable.Internal as TI

-- | Construct a representation for a type application.
mkTrApp :: forall {ma :: Matchability} k1 k2
                  (a :: FUN @ma 'Many k1 k2) (b :: k1).
           TypeRep a
        -> TypeRep (b :: k1)
        -> TypeRep (a b)
mkTrApp = TI.mkTrAppChecked
