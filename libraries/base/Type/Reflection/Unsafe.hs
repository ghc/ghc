-----------------------------------------------------------------------------
-- |
-- Module      :  Type.Reflection.Unsafe
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2015
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- The representations of the types TyCon and TypeRep, and the
-- function mkTyCon which is used by derived instances of Typeable to
-- construct a TyCon.
--
-- Be warned, these functions can be used to construct ill-typed
-- type representations.
--
-----------------------------------------------------------------------------

module Type.Reflection.Unsafe (
    tyConKindRep, tyConKindArgs,
    KindRep(..), TypeLitSort(..),
    mkTrCon, mkTrApp, mkTyCon
  ) where

import Data.Typeable.Internal
