-----------------------------------------------------------------------------
-- |
-- Module      :  Type.Reflection.Unsafe
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

module Type.Reflection.Unsafe (
      -- * Type representations
      TypeRep, mkTrApp, mkTyCon, typeRepFingerprint, someTypeRepFingerprint
      -- * Kind representations
    , KindRep(..), TypeLitSort(..)
      -- * Type constructors
    , TyCon, mkTrCon, tyConKindRep, tyConKindArgs, tyConFingerprint
  ) where

import Data.Typeable.Internal
