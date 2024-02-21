{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Foreign.C.ConstPtr
-- Copyright   :  (c) GHC Developers
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides typed @const@ pointers to foreign data. It is part
-- of the Foreign Function Interface (FFI).
--
-----------------------------------------------------------------------------

module GHC.Internal.Foreign.C.ConstPtr (
    ConstPtr(..)
) where

import GHC.Internal.Base
import GHC.Internal.Ptr
import GHC.Internal.Show

-- | A pointer with the C @const@ qualifier. For instance, an argument of type
-- @ConstPtr CInt@ would be marshalled as @const int*@.
--
-- While @const@-ness generally does not matter for @ccall@ imports (since
-- @const@ and non-@const@ pointers typically have equivalent calling
-- conventions), it does matter for @capi@ imports. See GHC #22043.
--
-- @since base-4.18.0.0
--
type ConstPtr :: Type -> Type
type role ConstPtr phantom
newtype ConstPtr a = ConstPtr { unConstPtr :: Ptr a }
    deriving (Eq, Ord)

-- doesn't use record syntax
instance Show (ConstPtr a) where
    showsPrec d (ConstPtr p) = showParen (d > 10) $ showString "ConstPtr " . showsPrec 11 p
