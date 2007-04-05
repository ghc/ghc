{-# OPTIONS_GHC -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Unsafe.Coerce
-- Copyright   :  Malcolm Wallace 2006
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The highly unsafe primitive 'unsafeCoerce' converts a value from any
-- type to any other type.  Needless to say, if you use this function,
-- it is your responsibility to ensure that the old and new types have
-- identical internal representations, in order to prevent runtime corruption.

module Unsafe.Coerce (unsafeCoerce) where

#if defined(__GLASGOW_HASKELL__)
import GHC.Base (unsafeCoerce#)
unsafeCoerce :: a -> b
unsafeCoerce = unsafeCoerce#
#endif

#if defined(__NHC__)
import NonStdUnsafeCoerce (unsafeCoerce)
#endif

#if defined(__HUGS__)
import Hugs.Base (unsafeCoerce)
#endif
