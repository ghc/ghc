{-# OPTIONS_GHC -XNoImplicitPrelude #-}
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
--
-- The types for which 'unsafeCoerce' is representation-safe may differ
-- from compiler to compiler (and version to version).
--
--   * Documentation for correct usage in GHC will be found under
--     'unsafeCoerce#' in GHC.Base (around which 'unsafeCoerce' is just a
--     trivial wrapper).
--
--   * In nhc98, the only representation-safe coercions are between Enum
--     types with the same range (e.g. Int, Int32, Char, Word32),
--     or between a newtype and the type that it wraps.

module Unsafe.Coerce (unsafeCoerce) where

#if defined(__GLASGOW_HASKELL__)
import GHC.Prim (unsafeCoerce#)
unsafeCoerce :: a -> b
unsafeCoerce = unsafeCoerce#
#endif

#if defined(__NHC__)
import NonStdUnsafeCoerce (unsafeCoerce)
#endif

#if defined(__HUGS__)
import Hugs.IOExts (unsafeCoerce)
#endif
