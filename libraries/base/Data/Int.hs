{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Int
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Signed integer types
--
-----------------------------------------------------------------------------

module Data.Int
  ( 
        -- * Signed integer types
        Int,
        Int8, Int16, Int32, Int64,

        -- * Notes

        -- $notes
        ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Base ( Int )
import GHC.Int  ( Int8, Int16, Int32, Int64 )
#endif

{- $notes

* All arithmetic is performed modulo 2^n, where @n@ is the number of
  bits in the type.

* For coercing between any two integer types, use 'Prelude.fromIntegral',
  which is specialized for all the common cases so should be fast
  enough.  Coercing word types (see "Data.Word") to and from integer
  types preserves representation, not sign.

* The rules that hold for 'Prelude.Enum' instances over a
  bounded type such as 'Int' (see the section of the
  Haskell report dealing with arithmetic sequences) also hold for the
  'Prelude.Enum' instances over the various
  'Int' types defined here.

* Right and left shifts by amounts greater than or equal to the width
  of the type result in either zero or -1, depending on the sign of
  the value being shifted.  This is contrary to the behaviour in C,
  which is undefined; a common interpretation is to truncate the shift
  count to the width of the type, for example @1 \<\< 32
  == 1@ in some C implementations.
-}

