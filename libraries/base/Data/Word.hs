{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Word
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Unsigned integer types.
--
-----------------------------------------------------------------------------

module Data.Word
  (
        -- * Unsigned integral types

        Word,
        Word8, Word16, Word32, Word64,

        -- * byte swapping
        byteSwap16, byteSwap32, byteSwap64,

        -- * Notes

        -- $notes
        ) where

import GHC.Word

{- $notes

* All arithmetic is performed modulo 2^n, where n is the number of
  bits in the type.  One non-obvious consequence of this is that 'Prelude.negate'
  should /not/ raise an error on negative arguments.

* For coercing between any two integer types, use
  'Prelude.fromIntegral', which is specialized for all the
  common cases so should be fast enough.  Coercing word types to and
  from integer types preserves representation, not sign.

* An unbounded size unsigned integer type is available with
  'Numeric.Natural.Natural'.

* The rules that hold for 'Prelude.Enum' instances over a bounded type
  such as 'Prelude.Int' (see the section of the Haskell report dealing
  with arithmetic sequences) also hold for the 'Prelude.Enum' instances
  over the various 'Word' types defined here.

* Right and left shifts by amounts greater than or equal to the width
  of the type result in a zero result.  This is contrary to the
  behaviour in C, which is undefined; a common interpretation is to
  truncate the shift count to the width of the type, for example @1 \<\<
  32 == 1@ in some C implementations. 
-}

