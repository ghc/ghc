{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}
{-# OPTIONS_GHC -O2 #-} -- See Note [-O2 Prelude]

-- See Note [Proxies for head and tail]
{-# OPTIONS_GHC -Wno-unrecognised-warning-flags -Wno-x-partial #-}

-- | Custom minimal GHC "Prelude"
--
-- This module serves as a replacement for the "Prelude" module
-- and abstracts over differences between the bootstrapping
-- GHC version, and may also provide a common default vocabulary.

-- Every module in GHC
--   * Is compiled with -XNoImplicitPrelude
--   * Explicitly imports GHC.Prelude.Basic or GHC.Prelude
--   * The later provides some functionality with within ghc itself
--     like pprTrace.

module GHC.Prelude.Basic
  ( module X
  , Applicative (..)
  , module Bits
  , bit
  , shiftL, shiftR
  , setBit, clearBit
  , head, tail
  ) where


{- Note [-O2 Prelude]
~~~~~~~~~~~~~~~~~~~~~
There is some code in GHC that is *always* compiled with -O[2] because
of it's impact on compile time performance. Some of this code might depend
on the definitions like shiftL being defined here being performant.

So we always compile this module with -O2. It's (currently) tiny so I
have little reason to suspect this impacts overall GHC compile times
negatively.

-}
-- We export the 'Semigroup' class but w/o the (<>) operator to avoid
-- clashing with the (Outputable.<>) operator which is heavily used
-- through GHC's code-base.

{-
Note [Why do we import Prelude here?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The files ghc-boot-th.cabal, ghc-boot.cabal, ghci.cabal and
ghc-heap.cabal contain the directive default-extensions:
NoImplicitPrelude. There are two motivations for this:
  - Consistency with the compiler directory, which enables
    NoImplicitPrelude;
  - Allows loading the above dependent packages with ghc-in-ghci,
    giving a smoother development experience when adding new
    extensions.
-}

import qualified Prelude
import Prelude as X hiding ((<>), Applicative(..), Foldable(..), head, tail)
import Control.Applicative (Applicative(..))
import Data.Foldable as X (Foldable(elem, foldMap, foldr, foldl, foldl', foldr1, foldl1, maximum, minimum, product, sum, null, length))
import GHC.Stack.Types (HasCallStack)

import GHC.Bits as Bits hiding (bit, shiftL, shiftR, setBit, clearBit)
# if defined(DEBUG)
import qualified GHC.Bits as Bits (shiftL, shiftR)
# endif


{- Note [Default to unsafe shifts inside GHC]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The safe shifts can introduce branches which come
at the cost of performance. We still want the additional
debuggability for debug builds. So we define it as one or the
other depending on the DEBUG setting.

Why do we then continue on to re-export the rest of Data.Bits?
If we would not what is likely to happen is:
* Someone imports Data.Bits, uses xor. Things are fine.
* They add a shift and get an ambiguous definition error.
* The are puzzled for a bit.
* They either:
  + Remove the import of Data.Bits and get an error because xor is not in scope.
  + Add the hiding clause to the Data.Bits import for the shifts.

Either is quite annoying. Simply re-exporting all of Data.Bits avoids this
making for a smoother developer experience. At the cost of having a few more
names in scope at all time. But that seems like a fair tradeoff.

See also #19618
-}

-- We always want the Data.Bits method to show up for rules etc.
{-# INLINE shiftL #-}
{-# INLINE shiftR #-}
shiftL, shiftR :: Bits.Bits a => a -> Int -> a
#if defined(DEBUG)
shiftL = Bits.shiftL
shiftR = Bits.shiftR
#else
shiftL = Bits.unsafeShiftL
shiftR = Bits.unsafeShiftR
#endif

{-# INLINE bit #-}
bit :: (Num a, Bits.Bits a) => Int -> a
bit = \ i -> 1 `shiftL` i
{-# INLINE setBit #-}
setBit :: (Num a, Bits.Bits a) => a -> Int -> a
setBit = \ x i -> x Bits..|. bit i
{-# INLINE clearBit #-}
clearBit :: (Num a, Bits.Bits a) => a -> Int -> a
clearBit = \ x i -> x Bits..&. Bits.complement (bit i)

{- Note [Proxies for head and tail]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Prelude.head and Prelude.tail have recently acquired {-# WARNING in "x-partial" #-},
but the GHC codebase uses them fairly extensively and insists on building warning-free.
Thus, instead of adding {-# OPTIONS_GHC -Wno-x-partial #-} to every module which
employs them, we define warning-less proxies and export them from GHC.Prelude.
-}

-- See Note [Proxies for head and tail]
head :: HasCallStack => [a] -> a
head = Prelude.head
{-# INLINE head #-}

-- See Note [Proxies for head and tail]
tail :: HasCallStack => [a] -> [a]
tail = Prelude.tail
{-# INLINE tail #-}
