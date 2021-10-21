{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Custom GHC "Prelude"
--
-- This module serves as a replacement for the "Prelude" module
-- and abstracts over differences between the bootstrapping
-- GHC version, and may also provide a common default vocabulary.

-- Every module in GHC
--   * Is compiled with -XNoImplicitPrelude
--   * Explicitly imports GHC.Prelude

module GHC.Prelude
  (module X
  ,module Bits
  ,shiftL, shiftR
#if __GLASGOW_HASKELL__ < 903
  ,SomeExceptionWithLocation
  ,pattern SomeExceptionWithLocation
#else
  ,SomeExceptionWithLocation(..)
#endif
  ) where


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

import Prelude as X hiding ((<>))
import Data.Foldable as X (foldl')
#if __GLASGOW_HASKELL__ < 903
import Control.Exception ( Exception, SomeException(..) )
#else
import Control.Exception ( SomeExceptionWithLocation(..) )
#endif

#if MIN_VERSION_base(4,16,0)
import GHC.Bits as Bits hiding (shiftL, shiftR)
# if defined(DEBUG)
import qualified GHC.Bits as Bits (shiftL, shiftR)
# endif

#else
--base <4.15
import Data.Bits as Bits hiding (shiftL, shiftR)
# if defined(DEBUG)
import qualified Data.Bits as Bits (shiftL, shiftR)
# endif
#endif

{- Note [Default to unsafe shifts inside GHC]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The safe shifts can introduce branches which come
at the cost of performance. We still want the additional
debugability for debug builds. So we define it as one or the
other depending on the DEBUG setting.

Why do we then continue on to re-export the rest of Data.Bits?
If we would not what is likely to happen is:
* Someone imports Data.Bits, uses xor. Things are fine.
* They add a shift and get an ambigious definition error.
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

#if __GLASGOW_HASKELL__ < 903
type SomeExceptionWithLocation = SomeException

{-# COMPLETE SomeExceptionWithLocation #-}
pattern SomeExceptionWithLocation :: () => forall e. Exception e => e -> SomeException
pattern SomeExceptionWithLocation e = SomeException e
#endif
