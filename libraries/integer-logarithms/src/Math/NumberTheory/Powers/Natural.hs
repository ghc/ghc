-- |
-- Module:      Math.NumberTheory.Powers.Natural
-- Copyright:   (c) 2011-2014 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Potentially faster power function for 'Natural' base and 'Int'
-- or 'Word' exponent.
--
{-# LANGUAGE CPP          #-}
#if __GLASGOW_HASKELL__ >= 702
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
#endif
module Math.NumberTheory.Powers.Natural
    {-# DEPRECATED "It is no faster than (^)" #-}
    ( naturalPower
    , naturalWordPower
    ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Word
#endif

import Numeric.Natural

-- | Power of an 'Natural' by the left-to-right repeated squaring algorithm.
--   This needs two multiplications in each step while the right-to-left
--   algorithm needs only one multiplication for 0-bits, but here the
--   two factors always have approximately the same size, which on average
--   gains a bit when the result is large.
--
--   For small results, it is unlikely to be any faster than '(^)', quite
--   possibly slower (though the difference shouldn't be large), and for
--   exponents with few bits set, the same holds. But for exponents with
--   many bits set, the speedup can be significant.
--
--   /Warning:/ No check for the negativity of the exponent is performed,
--   a negative exponent is interpreted as a large positive exponent.
naturalPower :: Natural -> Int -> Natural
naturalPower = (^)
{-# DEPRECATED naturalPower "Use (^) instead" #-}

-- | Same as 'naturalPower', but for exponents of type 'Word'.
naturalWordPower :: Natural -> Word -> Natural
naturalWordPower = (^)
{-# DEPRECATED naturalWordPower "Use (^) instead" #-}
