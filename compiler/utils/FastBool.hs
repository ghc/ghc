{-
(c) The University of Glasgow, 2000-2006

\section{Fast booleans}
-}

{-# LANGUAGE CPP, MagicHash #-}

module FastBool (
    --fastBool could be called bBox; isFastTrue, bUnbox; but they're not
    FastBool, fastBool, isFastTrue, fastOr, fastAnd
  ) where

-- Import the beggars
import GHC.Exts
#ifdef DEBUG
import Panic
#endif

type FastBool = Int#
fastBool True  = 1#
fastBool False = 0#

#ifdef DEBUG
--then waste time deciding whether to panic. FastBool should normally
--be at least as fast as Bool, one would hope...

isFastTrue 1# = True
isFastTrue 0# = False
isFastTrue _ = panic "FastTypes: isFastTrue"

-- note that fastOr and fastAnd are strict in both arguments
-- since they are unboxed
fastOr 1# _ = 1#
fastOr 0# x = x
fastOr _  _ = panicFastInt "FastTypes: fastOr"

fastAnd 0# _ = 0#
fastAnd 1# x = x
fastAnd _  _ = panicFastInt "FastTypes: fastAnd"

--these "panicFastInt"s (formerly known as "panic#") rely on
--FastInt = FastBool ( = Int# presumably),
--haha, true enough when __GLASGOW_HASKELL__.  Why can't we have functions
--that return _|_ be kind-polymorphic ( ?? to be precise ) ?

#else /* ! DEBUG */
--Isn't comparison to zero sometimes faster on CPUs than comparison to 1?
-- (since using Int# as _synonym_ fails to guarantee that it will
--   only take on values of 0 and 1)
isFastTrue 0# = False
isFastTrue _ = True

-- note that fastOr and fastAnd are strict in both arguments
-- since they are unboxed
-- Also, to avoid incomplete-pattern warning
-- (and avoid wasting time with redundant runtime checks),
-- we don't pattern-match on both 0# and 1# .
fastOr 0# x = x
fastOr _  _ = 1#

fastAnd 0# _ = 0#
fastAnd _  x = x

#endif /* ! DEBUG */

fastBool :: Bool -> FastBool
isFastTrue :: FastBool -> Bool
fastOr :: FastBool -> FastBool -> FastBool
fastAnd :: FastBool -> FastBool -> FastBool
