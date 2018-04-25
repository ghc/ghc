{-# LANGUAGE CPP #-}
module Distribution.Client.Utils.Assertion (expensiveAssert) where

#ifdef DEBUG_EXPENSIVE_ASSERTIONS
import Control.Exception (assert)
import Distribution.Compat.Stack
#endif

-- | Like 'assert', but only enabled with -fdebug-expensive-assertions. This
-- function can be used for expensive assertions that should only be turned on
-- during testing or debugging.
#ifdef DEBUG_EXPENSIVE_ASSERTIONS
expensiveAssert :: WithCallStack (Bool -> a -> a)
expensiveAssert = assert
#else
expensiveAssert :: Bool -> a -> a
expensiveAssert _ = id
#endif
