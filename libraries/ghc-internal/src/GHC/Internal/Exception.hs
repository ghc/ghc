{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude
           , ExistentialQuantification
           , MagicHash
           , PatternSynonyms
  #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Exception
-- Copyright   :  (c) The University of Glasgow, 1998-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Exceptions and exception-handling functions.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--
-----------------------------------------------------------------------------

module GHC.Internal.Exception
    ( -- * 'Exception' class
      Exception(..)

      -- * 'SomeException'
    , SomeException(..)
    , displayExceptionWithInfo

      -- * Exception context
    , someExceptionContext
    , addExceptionContext

      -- * Throwing
    , throw

      -- * Concrete exceptions
      -- ** Arithmetic exceptions
    , ArithException(..)
    , divZeroException
    , overflowException
    , ratioZeroDenomException
    , underflowException
      -- ** 'ErrorCall'
    , ErrorCall(..)
    , errorCallException
    , errorCallWithCallStackException
    , toExceptionWithBacktrace

      -- * Reexports
      -- Re-export CallStack and SrcLoc from GHC.Types
    , CallStack, fromCallSiteList, getCallStack, prettyCallStack
    , prettyCallStackLines
    , SrcLoc(..), prettySrcLoc
    ) where

import GHC.Internal.Base
import GHC.Internal.Show
import GHC.Internal.Stack.Types
import GHC.Internal.IO.Unsafe
import {-# SOURCE #-} GHC.Internal.Stack (prettyCallStackLines, prettyCallStack, prettySrcLoc, withFrozenCallStack)
import {-# SOURCE #-} GHC.Internal.Exception.Backtrace (collectBacktraces)
import GHC.Internal.Exception.Type

-- | Throw an exception.  Exceptions may be thrown from purely
-- functional code, but may only be caught within the 'IO' monad.
--
-- WARNING: You may want to use 'throwIO' instead so that your pure code
-- stays exception-free.
throw :: forall (r :: RuntimeRep). forall (a :: TYPE r). forall e.
         (HasCallStack, Exception e) => e -> a
throw e =
    -- Note the absolutely crucial bang "!" on this binding!
    --   See Note [Capturing the backtrace in throw]
    -- Note also the absolutely crucial `noinine` in the RHS!
    --   See Note [Hiding precise exception signature in throw]
    let se :: SomeException
        !se = noinline (unsafePerformIO (toExceptionWithBacktrace e))
    in raise# se

-- Note [Capturing the backtrace in throw]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- When `throw` captures a backtrace, it must be the backtrace *at the moment
-- that `throw` is called*.   That is why the binding of `se` is marked strict,
-- via the `!`:
--
--     !se = <rhs>
--
-- GHC can capture /four/ different sorts of backtraces (See Note [Backtrace
-- mechanisms] in "Control.Exception.Backtrace" for details). One of them
-- (`CallStack` constraints) does not need this strict-binding treatment,
-- because the `CallStack` constraint is captured in the thunk. However, the
-- other two (DWARF stack unwinding, and native Haskell stack unwinding) are
-- much more fragile, and can only be captured right at the call of `throw`.
--
-- However, making `se` strict has downsides: see
-- Note [Hiding precise exception signature in throw] below.
--
--
-- Note [Hiding precise exception signature in throw]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- In 'throw' we use `unsafePerformIO . toExceptionWithBacktrace' to collect
-- the backtraces which will be attached as the exception's 'ExceptionContext'.
-- We must ensure that this is evaluated immediately in `throw` since
-- `toExceptionWithBacktrace` must capture the execution state at the moment
-- that the exception is thrown (see Note [Capturing the backtrace in throw]).
-- Unfortunately, unless we take particular care this can lead to a
-- catastrophic regression in 'throw's demand signature which will infect
-- all callers (#25066)
--
-- Specifically, GHC's demand analysis has an approximate heuristic for tracking
-- whether divergent functions diverge with precise or imprecise exceptions (namely
-- the 'ExnOrDiv' and 'Diverges' constructors of 'GHC.Types.Demand.Divergence',
-- respectively). This is because we can take considerably more liberties in
-- optimising around functions which are known not to diverge via precise
-- exception (see Note [Precise exceptions and strictness analysis]).
-- For this reason, it is important that 'throw' have a 'Diverges' divergence
-- type.
--
-- Unfortunately, this is broken if we allow `unsafePerformIO` to inline. Specifically,
-- if we allow this inlining we will end up with Core of the form:
--
--   throw = \e ->
--     case runRW# (\s -> ... toExceptionWithBacktrace e s ...) of
--       se -> raise# se
--
-- so far this is fine; the demand analyzer's divergence heuristic
-- will give 'throw' the expected 'Diverges' divergence.
--
-- However, the simplifier will subsequently notice that `raise#` can be fruitfully
-- floated into the body of the `runRW#`:
--
--   throw = \e ->
--     runRW# (\s -> case toExceptionWithBacktrace e s of
--                     (# s', se #) -> raise# se)
--
-- This is problematic as one of the demand analyser's heuristics examines
-- `case` scrutinees, looking for those that result in a `RealWorld#` token
-- (see Note [Which scrutinees may throw precise exceptions], test (1)). The
-- `case toExceptionWithBacktrace e of ...` here fails this check, causing the
-- heuristic to conclude that `throw` may indeed diverge with a precise
-- exception. This resulted in the significant performance regression noted in
-- #25066.
--
-- To avoid this, we use `noinline` to ensure that `unsafePerformIO` does not unfold,
-- meaning that the `raise#` cannot be floated under the `toExceptionWithBacktrace`
-- case analysis.
--
-- Ultimately this is a bit of a horrible hack; the right solution would be to have
-- primops which allow more precise guidance of the demand analyser's heuristic
-- (e.g. #23847).

-- | @since base-4.20.0.0
toExceptionWithBacktrace :: (HasCallStack, Exception e)
                         => e -> IO SomeException
toExceptionWithBacktrace e
  | backtraceDesired e = do
      bt <- collectBacktraces
      return (addExceptionContext bt (toException e))
  | otherwise = return (toException e)

-- | This is thrown when the user calls 'error'. The @String@ is the
-- argument given to 'error'.
--
-- Historically, there was a second @String@ for the location, but it was subsumed by the backtrace mechanisms (since base-4.22).
data ErrorCall = ErrorCall String
    deriving ( Eq  -- ^ @since base-4.7.0.0
             , Ord -- ^ @since base-4.7.0.0
             )

{-# COMPLETE ErrorCall #-}

-- | @since base-4.0.0.0
instance Exception ErrorCall

-- | @since base-4.0.0.0
instance Show ErrorCall where
  showsPrec _ (ErrorCall err) = showString err

errorCallException :: String -> SomeException
errorCallException s = toException (ErrorCall s)

errorCallWithCallStackException :: String -> CallStack -> SomeException
errorCallWithCallStackException s stk = unsafeDupablePerformIO $ do
    withFrozenCallStack $ toExceptionWithBacktrace (ErrorCall s)
  where ?callStack = stk

