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
    , ErrorCall(..,ErrorCall)
    , errorCallException
    , errorCallWithCallStackException
    , toExceptionWithBacktrace

      -- * Reexports
      -- Re-export CallStack and SrcLoc from GHC.Types
    , CallStack, fromCallSiteList, getCallStack, prettyCallStack
    , prettyCallStackLines, showCCSStack
    , SrcLoc(..), prettySrcLoc
    ) where

import GHC.Internal.Base
import GHC.Internal.Show
import GHC.Internal.Stack.Types
import GHC.Internal.Data.OldList
import GHC.Internal.IO.Unsafe
import {-# SOURCE #-} GHC.Internal.Stack.CCS
import {-# SOURCE #-} GHC.Internal.Stack (prettyCallStackLines, prettyCallStack, prettySrcLoc)
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
    let !se = unsafePerformIO (toExceptionWithBacktrace e)
    in raise# se

-- | @since base-4.20.0.0
toExceptionWithBacktrace :: (HasCallStack, Exception e)
                         => e -> IO SomeException
toExceptionWithBacktrace e
  | backtraceDesired e = do
      bt <- collectBacktraces
      return (addExceptionContext bt (toException e))
  | otherwise = return (toException e)

-- | This is thrown when the user calls 'error'. The first @String@ is the
-- argument given to 'error', second @String@ is the location.
data ErrorCall = ErrorCallWithLocation String String
    deriving ( Eq  -- ^ @since base-4.7.0.0
             , Ord -- ^ @since base-4.7.0.0
             )

pattern ErrorCall :: String -> ErrorCall
pattern ErrorCall err <- ErrorCallWithLocation err _ where
  ErrorCall err = ErrorCallWithLocation err ""

{-# COMPLETE ErrorCall #-}

-- | @since base-4.0.0.0
instance Exception ErrorCall

-- | @since base-4.0.0.0
instance Show ErrorCall where
  showsPrec _ (ErrorCallWithLocation err "") = showString err
  showsPrec _ (ErrorCallWithLocation err loc) =
      showString err . showChar '\n' . showString loc

errorCallException :: String -> SomeException
errorCallException s = toException (ErrorCall s)

errorCallWithCallStackException :: String -> CallStack -> SomeException
errorCallWithCallStackException s stk = unsafeDupablePerformIO $ do
    ccsStack <- currentCallStack
    let implicitParamCallStack = prettyCallStackLines stk
        ccsCallStack = showCCSStack ccsStack
        stack = intercalate "\n" $ implicitParamCallStack ++ ccsCallStack
    toExceptionWithBacktrace (ErrorCallWithLocation s stack)
  where ?callStack = stk

showCCSStack :: [String] -> [String]
showCCSStack [] = []
showCCSStack stk = "CallStack (from -prof):" : map ("  " ++) (reverse stk)

