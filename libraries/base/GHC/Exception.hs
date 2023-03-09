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
-- Module      :  GHC.Exception
-- Copyright   :  (c) The University of Glasgow, 1998-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Exceptions and exception-handling functions.
--
-----------------------------------------------------------------------------

module GHC.Exception
       ( module GHC.Exception.Type
       , throw
       , ErrorCall(..,ErrorCall)
       , errorCallException
       , errorCallWithCallStackException
       , toExceptionWithBacktrace

         -- * Re-exports from GHC.Types
       , CallStack, fromCallSiteList, getCallStack, prettyCallStack
       , prettyCallStackLines, showCCSStack
       , SrcLoc(..), prettySrcLoc
       ) where

import GHC.Base
import GHC.Show
import GHC.Stack.Types
import GHC.OldList
import GHC.IO.Unsafe
import {-# SOURCE #-} GHC.Stack.CCS
import {-# SOURCE #-} GHC.Stack (prettyCallStackLines, prettyCallStack, prettySrcLoc)
import {-# SOURCE #-} GHC.Exception.Backtrace (collectBacktraces)
import GHC.Exception.Context
import GHC.Exception.Type

-- | Throw an exception.  Exceptions may be thrown from purely
-- functional code, but may only be caught within the 'IO' monad.
--
-- WARNING: You may want to use 'throwIO' instead so that your pure code
-- stays exception-free.
throw :: forall (r :: RuntimeRep). forall (a :: TYPE r). forall e.
         (?callStack :: CallStack, Exception e) => e -> a
throw e =
    let !se = unsafePerformIO (toExceptionWithBacktrace e)
    in raise# se

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
    deriving ( Eq  -- ^ @since 4.7.0.0
             , Ord -- ^ @since 4.7.0.0
             )

pattern ErrorCall :: String -> ErrorCall
pattern ErrorCall err <- ErrorCallWithLocation err _ where
  ErrorCall err = ErrorCallWithLocation err ""

{-# COMPLETE ErrorCall #-}

-- | @since 4.0.0.0
instance Exception ErrorCall

-- | @since 4.0.0.0
instance Show ErrorCall where
  showsPrec _ (ErrorCallWithLocation err "") = showString err
  showsPrec _ (ErrorCallWithLocation err loc) =
      showString err . showChar '\n' . showString loc

errorCallException :: String -> SomeException
errorCallException s = toException (ErrorCall s)

errorCallWithCallStackException :: String -> CallStack -> SomeException
errorCallWithCallStackException s stk = unsafeDupablePerformIO $ do
  ccsStack <- currentCallStack
  let
    implicitParamCallStack = prettyCallStackLines stk
    ccsCallStack = showCCSStack ccsStack
    stack = intercalate "\n" $ implicitParamCallStack ++ ccsCallStack
  toExceptionWithBacktrace (ErrorCallWithLocation s stack)

showCCSStack :: [String] -> [String]
showCCSStack [] = []
showCCSStack stk = "CallStack (from -prof):" : map ("  " ++) (reverse stk)

