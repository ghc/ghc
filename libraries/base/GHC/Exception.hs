{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude
           , ExistentialQuantification
           , MagicHash
           , UnboxedTuples
           , RecordWildCards
           , PatternSynonyms
  #-}
{-# LANGUAGE TypeInType #-}
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
         -- re-export CallStack and SrcLoc from GHC.Types
       , CallStack, fromCallSiteList, getCallStack, pprBacktraces, prettyCallStack
       , prettyCallStackLines, showCCSStack
       , SrcLoc(..), prettySrcLoc
       , throwWithCallStack
       , throwWithIPEStack
       , throwWithCostCenterStack
       , throwWithExecutionStack
       ) where

import GHC.Base
import GHC.Exception.Backtrace
import GHC.Exception.Type
import {-# SOURCE #-} GHC.ExecutionStack.Internal
import GHC.IO.Unsafe
import GHC.OldList
import GHC.Prim
import GHC.Show
import {-# SOURCE #-} GHC.Stack.CCS
import GHC.Stack.CloneStack.Types (pprStackEntry)
import GHC.Stack.Types

-- | Throw an exception. Exceptions may be thrown from purely
-- functional code, but may only be caught within the 'IO' monad.
-- 'Backtrace' backtraces are collected according to the configured
-- 'BacktraceMechanism's.
--
-- WARNING: You may want to use 'throwIO' instead so that your pure code
-- stays exception-free.
throw :: HasCallStack => forall (r :: RuntimeRep). forall (a :: TYPE r). forall e.
         Exception e => e -> a
throw e =
  runRW#
    ( \s0 ->
        let e'@(SomeExceptionWithLocation _ bts) = toException e
         in if null bts
              then case unIO collectBacktraces s0 of
                (# _, bts' #) ->
                  let e'' = foldr addBacktrace e' bts'
                   in raise# e''
              else raise# e'
    )

-- | Throw an exception with a backtrace gathered by the 'HasCallStackBacktraceMech' mechanism.
-- If the exception already has backtraces, the new one is added.
throwWithCallStack :: HasCallStack => forall (r :: RuntimeRep). forall (a :: TYPE r). forall e.
         Exception e => e -> a
throwWithCallStack e =
-- throwWithCallStack cannot call throwWithBacktraceMechanism because that would introduce
-- unnecessary HasCallStack constraints (that would decrease performance).
   runRW# (\s0 ->
    case unIO collectHasCallStackBacktrace s0 of
      (# _, maybeBt #) ->
        let e' = case maybeBt of
                  Just bt -> addBacktrace bt (toException e)
                  Nothing -> toException e
        in raise# e')

throwWithBacktraceMechanism :: forall (r :: RuntimeRep). forall (a :: TYPE r). forall e.
         Exception e => IO (Maybe Backtrace) -> e -> a
throwWithBacktraceMechanism mech e = runRW# (\s0 ->
    case unIO mech s0 of
      (# _, maybeBt #) ->
        let e' = case maybeBt of
                  Just bt -> addBacktrace bt (toException e)
                  Nothing -> toException e
        in raise# e')

-- | Throw an exception with a 'Backtrace' gathered by the 'IPEBacktraceMech' mechanism.
-- If the exception already has backtraces, the new one is added.
throwWithIPEStack :: forall (r :: RuntimeRep). forall (a :: TYPE r). forall e.
         Exception e => e -> a
throwWithIPEStack = throwWithBacktraceMechanism collectIPEBacktrace

-- | Throw an exception with a 'Backtrace' gathered by the 'CostCenterBacktraceMech' mechanism.
-- If the exception already has backtraces, the new one is added.
throwWithCostCenterStack :: forall (r :: RuntimeRep). forall (a :: TYPE r). forall e.
         Exception e => e -> a
throwWithCostCenterStack = throwWithBacktraceMechanism collectCostCenterBacktrace

-- | Throw an exception with a 'Backtrace' gathered by the 'ExecutionStackBacktraceMech' mechanism.
-- If the exception already has backtraces, the new one is added.
throwWithExecutionStack :: forall (r :: RuntimeRep). forall (a :: TYPE r). forall e.
         Exception e => e -> a
throwWithExecutionStack = throwWithBacktraceMechanism collectExecutionStackBacktrace

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

errorCallException :: String -> SomeExceptionWithLocation
errorCallException s = toException (ErrorCall s)

errorCallWithCallStackException :: String -> CallStack -> SomeExceptionWithLocation
errorCallWithCallStackException s stk = unsafeDupablePerformIO $ do
  ccsStack <- currentCallStack
  let
    implicitParamCallStack = prettyCallStackLines stk
    ccsCallStack = showCCSStack ccsStack
    stack = intercalate "\n" $ implicitParamCallStack ++ ccsCallStack
  return $ toException (ErrorCallWithLocation s stack)

showCCSStack :: [String] -> [String]
showCCSStack [] = []
showCCSStack stk = "CallStack (from -prof):" : map ("  " ++) (reverse stk)

-- prettySrcLoc and prettyCallStack are defined here to avoid hs-boot
-- files. See Note [Definition of CallStack]

-- | Pretty print a 'SrcLoc'.
--
-- @since 4.9.0.0
prettySrcLoc :: SrcLoc -> String
prettySrcLoc SrcLoc {..}
  = foldr (++) ""
      [ srcLocFile, ":"
      , show srcLocStartLine, ":"
      , show srcLocStartCol, " in "
      , srcLocPackage, ":", srcLocModule
      ]

-- | Pretty print a 'CallStack'.
--
-- @since 4.9.0.0
prettyCallStack :: CallStack -> String
prettyCallStack = intercalate "\n" . prettyCallStackLines

prettyCallStackLines :: CallStack -> [String]
prettyCallStackLines cs = case getCallStack cs of
  []  -> []
  stk -> "CallStack (from HasCallStack):"
       : map (("  " ++) . prettyCallSite) stk
  where
    prettyCallSite (f, loc) = f ++ ", called at " ++ prettySrcLoc loc

-- | Pretty print a list of 'Backtrace's
-- This function should be used to output the backtraces to a terminal.
-- The format is subject to change. The caller should not depend on it.
pprBacktraces :: SomeExceptionWithLocation -> String
pprBacktraces (SomeExceptionWithLocation _ bts) = vcat $ fmap pprBacktrace bts

pprBacktrace :: Backtrace -> String
pprBacktrace (IPEBacktrace entries) = "Info Table Provenance Entries (IPE) backtrace" ++ ":" $+$ nest 1 (vcat $ map pprStackEntry entries)
pprBacktrace (HasCallStackBacktrace callStack) = "HasCallStack backtrace" ++ ":" $+$ nest 1 (prettyCallStack callStack)
pprBacktrace (ExecutionBacktrace locations) = "Debug symbol (DWARF) backtrace" ++ ":" $+$ nest 1 (showStackFrames locations "")
pprBacktrace (CostCenterBacktrace ptr) = "Cost Centre backtrace" ++ ":" $+$ nest 1 ((renderCCS.unsafePerformIO.ccsToStrings) ptr)
  where
    renderCCS :: [String] -> String
    renderCCS strs = concatMap (\s -> s ++ "\n") (reverse strs)

vcat :: [String] -> String
vcat = trimFinalNewLines . unlines

nest:: Int -> String -> String
nest c s = trimFinalNewLines . unlines $ map (spaces ++) (lines s)
  where
    spaces :: String
    spaces = replicate c ' '

trimFinalNewLines :: String -> String
trimFinalNewLines = reverse . dropWhile ('\n' ==) . reverse

($+$) :: String -> String -> String
($+$) a b = trimFinalNewLines $ unlines [a,b]
