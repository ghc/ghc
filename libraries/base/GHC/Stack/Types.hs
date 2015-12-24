{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}
-- we hide this module from haddock to enforce GHC.Stack as the main
-- access point.

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Stack.Types
-- Copyright   :  (c) The University of Glasgow 2015
-- License     :  see libraries/ghc-prim/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- type definitions for call-stacks via implicit parameters.
-- Use "GHC.Stack" from the base package instead of importing this
-- module directly.
--
-----------------------------------------------------------------------------

module GHC.Stack.Types (
    -- * Implicit parameter call stacks
    CallStack(..), emptyCallStack, freezeCallStack, getCallStack, pushCallStack,
    -- * Source locations
    SrcLoc(..)
  ) where

{-
Ideally these would live in GHC.Stack but sadly they can't due to this
import cycle,

    Module imports form a cycle:
           module ‘Data.Maybe’ (libraries/base/Data/Maybe.hs)
          imports ‘GHC.Base’ (libraries/base/GHC/Base.hs)
    which imports ‘GHC.Err’ (libraries/base/GHC/Err.hs)
    which imports ‘GHC.Stack’ (libraries/base/dist-install/build/GHC/Stack.hs)
    which imports ‘GHC.Foreign’ (libraries/base/GHC/Foreign.hs)
    which imports ‘Data.Maybe’ (libraries/base/Data/Maybe.hs)
-}

import GHC.Types

-- Make implicit dependency known to build system
import GHC.Tuple ()
import GHC.Integer ()

----------------------------------------------------------------------
-- Explicit call-stacks built via ImplicitParams
----------------------------------------------------------------------

-- | Implicit @CallStack@s are an alternate method of obtaining the call stack
-- at a given point in the program.
--
-- GHC has two built-in rules for solving implicit-parameters of type
-- @CallStack@.
--
-- 1. If the @CallStack@ occurs in a function call, it appends the
--    source location of the call to the @CallStack@ in the environment.
-- 2. @CallStack@s that cannot be solved normally (i.e. unbound
--    occurrences) are defaulted to the empty @CallStack@.
--
-- Otherwise implicit @CallStack@s behave just like ordinary implicit
-- parameters. For example:
--
-- @
-- myerror :: (?callStack :: CallStack) => String -> a
-- myerror msg = error (msg ++ "\n" ++ prettyCallStack ?callStack)
-- @
--
-- Will produce the following when evaluated,
--
-- @
-- ghci> myerror "die"
-- *** Exception: die
-- CallStack (from ImplicitParams):
--   myerror, called at <interactive>:2:1 in interactive:Ghci1
-- @
--
-- @CallStack@s do not interact with the RTS and do not require compilation with
-- @-prof@. On the other hand, as they are built up explicitly using
-- implicit-parameters, they will generally not contain as much information as
-- the simulated call-stacks maintained by the RTS.
--
-- A @CallStack@ is a @[(String, SrcLoc)]@. The @String@ is the name of
-- function that was called, the 'SrcLoc' is the call-site. The list is
-- ordered with the most recently called function at the head.
--
-- @since 4.8.1.0
data CallStack
  = EmptyCallStack
  | PushCallStack ([Char], SrcLoc) CallStack
  | FreezeCallStack CallStack
    -- ^ Freeze the stack at the given @CallStack@, preventing any further
    -- call-sites from being pushed onto it.

  -- See Note [Overview of implicit CallStacks]

-- | Extract a list of call-sites from the 'CallStack'.
--
-- The list is ordered by most recent call.
--
-- @since 4.8.1.0
getCallStack :: CallStack -> [([Char], SrcLoc)]
getCallStack stk = case stk of
  EmptyCallStack        -> []
  PushCallStack cs stk' -> cs : getCallStack stk'
  FreezeCallStack stk'  -> getCallStack stk'


-- Note [Definition of CallStack]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Implicit CallStacks are defined very early in base because they are
-- used by error and undefined. At this point in the dependency graph,
-- we do not have enough functionality to (conveniently) write a nice
-- pretty-printer for CallStack. The sensible place to define the
-- pretty-printer would be GHC.Stack, which is the main access point,
-- but unfortunately GHC.Stack imports GHC.Exception, which *needs*
-- the pretty-printer. So the CallStack type and functions are split
-- between three modules:
--
-- 1. GHC.Stack.Types: defines the type and *simple* functions
-- 2. GHC.Exception: defines the pretty-printer
-- 3. GHC.Stack: exports everything and acts as the main access point


-- | Push a call-site onto the stack.
--
-- This function has no effect on a frozen 'CallStack'.
--
-- @since 4.9.0.0
pushCallStack :: ([Char], SrcLoc) -> CallStack -> CallStack
pushCallStack cs stk = case stk of
  FreezeCallStack _ -> stk
  _                 -> PushCallStack cs stk
{-# INLINE pushCallStack #-}


-- | The empty 'CallStack'.
--
-- @since 4.9.0.0
emptyCallStack :: CallStack
emptyCallStack = EmptyCallStack
{-# INLINE emptyCallStack #-}

-- | Freeze a call-stack, preventing any further call-sites from being appended.
--
-- prop> pushCallStack callSite (freezeCallStack callStack) = freezeCallStack callStack
--
-- @since 4.9.0.0
freezeCallStack :: CallStack -> CallStack
freezeCallStack stk = FreezeCallStack stk
{-# INLINE freezeCallStack #-}


-- | A single location in the source code.
--
-- @since 4.8.1.0
data SrcLoc = SrcLoc
  { srcLocPackage   :: [Char]
  , srcLocModule    :: [Char]
  , srcLocFile      :: [Char]
  , srcLocStartLine :: Int
  , srcLocStartCol  :: Int
  , srcLocEndLine   :: Int
  , srcLocEndCol    :: Int
  }
