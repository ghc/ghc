{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE Trustworthy       #-}

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
-- type definitions for implicit call-stacks.
-- Use "GHC.Stack" from the base package instead of importing this
-- module directly.
--
-----------------------------------------------------------------------------

module GHC.Stack.Types (
    -- * Implicit call stacks
    CallStack(..), HasCallStack,
    emptyCallStack, freezeCallStack, fromCallSiteList,
    getCallStack, pushCallStack,

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

import GHC.Classes (Eq)
import GHC.Types (Char, Int)

-- Make implicit dependency known to build system
import GHC.Tuple ()
import GHC.Integer ()

----------------------------------------------------------------------
-- Explicit call-stacks built via ImplicitParams
----------------------------------------------------------------------

-- | Request a CallStack.
--
-- NOTE: The implicit parameter @?callStack :: CallStack@ is an
-- implementation detail and __should not__ be considered part of the
-- 'CallStack' API, we may decide to change the implementation in the
-- future.
--
-- @since 4.9.0.0
type HasCallStack = (?callStack :: CallStack)

-- | 'CallStack's are a lightweight method of obtaining a
-- partial call-stack at any point in the program.
--
-- A function can request its call-site with the 'HasCallStack' constraint.
-- For example, we can define
--
-- @
-- errorWithCallStack :: HasCallStack => String -> a
-- @
--
-- as a variant of @error@ that will get its call-site. We can access the
-- call-stack inside @errorWithCallStack@ with 'GHC.Stack.callStack'.
--
-- @
-- errorWithCallStack :: HasCallStack => String -> a
-- errorWithCallStack msg = error (msg ++ "\n" ++ prettyCallStack callStack)
-- @
--
-- Thus, if we call @errorWithCallStack@ we will get a formatted call-stack
-- alongside our error message.
--
--
-- >>> errorWithCallStack "die"
-- *** Exception: die
-- CallStack (from HasCallStack):
--   errorWithCallStack, called at <interactive>:2:1 in interactive:Ghci1
--
--
-- GHC solves 'HasCallStack' constraints in three steps:
--
-- 1. If there is a 'CallStack' in scope -- i.e. the enclosing function
--    has a 'HasCallStack' constraint -- GHC will append the new
--    call-site to the existing 'CallStack'.
--
-- 2. If there is no 'CallStack' in scope -- e.g. in the GHCi session
--    above -- and the enclosing definition does not have an explicit
--    type signature, GHC will infer a 'HasCallStack' constraint for the
--    enclosing definition (subject to the monomorphism restriction).
--
-- 3. If there is no 'CallStack' in scope and the enclosing definition
--    has an explicit type signature, GHC will solve the 'HasCallStack'
--    constraint for the singleton 'CallStack' containing just the
--    current call-site.
--
-- 'CallStack's do not interact with the RTS and do not require compilation
-- with @-prof@. On the other hand, as they are built up explicitly via the
-- 'HasCallStack' constraints, they will generally not contain as much
-- information as the simulated call-stacks maintained by the RTS.
--
-- A 'CallStack' is a @[(String, SrcLoc)]@. The @String@ is the name of
-- function that was called, the 'SrcLoc' is the call-site. The list is
-- ordered with the most recently called function at the head.
--
-- NOTE: The intrepid user may notice that 'HasCallStack' is just an
-- alias for an implicit parameter @?callStack :: CallStack@. This is an
-- implementation detail and __should not__ be considered part of the
-- 'CallStack' API, we may decide to change the implementation in the
-- future.
--
-- @since 4.8.1.0
data CallStack
  = EmptyCallStack
  | PushCallStack [Char] SrcLoc CallStack
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
  EmptyCallStack            -> []
  PushCallStack fn loc stk' -> (fn,loc) : getCallStack stk'
  FreezeCallStack stk'      -> getCallStack stk'

-- | Convert a list of call-sites to a 'CallStack'.
--
-- @since 4.9.0.0
fromCallSiteList :: [([Char], SrcLoc)] -> CallStack
fromCallSiteList ((fn,loc):cs) = PushCallStack fn loc (fromCallSiteList cs)
fromCallSiteList []            = EmptyCallStack

-- Note [Definition of CallStack]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- CallStack is defined very early in base because it is
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
pushCallStack (fn, loc) stk = case stk of
  FreezeCallStack _ -> stk
  _                 -> PushCallStack fn loc stk
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
  } deriving Eq
