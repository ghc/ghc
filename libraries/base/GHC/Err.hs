{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash, ImplicitParams #-}
{-# LANGUAGE RankNTypes, PolyKinds, DataKinds #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Err
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- The "GHC.Err" module defines the code for the wired-in error functions,
-- which have a special type in the compiler (with \"open tyvars\").
--
-- We cannot define these functions in a module where they might be used
-- (e.g., "GHC.Base"), because the magical wired-in type will get confused
-- with what the typechecker figures out.
--
-----------------------------------------------------------------------------

module GHC.Err( absentErr, error, errorWithoutStackTrace, undefined ) where
import GHC.Types (Char, RuntimeRep)
import GHC.Stack.Types
import GHC.Prim
import {-# SOURCE #-} GHC.Exception
  ( errorCallWithCallStackException
  , errorCallException )

-- | 'error' stops execution and displays an error message.
error :: forall (r :: RuntimeRep). forall (a :: TYPE r).
         HasCallStack => [Char] -> a
error s = raise# (errorCallWithCallStackException s ?callStack)
          -- Bleh, we should be using 'GHC.Stack.callStack' instead of
          -- '?callStack' here, but 'GHC.Stack.callStack' depends on
          -- 'GHC.Stack.popCallStack', which is partial and depends on
          -- 'error'.. Do as I say, not as I do.

-- | A variant of 'error' that does not produce a stack trace.
--
-- @since 4.9.0.0
errorWithoutStackTrace :: forall (r :: RuntimeRep). forall (a :: TYPE r).
                          [Char] -> a
errorWithoutStackTrace s = raise# (errorCallException s)


-- Note [Errors in base]
-- ~~~~~~~~~~~~~~~~~~~~~
-- As of base-4.9.0.0, `error` produces a stack trace alongside the
-- error message using the HasCallStack machinery. This provides
-- a partial stack trace, containing the call-site of each function
-- with a HasCallStack constraint.
--
-- In base, error and undefined were the only functions that had such
-- constraint. Errors like "Prelude.!!: negative index" are good, yet if the
-- code base contains dozens of !! applications (including dependencies,
-- which code is not as easily accessible), pinpointing the bad call is
-- where the stack trace would help.  Therefore we annotate most calls to
-- error, so users have a chance to get a better idea.

-- | A special case of 'error'.
-- It is expected that compilers will recognize this and insert error
-- messages which are more appropriate to the context in which 'undefined'
-- appears.
undefined :: forall (r :: RuntimeRep). forall (a :: TYPE r).
             HasCallStack => a
-- This used to be
--   undefined = error "Prelude.undefined"
-- but that would add an extra call stack entry that is not actually helpful
-- nor wanted (see #19886). We’d like to use withFrozenCallStack, but that
-- is not available in this module yet, and making it so is hard. So let’s just
-- use raise# directly.
undefined = raise# (errorCallWithCallStackException "Prelude.undefined" ?callStack)

-- | Used for compiler-generated error message;
-- encoding saves bytes of string junk.
absentErr :: a
absentErr = errorWithoutStackTrace "Oops! The program has entered an `absent' argument!\n"
