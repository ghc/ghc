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
import GHC.CString
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
{-# NOINLINE error #-}

-- | A variant of 'error' that does not produce a stack trace.
--
-- @since 4.9.0.0
errorWithoutStackTrace :: forall (r :: RuntimeRep). forall (a :: TYPE r).
                          [Char] -> a
errorWithoutStackTrace s = raise# (errorCallException s)
{-# NOINLINE errorWithoutStackTrace #-}

{- Note [Ensuring that `error` isn't CAFfy]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
`error` expressions are quite common and consequently we want to ensure that
they don't incur a high cost. For this reason, we take pains to ensure that
common uses of `error` (e.g. applications to string literals) are not CAFfy.
For instance, consider a program like:

    head :: [a] -> a
    head (x:_) = x
    head _     = error "head: empty list"

The simplifier would typically float out the `error` application and its
argument to the top-level, resulting in:

    head = \xs -> case xs of 
                    x : _ -> x
                    []    -> lvl1
    lvl1 = error lvl2
    lvl2 = unpackCString# "uh oh"

While the code generator is smart enough not to make `lvl1` a CAF (see Note
[Don't update dead-end thunks] in GHC.CoreToStg), `lvl1`, and therefore `head`,
is nevertheless CAFfy on account of the dependence on `lvl2`, which is a CAF.

We avoid this by rewriting `error (unpackCString# lit)` to `errorCString lit`, ensuring that the literal does not become a CAF. This has a few happy effects:

 * it significantly reduces SRT sizes and consequently the amount of work that
   the GC must do.

 * it avoids increasing code size due to String closures which are ultimately
   in cold paths.

-}

errorCString
    :: forall (r :: RuntimeRep). forall (a :: TYPE r).
       HasCallStack => Addr# -> a
errorCString s = error (unpackCString# s)
{-# NOINLINE errorCString #-}

errorCStringUtf8
    :: forall (r :: RuntimeRep). forall (a :: TYPE r).
       HasCallStack => Addr# -> a
errorCStringUtf8 s = error (unpackCStringUtf8# s)
{-# NOINLINE errorCStringUtf8 #-}

errorWithoutStackTraceCString
    :: forall (r :: RuntimeRep). forall (a :: TYPE r).
       Addr# -> a
errorWithoutStackTraceCString s = errorWithoutStackTrace (unpackCString# s)
{-# NOINLINE errorWithoutStackTraceCString #-}

errorWithoutStackTraceCStringUtf8
    :: forall (r :: RuntimeRep). forall (a :: TYPE r).
       Addr# -> a
errorWithoutStackTraceCStringUtf8 s = errorWithoutStackTrace (unpackCStringUtf8# s)
{-# NOINLINE errorWithoutStackTraceCStringUtf8 #-}

{-# RULES
"errorWithoutStackTrace/unpackCString#"
   forall s. errorWithoutStackTrace (unpackCString# s) = errorWithoutStackTraceCString s
"errorWithoutStackTrace/unpackCStringUtf8#"
   forall s. errorWithoutStackTrace (unpackCStringUtf8# s) = errorWithoutStackTraceCStringUtf8 s

"error/unpackCString#"
   forall s. error (unpackCString# s) = errorCString s

"error/unpackCStringUtf8#"
   forall s. error (unpackCStringUtf8# s) = errorCStringUtf8 s
#-}


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
