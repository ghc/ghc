{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash, ImplicitParams #-}
{-# LANGUAGE RankNTypes, PolyKinds, DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Err
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
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

module GHC.Internal.Err( absentErr, error, errorWithoutStackTrace, undefined ) where
import {-# SOURCE #-} GHC.Internal.Exception
  ( error, errorWithoutStackTrace, undefined )
import GHC.Internal.Types
import GHC.Internal.Stack.Types


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


-- | Used for compiler-generated error message;
-- encoding saves bytes of string junk.
absentErr :: a
absentErr = errorWithoutStackTrace "Oops! The program has entered an `absent' argument!\n"
