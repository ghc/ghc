\begin{code}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

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

-- #hide
module GHC.Err
       (
         absentErr
       , divZeroError
       , ratioZeroDenominatorError
       , overflowError

       , error

       , undefined
       ) where

import GHC.Types
import GHC.Exception
\end{code}

%*********************************************************
%*                                                      *
\subsection{Error-ish functions}
%*                                                      *
%*********************************************************

\begin{code}
-- | 'error' stops execution and displays an error message.
error :: [Char] -> a
error s = throw (ErrorCall s)

-- | A special case of 'error'.
-- It is expected that compilers will recognize this and insert error
-- messages which are more appropriate to the context in which 'undefined'
-- appears. 

undefined :: a
undefined =  error "Prelude.undefined"
\end{code}

%*********************************************************
%*                                                       *
\subsection{Compiler generated errors + local utils}
%*                                                       *
%*********************************************************

Used for compiler-generated error message;
encoding saves bytes of string junk.

\begin{code}
absentErr :: a

absentErr = error "Oops! The program has entered an `absent' argument!\n"
\end{code}

Divide by zero and arithmetic overflow.
We put them here because they are needed relatively early
in the libraries before the Exception type has been defined yet.

\begin{code}
{-# NOINLINE divZeroError #-}
divZeroError :: a
divZeroError = throw DivideByZero

{-# NOINLINE ratioZeroDenominatorError #-}
ratioZeroDenominatorError :: a
ratioZeroDenominatorError = throw RatioZeroDenominator

{-# NOINLINE overflowError #-}
overflowError :: a
overflowError = throw Overflow
\end{code}

