%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[HsLit]{Abstract syntax: source-language literals}

\begin{code}
module HsLit where

#include "HsVersions.h"

import {-# SOURCE #-} HsExpr( SyntaxExpr )
import Type	( Type )
import Outputable
import FastString
import Ratio	( Rational )
\end{code}


%************************************************************************
%*									*
\subsection[HsLit]{Literals}
%*									*
%************************************************************************


\begin{code}
data HsLit
  = HsChar	    Char		-- Character
  | HsCharPrim	    Char		-- Unboxed character
  | HsString	    FastString		-- String
  | HsStringPrim    FastString		-- Packed string
  | HsInt	    Integer		-- Genuinely an Int; arises from TcGenDeriv, 
					--	and from TRANSLATION
  | HsIntPrim	    Integer		-- Unboxed Int
  | HsInteger	    Integer  Type	-- Genuinely an integer; arises only from TRANSLATION
					-- 	(overloaded literals are done with HsOverLit)
  | HsRat	    Rational Type	-- Genuinely a rational; arises only from TRANSLATION
					-- 	(overloaded literals are done with HsOverLit)
  | HsFloatPrim	    Rational		-- Unboxed Float
  | HsDoublePrim    Rational		-- Unboxed Double

instance Eq HsLit where
  (HsChar x1)	    == (HsChar x2)	 = x1==x2
  (HsCharPrim x1)   == (HsCharPrim x2)	 = x1==x2
  (HsString x1)     == (HsString x2)	 = x1==x2
  (HsStringPrim x1) == (HsStringPrim x2) = x1==x2
  (HsInt x1)	    == (HsInt x2)	 = x1==x2
  (HsIntPrim x1)    == (HsIntPrim x2)    = x1==x2
  (HsInteger x1 _)  == (HsInteger x2 _)  = x1==x2
  (HsRat x1 _)	    == (HsRat x2 _)      = x1==x2
  (HsFloatPrim x1)  == (HsFloatPrim x2)  = x1==x2
  (HsDoublePrim x1) == (HsDoublePrim x2) = x1==x2
  lit1		    == lit2		 = False

data HsOverLit id 	-- An overloaded literal
  = HsIntegral	 Integer  (SyntaxExpr id)	-- Integer-looking literals;
  | HsFractional Rational (SyntaxExpr id)	-- Frac-looking literals
  -- Before type checking, the SyntaxExpr is 'fromInteger' or 'fromRational'
  -- After type checking, it is (fromInteger 3) or lit_78; that is,
  -- the expression that should replace the literal.
  -- This is unusual, because we're replacing 'fromInteger' with a call 
  -- to fromInteger.  Reason: it allows commoning up of the fromInteger
  -- calls, which wouldn't be possible if the desguarar made the application

-- Comparison operations are needed when grouping literals
-- for compiling pattern-matching (module MatchLit)
instance Eq (HsOverLit id) where
  (HsIntegral i1 _)   == (HsIntegral i2 _)   = i1 == i2
  (HsFractional f1 _) == (HsFractional f2 _) = f1 == f2
  l1		      == l2		     = False

instance Ord (HsOverLit id) where
  compare (HsIntegral i1 _)   (HsIntegral i2 _)   = i1 `compare` i2
  compare (HsIntegral _ _)    (HsFractional _ _)  = LT
  compare (HsFractional f1 _) (HsFractional f2 _) = f1 `compare` f2
  compare (HsFractional f1 _) (HsIntegral _ _)    = GT
\end{code}

\begin{code}
instance Outputable HsLit where
	-- Use "show" because it puts in appropriate escapes
    ppr (HsChar c)	 = pprHsChar c
    ppr (HsCharPrim c)	 = pprHsChar c <> char '#'
    ppr (HsString s)	 = pprHsString s
    ppr (HsStringPrim s) = pprHsString s <> char '#'
    ppr (HsInt i)	 = integer i
    ppr (HsInteger i _)	 = integer i
    ppr (HsRat f _)	 = rational f
    ppr (HsFloatPrim f)	 = rational f <> char '#'
    ppr (HsDoublePrim d) = rational d <> text "##"
    ppr (HsIntPrim i)	 = integer i  <> char '#'

instance Outputable (HsOverLit id) where
  ppr (HsIntegral i _)   = integer i
  ppr (HsFractional f _) = rational f
\end{code}
