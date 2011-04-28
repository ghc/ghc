%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[HsLit]{Abstract syntax: source-language literals}

\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}

module HsLit where

#include "HsVersions.h"

import {-# SOURCE #-} HsExpr( SyntaxExpr, pprExpr )
import HsTypes (PostTcType)
import Type	( Type )
import Outputable
import FastString

import Data.Data
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
  | HsWordPrim	    Integer		-- Unboxed Word
  | HsInteger	    Integer  Type	-- Genuinely an integer; arises only from TRANSLATION
					-- 	(overloaded literals are done with HsOverLit)
  | HsRat	    Rational Type	-- Genuinely a rational; arises only from TRANSLATION
					-- 	(overloaded literals are done with HsOverLit)
  | HsFloatPrim	    Rational		-- Unboxed Float
  | HsDoublePrim    Rational		-- Unboxed Double
  deriving (Data, Typeable)

instance Eq HsLit where
  (HsChar x1)	    == (HsChar x2)	 = x1==x2
  (HsCharPrim x1)   == (HsCharPrim x2)	 = x1==x2
  (HsString x1)     == (HsString x2)	 = x1==x2
  (HsStringPrim x1) == (HsStringPrim x2) = x1==x2
  (HsInt x1)	    == (HsInt x2)	 = x1==x2
  (HsIntPrim x1)    == (HsIntPrim x2)    = x1==x2
  (HsWordPrim x1)   == (HsWordPrim x2)   = x1==x2
  (HsInteger x1 _)  == (HsInteger x2 _)  = x1==x2
  (HsRat x1 _)	    == (HsRat x2 _)      = x1==x2
  (HsFloatPrim x1)  == (HsFloatPrim x2)  = x1==x2
  (HsDoublePrim x1) == (HsDoublePrim x2) = x1==x2
  _                 == _                 = False

data HsOverLit id 	-- An overloaded literal
  = OverLit {
	ol_val :: OverLitVal, 
	ol_rebindable :: Bool,		-- 
	ol_witness :: SyntaxExpr id,	-- Note [Overloaded literal witnesses]
	ol_type :: PostTcType }
  deriving (Data, Typeable)

data OverLitVal
  = HsIntegral   !Integer   	-- Integer-looking literals;
  | HsFractional !Rational   	-- Frac-looking literals
  | HsIsString   !FastString 	-- String-looking literals
  deriving (Data, Typeable)

overLitType :: HsOverLit a -> Type
overLitType = ol_type
\end{code}

Note [ol_rebindable]
~~~~~~~~~~~~~~~~~~~~
The ol_rebindable field is True if this literal is actually 
using rebindable syntax.  Specifically:

  False iff ol_witness is the standard one
  True  iff ol_witness is non-standard

Equivalently it's True if
  a) RebindableSyntax is on
  b) the witness for fromInteger/fromRational/fromString
     that happens to be in scope isn't the standard one

Note [Overloaded literal witnesses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*Before* type checking, the SyntaxExpr in an HsOverLit is the
name of the coercion function, 'fromInteger' or 'fromRational'.
*After* type checking, it is a witness for the literal, such as
	(fromInteger 3) or lit_78
This witness should replace the literal.

This dual role is unusual, because we're replacing 'fromInteger' with 
a call to fromInteger.  Reason: it allows commoning up of the fromInteger
calls, which wouldn't be possible if the desguarar made the application

The PostTcType in each branch records the type the overload literal is
found to have.

\begin{code}
-- Comparison operations are needed when grouping literals
-- for compiling pattern-matching (module MatchLit)
instance Eq (HsOverLit id) where
  (OverLit {ol_val = val1}) == (OverLit {ol_val=val2}) = val1 == val2

instance Eq OverLitVal where
  (HsIntegral i1)   == (HsIntegral i2)   = i1 == i2
  (HsFractional f1) == (HsFractional f2) = f1 == f2
  (HsIsString s1)   == (HsIsString s2)   = s1 == s2
  _                 == _                 = False

instance Ord (HsOverLit id) where
  compare (OverLit {ol_val=val1}) (OverLit {ol_val=val2}) = val1 `compare` val2

instance Ord OverLitVal where
  compare (HsIntegral i1)   (HsIntegral i2)   = i1 `compare` i2
  compare (HsIntegral _)    (HsFractional _)  = LT
  compare (HsIntegral _)    (HsIsString _)    = LT
  compare (HsFractional f1) (HsFractional f2) = f1 `compare` f2
  compare (HsFractional _)  (HsIntegral _)    = GT
  compare (HsFractional _)  (HsIsString _)    = LT
  compare (HsIsString s1)   (HsIsString s2)   = s1 `compare` s2
  compare (HsIsString _)    (HsIntegral _)    = GT
  compare (HsIsString _)    (HsFractional _)  = GT
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
    ppr (HsWordPrim w)	 = integer w  <> text "##"

-- in debug mode, print the expression that it's resolved to, too
instance OutputableBndr id => Outputable (HsOverLit id) where
  ppr (OverLit {ol_val=val, ol_witness=witness}) 
	= ppr val <+> (ifPprDebug (parens (pprExpr witness)))

instance Outputable OverLitVal where
  ppr (HsIntegral i)   = integer i 
  ppr (HsFractional f) = rational f
  ppr (HsIsString s)   = pprHsString s
\end{code}
