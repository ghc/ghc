%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[HsLit]{Abstract syntax: source-language literals}

\begin{code}
module HsLit where

#include "HsVersions.h"

import Type	( Type )	
import Outputable
import Ratio	( Rational )
\end{code}


%************************************************************************
%*									*
\subsection[HsLit]{Literals}
%*									*
%************************************************************************


\begin{code}
data HsLit
  = HsChar	    Int			-- Character
  | HsCharPrim	    Int			-- Unboxed character
  | HsString	    FAST_STRING		-- String
  | HsStringPrim    FAST_STRING		-- Packed string
  | HsInt	    Integer		-- Genuinely an Int; arises from TcGenDeriv, 
					--	and from TRANSLATION
  | HsIntPrim	    Integer		-- Unboxed Int
  | HsInteger	    Integer		-- Genuinely an integer; arises only from TRANSLATION
  | HsRat	    Rational Type	-- Genuinely a rational; arises only from TRANSLATION
  | HsFloatPrim	    Rational		-- Unboxed Float
  | HsDoublePrim    Rational		-- Unboxed Double
  | HsLitLit	    FAST_STRING Type	-- to pass ``literal literals'' through to C
					-- also: "overloaded" type; but
					-- must resolve to boxed-primitive!
	-- The Type in HsLitLit is needed when desuaring;
	-- before the typechecker it's just an error value
  deriving( Eq )

data HsOverLit name	-- An overloaded literal
  = HsIntegral	    Integer name	-- Integer-looking literals;
					-- The names is "fromInteger"
  | HsFractional    Rational name	-- Frac-looking literals
					-- The name is "fromRational"

instance Eq (HsOverLit name) where
  (HsIntegral i1 _)   == (HsIntegral i2 _)   = i1 == i2
  (HsFractional f1 _) == (HsFractional f2 _) = f1 == f2

instance Ord (HsOverLit name) where
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
    ppr (HsInteger i)	 = integer i
    ppr (HsRat f _)	 = rational f
    ppr (HsFloatPrim f)	 = rational f <> char '#'
    ppr (HsDoublePrim d) = rational d <> text "##"
    ppr (HsIntPrim i)	 = integer i  <> char '#'
    ppr (HsLitLit s _)	 = hcat [text "``", ptext s, text "''"]

instance Outputable (HsOverLit name) where
  ppr (HsIntegral i _)   = integer i
  ppr (HsFractional f _) = rational f
\end{code}


