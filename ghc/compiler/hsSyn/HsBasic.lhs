%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[HsLit]{Abstract syntax: source-language literals}

\begin{code}
#include "HsVersions.h"

module HsBasic where

IMP_Ubiq(){-uitous-}

IMPORT_1_3(Ratio(Rational))

import Pretty
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection[HsLit]{Literals}
%*									*
%************************************************************************


\begin{code}
data HsLit
  = HsChar	    Char	-- characters
  | HsCharPrim	    Char	-- unboxed char literals
  | HsString	    FAST_STRING	-- strings
  | HsStringPrim    FAST_STRING	-- packed string

  | HsInt	    Integer	-- integer-looking literals
  | HsFrac	    Rational	-- frac-looking literals
	-- Up through dict-simplification, HsInt and HsFrac simply
	-- mean the literal was integral- or fractional-looking; i.e.,
	-- whether it had an explicit decimal-point in it.  *After*
	-- dict-simplification, they mean (boxed) "Integer" and
	-- "Rational" [Ratio Integer], respectively.

	-- Dict-simplification tries to replace such lits w/ more
	-- specific ones, using the unboxed variants that follow...
  | HsIntPrim	    Integer	-- unboxed Int literals
  | HsFloatPrim	    Rational	-- unboxed Float literals
  | HsDoublePrim    Rational	-- unboxed Double literals

  | HsLitLit	    FAST_STRING	-- to pass ``literal literals'' through to C
				-- also: "overloaded" type; but
				-- must resolve to boxed-primitive!
				-- (WDP 94/10)
\end{code}

\begin{code}
negLiteral (HsInt  i) = HsInt  (-i)
negLiteral (HsFrac f) = HsFrac (-f)
\end{code}

\begin{code}
instance Outputable HsLit where
    ppr sty (HsChar c)		= text (show c)
    ppr sty (HsCharPrim c)	= (<>) (text (show c)) (char '#')
    ppr sty (HsString s)	= text (show s)
    ppr sty (HsStringPrim s)	= (<>) (text (show s)) (char '#')
    ppr sty (HsInt i)		= integer i
    ppr sty (HsFrac f)		= rational f
    ppr sty (HsFloatPrim f)	= (<>) (rational f) (char '#')
    ppr sty (HsDoublePrim d)	= (<>) (rational d) (text "##")
    ppr sty (HsIntPrim i)	= (<>) (integer i) (char '#')
    ppr sty (HsLitLit s)	= hcat [text "``", ptext s, text "''"]
\end{code}


