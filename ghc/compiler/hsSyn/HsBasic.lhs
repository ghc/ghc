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
\end{code}

%************************************************************************
%*									*
\subsection[Version]{Module and identifier version numbers}
%*									*
%************************************************************************

\begin{code}
type Version = Int
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
    ppr sty (HsChar c)		= ppStr (show c)
    ppr sty (HsCharPrim c)	= ppBeside (ppStr (show c)) (ppChar '#')
    ppr sty (HsString s)	= ppStr (show s)
    ppr sty (HsStringPrim s)	= ppBeside (ppStr (show s)) (ppChar '#')
    ppr sty (HsInt i)		= ppInteger i
    ppr sty (HsFrac f)		= ppRational f
    ppr sty (HsFloatPrim f)	= ppBeside (ppRational f) (ppChar '#')
    ppr sty (HsDoublePrim d)	= ppBeside (ppRational d) (ppStr "##")
    ppr sty (HsIntPrim i)	= ppBeside (ppInteger i) (ppChar '#')
    ppr sty (HsLitLit s)	= ppBesides [ppStr "``", ppPStr s, ppStr "''"]
\end{code}

%************************************************************************
%*									*
\subsection[Fixity]{Fixity info}
%*									*
%************************************************************************

\begin{code}
data Fixity = Fixity Int FixityDirection
data FixityDirection = InfixL | InfixR | InfixN 
		     deriving(Eq)

instance Outputable Fixity where
    ppr sty (Fixity prec dir) = ppBesides [ppr sty dir, ppSP, ppInt prec]

instance Outputable FixityDirection where
    ppr sty InfixL = ppPStr SLIT("infixl")
    ppr sty InfixR = ppPStr SLIT("infixr")
    ppr sty InfixN = ppPStr SLIT("infix")

instance Eq Fixity where		-- Used to determine if two fixities conflict
  (Fixity p1 dir1) == (Fixity p2 dir2) = p1==p2 && dir1 == dir2
\end{code}

