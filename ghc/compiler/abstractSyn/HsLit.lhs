%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
%
\section[HsLit]{Abstract syntax: source-language literals}

\begin{code}
#include "HsVersions.h"

module HsLit where

import AbsPrel		( PrimKind )
import Outputable
import Pretty
import Util
\end{code}

\begin{code}
data Literal
  = CharLit	    Char	-- characters
  | CharPrimLit	    Char	-- unboxed char literals
  | StringLit	    FAST_STRING	-- strings
  | StringPrimLit   FAST_STRING	-- packed string

  | IntLit	    Integer	-- integer-looking literals
  | FracLit	    Rational	-- frac-looking literals
	-- Up through dict-simplification, IntLit and FracLit simply
	-- mean the literal was integral- or fractional-looking; i.e.,
	-- whether it had an explicit decimal-point in it.  *After*
	-- dict-simplification, they mean (boxed) "Integer" and
	-- "Rational" [Ratio Integer], respectively.

	-- Dict-simplification tries to replace such lits w/ more
	-- specific ones, using the unboxed variants that follow...
  | LitLitLitIn	    FAST_STRING	-- to pass ``literal literals'' through to C
				-- also: "overloaded" type; but
				-- must resolve to boxed-primitive!
				-- (WDP 94/10)
  | LitLitLit	    FAST_STRING
		    UniType	-- and now we know the type
				-- Must be a boxed-primitive type

  | IntPrimLit	    Integer	-- unboxed Int literals
#if __GLASGOW_HASKELL__ <= 22
  | FloatPrimLit    Double	-- unboxed Float literals
  | DoublePrimLit   Double	-- unboxed Double literals
#else
  | FloatPrimLit    Rational	-- unboxed Float literals
  | DoublePrimLit   Rational	-- unboxed Double literals
#endif
\end{code}

\begin{code}
negLiteral (IntLit  i) = IntLit  (-i)
negLiteral (FracLit f) = FracLit (-f)
\end{code}

\begin{code}
instance Outputable Literal where
    ppr sty (CharLit c)		= ppStr (show c)
    ppr sty (CharPrimLit c)	= ppBeside (ppStr (show c)) (ppChar '#')
    ppr sty (StringLit s)	= ppStr (show s)
    ppr sty (StringPrimLit s)	= ppBeside (ppStr (show s)) (ppChar '#')
    ppr sty (IntLit i)		= ppInteger i
#if __GLASGOW_HASKELL__ <= 22
    ppr sty (FracLit f)		= ppDouble (fromRational f) -- ToDo: better??
    ppr sty (FloatPrimLit f)	= ppBeside (ppDouble f) (ppChar '#')
    ppr sty (DoublePrimLit d)	= ppBeside (ppDouble d) (ppStr "##")
#else
    ppr sty (FracLit f)		= ppRational f
    ppr sty (FloatPrimLit f)	= ppBeside (ppRational f) (ppChar '#')
    ppr sty (DoublePrimLit d)	= ppBeside (ppRational d) (ppStr "##")
#endif
    ppr sty (IntPrimLit i)	= ppBeside (ppInteger i) (ppChar '#')
    ppr sty (LitLitLitIn s)	= ppBesides [ppStr "``", ppPStr s, ppStr "''"]
    ppr sty (LitLitLit s k)	= ppBesides [ppStr "``", ppPStr s, ppStr "''"]
\end{code}
