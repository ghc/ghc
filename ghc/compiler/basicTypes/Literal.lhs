%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Literal]{@Literal@: Machine literals (unboxed, of course)}

\begin{code}
module Literal (
	Literal(..),

	mkMachInt, mkMachWord,
	literalType, literalPrimRep,
	showLiteral,
	isNoRepLit, isLitLitLit
    ) where

#include "HsVersions.h"

-- friends:
import PrimRep		( PrimRep(..), ppPrimRep ) -- non-abstract
import TysPrim		( getPrimRepInfo, 
			  addrPrimTy, intPrimTy, floatPrimTy,
			  doublePrimTy, charPrimTy, wordPrimTy
			)

-- others:
import Type		( Type )
import CStrings		( stringToC, charToC, charToEasyHaskell )
import TysWiredIn	( stringTy )
import Outputable
import Util		( thenCmp )

import GlaExts		( (<#) )
\end{code}

So-called @Literals@ are {\em either}:
\begin{itemize}
\item
An unboxed (``machine'') literal (type: @IntPrim@, @FloatPrim@, etc.),
which is presumed to be surrounded by appropriate constructors
(@mKINT@, etc.), so that the overall thing makes sense.
\item
An Integer, Rational, or String literal whose representation we are
{\em uncommitted} about; i.e., the surrounding with constructors,
function applications, etc., etc., has not yet been done.
\end{itemize}

\begin{code}
data Literal
  = MachChar	Char
  | MachStr	FAST_STRING

  | MachAddr	Integer	-- whatever this machine thinks is a "pointer"

  | MachInt	Integer	-- for the numeric types, these are
		Bool	-- True <=> signed (Int#); False <=> unsigned (Word#)

  | MachFloat	Rational
  | MachDouble	Rational

  | MachLitLit  FAST_STRING
		PrimRep

  | NoRepStr	    FAST_STRING
  | NoRepInteger    Integer  Type	-- This Type is always Integer
  | NoRepRational   Rational Type	-- This Type is always Rational
			-- We keep these Types in the literal because Rational isn't
			-- (currently) wired in, so we can't conjure up its type out of
			-- thin air.    Integer is, so the type here is really redundant.

  -- deriving (Eq, Ord): no, don't want to compare Types
  -- The Ord is needed for the FiniteMap used in the lookForConstructor
  -- in SimplEnv.  If you declared that lookForConstructor *ignores*
  -- constructor-applications with LitArg args, then you could get
  -- rid of this Ord.

mkMachInt, mkMachWord :: Integer -> Literal

mkMachInt  x = MachInt x True{-signed-}
mkMachWord x = MachInt x False{-unsigned-}

cmpLit (MachChar      a)   (MachChar	   b)   = a `compare` b
cmpLit (MachStr       a)   (MachStr	   b)   = a `compare` b
cmpLit (MachAddr      a)   (MachAddr	   b)   = a `compare` b
cmpLit (MachInt       a b) (MachInt	   c d) = (a `compare` c) `thenCmp` (b `compare` d)
cmpLit (MachFloat     a)   (MachFloat	   b)   = a `compare` b
cmpLit (MachDouble    a)   (MachDouble	   b)   = a `compare` b
cmpLit (MachLitLit    a b) (MachLitLit    c d) = (a `compare` c) `thenCmp` (b `compare` d)
cmpLit (NoRepStr      a)   (NoRepStr	   b)   = a `compare` b
cmpLit (NoRepInteger  a _) (NoRepInteger  b _) = a `compare` b
cmpLit (NoRepRational a _) (NoRepRational b _) = a `compare` b

  -- now we *know* the tags are different, so...
cmpLit other_1 other_2
  | tag1 _LT_ tag2 = LT
  | otherwise      = GT
  where
    tag1 = tagof other_1
    tag2 = tagof other_2

    tagof (MachChar      _)	  = ILIT(1)
    tagof (MachStr       _)	  = ILIT(2)
    tagof (MachAddr      _)	  = ILIT(3)
    tagof (MachInt       _ _) = ILIT(4)
    tagof (MachFloat     _)	  = ILIT(5)
    tagof (MachDouble    _)	  = ILIT(6)
    tagof (MachLitLit    _ _) = ILIT(7)
    tagof (NoRepStr      _)	  = ILIT(8)
    tagof (NoRepInteger  _ _) = ILIT(9)
    tagof (NoRepRational _ _) = ILIT(10)
    
instance Eq Literal where
    a == b = case (a `compare` b) of { EQ -> True;   _ -> False }
    a /= b = case (a `compare` b) of { EQ -> False;  _ -> True  }

instance Ord Literal where
    a <= b = case (a `compare` b) of { LT -> True;  EQ -> True;  GT -> False }
    a <	 b = case (a `compare` b) of { LT -> True;  EQ -> False; GT -> False }
    a >= b = case (a `compare` b) of { LT -> False; EQ -> True;  GT -> True  }
    a >	 b = case (a `compare` b) of { LT -> False; EQ -> False; GT -> True  }
    compare a b = cmpLit a b
\end{code}

\begin{code}
isNoRepLit (NoRepStr _)     	= True -- these are not primitive typed!
isNoRepLit (NoRepInteger  _ _) 	= True
isNoRepLit (NoRepRational _ _)	= True
isNoRepLit _			= False

isLitLitLit (MachLitLit _ _) = True
isLitLitLit _	    	     = False
\end{code}

\begin{code}
literalType :: Literal -> Type

literalType (MachChar _)	= charPrimTy
literalType (MachStr  _)	= addrPrimTy
literalType (MachAddr _)	= addrPrimTy
literalType (MachInt  _ signed) = if signed then intPrimTy else wordPrimTy
literalType (MachFloat _)	= floatPrimTy
literalType (MachDouble _)	= doublePrimTy
literalType (MachLitLit _ k)	= case (getPrimRepInfo k) of { (_,t,_) -> t }
literalType (NoRepInteger  _ t)	= t
literalType (NoRepRational _ t) = t
literalType (NoRepStr _)	= stringTy
\end{code}

\begin{code}
literalPrimRep :: Literal -> PrimRep

literalPrimRep (MachChar _)	= CharRep
literalPrimRep (MachStr _)	= AddrRep  -- specifically: "char *"
literalPrimRep (MachAddr  _)	= AddrRep
literalPrimRep (MachInt _ signed) = if signed then IntRep else WordRep
literalPrimRep (MachFloat _)	= FloatRep
literalPrimRep (MachDouble _)	= DoubleRep
literalPrimRep (MachLitLit _ k)	= k
#ifdef DEBUG
literalPrimRep (NoRepInteger  _ _) = panic "literalPrimRep:NoRepInteger"
literalPrimRep (NoRepRational _ _) = panic "literalPrimRep:NoRepRational"
literalPrimRep (NoRepStr _)	   = panic "literalPrimRep:NoRepString"
#endif
\end{code}

The boring old output stuff:
\begin{code}
-- MachX (i.e. unboxed) things are printed unadornded (e.g. 3, 'a', "foo")
-- 	exceptions: MachFloat and MachAddr get an initial keyword prefix
--
-- NoRep things get an initial keyword prefix (e.g. _integer_ 3)

instance Outputable Literal where
    ppr lit = pprLit lit

pprLit lit
  = getPprStyle $ \ sty ->
    let
      code_style = codeStyle sty
    in
    case lit of
      MachChar ch | code_style     -> hcat [ptext SLIT("(C_)"), char '\'', text (charToC ch), char '\'']
	          | ifaceStyle sty -> char '\'' <> text (charToEasyHaskell ch) <> char '\''
		  | otherwise      -> text ['\'', ch, '\'']

      MachStr s | code_style -> doubleQuotes (text (stringToC (_UNPK_ s)))
	        | otherwise  -> text (show (_UNPK_ s))

      NoRepStr s | code_style -> pprPanic "NoRep in code style" (ppr lit)
	         | otherwise  -> ptext SLIT("_string_") <+> text (show (_UNPK_ s))

      MachInt i signed | code_style && out_of_range 
		       -> pprPanic "" (hsep [text "ERROR: Int ", text (show i), text "out of range",
				             brackets (ppr range_min <+> text ".." <+> ppr range_max)])
		       | otherwise -> integer i

		       where
		        range_min = if signed then minInt else 0
			range_max = maxInt
			out_of_range = not (i >= toInteger range_min && i <= toInteger range_max)

      MachFloat f | code_style -> ptext SLIT("(StgFloat)") <> rational f
                  | otherwise  -> ptext SLIT("_float_") <+> rational f

      MachDouble d -> rational d

      MachAddr p | code_style -> ptext SLIT("(void*)") <> integer p
	         | otherwise  -> ptext SLIT("_addr_") <+> integer p

      NoRepInteger i _ | code_style -> pprPanic "NoRep in code style" (ppr lit)
		       | otherwise  -> ptext SLIT("_integer_") <+> integer i

      NoRepRational r _ | code_style -> pprPanic "NoRep in code style" (ppr lit)
		        | otherwise  -> hsep [ptext SLIT("_rational_"), integer (numerator r), 
									integer (denominator r)]

      MachLitLit s k | code_style -> ptext s
		     | otherwise  -> hsep [ptext SLIT("_litlit_"), ppPrimRep k, text (show (_UNPK_ s))]

showLiteral :: Literal -> String
showLiteral lit = showSDoc (ppr lit)
\end{code}

