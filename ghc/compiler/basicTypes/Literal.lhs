%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Literal]{@Literal@: Machine literals (unboxed, of course)}

\begin{code}
#include "HsVersions.h"

module Literal (
	Literal(..),

	mkMachInt, mkMachWord,
	literalType, literalPrimRep,
	showLiteral,
	isNoRepLit, isLitLitLit
    ) where

IMP_Ubiq(){-uitous-}
IMPORT_1_3(Ratio)

-- friends:
import PrimRep		( PrimRep(..) ) -- non-abstract
import TysPrim		( getPrimRepInfo, 
			  addrPrimTy, intPrimTy, floatPrimTy,
			  doublePrimTy, charPrimTy, wordPrimTy )

-- others:
import CStrings		( stringToC, charToC, charToEasyHaskell )
import TysWiredIn	( stringTy )
import Pretty		-- pretty-printing stuff
import PprStyle		( PprStyle(..), codeStyle )
import Util		( thenCmp, panic )
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

  | NoRepStr	    FAST_STRING	-- the uncommitted ones
  | NoRepInteger    Integer  Type{-save what we learned in the typechecker-}
  | NoRepRational   Rational Type{-ditto-}

  -- deriving (Eq, Ord): no, don't want to compare Types
  -- The Ord is needed for the FiniteMap used in the lookForConstructor
  -- in SimplEnv.  If you declared that lookForConstructor *ignores*
  -- constructor-applications with LitArg args, then you could get
  -- rid of this Ord.

mkMachInt, mkMachWord :: Integer -> Literal

mkMachInt  x = MachInt x True{-signed-}
mkMachWord x = MachInt x False{-unsigned-}

instance Ord3 Literal where
    cmp (MachChar      a)   (MachChar	   b)   = a `tcmp` b
    cmp (MachStr       a)   (MachStr	   b)   = a `tcmp` b
    cmp (MachAddr      a)   (MachAddr	   b)   = a `tcmp` b
    cmp (MachInt       a b) (MachInt	   c d) = (a `tcmp` c) `thenCmp` (b `tcmp` d)
    cmp (MachFloat     a)   (MachFloat	   b)   = a `tcmp` b
    cmp (MachDouble    a)   (MachDouble	   b)   = a `tcmp` b
    cmp (MachLitLit    a b) (MachLitLit    c d) = (a `tcmp` c) `thenCmp` (b `tcmp` d)
    cmp (NoRepStr      a)   (NoRepStr	   b)   = a `tcmp` b
    cmp (NoRepInteger  a _) (NoRepInteger  b _) = a `tcmp` b
    cmp (NoRepRational a _) (NoRepRational b _) = a `tcmp` b

      -- now we *know* the tags are different, so...
    cmp other_1 other_2
      | tag1 _LT_ tag2 = LT_
      | otherwise      = GT_
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
    
tcmp x y = case _tagCmp x y of { _LT -> LT_; _EQ -> EQ_; GT__ -> GT_ }

instance Eq Literal where
    a == b = case (a `cmp` b) of { EQ_ -> True;   _ -> False }
    a /= b = case (a `cmp` b) of { EQ_ -> False;  _ -> True  }

instance Ord Literal where
    a <= b = case (a `cmp` b) of { LT_ -> True;  EQ_ -> True;  GT__ -> False }
    a <	 b = case (a `cmp` b) of { LT_ -> True;  EQ_ -> False; GT__ -> False }
    a >= b = case (a `cmp` b) of { LT_ -> False; EQ_ -> True;  GT__ -> True  }
    a >	 b = case (a `cmp` b) of { LT_ -> False; EQ_ -> False; GT__ -> True  }
    _tagCmp a b = case (a `cmp` b) of { LT_ -> _LT; EQ_ -> _EQ; GT__ -> _GT }
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
ppCast :: PprStyle -> FAST_STRING -> Pretty
ppCast PprForC cast = ppPStr cast
ppCast _       _    = ppNil

instance Outputable Literal where
    ppr sty (MachChar ch)
      = let
	    char_encoding
	      = case sty of
		  PprForC  	-> charToC ch
		  PprForAsm _ _ -> charToC ch
		  PprUnfolding	-> charToEasyHaskell ch
		  _		-> [ch]
	in
	ppBeside (ppBesides [ppCast sty SLIT("(C_)"), ppChar '\'', ppStr char_encoding, ppChar '\''])
		 (if_ubxd sty)

    ppr sty (MachStr s)
      = ppBeside (if codeStyle sty
		  then ppBesides [ppChar '"', ppStr (stringToC (_UNPK_ s)), ppChar '"']
		  else ppStr (show (_UNPK_ s)))
		 (if_ubxd sty)

    ppr sty (MachAddr p) = ppBesides [ppCast sty SLIT("(void*)"), ppInteger p, if_ubxd sty]
    ppr sty (MachInt i signed)
      | codeStyle sty
      && ((signed     && (i >= toInteger minInt && i <= toInteger maxInt))
       || (not signed && (i >= toInteger 0      && i <= toInteger maxInt)))
      -- ToDo: Think about these ranges!
      = ppBesides [ppInteger i, if_ubxd sty]

      | not (codeStyle sty) -- we'd prefer the code to the error message
      = ppBesides [ppInteger i, if_ubxd sty]

      | otherwise
      = error ("ERROR: Int " ++ show i ++ " out of range [" ++
		show range_min ++ " .. " ++ show maxInt ++ "]\n")
      where
	range_min = if signed then minInt else 0

    ppr sty (MachFloat f)  = ppBesides [ppCast sty SLIT("(StgFloat)"), ppRational f, if_ubxd sty]
    ppr sty (MachDouble d) = ppBesides [ppRational d, if_ubxd sty, if_ubxd sty]

    ppr sty (NoRepInteger i _)
      | codeStyle sty  = ppInteger i
      | ufStyle sty    = ppCat [ppStr "_NOREP_I_", ppInteger i]
      | otherwise      = ppBesides [ppInteger i, ppChar 'I']

    ppr sty (NoRepRational r _)
      | ufStyle sty    = ppCat [ppStr "_NOREP_R_", ppInteger (numerator r), ppInteger (denominator r)]
      | codeStyle sty = panic "ppr.ForC.NoRepRational"
      | otherwise     = ppBesides [ppRational r,  ppChar 'R']

    ppr sty (NoRepStr s)
      | codeStyle sty = ppBesides [ppStr (show (_UNPK_ s))]
      | ufStyle   sty = ppCat [ppStr "_NOREP_S_", ppStr (show (_UNPK_ s))]
      | otherwise     = ppBesides [ppStr (show (_UNPK_ s)), ppChar 'S']

    ppr sty (MachLitLit s k)
      | codeStyle sty = ppPStr s
      | ufStyle   sty = ppBesides [ppStr "``", ppPStr s, ppStr "'' _K_ ", ppr sty k]
      | otherwise     = ppBesides [ppStr "``", ppPStr s, ppStr "''"]

ufStyle PprUnfolding = True
ufStyle _   	     = False

if_ubxd sty = if codeStyle sty then ppNil else ppChar '#'

showLiteral :: PprStyle -> Literal -> String

showLiteral sty lit = ppShow 80 (ppr sty lit)
\end{code}
