%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[BasicLit]{@BasicLit@: Machine literals (unboxed, of course)}

\begin{code}
#include "HsVersions.h"

module BasicLit (
	BasicLit(..),
	mkMachInt, mkMachWord,
	typeOfBasicLit, kindOfBasicLit,
	showBasicLit,
	isNoRepLit, isLitLitLit,

	-- and to make the interface self-sufficient....
	UniType, PrimKind
    ) where

import AbsPrel		( addrPrimTy, intPrimTy, floatPrimTy, doublePrimTy,
			  charPrimTy, wordPrimTy,
			  integerTy, rationalTy, stringTy, UniType,
			  TauType(..)
			  IF_ATTACK_PRAGMAS(COMMA mkListTy COMMA charTy)
			  IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
			)
import AbsUniType	( TyCon IF_ATTACK_PRAGMAS(COMMA cmpTyCon) )
import PrimKind		( getKindInfo ) -- ToDo: *** HACK import ****
import CLabelInfo	( stringToC, charToC, charToEasyHaskell )
import Outputable	-- class for printing, forcing
import Pretty		-- pretty-printing stuff
import PrimKind		( PrimKind(..) )
import Util
\end{code}

So-called @BasicLits@ are {\em either}:
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
data BasicLit
  = MachChar	Char
  | MachStr	FAST_STRING
  | MachAddr	Integer	-- whatever this machine thinks is a "pointer"
  | MachInt	Integer	-- for the numeric types, these are
		Bool	-- True <=> signed (Int#); False <=> unsigned (Word#) 
  | MachFloat	Rational
  | MachDouble	Rational
  | MachLitLit  FAST_STRING
		PrimKind

  | NoRepStr	    FAST_STRING	-- the uncommitted ones
  | NoRepInteger    Integer
  | NoRepRational   Rational

  deriving (Eq, Ord)
  -- The Ord is needed for the FiniteMap used in the lookForConstructor
  -- in SimplEnv.  If you declared that lookForConstructor *ignores*
  -- constructor-applications with CoLitAtom args, then you could get
  -- rid of this Ord.

mkMachInt, mkMachWord :: Integer -> BasicLit

mkMachInt  x = MachInt x True{-signed-}
mkMachWord x = MachInt x False{-unsigned-}
\end{code}

\begin{code}
isNoRepLit (NoRepStr _)     	= True -- these are not primitive typed!
isNoRepLit (NoRepInteger _) 	= True
isNoRepLit (NoRepRational _)	= True
isNoRepLit _			= False

isLitLitLit (MachLitLit _ _) = True
isLitLitLit _	    	     = False
\end{code}

\begin{code}
typeOfBasicLit :: BasicLit -> UniType

typeOfBasicLit (MachChar _)	= charPrimTy
typeOfBasicLit (MachStr  _)	= addrPrimTy
typeOfBasicLit (MachAddr _)	= addrPrimTy
typeOfBasicLit (MachInt  _ signed) = if signed then intPrimTy else wordPrimTy
typeOfBasicLit (MachFloat _)	= floatPrimTy
typeOfBasicLit (MachDouble _)	= doublePrimTy
typeOfBasicLit (MachLitLit _ k)	= case (getKindInfo k) of { (_,t,_) -> t }
typeOfBasicLit (NoRepInteger _)	= integerTy
typeOfBasicLit (NoRepRational _)= rationalTy
typeOfBasicLit (NoRepStr _)	= stringTy
\end{code}

\begin{code}
kindOfBasicLit :: BasicLit -> PrimKind

kindOfBasicLit (MachChar _)	= CharKind
kindOfBasicLit (MachStr _)	= AddrKind  -- specifically: "char *"
kindOfBasicLit (MachAddr  _)	= AddrKind
kindOfBasicLit (MachInt _ signed) = if signed then IntKind else WordKind
kindOfBasicLit (MachFloat _)	= FloatKind
kindOfBasicLit (MachDouble _)	= DoubleKind
kindOfBasicLit (MachLitLit _ k)	= k
kindOfBasicLit (NoRepInteger _)	= panic "kindOfBasicLit:NoRepInteger"
kindOfBasicLit (NoRepRational _)= panic "kindOfBasicLit:NoRepRational"
kindOfBasicLit (NoRepStr _)	= panic "kindOfBasicLit:NoRepString"
\end{code}

The boring old output stuff:
\begin{code}
ppCast :: PprStyle -> FAST_STRING -> Pretty
ppCast (PprForC _) cast = ppPStr cast
ppCast _           _    = ppNil

instance Outputable BasicLit where
    ppr sty (MachChar ch)
      = let
	    char_encoding
	      = case sty of
		  PprForC _ 	 -> charToC ch
		  PprForAsm _ _ _ -> charToC ch
		  PprUnfolding _ -> charToEasyHaskell ch
		  _ 	    	 -> [ch]
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

#ifdef DPH
    -- I know that this thing shouldnt pop out of the compiler, but the
    -- native code generator tries to generate code to initilialise a closure
    -- with this value... (in glaExts/PreludeGlaInOut.lhs)
    ppr sty MachVoid		= ppStr "0 ! {- void# -}"
#endif {- Data Parallel Haskell -}
    
    ppr sty (NoRepInteger i)
      | codeStyle sty  = ppInteger i
      | ufStyle sty    = ppCat [ppStr "_NOREP_I_", ppInteger i]
      | otherwise      = ppBesides [ppInteger i, ppChar 'I']

    ppr sty (NoRepRational r)
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

ufStyle (PprUnfolding _) = True
ufStyle _   	    	 = False

if_ubxd sty = if codeStyle sty then ppNil else ppChar '#'

showBasicLit :: PprStyle -> BasicLit -> String

showBasicLit sty lit = ppShow 80 (ppr sty lit)
\end{code}
