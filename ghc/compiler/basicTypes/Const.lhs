%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[Literal]{@Literal@: Machine literals (unboxed, of course)}

\begin{code}
module Const (
	Con(..),
	conType, conPrimRep,
	conOkForApp, conOkForAlt, isWHNFCon, isDataCon,
	conIsTrivial, conIsCheap, conIsDupable, conStrictness, 
	conOkForSpeculation,

	DataCon, PrimOp,	-- For completeness

	-- Defined here
	Literal(..),		-- Exported to ParseIface
	mkMachInt, mkMachWord,
	mkMachInt_safe, mkMachInt64, mkMachWord64,
	mkStrLit,			-- ToDo: rm (not used anywhere)
	isNoRepLit, isLitLitLit,
	literalType, literalPrimRep
    ) where

#include "HsVersions.h"

import TysPrim		( charPrimTy, addrPrimTy, floatPrimTy, doublePrimTy,
			  intPrimTy, wordPrimTy, int64PrimTy, word64PrimTy
			)
import PrimOp		( PrimOp, primOpType, primOpIsDupable,
			  primOpIsCheap, primOpStrictness, primOpOkForSpeculation )
import PrimRep		( PrimRep(..) )
import DataCon		( DataCon, dataConType, dataConTyCon, isNullaryDataCon, dataConRepStrictness )
import TyCon		( isNewTyCon )
import Type		( Type, typePrimRep )
import PprType		( pprParendType )
import Demand		( Demand )
import CStrings		( stringToC, charToC, charToEasyHaskell )

import Outputable
import Util		( thenCmp )

import Ratio 		( numerator, denominator )
\end{code}


%************************************************************************
%*									*
\subsection{The main data type}
%*									*
%************************************************************************

\begin{code}
data Con
  = DataCon  DataCon
  | Literal  Literal
  | PrimOp   PrimOp
  | DEFAULT			-- Used in case clauses
  deriving (Eq, Ord)

-- The Ord is needed for the FiniteMap used in the lookForConstructor
-- in SimplEnv.  If you declared that lookForConstructor *ignores*
-- constructor-applications with LitArg args, then you could get
-- rid of this Ord.

instance Outputable Con where
  ppr (DataCon dc)  = ppr dc
  ppr (Literal lit) = ppr lit
  ppr (PrimOp op)   = ppr op
  ppr DEFAULT       = ptext SLIT("__DEFAULT")

instance Show Con where
  showsPrec p con = showsPrecSDoc p (ppr con)

conType :: Con -> Type
conType (DataCon dc)  = dataConType dc
conType (Literal lit) = literalType lit
conType (PrimOp op)   = primOpType op

conStrictness :: Con -> ([Demand], Bool)
conStrictness (DataCon dc)  = (dataConRepStrictness dc, False)
conStrictness (PrimOp op)   = primOpStrictness op
conStrictness (Literal lit) = ([], False)

conPrimRep :: Con -> PrimRep	-- Only data valued constants
conPrimRep (DataCon dc)  = ASSERT( isNullaryDataCon dc) PtrRep
conPrimRep (Literal lit) = literalPrimRep lit

conOkForApp, conOkForAlt :: Con -> Bool

-- OK for appliation site
conOkForApp (DataCon dc) = not (isNewTyCon (dataConTyCon dc))
conOkForApp (Literal _)  = True
conOkForApp (PrimOp op)  = True
conOkForApp DEFAULT      = False

-- OK for case alternative pattern
conOkForAlt (DataCon dc)  = not (isNewTyCon (dataConTyCon dc))
conOkForAlt (Literal lit) = not (isNoRepLit lit)
conOkForAlt (PrimOp _)    = False
conOkForAlt DEFAULT	  = True

	-- isWHNFCon is false for PrimOps, which contain work
	-- Ditto for newtype constructors, which can occur in the output
	-- of the desugarer, but which will be inlined right away thereafter
isWHNFCon (DataCon dc) = not (isNewTyCon (dataConTyCon dc))
isWHNFCon (Literal _)  = True
isWHNFCon (PrimOp _)   = False

isDataCon (DataCon dc) = True
isDataCon other	       = False

-- conIsTrivial is true for constants we are unconditionally happy to duplicate
-- cf CoreUtils.exprIsTrivial
conIsTrivial (Literal lit) = not (isNoRepLit lit)
conIsTrivial (PrimOp _)    = False
conIsTrivial con	   = True

-- conIsCheap is true for constants whose applications we are willing
-- to duplicate in exchange for some modest gain.  cf CoreUtils.exprIsCheap
conIsCheap (Literal lit) = not (isNoRepLit lit)
conIsCheap (DataCon con) = True
conIsCheap (PrimOp op)   = primOpIsCheap op

-- conIsDupable is true for constants whose applications we are willing
-- to duplicate in different case branches; i.e no issue about loss of
-- work, just space
conIsDupable (Literal lit) = not (isNoRepLit lit)
conIsDupable (DataCon con) = True
conIsDupable (PrimOp op)   = primOpIsDupable op

-- Similarly conOkForSpeculation
conOkForSpeculation (Literal lit) = True
conOkForSpeculation (DataCon con) = True
conOkForSpeculation (PrimOp op)   = primOpOkForSpeculation op
\end{code}


%************************************************************************
%*									*
\subsection{Literals}
%*									*
%************************************************************************

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
  =	------------------
	-- First the primitive guys
    MachChar	Char
  | MachStr	FAST_STRING

  | MachAddr	Integer	-- Whatever this machine thinks is a "pointer"

  | MachInt	Integer	-- For the numeric types, these are
		Bool	-- True <=> signed (Int#); False <=> unsigned (Word#)

  | MachInt64	Integer	-- guaranteed 64-bit versions of the above.
		Bool	-- True <=> signed (Int#); False <=> unsigned (Word#)


  | MachFloat	Rational
  | MachDouble	Rational

  | MachLitLit  FAST_STRING Type	-- Type might be Add# or Int# etc

	------------------
	-- The no-rep guys
  | NoRepStr	    FAST_STRING Type	-- This Type is always String
  | NoRepInteger    Integer     Type	-- This Type is always Integer
  | NoRepRational   Rational    Type	-- This Type is always Rational
			-- We keep these Types in the literal because Rational isn't
			-- (currently) wired in, so we can't conjure up its type out of
			-- thin air.    Integer is, so the type here is really redundant.
\end{code}


\begin{code}
instance Outputable Literal where
    ppr lit = pprLit lit

instance Show Literal where
    showsPrec p lit = showsPrecSDoc p (ppr lit)

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


	Construction
	~~~~~~~~~~~~
\begin{code}
mkMachInt, mkMachWord :: Integer -> Literal

mkMachInt  x = MachInt x True{-signed-}
mkMachWord x = MachInt x False{-unsigned-}

-- check if the int is within range
mkMachInt_safe :: Integer -> Literal
mkMachInt_safe i
 | out_of_range = 
   pprPanic "mkMachInt_safe" 
	    (hsep [text "ERROR: Int ", text (show i), text "out of range",
		   brackets (int minInt <+> text ".." <+> int maxInt)])
 | otherwise = MachInt i True{-signed-}
 where
  out_of_range =
--    i < fromInt minBound ||
    i > fromInt maxInt

mkMachInt64  x = MachInt64 x True{-signed-}
mkMachWord64 x = MachInt64 x False{-unsigned-}

mkStrLit :: String -> Type -> Literal
mkStrLit s ty = NoRepStr (_PK_ s) ty
\end{code}


	Predicates
	~~~~~~~~~~
\begin{code}
isNoRepLit (NoRepStr _ _)     	= True -- these are not primitive typed!
isNoRepLit (NoRepInteger  _ _) 	= True
isNoRepLit (NoRepRational _ _)	= True
isNoRepLit _			= False

isLitLitLit (MachLitLit _ _) = True
isLitLitLit _	    	     = False
\end{code}

	Types
	~~~~~
\begin{code}
literalType :: Literal -> Type
literalType (MachChar _)	  = charPrimTy
literalType (MachStr  _)	  = addrPrimTy
literalType (MachAddr _)	  = addrPrimTy
literalType (MachInt  _ signed)   = if signed then intPrimTy else wordPrimTy
literalType (MachInt64  _ signed) = if signed then int64PrimTy else word64PrimTy
literalType (MachFloat _)	  = floatPrimTy
literalType (MachDouble _)	  = doublePrimTy
literalType (MachLitLit _ ty)	  = ty
literalType (NoRepInteger  _ ty)  = ty
literalType (NoRepRational _ ty)  = ty
literalType (NoRepStr _ ty)	  = ty
\end{code}

\begin{code}
literalPrimRep :: Literal -> PrimRep

literalPrimRep (MachChar _)	  = CharRep
literalPrimRep (MachStr _)	  = AddrRep  -- specifically: "char *"
literalPrimRep (MachAddr  _)	  = AddrRep
literalPrimRep (MachInt _ signed) = if signed then IntRep else WordRep
literalPrimRep (MachInt64 _ signed) = if signed then Int64Rep else Word64Rep
literalPrimRep (MachFloat _)	  = FloatRep
literalPrimRep (MachDouble _)	  = DoubleRep
literalPrimRep (MachLitLit _ ty)  = typePrimRep ty
#ifdef DEBUG
literalPrimRep (NoRepInteger  _ _) = panic "literalPrimRep:NoRepInteger"
literalPrimRep (NoRepRational _ _) = panic "literalPrimRep:NoRepRational"
literalPrimRep (NoRepStr _ _)	   = panic "literalPrimRep:NoRepString"
#endif
\end{code}


	Comparison
	~~~~~~~~~~
\begin{code}
cmpLit (MachChar      a)   (MachChar	   b)   = a `compare` b
cmpLit (MachStr       a)   (MachStr	   b)   = a `compare` b
cmpLit (MachAddr      a)   (MachAddr	   b)   = a `compare` b
cmpLit (MachInt       a b) (MachInt	   c d) = (a `compare` c) `thenCmp` (b `compare` d)
cmpLit (MachFloat     a)   (MachFloat	   b)   = a `compare` b
cmpLit (MachDouble    a)   (MachDouble	   b)   = a `compare` b
cmpLit (MachLitLit    a b) (MachLitLit    c d)  = (a `compare` c) `thenCmp` (b `compare` d)
cmpLit (NoRepStr      a _) (NoRepStr	  b _)  = a `compare` b
cmpLit (NoRepInteger  a _) (NoRepInteger  b _)  = a `compare` b
cmpLit (NoRepRational a _) (NoRepRational b _)  = a `compare` b
cmpLit lit1		   lit2		        | litTag lit1 _LT_ litTag lit2 = LT
					        | otherwise  		       = GT

litTag (MachChar      _)   = ILIT(1)
litTag (MachStr       _)   = ILIT(2)
litTag (MachAddr      _)   = ILIT(3)
litTag (MachInt       _ _) = ILIT(4)
litTag (MachFloat     _)   = ILIT(5)
litTag (MachDouble    _)   = ILIT(6)
litTag (MachLitLit    _ _) = ILIT(7)
litTag (NoRepStr      _ _) = ILIT(8)
litTag (NoRepInteger  _ _) = ILIT(9)
litTag (NoRepRational _ _) = ILIT(10)
\end{code}

	Printing
	~~~~~~~~
* MachX (i.e. unboxed) things are printed unadornded (e.g. 3, 'a', "foo")
  exceptions: MachFloat and MachAddr get an initial keyword prefix

* NoRep things get an initial keyword prefix (e.g. _integer_ 3)

\begin{code}
pprLit lit
  = getPprStyle $ \ sty ->
    let
      code_style = codeStyle sty
    in
    case lit of
      MachChar ch | code_style     -> hcat [ptext SLIT("(C_)"), char '\'', 
					    text (charToC ch), char '\'']
	          | ifaceStyle sty -> char '\'' <> text (charToEasyHaskell ch) <> char '\''
		  | otherwise      -> text ['\'', ch, '\'']

      MachStr s | code_style -> doubleQuotes (text (stringToC (_UNPK_ s)))
	        | otherwise  -> pprFSAsString s


      NoRepStr s ty | code_style -> pprPanic "NoRep in code style" (ppr lit)
	            | otherwise  -> ptext SLIT("__string") <+> pprFSAsString s

      MachInt i signed | code_style && out_of_range 
		       -> pprPanic "" (hsep [text "ERROR: Int ", text (show i), 
					     text "out of range",
				             brackets (ppr range_min <+> text ".." 
							<+> ppr range_max)])
			-- in interface files, parenthesize raw negative ints.
			-- this avoids problems like {-1} being interpreted
			-- as a comment starter.
		       | ifaceStyle sty && i < 0 -> parens (integer i)
		       | otherwise -> integer i

		       where
		        range_min = if signed then minInt else 0
			range_max = maxInt
			out_of_range = not (i >= toInteger range_min && i <= toInteger range_max)

      MachFloat f | code_style -> ptext SLIT("(StgFloat)") <> rational f
                  | otherwise  -> ptext SLIT("__float") <+> rational f

      MachDouble d | ifaceStyle sty && d < 0 -> parens (rational d)
		   | otherwise -> rational d

      MachAddr p | code_style -> ptext SLIT("(void*)") <> integer p
	         | otherwise  -> ptext SLIT("__addr") <+> integer p

      NoRepInteger i _ | code_style -> pprPanic "NoRep in code style" (ppr lit)
		       | otherwise  -> ptext SLIT("__integer") <+> integer i

      NoRepRational r _ | code_style -> pprPanic "NoRep in code style" (ppr lit)
		        | otherwise  -> hsep [ptext SLIT("__rational"), integer (numerator r), 
									integer (denominator r)]

      MachLitLit s ty | code_style -> ptext s
		      | otherwise  -> parens (hsep [ptext SLIT("__litlit"), 
						    pprFSAsString s,
						    pprParendType ty])
\end{code}
