%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[Literal]{@Literal@: Machine literals (unboxed, of course)}

\begin{code}
module Literal
	( Literal(..)		-- Exported to ParseIface
	, mkMachInt, mkMachWord
	, mkMachInt64, mkMachWord64
	, isLitLitLit, maybeLitLit
	, literalType, literalPrimRep
	, hashLiteral

	, inIntRange, inWordRange

	, word2IntLit, int2WordLit, char2IntLit, int2CharLit
	, float2IntLit, int2FloatLit, double2IntLit, int2DoubleLit
	, addr2IntLit, int2AddrLit, float2DoubleLit, double2FloatLit
	) where

#include "HsVersions.h"

import TysPrim		( charPrimTy, addrPrimTy, floatPrimTy, doublePrimTy,
			  intPrimTy, wordPrimTy, int64PrimTy, word64PrimTy
			)
import Name		( hashName )
import PrimRep		( PrimRep(..) )
import TyCon		( isNewTyCon )
import Type		( Type, typePrimRep )
import PprType		( pprParendType )
import Demand		( Demand )
import CStrings		( charToC, charToEasyHaskell, pprFSInCStyle )

import Outputable
import Util		( thenCmp )

import Ratio 		( numerator, denominator )
import FastString	( uniqueOfFS )
import Char		( ord, chr )
\end{code}



%************************************************************************
%*									*
\subsection{Sizes}
%*									*
%************************************************************************

If we're compiling with GHC (and we're not cross-compiling), then we
know that minBound and maxBound :: Int are the right values for the
target architecture.  Otherwise, we assume -2^31 and 2^31-1
respectively (which will be wrong on a 64-bit machine).

\begin{code}
tARGET_MIN_INT, tARGET_MAX_INT, tARGET_MAX_WORD :: Integer
#if __GLASGOW_HASKELL__
tARGET_MIN_INT  = toInteger (minBound :: Int)
tARGET_MAX_INT  = toInteger (maxBound :: Int)
#else
tARGET_MIN_INT = -2147483648
tARGET_MAX_INT =  2147483647
#endif
tARGET_MAX_WORD = (tARGET_MAX_INT * 2) + 1
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

  | MachInt	Integer		-- Int#		At least 32 bits
  | MachInt64	Integer		-- Int64#	At least 64 bits
  | MachWord	Integer		-- Word#	At least 32 bits
  | MachWord64	Integer		-- Word64#	At least 64 bits

  | MachFloat	Rational
  | MachDouble	Rational

  | MachLitLit  FAST_STRING Type	-- Type might be Add# or Int# etc
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
mkMachInt, mkMachWord, mkMachInt64, mkMachWord64 :: Integer -> Literal

mkMachInt  x   = ASSERT2( inIntRange x,  integer x ) MachInt x
mkMachWord x   = ASSERT2( inWordRange x, integer x ) MachWord x
mkMachInt64  x = MachInt64 x	-- Assertions?
mkMachWord64 x = MachWord64 x	-- Ditto?

inIntRange, inWordRange :: Integer -> Bool
inIntRange  x = x >= tARGET_MIN_INT && x <= tARGET_MAX_INT
inWordRange x = x >= 0		    && x <= tARGET_MAX_WORD
\end{code}

	Coercions
	~~~~~~~~~
\begin{code}
word2IntLit, int2WordLit, char2IntLit, int2CharLit,
 float2IntLit, int2FloatLit, double2IntLit, int2DoubleLit,
 addr2IntLit, int2AddrLit, float2DoubleLit, double2FloatLit :: Literal -> Literal

word2IntLit (MachWord w) 
  | w > tARGET_MAX_INT = MachInt ((-1) + tARGET_MAX_WORD - w)
  | otherwise	       = MachInt w

int2WordLit (MachInt i)
  | i < 0     = MachWord (1 + tARGET_MAX_WORD + i)	-- (-1)  --->  tARGET_MAX_WORD
  | otherwise = MachWord i

char2IntLit (MachChar c) = MachInt  (toInteger (ord c))
int2CharLit (MachInt  i) = MachChar (chr (fromInteger i))

float2IntLit (MachFloat f) = MachInt   (truncate    f)
int2FloatLit (MachInt   i) = MachFloat (fromInteger i)

double2IntLit (MachFloat f) = MachInt    (truncate    f)
int2DoubleLit (MachInt   i) = MachDouble (fromInteger i)

addr2IntLit (MachAddr a) = MachInt  a
int2AddrLit (MachInt  i) = MachAddr i

float2DoubleLit (MachFloat  f) = MachDouble f
double2FloatLit (MachDouble d) = MachFloat  d
\end{code}

	Predicates
	~~~~~~~~~~
\begin{code}
isLitLitLit (MachLitLit _ _) = True
isLitLitLit _	    	     = False

maybeLitLit (MachLitLit s t) = Just (s,t)
maybeLitLit _		     = Nothing
\end{code}

	Types
	~~~~~
\begin{code}
literalType :: Literal -> Type
literalType (MachChar _)	  = charPrimTy
literalType (MachStr  _)	  = addrPrimTy
literalType (MachAddr _)	  = addrPrimTy
literalType (MachInt  _)	  = intPrimTy
literalType (MachWord  _)	  = wordPrimTy
literalType (MachInt64  _)	  = int64PrimTy
literalType (MachWord64  _)	  = word64PrimTy
literalType (MachFloat _)	  = floatPrimTy
literalType (MachDouble _)	  = doublePrimTy
literalType (MachLitLit _ ty)	  = ty
\end{code}

\begin{code}
literalPrimRep :: Literal -> PrimRep

literalPrimRep (MachChar _)	  = CharRep
literalPrimRep (MachStr _)	  = AddrRep  -- specifically: "char *"
literalPrimRep (MachAddr  _)	  = AddrRep
literalPrimRep (MachInt _) 	  = IntRep
literalPrimRep (MachWord _) 	  = WordRep
literalPrimRep (MachInt64 _)	  = Int64Rep
literalPrimRep (MachWord64 _)	  = Word64Rep
literalPrimRep (MachFloat _)	  = FloatRep
literalPrimRep (MachDouble _)	  = DoubleRep
literalPrimRep (MachLitLit _ ty)  = typePrimRep ty
\end{code}


	Comparison
	~~~~~~~~~~
\begin{code}
cmpLit (MachChar      a)   (MachChar	   b)   = a `compare` b
cmpLit (MachStr       a)   (MachStr	   b)   = a `compare` b
cmpLit (MachAddr      a)   (MachAddr	   b)   = a `compare` b
cmpLit (MachInt       a)   (MachInt	   b)   = a `compare` b
cmpLit (MachWord      a)   (MachWord	   b)   = a `compare` b
cmpLit (MachInt64     a)   (MachInt64	   b)   = a `compare` b
cmpLit (MachWord64    a)   (MachWord64	   b)   = a `compare` b
cmpLit (MachFloat     a)   (MachFloat	   b)   = a `compare` b
cmpLit (MachDouble    a)   (MachDouble	   b)   = a `compare` b
cmpLit (MachLitLit    a b) (MachLitLit    c d)  = (a `compare` c) `thenCmp` (b `compare` d)
cmpLit lit1		   lit2		        | litTag lit1 _LT_ litTag lit2 = LT
					        | otherwise  		       = GT

litTag (MachChar      _)   = ILIT(1)
litTag (MachStr       _)   = ILIT(2)
litTag (MachAddr      _)   = ILIT(3)
litTag (MachInt       _)   = ILIT(4)
litTag (MachWord      _)   = ILIT(5)
litTag (MachInt64     _)   = ILIT(6)
litTag (MachWord64    _)   = ILIT(7)
litTag (MachFloat     _)   = ILIT(8)
litTag (MachDouble    _)   = ILIT(9)
litTag (MachLitLit    _ _) = ILIT(10)
\end{code}

	Printing
	~~~~~~~~
* MachX (i.e. unboxed) things are printed unadornded (e.g. 3, 'a', "foo")
  exceptions: MachFloat and MachAddr get an initial keyword prefix

\begin{code}
pprLit lit
  = getPprStyle $ \ sty ->
    let
      code_style  = codeStyle  sty
      iface_style = ifaceStyle sty
    in
    case lit of
      MachChar ch | code_style  -> hcat [ptext SLIT("(C_)"), char '\'', 
					 text (charToC ch), char '\'']
	          | iface_style -> char '\'' <> text (charToEasyHaskell ch) <> char '\''
		  | otherwise   -> text ['\'', ch, '\'']

      MachStr s | code_style -> pprFSInCStyle s
	        | otherwise  -> pprFSAsString s

      MachInt i | code_style && i == tARGET_MIN_INT -> parens (integer (i+1) <> text "-1")
				-- Avoid a problem whereby gcc interprets
				-- the constant minInt as unsigned.
		| otherwise -> pprIntVal i

      MachInt64 i | code_style -> pprIntVal i		-- Same problem with gcc???
		  | otherwise -> ptext SLIT("__int64") <+> integer i

      MachWord w | code_style -> pprHexVal w
		 | otherwise  -> ptext SLIT("__word") <+> integer w

      MachWord64 w | code_style -> pprHexVal w
		   | otherwise  -> ptext SLIT("__word64") <+> integer w

      MachFloat f | code_style -> ptext SLIT("(StgFloat)") <> rational f
                  | otherwise  -> ptext SLIT("__float") <+> rational f

      MachDouble d | iface_style && d < 0 -> parens (rational d)
		   | otherwise            -> rational d

      MachAddr p | code_style -> ptext SLIT("(void*)") <> integer p
	         | otherwise  -> ptext SLIT("__addr") <+> integer p

      MachLitLit s ty | code_style  -> ptext s
		      | otherwise   -> parens (hsep [ptext SLIT("__litlit"), 
						     pprFSAsString s,
						     pprParendType ty])

pprIntVal :: Integer -> SDoc
-- Print negative integers with parens to be sure it's unambiguous
pprIntVal i | i < 0     = parens (integer i)
	    | otherwise = integer i
		
pprHexVal :: Integer -> SDoc
-- Print in C hex format: 0x13fa 
pprHexVal 0 = ptext SLIT("0x0")
pprHexVal w = ptext SLIT("0x") <> go w
	    where
	      go 0 = empty
	      go w = go quot <> dig
		   where
		     (quot,rem) = w `quotRem` 16
		     dig | rem < 10  = char (chr (fromInteger rem + ord '0'))
			 | otherwise = char (chr (fromInteger rem - 10 + ord 'a'))
\end{code}


%************************************************************************
%*									*
\subsection{Hashing}
%*									*
%************************************************************************

Hash values should be zero or a positive integer.  No negatives please.
(They mess up the UniqFM for some reason.)

\begin{code}
hashLiteral :: Literal -> Int
hashLiteral (MachChar c)    	= ord c + 1000	-- Keep it out of range of common ints
hashLiteral (MachStr s)     	= hashFS s
hashLiteral (MachAddr i)    	= hashInteger i
hashLiteral (MachInt i)   	= hashInteger i
hashLiteral (MachInt64 i) 	= hashInteger i
hashLiteral (MachWord i)   	= hashInteger i
hashLiteral (MachWord64 i) 	= hashInteger i
hashLiteral (MachFloat r)   	= hashRational r
hashLiteral (MachDouble r)  	= hashRational r
hashLiteral (MachLitLit s _)    = hashFS s

hashRational :: Rational -> Int
hashRational r = hashInteger (numerator r)

hashInteger :: Integer -> Int
hashInteger i = 1 + abs (fromInteger (i `rem` 10000))
		-- The 1+ is to avoid zero, which is a Bad Number
		-- since we use * to combine hash values

hashFS :: FAST_STRING -> Int
hashFS s = IBOX( uniqueOfFS s )
\end{code}
