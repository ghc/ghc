%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[Literal]{@Literal@: Machine literals (unboxed, of course)}

\begin{code}
module Literal
	( Literal(..)		-- Exported to ParseIface
	, mkMachInt, mkMachWord
	, mkMachInt64, mkMachWord64
	, isLitLitLit, maybeLitLit, litSize, litIsDupable,
	, literalType, literalPrimRep
	, hashLiteral

	, inIntRange, inWordRange, tARGET_MAX_INT, inCharRange

	, word2IntLit, int2WordLit
	, narrow8IntLit, narrow16IntLit, narrow32IntLit
	, narrow8WordLit, narrow16WordLit, narrow32WordLit
	, char2IntLit, int2CharLit
	, float2IntLit, int2FloatLit, double2IntLit, int2DoubleLit
	, nullAddrLit, float2DoubleLit, double2FloatLit
	) where

#include "HsVersions.h"

import TysPrim		( charPrimTy, addrPrimTy, floatPrimTy, doublePrimTy,
			  intPrimTy, wordPrimTy, int64PrimTy, word64PrimTy
			)
import PrimRep		( PrimRep(..) )
import TcType		( Type, tcCmpType )
import Type		( typePrimRep )
import PprType		( pprParendType )
import CStrings		( pprFSInCStyle )

import Outputable
import FastTypes
import Util		( thenCmp )

import Ratio 		( numerator )
import FastString	( uniqueOfFS, lengthFS )
import Int		( Int8,  Int16,  Int32 )
import Word		( Word8, Word16, Word32 )
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

tARGET_MAX_CHAR :: Int
tARGET_MAX_CHAR = 0x10ffff
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
    MachChar	Int             -- Char#        At least 31 bits
  | MachStr	FAST_STRING

  | MachAddr	Integer	-- Whatever this machine thinks is a "pointer"

  | MachInt	Integer		-- Int#		At least WORD_SIZE_IN_BITS bits
  | MachInt64	Integer		-- Int64#	At least 64 bits
  | MachWord	Integer		-- Word#	At least WORD_SIZE_IN_BITS bits
  | MachWord64	Integer		-- Word64#	At least 64 bits

  | MachFloat	Rational
  | MachDouble	Rational

        -- MachLabel is used (only) for the literal derived from a 
	-- "foreign label" declaration.
	-- string argument is the name of a symbol.  This literal
	-- refers to the *address* of the label.
  | MachLabel   FAST_STRING		-- always an Addr#

	-- lit-lits only work for via-C compilation, hence they
	-- are deprecated.  The string is emitted verbatim into
	-- the C file, and can therefore be any C expression,
	-- macro call, #defined constant etc.
  | MachLitLit  FAST_STRING Type	-- Type might be Addr# or Int# etc
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

mkMachInt  x   = -- ASSERT2( inIntRange x,  integer x ) 
	 	 -- Not true: you can write out of range Int# literals
		 -- For example, one can write (intToWord# 0xffff0000) to
		 -- get a particular Word bit-pattern, and there's no other
		 -- convenient way to write such literals, which is why we allow it.
		 MachInt x
mkMachWord x   = -- ASSERT2( inWordRange x, integer x ) 
		 MachWord x
mkMachInt64  x = MachInt64 x
mkMachWord64 x = MachWord64 x

inIntRange, inWordRange :: Integer -> Bool
inIntRange  x = x >= tARGET_MIN_INT && x <= tARGET_MAX_INT
inWordRange x = x >= 0		    && x <= tARGET_MAX_WORD

inCharRange :: Int -> Bool
inCharRange c =  c >= 0 && c <= tARGET_MAX_CHAR
\end{code}

	Coercions
	~~~~~~~~~
\begin{code}
word2IntLit, int2WordLit,
  narrow8IntLit, narrow16IntLit, narrow32IntLit,
  narrow8WordLit, narrow16WordLit, narrow32WordLit,
  char2IntLit, int2CharLit,
  float2IntLit, int2FloatLit, double2IntLit, int2DoubleLit,
  float2DoubleLit, double2FloatLit
  :: Literal -> Literal

word2IntLit (MachWord w) 
  | w > tARGET_MAX_INT = MachInt (w - tARGET_MAX_WORD - 1)
  | otherwise	       = MachInt w

int2WordLit (MachInt i)
  | i < 0     = MachWord (1 + tARGET_MAX_WORD + i)	-- (-1)  --->  tARGET_MAX_WORD
  | otherwise = MachWord i

narrow8IntLit    (MachInt  i) = MachInt  (toInteger (fromInteger i :: Int8))
narrow16IntLit   (MachInt  i) = MachInt  (toInteger (fromInteger i :: Int16))
narrow32IntLit   (MachInt  i) = MachInt  (toInteger (fromInteger i :: Int32))
narrow8WordLit   (MachWord w) = MachWord (toInteger (fromInteger w :: Word8))
narrow16WordLit  (MachWord w) = MachWord (toInteger (fromInteger w :: Word16))
narrow32WordLit  (MachWord w) = MachWord (toInteger (fromInteger w :: Word32))

char2IntLit (MachChar c) = MachInt  (toInteger c)
int2CharLit (MachInt  i) = MachChar (fromInteger i)

float2IntLit (MachFloat f) = MachInt   (truncate    f)
int2FloatLit (MachInt   i) = MachFloat (fromInteger i)

double2IntLit (MachDouble f) = MachInt    (truncate    f)
int2DoubleLit (MachInt   i) = MachDouble (fromInteger i)

float2DoubleLit (MachFloat  f) = MachDouble f
double2FloatLit (MachDouble d) = MachFloat  d

nullAddrLit :: Literal
nullAddrLit = MachAddr 0
\end{code}

	Predicates
	~~~~~~~~~~
\begin{code}
isLitLitLit (MachLitLit _ _) = True
isLitLitLit _	    	     = False

maybeLitLit (MachLitLit s t) = Just (s,t)
maybeLitLit _		     = Nothing

litIsDupable :: Literal -> Bool
	-- True if code space does not go bad if we duplicate this literal
	-- False principally of strings
litIsDupable (MachStr _) = False
litIsDupable other	 = True

litSize :: Literal -> Int
	-- used by CoreUnfold.sizeExpr
litSize (MachStr str) = lengthFS str `div` 4
litSize _other	      = 1
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
literalType (MachLabel _)	  = addrPrimTy
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
literalPrimRep (MachLabel _)	  = AddrRep
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
cmpLit (MachLabel     a)   (MachLabel      b)   = a `compare` b
cmpLit (MachLitLit    a b) (MachLitLit    c d)  = (a `compare` c) `thenCmp` (b `tcCmpType` d)
cmpLit lit1		   lit2		        | litTag lit1 <# litTag lit2 = LT
					        | otherwise  		       = GT

litTag (MachChar      _)   = _ILIT(1)
litTag (MachStr       _)   = _ILIT(2)
litTag (MachAddr      _)   = _ILIT(3)
litTag (MachInt       _)   = _ILIT(4)
litTag (MachWord      _)   = _ILIT(5)
litTag (MachInt64     _)   = _ILIT(6)
litTag (MachWord64    _)   = _ILIT(7)
litTag (MachFloat     _)   = _ILIT(8)
litTag (MachDouble    _)   = _ILIT(9)
litTag (MachLabel     _)   = _ILIT(10)
litTag (MachLitLit    _ _) = _ILIT(11)
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
      MachChar ch | code_style -> hcat [ptext SLIT("(C_)"), text (show ch)]
	          | otherwise  -> pprHsChar ch

      MachStr s | code_style -> pprFSInCStyle s
	        | otherwise  -> pprHsString s
      -- Warning: printing MachStr in code_style assumes it contains
      -- only characters '\0'..'\xFF'!

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

      MachLabel l | code_style -> ptext SLIT("(&") <> ptext l <> char ')'
		  | otherwise  -> ptext SLIT("__label") <+> pprHsString l

      MachLitLit s ty | code_style  -> ptext s
		      | otherwise   -> parens (hsep [ptext SLIT("__litlit"), 
						     pprHsString s,
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
hashLiteral (MachChar c)    	= c + 1000	-- Keep it out of range of common ints
hashLiteral (MachStr s)     	= hashFS s
hashLiteral (MachAddr i)    	= hashInteger i
hashLiteral (MachInt i)   	= hashInteger i
hashLiteral (MachInt64 i) 	= hashInteger i
hashLiteral (MachWord i)   	= hashInteger i
hashLiteral (MachWord64 i) 	= hashInteger i
hashLiteral (MachFloat r)   	= hashRational r
hashLiteral (MachDouble r)  	= hashRational r
hashLiteral (MachLabel s)       = hashFS s
hashLiteral (MachLitLit s _)    = hashFS s

hashRational :: Rational -> Int
hashRational r = hashInteger (numerator r)

hashInteger :: Integer -> Int
hashInteger i = 1 + abs (fromInteger (i `rem` 10000))
		-- The 1+ is to avoid zero, which is a Bad Number
		-- since we use * to combine hash values

hashFS :: FAST_STRING -> Int
hashFS s = iBox (uniqueOfFS s)
\end{code}
