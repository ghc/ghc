%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[Literal]{@Literal@: Machine literals (unboxed, of course)}

\begin{code}
module Literal
	( Literal(..)		-- Exported to ParseIface
	, mkMachInt, mkMachWord
	, mkMachInt64, mkMachWord64
	, litSize
	, litIsDupable, litIsTrivial
	, literalType, literalPrimRep
	, hashLiteral

	, inIntRange, inWordRange, tARGET_MAX_INT, inCharRange
	, isZeroLit,

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
import FastString
import Binary
import Util		( thenCmp )

import Ratio 		( numerator )
import FastString	( uniqueOfFS, lengthFS )
import DATA_INT		( Int8,  Int16,  Int32 )
import DATA_WORD	( Word8, Word16, Word32 )
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
  | MachStr	FastString

  | MachNullAddr                -- the NULL pointer, the only pointer value
                                -- that can be represented as a Literal.

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
  | MachLabel   FastString		-- always an Addr#
  		(Maybe Int)             -- the size (in bytes) of the arguments
					-- the label expects. Only applicable with
					-- 'stdcall' labels.
					-- Just x => "@<x>" will be appended to label
					--           name when emitting asm.
\end{code}

Binary instance

\begin{code}
instance Binary Literal where
    put_ bh (MachChar aa)     = do putByte bh 0; put_ bh aa
    put_ bh (MachStr ab)      = do putByte bh 1; put_ bh ab
    put_ bh (MachNullAddr)    = do putByte bh 2
    put_ bh (MachInt ad)      = do putByte bh 3; put_ bh ad
    put_ bh (MachInt64 ae)    = do putByte bh 4; put_ bh ae
    put_ bh (MachWord af)     = do putByte bh 5; put_ bh af
    put_ bh (MachWord64 ag)   = do putByte bh 6; put_ bh ag
    put_ bh (MachFloat ah)    = do putByte bh 7; put_ bh ah
    put_ bh (MachDouble ai)   = do putByte bh 8; put_ bh ai
    put_ bh (MachLabel aj mb) = do putByte bh 9; put_ bh aj ; put_ bh mb
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do
		    aa <- get bh
		    return (MachChar aa)
	      1 -> do
		    ab <- get bh
		    return (MachStr ab)
	      2 -> do
		    return (MachNullAddr)
	      3 -> do
		    ad <- get bh
		    return (MachInt ad)
	      4 -> do
		    ae <- get bh
		    return (MachInt64 ae)
	      5 -> do
		    af <- get bh
		    return (MachWord af)
	      6 -> do
		    ag <- get bh
		    return (MachWord64 ag)
	      7 -> do
		    ah <- get bh
		    return (MachFloat ah)
	      8 -> do
		    ai <- get bh
		    return (MachDouble ai)
	      9 -> do
		    aj <- get bh
		    mb <- get bh
		    return (MachLabel aj mb)
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

isZeroLit :: Literal -> Bool
isZeroLit (MachInt    0) = True
isZeroLit (MachInt64  0) = True
isZeroLit (MachWord   0) = True
isZeroLit (MachWord64 0) = True
isZeroLit (MachFloat  0) = True
isZeroLit (MachDouble 0) = True
isZeroLit other		 = False
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
nullAddrLit = MachNullAddr
\end{code}

	Predicates
	~~~~~~~~~~
\begin{code}
litIsTrivial :: Literal -> Bool
-- True if there is absolutely no penalty to duplicating the literal
--	c.f. CoreUtils.exprIsTrivial
-- False principally of strings
litIsTrivial (MachStr _) = False
litIsTrivial other	 = True

litIsDupable :: Literal -> Bool
-- True if code space does not go bad if we duplicate this literal
--	c.f. CoreUtils.exprIsDupable
-- Currently we treat it just like litIsTrivial
litIsDupable (MachStr _) = False
litIsDupable other	 = True

litSize :: Literal -> Int
-- Used by CoreUnfold.sizeExpr
litSize (MachStr str) = 1 + ((lengthFS str + 3) `div` 4)
	-- Every literal has size at least 1, otherwise
	-- 	f "x" 
	-- might be too small
	-- [Sept03: make literal strings a bit bigger to avoid fruitless 
	--  duplication of little strings]
litSize _other	      = 1
\end{code}

	Types
	~~~~~
\begin{code}
literalType :: Literal -> Type
literalType (MachChar _)	  = charPrimTy
literalType (MachStr  _)	  = addrPrimTy
literalType (MachNullAddr)	  = addrPrimTy
literalType (MachInt  _)	  = intPrimTy
literalType (MachWord  _)	  = wordPrimTy
literalType (MachInt64  _)	  = int64PrimTy
literalType (MachWord64  _)	  = word64PrimTy
literalType (MachFloat _)	  = floatPrimTy
literalType (MachDouble _)	  = doublePrimTy
literalType (MachLabel _ _)	  = addrPrimTy
\end{code}

\begin{code}
literalPrimRep :: Literal -> PrimRep

literalPrimRep (MachChar _)	  = CharRep
literalPrimRep (MachStr _)	  = AddrRep  -- specifically: "char *"
literalPrimRep (MachNullAddr)	  = AddrRep
literalPrimRep (MachInt _) 	  = IntRep
literalPrimRep (MachWord _) 	  = WordRep
literalPrimRep (MachInt64 _)	  = Int64Rep
literalPrimRep (MachWord64 _)	  = Word64Rep
literalPrimRep (MachFloat _)	  = FloatRep
literalPrimRep (MachDouble _)	  = DoubleRep
literalPrimRep (MachLabel _ _)	  = AddrRep
\end{code}


	Comparison
	~~~~~~~~~~
\begin{code}
cmpLit (MachChar      a)   (MachChar	   b)   = a `compare` b
cmpLit (MachStr       a)   (MachStr	   b)   = a `compare` b
cmpLit (MachNullAddr)      (MachNullAddr)       = EQ
cmpLit (MachInt       a)   (MachInt	   b)   = a `compare` b
cmpLit (MachWord      a)   (MachWord	   b)   = a `compare` b
cmpLit (MachInt64     a)   (MachInt64	   b)   = a `compare` b
cmpLit (MachWord64    a)   (MachWord64	   b)   = a `compare` b
cmpLit (MachFloat     a)   (MachFloat	   b)   = a `compare` b
cmpLit (MachDouble    a)   (MachDouble	   b)   = a `compare` b
cmpLit (MachLabel     a _) (MachLabel      b _) = a `compare` b
cmpLit lit1		   lit2		        | litTag lit1 <# litTag lit2 = LT
					        | otherwise  		       = GT

litTag (MachChar      _)   = _ILIT(1)
litTag (MachStr       _)   = _ILIT(2)
litTag (MachNullAddr)      = _ILIT(3)
litTag (MachInt       _)   = _ILIT(4)
litTag (MachWord      _)   = _ILIT(5)
litTag (MachInt64     _)   = _ILIT(6)
litTag (MachWord64    _)   = _ILIT(7)
litTag (MachFloat     _)   = _ILIT(8)
litTag (MachDouble    _)   = _ILIT(9)
litTag (MachLabel   _ _)   = _ILIT(10)
\end{code}

	Printing
	~~~~~~~~
* MachX (i.e. unboxed) things are printed unadornded (e.g. 3, 'a', "foo")
  exceptions: MachFloat gets an initial keyword prefix.

\begin{code}
pprLit lit
  = getPprStyle $ \ sty ->
    let
      code_style  = codeStyle  sty
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

      MachFloat f | code_style -> ptext SLIT("(StgFloat)") <> code_rational f
                  | otherwise  -> ptext SLIT("__float") <+> rational f

      MachDouble d | code_style -> code_rational d
		   | otherwise  -> rational d

      MachNullAddr | code_style -> ptext SLIT("(void*)0")
	           | otherwise  -> ptext SLIT("__NULL")

      MachLabel l mb
         | code_style -> ptext SLIT("(&") <> ftext l <> char ')'
	 | otherwise  -> ptext SLIT("__label") <+> 
	     case mb of
	       Nothing -> pprHsString l
	       Just x  -> doubleQuotes (text (unpackFS l ++ '@':show x))

-- negative floating literals in code style need parentheses to avoid
-- interacting with surrounding syntax.
code_rational d | d < 0     = parens (rational d)
                | otherwise = rational d

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
hashLiteral (MachNullAddr)    	= 0
hashLiteral (MachInt i)   	= hashInteger i
hashLiteral (MachInt64 i) 	= hashInteger i
hashLiteral (MachWord i)   	= hashInteger i
hashLiteral (MachWord64 i) 	= hashInteger i
hashLiteral (MachFloat r)   	= hashRational r
hashLiteral (MachDouble r)  	= hashRational r
hashLiteral (MachLabel s _)     = hashFS s

hashRational :: Rational -> Int
hashRational r = hashInteger (numerator r)

hashInteger :: Integer -> Int
hashInteger i = 1 + abs (fromInteger (i `rem` 10000))
		-- The 1+ is to avoid zero, which is a Bad Number
		-- since we use * to combine hash values

hashFS :: FastString -> Int
hashFS s = iBox (uniqueOfFS s)
\end{code}
