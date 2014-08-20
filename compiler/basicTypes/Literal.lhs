%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[Literal]{@Literal@: Machine literals (unboxed, of course)}

\begin{code}
{-# LANGUAGE CPP, DeriveDataTypeable #-}

module Literal
        (
        -- * Main data type
          Literal(..)           -- Exported to ParseIface

        -- ** Creating Literals
        , mkMachInt, mkMachWord
        , mkMachInt64, mkMachWord64
        , mkMachFloat, mkMachDouble
        , mkMachChar, mkMachString
        , mkLitInteger

        -- ** Operations on Literals
        , literalType
        , hashLiteral
        , absentLiteralOf
        , pprLiteral

        -- ** Predicates on Literals and their contents
        , litIsDupable, litIsTrivial, litIsLifted
        , inIntRange, inWordRange, tARGET_MAX_INT, inCharRange
        , isZeroLit
        , litFitsInChar

        -- ** Coercions
        , word2IntLit, int2WordLit
        , narrow8IntLit, narrow16IntLit, narrow32IntLit
        , narrow8WordLit, narrow16WordLit, narrow32WordLit
        , char2IntLit, int2CharLit
        , float2IntLit, int2FloatLit, double2IntLit, int2DoubleLit
        , nullAddrLit, float2DoubleLit, double2FloatLit
        ) where

#include "HsVersions.h"

import TysPrim
import PrelNames
import Type
import TyCon
import Outputable
import FastTypes
import FastString
import BasicTypes
import Binary
import Constants
import DynFlags
import UniqFM
import Util

import Data.ByteString (ByteString)
import Data.Int
import Data.Ratio
import Data.Word
import Data.Char
import Data.Data ( Data, Typeable )
import Numeric ( fromRat )
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Literals}
%*                                                                      *
%************************************************************************

\begin{code}
-- | So-called 'Literal's are one of:
--
-- * An unboxed (/machine/) literal ('MachInt', 'MachFloat', etc.),
--   which is presumed to be surrounded by appropriate constructors
--   (@Int#@, etc.), so that the overall thing makes sense.
--
-- * The literal derived from the label mentioned in a \"foreign label\"
--   declaration ('MachLabel')
data Literal
  =     ------------------
        -- First the primitive guys
    MachChar    Char            -- ^ @Char#@ - at least 31 bits. Create with 'mkMachChar'

  | MachStr     ByteString      -- ^ A string-literal: stored and emitted
                                -- UTF-8 encoded, we'll arrange to decode it
                                -- at runtime.  Also emitted with a @'\0'@
                                -- terminator. Create with 'mkMachString'

  | MachNullAddr                -- ^ The @NULL@ pointer, the only pointer value
                                -- that can be represented as a Literal. Create
                                -- with 'nullAddrLit'

  | MachInt     Integer         -- ^ @Int#@ - at least @WORD_SIZE_IN_BITS@ bits. Create with 'mkMachInt'
  | MachInt64   Integer         -- ^ @Int64#@ - at least 64 bits. Create with 'mkMachInt64'
  | MachWord    Integer         -- ^ @Word#@ - at least @WORD_SIZE_IN_BITS@ bits. Create with 'mkMachWord'
  | MachWord64  Integer         -- ^ @Word64#@ - at least 64 bits. Create with 'mkMachWord64'

  | MachFloat   Rational        -- ^ @Float#@. Create with 'mkMachFloat'
  | MachDouble  Rational        -- ^ @Double#@. Create with 'mkMachDouble'

  | MachLabel   FastString
                (Maybe Int)
        FunctionOrData
                -- ^ A label literal. Parameters:
                        --
                        -- 1) The name of the symbol mentioned in the declaration
                        --
                        -- 2) The size (in bytes) of the arguments
                                --    the label expects. Only applicable with
                                --    @stdcall@ labels. @Just x@ => @\<x\>@ will
                                --    be appended to label name when emitting assembly.

  | LitInteger Integer Type --  ^ Integer literals
                            -- See Note [Integer literals]
  deriving (Data, Typeable)
\end{code}

Note [Integer literals]
~~~~~~~~~~~~~~~~~~~~~~~
An Integer literal is represented using, well, an Integer, to make it
easier to write RULEs for them. They also contain the Integer type, so
that e.g. literalType can return the right Type for them.

They only get converted into real Core,
    mkInteger [c1, c2, .., cn]
during the CorePrep phase, although TidyPgm looks ahead at what the
core will be, so that it can see whether it involves CAFs.

When we initally build an Integer literal, notably when
deserialising it from an interface file (see the Binary instance
below), we don't have convenient access to the mkInteger Id.  So we
just use an error thunk, and fill in the real Id when we do tcIfaceLit
in TcIface.


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
    put_ bh (MachLabel aj mb fod)
        = do putByte bh 9
             put_ bh aj
             put_ bh mb
             put_ bh fod
    put_ bh (LitInteger i _) = do putByte bh 10; put_ bh i
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
                    fod <- get bh
                    return (MachLabel aj mb fod)
              _ -> do
                    i <- get bh
                    -- See Note [Integer literals]
                    return $ mkLitInteger i (panic "Evaluated the place holder for mkInteger")
\end{code}

\begin{code}
instance Outputable Literal where
    ppr lit = pprLiteral (\d -> d) lit

instance Eq Literal where
    a == b = case (a `compare` b) of { EQ -> True;   _ -> False }
    a /= b = case (a `compare` b) of { EQ -> False;  _ -> True  }

instance Ord Literal where
    a <= b = case (a `compare` b) of { LT -> True;  EQ -> True;  GT -> False }
    a <  b = case (a `compare` b) of { LT -> True;  EQ -> False; GT -> False }
    a >= b = case (a `compare` b) of { LT -> False; EQ -> True;  GT -> True  }
    a >  b = case (a `compare` b) of { LT -> False; EQ -> False; GT -> True  }
    compare a b = cmpLit a b
\end{code}


        Construction
        ~~~~~~~~~~~~
\begin{code}
-- | Creates a 'Literal' of type @Int#@
mkMachInt :: DynFlags -> Integer -> Literal
mkMachInt dflags x   = ASSERT2( inIntRange dflags x,  integer x )
                       MachInt x

-- | Creates a 'Literal' of type @Word#@
mkMachWord :: DynFlags -> Integer -> Literal
mkMachWord dflags x   = ASSERT2( inWordRange dflags x, integer x )
                        MachWord x

-- | Creates a 'Literal' of type @Int64#@
mkMachInt64 :: Integer -> Literal
mkMachInt64  x = MachInt64 x

-- | Creates a 'Literal' of type @Word64#@
mkMachWord64 :: Integer -> Literal
mkMachWord64 x = MachWord64 x

-- | Creates a 'Literal' of type @Float#@
mkMachFloat :: Rational -> Literal
mkMachFloat = MachFloat

-- | Creates a 'Literal' of type @Double#@
mkMachDouble :: Rational -> Literal
mkMachDouble = MachDouble

-- | Creates a 'Literal' of type @Char#@
mkMachChar :: Char -> Literal
mkMachChar = MachChar

-- | Creates a 'Literal' of type @Addr#@, which is appropriate for passing to
-- e.g. some of the \"error\" functions in GHC.Err such as @GHC.Err.runtimeError@
mkMachString :: String -> Literal
-- stored UTF-8 encoded
mkMachString s = MachStr (fastStringToByteString $ mkFastString s)

mkLitInteger :: Integer -> Type -> Literal
mkLitInteger = LitInteger

inIntRange, inWordRange :: DynFlags -> Integer -> Bool
inIntRange  dflags x = x >= tARGET_MIN_INT dflags && x <= tARGET_MAX_INT dflags
inWordRange dflags x = x >= 0                     && x <= tARGET_MAX_WORD dflags

inCharRange :: Char -> Bool
inCharRange c =  c >= '\0' && c <= chr tARGET_MAX_CHAR

-- | Tests whether the literal represents a zero of whatever type it is
isZeroLit :: Literal -> Bool
isZeroLit (MachInt    0) = True
isZeroLit (MachInt64  0) = True
isZeroLit (MachWord   0) = True
isZeroLit (MachWord64 0) = True
isZeroLit (MachFloat  0) = True
isZeroLit (MachDouble 0) = True
isZeroLit _              = False
\end{code}

        Coercions
        ~~~~~~~~~
\begin{code}
narrow8IntLit, narrow16IntLit, narrow32IntLit,
  narrow8WordLit, narrow16WordLit, narrow32WordLit,
  char2IntLit, int2CharLit,
  float2IntLit, int2FloatLit, double2IntLit, int2DoubleLit,
  float2DoubleLit, double2FloatLit
  :: Literal -> Literal

word2IntLit, int2WordLit :: DynFlags -> Literal -> Literal
word2IntLit dflags (MachWord w)
  | w > tARGET_MAX_INT dflags = MachInt (w - tARGET_MAX_WORD dflags - 1)
  | otherwise                 = MachInt w
word2IntLit _ l = pprPanic "word2IntLit" (ppr l)

int2WordLit dflags (MachInt i)
  | i < 0     = MachWord (1 + tARGET_MAX_WORD dflags + i)      -- (-1)  --->  tARGET_MAX_WORD
  | otherwise = MachWord i
int2WordLit _ l = pprPanic "int2WordLit" (ppr l)

narrow8IntLit    (MachInt  i) = MachInt  (toInteger (fromInteger i :: Int8))
narrow8IntLit    l            = pprPanic "narrow8IntLit" (ppr l)
narrow16IntLit   (MachInt  i) = MachInt  (toInteger (fromInteger i :: Int16))
narrow16IntLit   l            = pprPanic "narrow16IntLit" (ppr l)
narrow32IntLit   (MachInt  i) = MachInt  (toInteger (fromInteger i :: Int32))
narrow32IntLit   l            = pprPanic "narrow32IntLit" (ppr l)
narrow8WordLit   (MachWord w) = MachWord (toInteger (fromInteger w :: Word8))
narrow8WordLit   l            = pprPanic "narrow8WordLit" (ppr l)
narrow16WordLit  (MachWord w) = MachWord (toInteger (fromInteger w :: Word16))
narrow16WordLit  l            = pprPanic "narrow16WordLit" (ppr l)
narrow32WordLit  (MachWord w) = MachWord (toInteger (fromInteger w :: Word32))
narrow32WordLit  l            = pprPanic "narrow32WordLit" (ppr l)

char2IntLit (MachChar c) = MachInt  (toInteger (ord c))
char2IntLit l            = pprPanic "char2IntLit" (ppr l)
int2CharLit (MachInt  i) = MachChar (chr (fromInteger i))
int2CharLit l            = pprPanic "int2CharLit" (ppr l)

float2IntLit (MachFloat f) = MachInt   (truncate    f)
float2IntLit l             = pprPanic "float2IntLit" (ppr l)
int2FloatLit (MachInt   i) = MachFloat (fromInteger i)
int2FloatLit l             = pprPanic "int2FloatLit" (ppr l)

double2IntLit (MachDouble f) = MachInt    (truncate    f)
double2IntLit l              = pprPanic "double2IntLit" (ppr l)
int2DoubleLit (MachInt    i) = MachDouble (fromInteger i)
int2DoubleLit l              = pprPanic "int2DoubleLit" (ppr l)

float2DoubleLit (MachFloat  f) = MachDouble f
float2DoubleLit l              = pprPanic "float2DoubleLit" (ppr l)
double2FloatLit (MachDouble d) = MachFloat  d
double2FloatLit l              = pprPanic "double2FloatLit" (ppr l)

nullAddrLit :: Literal
nullAddrLit = MachNullAddr
\end{code}

        Predicates
        ~~~~~~~~~~
\begin{code}
-- | True if there is absolutely no penalty to duplicating the literal.
-- False principally of strings
litIsTrivial :: Literal -> Bool
--      c.f. CoreUtils.exprIsTrivial
litIsTrivial (MachStr _)      = False
litIsTrivial (LitInteger {})  = False
litIsTrivial _                = True

-- | True if code space does not go bad if we duplicate this literal
-- Currently we treat it just like 'litIsTrivial'
litIsDupable :: DynFlags -> Literal -> Bool
--      c.f. CoreUtils.exprIsDupable
litIsDupable _      (MachStr _)      = False
litIsDupable dflags (LitInteger i _) = inIntRange dflags i
litIsDupable _      _                = True

litFitsInChar :: Literal -> Bool
litFitsInChar (MachInt i) = i >= toInteger (ord minBound)
                         && i <= toInteger (ord maxBound)
litFitsInChar _           = False

litIsLifted :: Literal -> Bool
litIsLifted (LitInteger {}) = True
litIsLifted _               = False
\end{code}

        Types
        ~~~~~
\begin{code}
-- | Find the Haskell 'Type' the literal occupies
literalType :: Literal -> Type
literalType MachNullAddr    = addrPrimTy
literalType (MachChar _)    = charPrimTy
literalType (MachStr  _)    = addrPrimTy
literalType (MachInt  _)    = intPrimTy
literalType (MachWord  _)   = wordPrimTy
literalType (MachInt64  _)  = int64PrimTy
literalType (MachWord64  _) = word64PrimTy
literalType (MachFloat _)   = floatPrimTy
literalType (MachDouble _)  = doublePrimTy
literalType (MachLabel _ _ _) = addrPrimTy
literalType (LitInteger _ t) = t

absentLiteralOf :: TyCon -> Maybe Literal
-- Return a literal of the appropriate primtive
-- TyCon, to use as a placeholder when it doesn't matter
absentLiteralOf tc = lookupUFM absent_lits (tyConName tc)

absent_lits :: UniqFM Literal
absent_lits = listToUFM [ (addrPrimTyConKey,    MachNullAddr)
                        , (charPrimTyConKey,    MachChar 'x')
                        , (intPrimTyConKey,     MachInt 0)
                        , (int64PrimTyConKey,   MachInt64 0)
                        , (floatPrimTyConKey,   MachFloat 0)
                        , (doublePrimTyConKey,  MachDouble 0)
                        , (wordPrimTyConKey,    MachWord 0)
                        , (word64PrimTyConKey,  MachWord64 0) ]
\end{code}


        Comparison
        ~~~~~~~~~~
\begin{code}
cmpLit :: Literal -> Literal -> Ordering
cmpLit (MachChar      a)   (MachChar       b)   = a `compare` b
cmpLit (MachStr       a)   (MachStr        b)   = a `compare` b
cmpLit (MachNullAddr)      (MachNullAddr)       = EQ
cmpLit (MachInt       a)   (MachInt        b)   = a `compare` b
cmpLit (MachWord      a)   (MachWord       b)   = a `compare` b
cmpLit (MachInt64     a)   (MachInt64      b)   = a `compare` b
cmpLit (MachWord64    a)   (MachWord64     b)   = a `compare` b
cmpLit (MachFloat     a)   (MachFloat      b)   = a `compare` b
cmpLit (MachDouble    a)   (MachDouble     b)   = a `compare` b
cmpLit (MachLabel     a _ _) (MachLabel      b _ _) = a `compare` b
cmpLit (LitInteger    a _) (LitInteger     b _) = a `compare` b
cmpLit lit1                lit2                 | litTag lit1 <# litTag lit2 = LT
                                                | otherwise                  = GT

litTag :: Literal -> FastInt
litTag (MachChar      _)   = _ILIT(1)
litTag (MachStr       _)   = _ILIT(2)
litTag (MachNullAddr)      = _ILIT(3)
litTag (MachInt       _)   = _ILIT(4)
litTag (MachWord      _)   = _ILIT(5)
litTag (MachInt64     _)   = _ILIT(6)
litTag (MachWord64    _)   = _ILIT(7)
litTag (MachFloat     _)   = _ILIT(8)
litTag (MachDouble    _)   = _ILIT(9)
litTag (MachLabel _ _ _)   = _ILIT(10)
litTag (LitInteger  {})    = _ILIT(11)
\end{code}

        Printing
        ~~~~~~~~
* MachX (i.e. unboxed) things are printed unadornded (e.g. 3, 'a', "foo")
  exceptions: MachFloat gets an initial keyword prefix.

\begin{code}
pprLiteral :: (SDoc -> SDoc) -> Literal -> SDoc
-- The function is used on non-atomic literals
-- to wrap parens around literals that occur in
-- a context requiring an atomic thing
pprLiteral _       (MachChar ch)    = pprHsChar ch
pprLiteral _       (MachStr s)      = pprHsBytes s
pprLiteral _       (MachInt i)      = pprIntVal i
pprLiteral _       (MachDouble d)   = double (fromRat d)
pprLiteral _       (MachNullAddr)   = ptext (sLit "__NULL")
pprLiteral add_par (LitInteger i _) = add_par (ptext (sLit "__integer") <+> integer i)
pprLiteral add_par (MachInt64 i)    = add_par (ptext (sLit "__int64") <+> integer i)
pprLiteral add_par (MachWord w)     = add_par (ptext (sLit "__word") <+> integer w)
pprLiteral add_par (MachWord64 w)   = add_par (ptext (sLit "__word64") <+> integer w)
pprLiteral add_par (MachFloat f)    = add_par (ptext (sLit "__float") <+> float (fromRat f))
pprLiteral add_par (MachLabel l mb fod) = add_par (ptext (sLit "__label") <+> b <+> ppr fod)
    where b = case mb of
              Nothing -> pprHsString l
              Just x  -> doubleQuotes (text (unpackFS l ++ '@':show x))

pprIntVal :: Integer -> SDoc
-- ^ Print negative integers with parens to be sure it's unambiguous
pprIntVal i | i < 0     = parens (integer i)
            | otherwise = integer i
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Hashing}
%*                                                                      *
%************************************************************************

Hash values should be zero or a positive integer.  No negatives please.
(They mess up the UniqFM for some reason.)

\begin{code}
hashLiteral :: Literal -> Int
hashLiteral (MachChar c)        = ord c + 1000  -- Keep it out of range of common ints
hashLiteral (MachStr s)         = hashByteString s
hashLiteral (MachNullAddr)      = 0
hashLiteral (MachInt i)         = hashInteger i
hashLiteral (MachInt64 i)       = hashInteger i
hashLiteral (MachWord i)        = hashInteger i
hashLiteral (MachWord64 i)      = hashInteger i
hashLiteral (MachFloat r)       = hashRational r
hashLiteral (MachDouble r)      = hashRational r
hashLiteral (MachLabel s _ _)     = hashFS s
hashLiteral (LitInteger i _)    = hashInteger i

hashRational :: Rational -> Int
hashRational r = hashInteger (numerator r)

hashInteger :: Integer -> Int
hashInteger i = 1 + abs (fromInteger (i `rem` 10000))
                -- The 1+ is to avoid zero, which is a Bad Number
                -- since we use * to combine hash values

hashFS :: FastString -> Int
hashFS s = iBox (uniqueOfFS s)
\end{code}
