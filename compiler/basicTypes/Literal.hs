{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1998

\section[Literal]{@Literal@: Machine literals (unboxed, of course)}
-}

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
        , absentLiteralOf
        , pprLiteral

        -- ** Predicates on Literals and their contents
        , litIsDupable, litIsTrivial, litIsLifted
        , inIntRange, inWordRange, tARGET_MAX_INT, inCharRange
        , isZeroLit
        , litFitsInChar
        , litValue

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
import FastString
import BasicTypes
import Binary
import Constants
import DynFlags
import UniqFM
import Util

import Data.ByteString (ByteString)
import Data.Int
import Data.Word
import Data.Char
import Data.Data ( Data )
import Numeric ( fromRat )

{-
************************************************************************
*                                                                      *
\subsection{Literals}
*                                                                      *
************************************************************************
-}

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
  deriving Data

{-
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
-}

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

{-
        Construction
        ~~~~~~~~~~~~
-}

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

-- | Returns the 'Integer' contained in the 'Literal', for when that makes
-- sense, i.e. for 'Char', 'Int', 'Word' and 'LitInteger'.
litValue  :: Literal -> Integer
litValue (MachChar   c) = toInteger $ ord c
litValue (MachInt    i) = i
litValue (MachInt64  i) = i
litValue (MachWord   i) = i
litValue (MachWord64 i) = i
litValue (LitInteger i _) = i
litValue l = pprPanic "litValue" (ppr l)

{-
        Coercions
        ~~~~~~~~~
-}

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

{-
        Predicates
        ~~~~~~~~~~
-}

-- | True if there is absolutely no penalty to duplicating the literal.
-- False principally of strings.
--
-- "Why?", you say? I'm glad you asked. Well, for one duplicating strings would
-- blow up code sizes. Not only this, it's also unsafe.
--
-- Consider a program that wants to traverse a string. One way it might do this
-- is to first compute the Addr# pointing to the end of the string, and then,
-- starting from the beginning, bump a pointer using eqAddr# to determine the
-- end. For instance,
--
-- @
-- -- Given pointers to the start and end of a string, count how many zeros
-- -- the string contains.
-- countZeros :: Addr# -> Addr# -> -> Int
-- countZeros start end = go start 0
--   where
--     go off n
--       | off `addrEq#` end = n
--       | otherwise         = go (off `plusAddr#` 1) n'
--       where n' | isTrue# (indexInt8OffAddr# off 0# ==# 0#) = n + 1
--                | otherwise                                 = n
-- @
--
-- Consider what happens if we considered strings to be trivial (and therefore
-- duplicable) and emitted a call like @countZeros "hello"# ("hello"#
-- `plusAddr`# 5)@. The beginning and end pointers do not belong to the same
-- string, meaning that an iteration like the above would blow up terribly.
-- This is what happened in #12757.
--
-- Ultimately the solution here is to make primitive strings a bit more
-- structured, ensuring that the compiler can't inline in ways that will break
-- user code. One approach to this is described in #8472.
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

{-
        Types
        ~~~~~
-}

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

{-
        Comparison
        ~~~~~~~~~~
-}

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
cmpLit lit1                lit2                 | litTag lit1 < litTag lit2 = LT
                                                | otherwise                 = GT

litTag :: Literal -> Int
litTag (MachChar      _)   = 1
litTag (MachStr       _)   = 2
litTag (MachNullAddr)      = 3
litTag (MachInt       _)   = 4
litTag (MachWord      _)   = 5
litTag (MachInt64     _)   = 6
litTag (MachWord64    _)   = 7
litTag (MachFloat     _)   = 8
litTag (MachDouble    _)   = 9
litTag (MachLabel _ _ _)   = 10
litTag (LitInteger  {})    = 11

{-
        Printing
        ~~~~~~~~
* See Note [Printing of literals in Core]
-}

pprLiteral :: (SDoc -> SDoc) -> Literal -> SDoc
pprLiteral _       (MachChar c)     = pprPrimChar c
pprLiteral _       (MachStr s)      = pprHsBytes s
pprLiteral _       (MachNullAddr)   = text "__NULL"
pprLiteral _       (MachInt i)      = pprPrimInt i
pprLiteral _       (MachInt64 i)    = pprPrimInt64 i
pprLiteral _       (MachWord w)     = pprPrimWord w
pprLiteral _       (MachWord64 w)   = pprPrimWord64 w
pprLiteral _       (MachFloat f)    = float (fromRat f) <> primFloatSuffix
pprLiteral _       (MachDouble d)   = double (fromRat d) <> primDoubleSuffix
pprLiteral add_par (LitInteger i _) = pprIntegerVal add_par i
pprLiteral add_par (MachLabel l mb fod) = add_par (text "__label" <+> b <+> ppr fod)
    where b = case mb of
              Nothing -> pprHsString l
              Just x  -> doubleQuotes (text (unpackFS l ++ '@':show x))

pprIntegerVal :: (SDoc -> SDoc) -> Integer -> SDoc
-- See Note [Printing of literals in Core].
pprIntegerVal add_par i | i < 0     = add_par (integer i)
                        | otherwise = integer i

{-
Note [Printing of literals in Core]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The function `add_par` is used to wrap parenthesis around negative integers
(`LitInteger`) and labels (`MachLabel`), if they occur in a context requiring
an atomic thing (for example function application).

Although not all Core literals would be valid Haskell, we are trying to stay
as close as possible to Haskell syntax in the printing of Core, to make it
easier for a Haskell user to read Core.

To that end:
  * We do print parenthesis around negative `LitInteger`, because we print
  `LitInteger` using plain number literals (no prefix or suffix), and plain
  number literals in Haskell require parenthesis in contexts like function
  application (i.e. `1 - -1` is not valid Haskell).

  * We don't print parenthesis around other (negative) literals, because they
  aren't needed in GHC/Haskell either (i.e. `1# -# -1#` is accepted by GHC's
  parser).

Literal         Output             Output if context requires
                                   an atom (if different)
-------         -------            ----------------------
MachChar        'a'#
MachStr         "aaa"#
MachNullAddr    "__NULL"
MachInt         -1#
MachInt64       -1L#
MachWord         1##
MachWord64       1L##
MachFloat       -1.0#
MachDouble      -1.0##
LitInteger      -1                 (-1)
MachLabel       "__label" ...      ("__label" ...)
-}
