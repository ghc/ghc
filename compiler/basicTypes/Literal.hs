{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1998

\section[Literal]{@Literal@: literals}
-}

{-# LANGUAGE CPP, DeriveDataTypeable, ScopedTypeVariables #-}

module Literal
        (
        -- * Main data type
          Literal(..)           -- Exported to ParseIface
        , LitNumType(..)

        -- ** Creating Literals
        , mkLitInt, mkLitIntWrap, mkLitIntWrapC
        , mkLitWord, mkLitWordWrap, mkLitWordWrapC
        , mkLitInt64, mkLitInt64Wrap
        , mkLitWord64, mkLitWord64Wrap
        , mkLitFloat, mkLitDouble
        , mkLitChar, mkLitString
        , mkLitInteger, mkLitNatural
        , mkLitNumber, mkLitNumberWrap

        -- ** Operations on Literals
        , literalType
        , absentLiteralOf
        , pprLiteral
        , litNumIsSigned
        , litNumCheckRange

        -- ** Predicates on Literals and their contents
        , litIsDupable, litIsTrivial, litIsLifted
        , inIntRange, inWordRange, tARGET_MAX_INT, inCharRange
        , isZeroLit
        , litFitsInChar
        , litValue, isLitValue, isLitValue_maybe, mapLitValue

        -- ** Coercions
        , word2IntLit, int2WordLit
        , narrowLit
        , narrow8IntLit, narrow16IntLit, narrow32IntLit
        , narrow8WordLit, narrow16WordLit, narrow32WordLit
        , char2IntLit, int2CharLit
        , float2IntLit, int2FloatLit, double2IntLit, int2DoubleLit
        , nullAddrLit, rubbishLit, float2DoubleLit, double2FloatLit
        ) where

#include "HsVersions.h"

import GhcPrelude

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
import Platform
import UniqFM
import Util

import Data.ByteString (ByteString)
import Data.Int
import Data.Word
import Data.Char
import Data.Maybe ( isJust )
import Data.Data ( Data )
import Data.Proxy
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
-- * An unboxed numeric literal or floating-point literal which is presumed
--   to be surrounded by appropriate constructors (@Int#@, etc.), so that
--   the overall thing makes sense.
--
--   We maintain the invariant that the 'Integer' in the 'LitNumber'
--   constructor is actually in the (possibly target-dependent) range.
--   The mkLit{Int,Word}*Wrap smart constructors ensure this by applying
--   the target machine's wrapping semantics. Use these in situations
--   where you know the wrapping semantics are correct.
--
-- * The literal derived from the label mentioned in a \"foreign label\"
--   declaration ('LitLabel')
--
-- * A 'LitRubbish' to be used in place of values of 'UnliftedRep'
--   (i.e. 'MutVar#') when the the value is never used.
--
-- * A character
-- * A string
-- * The NULL pointer
--
data Literal
  = LitChar    Char             -- ^ @Char#@ - at least 31 bits. Create with
                                -- 'mkLitChar'

  | LitNumber !LitNumType !Integer Type
                                -- ^ Any numeric literal that can be
                                -- internally represented with an Integer

  | LitString  ByteString       -- ^ A string-literal: stored and emitted
                                -- UTF-8 encoded, we'll arrange to decode it
                                -- at runtime.  Also emitted with a @\'\\0\'@
                                -- terminator. Create with 'mkLitString'

  | LitNullAddr                 -- ^ The @NULL@ pointer, the only pointer value
                                -- that can be represented as a Literal. Create
                                -- with 'nullAddrLit'

  | LitRubbish                  -- ^ A nonsense value, used when an unlifted
                                -- binding is absent and has type
                                -- @forall (a :: 'TYPE' 'UnliftedRep'). a@.
                                -- May be lowered by code-gen to any possible
                                -- value. Also see Note [Rubbish literals]

  | LitFloat   Rational         -- ^ @Float#@. Create with 'mkLitFloat'
  | LitDouble  Rational         -- ^ @Double#@. Create with 'mkLitDouble'

  | LitLabel   FastString (Maybe Int) FunctionOrData
                                -- ^ A label literal. Parameters:
                                --
                                -- 1) The name of the symbol mentioned in the
                                --    declaration
                                --
                                -- 2) The size (in bytes) of the arguments
                                --    the label expects. Only applicable with
                                --    @stdcall@ labels. @Just x@ => @\<x\>@ will
                                --    be appended to label name when emitting
                                --    assembly.
                                --
                                -- 3) Flag indicating whether the symbol
                                --    references a function or a data
  deriving Data

-- | Numeric literal type
data LitNumType
  = LitNumInteger -- ^ @Integer@ (see Note [Integer literals])
  | LitNumNatural -- ^ @Natural@ (see Note [Natural literals])
  | LitNumInt     -- ^ @Int#@ - according to target machine
  | LitNumInt64   -- ^ @Int64#@ - exactly 64 bits
  | LitNumWord    -- ^ @Word#@ - according to target machine
  | LitNumWord64  -- ^ @Word64#@ - exactly 64 bits
  deriving (Data,Enum,Eq,Ord)

-- | Indicate if a numeric literal type supports negative numbers
litNumIsSigned :: LitNumType -> Bool
litNumIsSigned nt = case nt of
  LitNumInteger -> True
  LitNumNatural -> False
  LitNumInt     -> True
  LitNumInt64   -> True
  LitNumWord    -> False
  LitNumWord64  -> False

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

Note [Natural literals]
~~~~~~~~~~~~~~~~~~~~~~~
Similar to Integer literals.

-}

instance Binary LitNumType where
   put_ bh numTyp = putByte bh (fromIntegral (fromEnum numTyp))
   get bh = do
      h <- getByte bh
      return (toEnum (fromIntegral h))

instance Binary Literal where
    put_ bh (LitChar aa)     = do putByte bh 0; put_ bh aa
    put_ bh (LitString ab)   = do putByte bh 1; put_ bh ab
    put_ bh (LitNullAddr)    = do putByte bh 2
    put_ bh (LitFloat ah)    = do putByte bh 3; put_ bh ah
    put_ bh (LitDouble ai)   = do putByte bh 4; put_ bh ai
    put_ bh (LitLabel aj mb fod)
        = do putByte bh 5
             put_ bh aj
             put_ bh mb
             put_ bh fod
    put_ bh (LitNumber nt i _)
        = do putByte bh 6
             put_ bh nt
             put_ bh i
    put_ bh (LitRubbish)     = do putByte bh 7
    get bh = do
            h <- getByte bh
            case h of
              0 -> do
                    aa <- get bh
                    return (LitChar aa)
              1 -> do
                    ab <- get bh
                    return (LitString ab)
              2 -> do
                    return (LitNullAddr)
              3 -> do
                    ah <- get bh
                    return (LitFloat ah)
              4 -> do
                    ai <- get bh
                    return (LitDouble ai)
              5 -> do
                    aj <- get bh
                    mb <- get bh
                    fod <- get bh
                    return (LitLabel aj mb fod)
              6 -> do
                    nt <- get bh
                    i  <- get bh
                    let t = case nt of
                            LitNumInt     -> intPrimTy
                            LitNumInt64   -> int64PrimTy
                            LitNumWord    -> wordPrimTy
                            LitNumWord64  -> word64PrimTy
                            -- See Note [Integer literals]
                            LitNumInteger ->
                              panic "Evaluated the place holder for mkInteger"
                            -- and Note [Natural literals]
                            LitNumNatural ->
                              panic "Evaluated the place holder for mkNatural"
                    return (LitNumber nt i t)
              _ -> do
                    return (LitRubbish)

instance Outputable Literal where
    ppr lit = pprLiteral (\d -> d) lit

instance Eq Literal where
    a == b = case (a `compare` b) of { EQ -> True;   _ -> False }
    a /= b = case (a `compare` b) of { EQ -> False;  _ -> True  }

-- | Needed for the @Ord@ instance of 'AltCon', which in turn is needed in
-- 'TrieMap.CoreMap'.
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

{- Note [Word/Int underflow/overflow]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
According to the Haskell Report 2010 (Sections 18.1 and 23.1 about signed and
unsigned integral types): "All arithmetic is performed modulo 2^n, where n is
the number of bits in the type."

GHC stores Word# and Int# constant values as Integer. Core optimizations such
as constant folding must ensure that the Integer value remains in the valid
target Word/Int range (see #13172). The following functions are used to
ensure this.

Note that we *don't* warn the user about overflow. It's not done at runtime
either, and compilation of completely harmless things like
   ((124076834 :: Word32) + (2147483647 :: Word32))
doesn't yield a warning. Instead we simply squash the value into the *target*
Int/Word range.
-}

-- | Wrap a literal number according to its type
wrapLitNumber :: DynFlags -> Literal -> Literal
wrapLitNumber dflags v@(LitNumber nt i t) = case nt of
  LitNumInt -> case platformWordSize (targetPlatform dflags) of
    4 -> LitNumber nt (toInteger (fromIntegral i :: Int32)) t
    8 -> LitNumber nt (toInteger (fromIntegral i :: Int64)) t
    w -> panic ("wrapLitNumber: Unknown platformWordSize: " ++ show w)
  LitNumWord -> case platformWordSize (targetPlatform dflags) of
    4 -> LitNumber nt (toInteger (fromIntegral i :: Word32)) t
    8 -> LitNumber nt (toInteger (fromIntegral i :: Word64)) t
    w -> panic ("wrapLitNumber: Unknown platformWordSize: " ++ show w)
  LitNumInt64   -> LitNumber nt (toInteger (fromIntegral i :: Int64)) t
  LitNumWord64  -> LitNumber nt (toInteger (fromIntegral i :: Word64)) t
  LitNumInteger -> v
  LitNumNatural -> v
wrapLitNumber _ x = x

-- | Create a numeric 'Literal' of the given type
mkLitNumberWrap :: DynFlags -> LitNumType -> Integer -> Type -> Literal
mkLitNumberWrap dflags nt i t = wrapLitNumber dflags (LitNumber nt i t)

-- | Check that a given number is in the range of a numeric literal
litNumCheckRange :: DynFlags -> LitNumType -> Integer -> Bool
litNumCheckRange dflags nt i = case nt of
     LitNumInt     -> inIntRange dflags i
     LitNumWord    -> inWordRange dflags i
     LitNumInt64   -> inInt64Range i
     LitNumWord64  -> inWord64Range i
     LitNumNatural -> i >= 0
     LitNumInteger -> True

-- | Create a numeric 'Literal' of the given type
mkLitNumber :: DynFlags -> LitNumType -> Integer -> Type -> Literal
mkLitNumber dflags nt i t =
  ASSERT2(litNumCheckRange dflags nt i, integer i)
  (LitNumber nt i t)

-- | Creates a 'Literal' of type @Int#@
mkLitInt :: DynFlags -> Integer -> Literal
mkLitInt dflags x   = ASSERT2( inIntRange dflags x,  integer x )
                       (mkLitIntUnchecked x)

-- | Creates a 'Literal' of type @Int#@.
--   If the argument is out of the (target-dependent) range, it is wrapped.
--   See Note [Word/Int underflow/overflow]
mkLitIntWrap :: DynFlags -> Integer -> Literal
mkLitIntWrap dflags i = wrapLitNumber dflags $ mkLitIntUnchecked i

-- | Creates a 'Literal' of type @Int#@ without checking its range.
mkLitIntUnchecked :: Integer -> Literal
mkLitIntUnchecked i = LitNumber LitNumInt i intPrimTy

-- | Creates a 'Literal' of type @Int#@, as well as a 'Bool'ean flag indicating
--   overflow. That is, if the argument is out of the (target-dependent) range
--   the argument is wrapped and the overflow flag will be set.
--   See Note [Word/Int underflow/overflow]
mkLitIntWrapC :: DynFlags -> Integer -> (Literal, Bool)
mkLitIntWrapC dflags i = (n, i /= i')
  where
    n@(LitNumber _ i' _) = mkLitIntWrap dflags i

-- | Creates a 'Literal' of type @Word#@
mkLitWord :: DynFlags -> Integer -> Literal
mkLitWord dflags x   = ASSERT2( inWordRange dflags x, integer x )
                        (mkLitWordUnchecked x)

-- | Creates a 'Literal' of type @Word#@.
--   If the argument is out of the (target-dependent) range, it is wrapped.
--   See Note [Word/Int underflow/overflow]
mkLitWordWrap :: DynFlags -> Integer -> Literal
mkLitWordWrap dflags i = wrapLitNumber dflags $ mkLitWordUnchecked i

-- | Creates a 'Literal' of type @Word#@ without checking its range.
mkLitWordUnchecked :: Integer -> Literal
mkLitWordUnchecked i = LitNumber LitNumWord i wordPrimTy

-- | Creates a 'Literal' of type @Word#@, as well as a 'Bool'ean flag indicating
--   carry. That is, if the argument is out of the (target-dependent) range
--   the argument is wrapped and the carry flag will be set.
--   See Note [Word/Int underflow/overflow]
mkLitWordWrapC :: DynFlags -> Integer -> (Literal, Bool)
mkLitWordWrapC dflags i = (n, i /= i')
  where
    n@(LitNumber _ i' _) = mkLitWordWrap dflags i

-- | Creates a 'Literal' of type @Int64#@
mkLitInt64 :: Integer -> Literal
mkLitInt64  x = ASSERT2( inInt64Range x, integer x ) (mkLitInt64Unchecked x)

-- | Creates a 'Literal' of type @Int64#@.
--   If the argument is out of the range, it is wrapped.
mkLitInt64Wrap :: DynFlags -> Integer -> Literal
mkLitInt64Wrap dflags i = wrapLitNumber dflags $ mkLitInt64Unchecked i

-- | Creates a 'Literal' of type @Int64#@ without checking its range.
mkLitInt64Unchecked :: Integer -> Literal
mkLitInt64Unchecked i = LitNumber LitNumInt64 i int64PrimTy

-- | Creates a 'Literal' of type @Word64#@
mkLitWord64 :: Integer -> Literal
mkLitWord64 x = ASSERT2( inWord64Range x, integer x ) (mkLitWord64Unchecked x)

-- | Creates a 'Literal' of type @Word64#@.
--   If the argument is out of the range, it is wrapped.
mkLitWord64Wrap :: DynFlags -> Integer -> Literal
mkLitWord64Wrap dflags i = wrapLitNumber dflags $ mkLitWord64Unchecked i

-- | Creates a 'Literal' of type @Word64#@ without checking its range.
mkLitWord64Unchecked :: Integer -> Literal
mkLitWord64Unchecked i = LitNumber LitNumWord64 i word64PrimTy

-- | Creates a 'Literal' of type @Float#@
mkLitFloat :: Rational -> Literal
mkLitFloat = LitFloat

-- | Creates a 'Literal' of type @Double#@
mkLitDouble :: Rational -> Literal
mkLitDouble = LitDouble

-- | Creates a 'Literal' of type @Char#@
mkLitChar :: Char -> Literal
mkLitChar = LitChar

-- | Creates a 'Literal' of type @Addr#@, which is appropriate for passing to
-- e.g. some of the \"error\" functions in GHC.Err such as @GHC.Err.runtimeError@
mkLitString :: String -> Literal
-- stored UTF-8 encoded
mkLitString s = LitString (fastStringToByteString $ mkFastString s)

mkLitInteger :: Integer -> Type -> Literal
mkLitInteger x ty = LitNumber LitNumInteger x ty

mkLitNatural :: Integer -> Type -> Literal
mkLitNatural x ty = ASSERT2( inNaturalRange x,  integer x )
                    (LitNumber LitNumNatural x ty)

inIntRange, inWordRange :: DynFlags -> Integer -> Bool
inIntRange  dflags x = x >= tARGET_MIN_INT dflags && x <= tARGET_MAX_INT dflags
inWordRange dflags x = x >= 0                     && x <= tARGET_MAX_WORD dflags

inNaturalRange :: Integer -> Bool
inNaturalRange x = x >= 0

inInt64Range, inWord64Range :: Integer -> Bool
inInt64Range x  = x >= toInteger (minBound :: Int64) &&
                  x <= toInteger (maxBound :: Int64)
inWord64Range x = x >= toInteger (minBound :: Word64) &&
                  x <= toInteger (maxBound :: Word64)

inCharRange :: Char -> Bool
inCharRange c =  c >= '\0' && c <= chr tARGET_MAX_CHAR

-- | Tests whether the literal represents a zero of whatever type it is
isZeroLit :: Literal -> Bool
isZeroLit (LitNumber _ 0 _) = True
isZeroLit (LitFloat  0)     = True
isZeroLit (LitDouble 0)     = True
isZeroLit _                 = False

-- | Returns the 'Integer' contained in the 'Literal', for when that makes
-- sense, i.e. for 'Char', 'Int', 'Word', 'LitInteger' and 'LitNatural'.
litValue  :: Literal -> Integer
litValue l = case isLitValue_maybe l of
   Just x  -> x
   Nothing -> pprPanic "litValue" (ppr l)

-- | Returns the 'Integer' contained in the 'Literal', for when that makes
-- sense, i.e. for 'Char' and numbers.
isLitValue_maybe  :: Literal -> Maybe Integer
isLitValue_maybe (LitChar   c)     = Just $ toInteger $ ord c
isLitValue_maybe (LitNumber _ i _) = Just i
isLitValue_maybe _                 = Nothing

-- | Apply a function to the 'Integer' contained in the 'Literal', for when that
-- makes sense, e.g. for 'Char' and numbers.
-- For fixed-size integral literals, the result will be wrapped in accordance
-- with the semantics of the target type.
-- See Note [Word/Int underflow/overflow]
mapLitValue  :: DynFlags -> (Integer -> Integer) -> Literal -> Literal
mapLitValue _      f (LitChar   c)      = mkLitChar (fchar c)
   where fchar = chr . fromInteger . f . toInteger . ord
mapLitValue dflags f (LitNumber nt i t) = wrapLitNumber dflags
                                                        (LitNumber nt (f i) t)
mapLitValue _      _ l                  = pprPanic "mapLitValue" (ppr l)

-- | Indicate if the `Literal` contains an 'Integer' value, e.g. 'Char',
-- 'Int', 'Word', 'LitInteger' and 'LitNatural'.
isLitValue  :: Literal -> Bool
isLitValue = isJust . isLitValue_maybe

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
word2IntLit dflags (LitNumber LitNumWord w _)
  -- Map Word range [max_int+1, max_word]
  -- to Int range   [min_int  , -1]
  -- Range [0,max_int] has the same representation with both Int and Word
  | w > tARGET_MAX_INT dflags = mkLitInt dflags (w - tARGET_MAX_WORD dflags - 1)
  | otherwise                 = mkLitInt dflags w
word2IntLit _ l = pprPanic "word2IntLit" (ppr l)

int2WordLit dflags (LitNumber LitNumInt i _)
  -- Map Int range [min_int  , -1]
  -- to Word range [max_int+1, max_word]
  -- Range [0,max_int] has the same representation with both Int and Word
  | i < 0     = mkLitWord dflags (1 + tARGET_MAX_WORD dflags + i)
  | otherwise = mkLitWord dflags i
int2WordLit _ l = pprPanic "int2WordLit" (ppr l)

-- | Narrow a literal number (unchecked result range)
narrowLit :: forall a. Integral a => Proxy a -> Literal -> Literal
narrowLit _ (LitNumber nt i t) = LitNumber nt (toInteger (fromInteger i :: a)) t
narrowLit _ l                  = pprPanic "narrowLit" (ppr l)

narrow8IntLit   = narrowLit (Proxy :: Proxy Int8)
narrow16IntLit  = narrowLit (Proxy :: Proxy Int16)
narrow32IntLit  = narrowLit (Proxy :: Proxy Int32)
narrow8WordLit  = narrowLit (Proxy :: Proxy Word8)
narrow16WordLit = narrowLit (Proxy :: Proxy Word16)
narrow32WordLit = narrowLit (Proxy :: Proxy Word32)

char2IntLit (LitChar c)       = mkLitIntUnchecked (toInteger (ord c))
char2IntLit l                 = pprPanic "char2IntLit" (ppr l)
int2CharLit (LitNumber _ i _) = LitChar (chr (fromInteger i))
int2CharLit l                 = pprPanic "int2CharLit" (ppr l)

float2IntLit (LitFloat f)      = mkLitIntUnchecked (truncate f)
float2IntLit l                 = pprPanic "float2IntLit" (ppr l)
int2FloatLit (LitNumber _ i _) = LitFloat (fromInteger i)
int2FloatLit l                 = pprPanic "int2FloatLit" (ppr l)

double2IntLit (LitDouble f)     = mkLitIntUnchecked (truncate f)
double2IntLit l                 = pprPanic "double2IntLit" (ppr l)
int2DoubleLit (LitNumber _ i _) = LitDouble (fromInteger i)
int2DoubleLit l                 = pprPanic "int2DoubleLit" (ppr l)

float2DoubleLit (LitFloat  f) = LitDouble f
float2DoubleLit l             = pprPanic "float2DoubleLit" (ppr l)
double2FloatLit (LitDouble d) = LitFloat  d
double2FloatLit l             = pprPanic "double2FloatLit" (ppr l)

nullAddrLit :: Literal
nullAddrLit = LitNullAddr

-- | A nonsense literal of type @forall (a :: 'TYPE' 'UnliftedRep'). a@.
rubbishLit :: Literal
rubbishLit = LitRubbish

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
litIsTrivial (LitString _)      = False
litIsTrivial (LitNumber nt _ _) = case nt of
  LitNumInteger -> False
  LitNumNatural -> False
  LitNumInt     -> True
  LitNumInt64   -> True
  LitNumWord    -> True
  LitNumWord64  -> True
litIsTrivial _                  = True

-- | True if code space does not go bad if we duplicate this literal
litIsDupable :: DynFlags -> Literal -> Bool
--      c.f. CoreUtils.exprIsDupable
litIsDupable _      (LitString _)      = False
litIsDupable dflags (LitNumber nt i _) = case nt of
  LitNumInteger -> inIntRange dflags i
  LitNumNatural -> inIntRange dflags i
  LitNumInt     -> True
  LitNumInt64   -> True
  LitNumWord    -> True
  LitNumWord64  -> True
litIsDupable _      _                  = True

litFitsInChar :: Literal -> Bool
litFitsInChar (LitNumber _ i _) = i >= toInteger (ord minBound)
                               && i <= toInteger (ord maxBound)
litFitsInChar _                 = False

litIsLifted :: Literal -> Bool
litIsLifted (LitNumber nt _ _) = case nt of
  LitNumInteger -> True
  LitNumNatural -> True
  LitNumInt     -> False
  LitNumInt64   -> False
  LitNumWord    -> False
  LitNumWord64  -> False
litIsLifted _                  = False

{-
        Types
        ~~~~~
-}

-- | Find the Haskell 'Type' the literal occupies
literalType :: Literal -> Type
literalType LitNullAddr       = addrPrimTy
literalType (LitChar _)       = charPrimTy
literalType (LitString  _)    = addrPrimTy
literalType (LitFloat _)      = floatPrimTy
literalType (LitDouble _)     = doublePrimTy
literalType (LitLabel _ _ _)  = addrPrimTy
literalType (LitNumber _ _ t) = t
literalType (LitRubbish)      = mkForAllTy a Inferred (mkTyVarTy a)
  where
    a = alphaTyVarUnliftedRep

absentLiteralOf :: TyCon -> Maybe Literal
-- Return a literal of the appropriate primitive
-- TyCon, to use as a placeholder when it doesn't matter
-- Rubbish literals are handled in WwLib, because
--  1. Looking at the TyCon is not enough, we need the actual type
--  2. This would need to return a type application to a literal
absentLiteralOf tc = lookupUFM absent_lits (tyConName tc)

absent_lits :: UniqFM Literal
absent_lits = listToUFM [ (addrPrimTyConKey,    LitNullAddr)
                        , (charPrimTyConKey,    LitChar 'x')
                        , (intPrimTyConKey,     mkLitIntUnchecked 0)
                        , (int64PrimTyConKey,   mkLitInt64Unchecked 0)
                        , (wordPrimTyConKey,    mkLitWordUnchecked 0)
                        , (word64PrimTyConKey,  mkLitWord64Unchecked 0)
                        , (floatPrimTyConKey,   LitFloat 0)
                        , (doublePrimTyConKey,  LitDouble 0)
                        ]

{-
        Comparison
        ~~~~~~~~~~
-}

cmpLit :: Literal -> Literal -> Ordering
cmpLit (LitChar      a)     (LitChar       b)     = a `compare` b
cmpLit (LitString    a)     (LitString     b)     = a `compare` b
cmpLit (LitNullAddr)        (LitNullAddr)         = EQ
cmpLit (LitFloat     a)     (LitFloat      b)     = a `compare` b
cmpLit (LitDouble    a)     (LitDouble     b)     = a `compare` b
cmpLit (LitLabel     a _ _) (LitLabel      b _ _) = a `compare` b
cmpLit (LitNumber nt1 a _)  (LitNumber nt2  b _)
  | nt1 == nt2 = a   `compare` b
  | otherwise  = nt1 `compare` nt2
cmpLit (LitRubbish)         (LitRubbish)          = EQ
cmpLit lit1 lit2
  | litTag lit1 < litTag lit2 = LT
  | otherwise                 = GT

litTag :: Literal -> Int
litTag (LitChar      _)   = 1
litTag (LitString    _)   = 2
litTag (LitNullAddr)      = 3
litTag (LitFloat     _)   = 4
litTag (LitDouble    _)   = 5
litTag (LitLabel _ _ _)   = 6
litTag (LitNumber  {})    = 7
litTag (LitRubbish)       = 8

{-
        Printing
        ~~~~~~~~
* See Note [Printing of literals in Core]
-}

pprLiteral :: (SDoc -> SDoc) -> Literal -> SDoc
pprLiteral _       (LitChar c)     = pprPrimChar c
pprLiteral _       (LitString s)   = pprHsBytes s
pprLiteral _       (LitNullAddr)   = text "__NULL"
pprLiteral _       (LitFloat f)    = float (fromRat f) <> primFloatSuffix
pprLiteral _       (LitDouble d)   = double (fromRat d) <> primDoubleSuffix
pprLiteral add_par (LitNumber nt i _)
   = case nt of
       LitNumInteger -> pprIntegerVal add_par i
       LitNumNatural -> pprIntegerVal add_par i
       LitNumInt     -> pprPrimInt i
       LitNumInt64   -> pprPrimInt64 i
       LitNumWord    -> pprPrimWord i
       LitNumWord64  -> pprPrimWord64 i
pprLiteral add_par (LitLabel l mb fod) =
    add_par (text "__label" <+> b <+> ppr fod)
    where b = case mb of
              Nothing -> pprHsString l
              Just x  -> doubleQuotes (text (unpackFS l ++ '@':show x))
pprLiteral _       (LitRubbish)     = text "__RUBBISH"

pprIntegerVal :: (SDoc -> SDoc) -> Integer -> SDoc
-- See Note [Printing of literals in Core].
pprIntegerVal add_par i | i < 0     = add_par (integer i)
                        | otherwise = integer i

{-
Note [Printing of literals in Core]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The function `add_par` is used to wrap parenthesis around negative integers
(`LitInteger`) and labels (`LitLabel`), if they occur in a context requiring
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
LitChar         'a'#
LitString       "aaa"#
LitNullAddr     "__NULL"
LitInt          -1#
LitInt64        -1L#
LitWord          1##
LitWord64        1L##
LitFloat        -1.0#
LitDouble       -1.0##
LitInteger      -1                 (-1)
LitLabel        "__label" ...      ("__label" ...)
LitRubbish      "__RUBBISH"

Note [Rubbish literals]
~~~~~~~~~~~~~~~~~~~~~~~
During worker/wrapper after demand analysis, where an argument
is unused (absent) we do the following w/w split (supposing that
y is absent):

  f x y z = e
===>
  f x y z = $wf x z
  $wf x z = let y = <absent value>
            in e

Usually the binding for y is ultimately optimised away, and
even if not it should never be evaluated -- but that's the
way the w/w split starts off.

What is <absent value>?
* For lifted values <absent value> can be a call to 'error'.
* For primitive types like Int# or Word# we can use any random
  value of that type.
* But what about /unlifted/ but /boxed/ types like MutVar# or
  Array#?   We need a literal value of that type.

That is 'LitRubbish'.  Since we need a rubbish literal for
many boxed, unlifted types, we say that LitRubbish has type
  LitRubbish :: forall (a :: TYPE UnliftedRep). a

So we might see a w/w split like
  $wf x z = let y :: Array# Int = LitRubbish @(Array# Int)
            in e

Recall that (TYPE UnliftedRep) is the kind of boxed, unlifted
heap pointers.

Here are the moving parts:

* We define LitRubbish as a constructor in Literal.Literal

* It is given its polymoprhic type by Literal.literalType

* WwLib.mk_absent_let introduces a LitRubbish for absent
  arguments of boxed, unlifted type.

* In CoreToSTG we convert (RubishLit @t) to just ().  STG is
  untyped, so it doesn't matter that it points to a lifted
  value. The important thing is that it is a heap pointer,
  which the garbage collector can follow if it encounters it.

  We considered maintaining LitRubbish in STG, and lowering
  it in the code genreators, but it seems simpler to do it
  once and for all in CoreToSTG.

  In ByteCodeAsm we just lower it as a 0 literal, because
  it's all boxed and lifted to the host GC anyway.
-}
