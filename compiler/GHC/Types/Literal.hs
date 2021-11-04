{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1998

-}

{-# LANGUAGE CPP, DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Core literals
module GHC.Types.Literal
        (
        -- * Main data type
          Literal(..)           -- Exported to ParseIface
        , LitNumType(..)

        -- ** Creating Literals
        , mkLitInt, mkLitIntWrap, mkLitIntWrapC, mkLitIntUnchecked
        , mkLitWord, mkLitWordWrap, mkLitWordWrapC
        , mkLitInt8, mkLitInt8Wrap
        , mkLitWord8, mkLitWord8Wrap
        , mkLitInt16, mkLitInt16Wrap
        , mkLitWord16, mkLitWord16Wrap
        , mkLitInt32, mkLitInt32Wrap
        , mkLitWord32, mkLitWord32Wrap
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
        , litNumWrap
        , litNumCoerce
        , litNumNarrow
        , litNumBitSize
        , isMinBound
        , isMaxBound

        -- ** Predicates on Literals and their contents
        , litIsDupable, litIsTrivial, litIsLifted
        , inCharRange
        , isZeroLit, isOneLit
        , litFitsInChar
        , litValue, mapLitValue
        , isLitValue_maybe

        -- ** Coercions
        , narrowInt8Lit, narrowInt16Lit, narrowInt32Lit, narrowInt64Lit
        , narrowWord8Lit, narrowWord16Lit, narrowWord32Lit, narrowWord64Lit
        , convertToIntLit, convertToWordLit
        , charToIntLit, intToCharLit
        , floatToIntLit, intToFloatLit, doubleToIntLit, intToDoubleLit
        , nullAddrLit, floatToDoubleLit, doubleToFloatLit
        , rubbishLit, isRubbishLit
        ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Builtin.Types.Prim
import {-# SOURCE #-} GHC.Builtin.Types
import GHC.Builtin.Names
import GHC.Core.Type
import GHC.Core.TyCon
import GHC.Utils.Outputable
import GHC.Data.FastString
import GHC.Types.Basic
import GHC.Utils.Binary
import GHC.Settings.Constants
import GHC.Platform
import GHC.Types.Unique.FM
import GHC.Utils.Misc
import GHC.Utils.Panic

import Data.ByteString (ByteString)
import Data.Int
import Data.Word
import Data.Char
import Data.Data ( Data )
import GHC.Exts
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
--   (i.e. 'MutVar#') when the value is never used.
--
-- * A character
-- * A string
-- * The NULL pointer
--
data Literal
  = LitChar    Char             -- ^ @Char#@ - at least 31 bits. Create with
                                -- 'mkLitChar'

  | LitNumber !LitNumType !Integer
                                -- ^ Any numeric literal that can be
                                -- internally represented with an Integer.

  | LitString !ByteString       -- ^ A string-literal: stored and emitted
                                -- UTF-8 encoded, we'll arrange to decode it
                                -- at runtime.  Also emitted with a @\'\\0\'@
                                -- terminator. Create with 'mkLitString'

  | LitNullAddr                 -- ^ The @NULL@ pointer, the only pointer value
                                -- that can be represented as a Literal. Create
                                -- with 'nullAddrLit'

  | LitRubbish Bool             -- ^ A nonsense value; always boxed, but
                                --      True <=> lifted, False <=> unlifted
                                -- Used when a binding is absent.
                                -- See Note [Rubbish literals]

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
  = LitNumInteger -- ^ @Integer@ (see Note [BigNum literals])
  | LitNumNatural -- ^ @Natural@ (see Note [BigNum literals])
  | LitNumInt     -- ^ @Int#@ - according to target machine
  | LitNumInt8    -- ^ @Int8#@ - exactly 8 bits
  | LitNumInt16   -- ^ @Int16#@ - exactly 16 bits
  | LitNumInt32   -- ^ @Int32#@ - exactly 32 bits
  | LitNumInt64   -- ^ @Int64#@ - exactly 64 bits
  | LitNumWord    -- ^ @Word#@ - according to target machine
  | LitNumWord8   -- ^ @Word8#@ - exactly 8 bits
  | LitNumWord16  -- ^ @Word16#@ - exactly 16 bits
  | LitNumWord32  -- ^ @Word32#@ - exactly 32 bits
  | LitNumWord64  -- ^ @Word64#@ - exactly 64 bits
  deriving (Data,Enum,Eq,Ord)

-- | Indicate if a numeric literal type supports negative numbers
litNumIsSigned :: LitNumType -> Bool
litNumIsSigned nt = case nt of
  LitNumInteger -> True
  LitNumNatural -> False
  LitNumInt     -> True
  LitNumInt8    -> True
  LitNumInt16   -> True
  LitNumInt32   -> True
  LitNumInt64   -> True
  LitNumWord    -> False
  LitNumWord8   -> False
  LitNumWord16  -> False
  LitNumWord32  -> False
  LitNumWord64  -> False

-- | Number of bits
litNumBitSize :: Platform -> LitNumType -> Maybe Word
litNumBitSize platform nt = case nt of
  LitNumInteger -> Nothing
  LitNumNatural -> Nothing
  LitNumInt     -> Just (fromIntegral (platformWordSizeInBits platform))
  LitNumInt8    -> Just 8
  LitNumInt16   -> Just 16
  LitNumInt32   -> Just 32
  LitNumInt64   -> Just 64
  LitNumWord    -> Just (fromIntegral (platformWordSizeInBits platform))
  LitNumWord8   -> Just 8
  LitNumWord16  -> Just 16
  LitNumWord32  -> Just 32
  LitNumWord64  -> Just 64

instance Binary LitNumType where
   put_ bh numTyp = putByte bh (fromIntegral (fromEnum numTyp))
   get bh = do
      h <- getByte bh
      return (toEnum (fromIntegral h))

{-
Note [BigNum literals]
~~~~~~~~~~~~~~~~~~~~~~

GHC supports 2 kinds of arbitrary precision integers (a.k.a BigNum):

   * Natural: natural represented as a Word# or as a BigNat

   * Integer: integer represented a an Int# or as a BigNat (Integer's
   constructors indicate the sign)

BigNum literal instances are removed from Core during the CorePrep phase. They
are replaced with expression to build them at runtime from machine literals
(Word#, Int#, etc.) or from a list of Word#s.

Note [String literals]
~~~~~~~~~~~~~~~~~~~~~~

String literals are UTF-8 encoded and stored into ByteStrings in the following
ASTs: Haskell, Core, Stg, Cmm. TH can also emit ByteString based string literals
with the BytesPrimL constructor (see #14741).

It wasn't true before as [Word8] was used in Cmm AST and in TH which was quite
bad for performance with large strings (see #16198 and #14741).

To include string literals into output objects, the assembler code generator has
to embed the UTF-8 encoded binary blob. See Note [Embedding large binary blobs]
for more details.

-}

instance Binary Literal where
    put_ bh (LitChar aa)     = do putByte bh 0; put_ bh aa
    put_ bh (LitString ab)   = do putByte bh 1; put_ bh ab
    put_ bh (LitNullAddr)    = putByte bh 2
    put_ bh (LitFloat ah)    = do putByte bh 3; put_ bh ah
    put_ bh (LitDouble ai)   = do putByte bh 4; put_ bh ai
    put_ bh (LitLabel aj mb fod)
        = do putByte bh 5
             put_ bh aj
             put_ bh mb
             put_ bh fod
    put_ bh (LitNumber nt i)
        = do putByte bh 6
             put_ bh nt
             put_ bh i
    put_ bh (LitRubbish b) = do putByte bh 7; put_ bh b
    get bh = do
            h <- getByte bh
            case h of
              0 -> do
                    aa <- get bh
                    return (LitChar aa)
              1 -> do
                    ab <- get bh
                    return (LitString ab)
              2 -> return (LitNullAddr)
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
                    return (LitNumber nt i)
              _ -> do
                    b <- get bh
                    return (LitRubbish b)

instance Outputable Literal where
    ppr = pprLiteral id

instance Eq Literal where
    a == b = compare a b == EQ

-- | Needed for the @Ord@ instance of 'AltCon', which in turn is needed in
-- 'GHC.Data.TrieMap.CoreMap'.
instance Ord Literal where
    compare = cmpLit

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

-- | Make a literal number using wrapping semantics if the value is out of
-- bound.
mkLitNumberWrap :: Platform -> LitNumType -> Integer -> Literal
mkLitNumberWrap platform nt i = case nt of
  LitNumInt -> case platformWordSize platform of
    PW4 -> wrap @Int32
    PW8 -> wrap @Int64
  LitNumWord -> case platformWordSize platform of
    PW4 -> wrap @Word32
    PW8 -> wrap @Word64
  LitNumInt8    -> wrap @Int8
  LitNumInt16   -> wrap @Int16
  LitNumInt32   -> wrap @Int32
  LitNumInt64   -> wrap @Int64
  LitNumWord8   -> wrap @Word8
  LitNumWord16  -> wrap @Word16
  LitNumWord32  -> wrap @Word32
  LitNumWord64  -> wrap @Word64
  LitNumInteger -> LitNumber nt i
  LitNumNatural
    | i < 0     -> panic "mkLitNumberWrap: trying to create a negative Natural"
    | otherwise -> LitNumber nt i
  where
    wrap :: forall a. (Integral a, Num a) => Literal
    wrap = LitNumber nt (toInteger (fromIntegral i :: a))

-- | Wrap a literal number according to its type using wrapping semantics.
litNumWrap :: Platform -> Literal -> Literal
litNumWrap platform (LitNumber nt i) = mkLitNumberWrap platform nt i
litNumWrap _        l                = pprPanic "litNumWrap" (ppr l)

-- | Coerce a literal number into another using wrapping semantics.
litNumCoerce :: LitNumType -> Platform -> Literal -> Literal
litNumCoerce pt platform (LitNumber _nt i) = mkLitNumberWrap platform pt i
litNumCoerce _  _        l                 = pprPanic "litNumWrapCoerce: not a number" (ppr l)

-- | Narrow a literal number by converting it into another number type and then
-- converting it back to its original type.
litNumNarrow :: LitNumType -> Platform -> Literal -> Literal
litNumNarrow pt platform (LitNumber nt i)
   = case mkLitNumberWrap platform pt i of
      LitNumber _ j -> mkLitNumberWrap platform nt j
      l             -> pprPanic "litNumNarrow: got invalid literal" (ppr l)
litNumNarrow _ _ l = pprPanic "litNumNarrow: invalid literal" (ppr l)


-- | Check that a given number is in the range of a numeric literal
litNumCheckRange :: Platform -> LitNumType -> Integer -> Bool
litNumCheckRange platform nt i = case nt of
     LitNumInt     -> platformInIntRange platform i
     LitNumWord    -> platformInWordRange platform i
     LitNumInt8    -> inBoundedRange @Int8 i
     LitNumInt16   -> inBoundedRange @Int16 i
     LitNumInt32   -> inBoundedRange @Int32 i
     LitNumInt64   -> inBoundedRange @Int64 i
     LitNumWord8   -> inBoundedRange @Word8 i
     LitNumWord16  -> inBoundedRange @Word16 i
     LitNumWord32  -> inBoundedRange @Word32 i
     LitNumWord64  -> inBoundedRange @Word64 i
     LitNumNatural -> i >= 0
     LitNumInteger -> True

-- | Create a numeric 'Literal' of the given type
mkLitNumber :: Platform -> LitNumType -> Integer -> Literal
mkLitNumber platform nt i =
  ASSERT2(litNumCheckRange platform nt i, integer i)
  (LitNumber nt i)

-- | Creates a 'Literal' of type @Int#@
mkLitInt :: Platform -> Integer -> Literal
mkLitInt platform x = ASSERT2( platformInIntRange platform x,  integer x )
                       (mkLitIntUnchecked x)

-- | Creates a 'Literal' of type @Int#@.
--   If the argument is out of the (target-dependent) range, it is wrapped.
--   See Note [Word/Int underflow/overflow]
mkLitIntWrap :: Platform -> Integer -> Literal
mkLitIntWrap platform i = mkLitNumberWrap platform LitNumInt i

-- | Creates a 'Literal' of type @Int#@ without checking its range.
mkLitIntUnchecked :: Integer -> Literal
mkLitIntUnchecked i = LitNumber LitNumInt i

-- | Creates a 'Literal' of type @Int#@, as well as a 'Bool'ean flag indicating
--   overflow. That is, if the argument is out of the (target-dependent) range
--   the argument is wrapped and the overflow flag will be set.
--   See Note [Word/Int underflow/overflow]
mkLitIntWrapC :: Platform -> Integer -> (Literal, Bool)
mkLitIntWrapC platform i = (n, i /= i')
  where
    n@(LitNumber _ i') = mkLitIntWrap platform i

-- | Creates a 'Literal' of type @Word#@
mkLitWord :: Platform -> Integer -> Literal
mkLitWord platform x = ASSERT2( platformInWordRange platform x, integer x )
                        (mkLitWordUnchecked x)

-- | Creates a 'Literal' of type @Word#@.
--   If the argument is out of the (target-dependent) range, it is wrapped.
--   See Note [Word/Int underflow/overflow]
mkLitWordWrap :: Platform -> Integer -> Literal
mkLitWordWrap platform i = mkLitNumberWrap platform LitNumWord i

-- | Creates a 'Literal' of type @Word#@ without checking its range.
mkLitWordUnchecked :: Integer -> Literal
mkLitWordUnchecked i = LitNumber LitNumWord i

-- | Creates a 'Literal' of type @Word#@, as well as a 'Bool'ean flag indicating
--   carry. That is, if the argument is out of the (target-dependent) range
--   the argument is wrapped and the carry flag will be set.
--   See Note [Word/Int underflow/overflow]
mkLitWordWrapC :: Platform -> Integer -> (Literal, Bool)
mkLitWordWrapC platform i = (n, i /= i')
  where
    n@(LitNumber _ i') = mkLitWordWrap platform i

-- | Creates a 'Literal' of type @Int8#@
mkLitInt8 :: Integer -> Literal
mkLitInt8  x = ASSERT2( inBoundedRange @Int8 x, integer x ) (mkLitInt8Unchecked x)

-- | Creates a 'Literal' of type @Int8#@.
--   If the argument is out of the range, it is wrapped.
mkLitInt8Wrap :: Integer -> Literal
mkLitInt8Wrap i = mkLitInt8Unchecked (toInteger (fromIntegral i :: Int8))

-- | Creates a 'Literal' of type @Int8#@ without checking its range.
mkLitInt8Unchecked :: Integer -> Literal
mkLitInt8Unchecked i = LitNumber LitNumInt8 i

-- | Creates a 'Literal' of type @Word8#@
mkLitWord8 :: Integer -> Literal
mkLitWord8 x = ASSERT2( inBoundedRange @Word8 x, integer x ) (mkLitWord8Unchecked x)

-- | Creates a 'Literal' of type @Word8#@.
--   If the argument is out of the range, it is wrapped.
mkLitWord8Wrap :: Integer -> Literal
mkLitWord8Wrap i = mkLitWord8Unchecked (toInteger (fromIntegral i :: Word8))

-- | Creates a 'Literal' of type @Word8#@ without checking its range.
mkLitWord8Unchecked :: Integer -> Literal
mkLitWord8Unchecked i = LitNumber LitNumWord8 i

-- | Creates a 'Literal' of type @Int16#@
mkLitInt16 :: Integer -> Literal
mkLitInt16  x = ASSERT2( inBoundedRange @Int16 x, integer x ) (mkLitInt16Unchecked x)

-- | Creates a 'Literal' of type @Int16#@.
--   If the argument is out of the range, it is wrapped.
mkLitInt16Wrap :: Integer -> Literal
mkLitInt16Wrap i = mkLitInt16Unchecked (toInteger (fromIntegral i :: Int16))

-- | Creates a 'Literal' of type @Int16#@ without checking its range.
mkLitInt16Unchecked :: Integer -> Literal
mkLitInt16Unchecked i = LitNumber LitNumInt16 i

-- | Creates a 'Literal' of type @Word16#@
mkLitWord16 :: Integer -> Literal
mkLitWord16 x = ASSERT2( inBoundedRange @Word16 x, integer x ) (mkLitWord16Unchecked x)

-- | Creates a 'Literal' of type @Word16#@.
--   If the argument is out of the range, it is wrapped.
mkLitWord16Wrap :: Integer -> Literal
mkLitWord16Wrap i = mkLitWord16Unchecked (toInteger (fromIntegral i :: Word16))

-- | Creates a 'Literal' of type @Word16#@ without checking its range.
mkLitWord16Unchecked :: Integer -> Literal
mkLitWord16Unchecked i = LitNumber LitNumWord16 i

-- | Creates a 'Literal' of type @Int32#@
mkLitInt32 :: Integer -> Literal
mkLitInt32  x = ASSERT2( inBoundedRange @Int32 x, integer x ) (mkLitInt32Unchecked x)

-- | Creates a 'Literal' of type @Int32#@.
--   If the argument is out of the range, it is wrapped.
mkLitInt32Wrap :: Integer -> Literal
mkLitInt32Wrap i = mkLitInt32Unchecked (toInteger (fromIntegral i :: Int32))

-- | Creates a 'Literal' of type @Int32#@ without checking its range.
mkLitInt32Unchecked :: Integer -> Literal
mkLitInt32Unchecked i = LitNumber LitNumInt32 i

-- | Creates a 'Literal' of type @Word32#@
mkLitWord32 :: Integer -> Literal
mkLitWord32 x = ASSERT2( inBoundedRange @Word32 x, integer x ) (mkLitWord32Unchecked x)

-- | Creates a 'Literal' of type @Word32#@.
--   If the argument is out of the range, it is wrapped.
mkLitWord32Wrap :: Integer -> Literal
mkLitWord32Wrap i = mkLitWord32Unchecked (toInteger (fromIntegral i :: Word32))

-- | Creates a 'Literal' of type @Word32#@ without checking its range.
mkLitWord32Unchecked :: Integer -> Literal
mkLitWord32Unchecked i = LitNumber LitNumWord32 i

-- | Creates a 'Literal' of type @Int64#@
mkLitInt64 :: Integer -> Literal
mkLitInt64  x = ASSERT2( inBoundedRange @Int64 x, integer x ) (mkLitInt64Unchecked x)

-- | Creates a 'Literal' of type @Int64#@.
--   If the argument is out of the range, it is wrapped.
mkLitInt64Wrap :: Integer -> Literal
mkLitInt64Wrap i = mkLitInt64Unchecked (toInteger (fromIntegral i :: Int64))

-- | Creates a 'Literal' of type @Int64#@ without checking its range.
mkLitInt64Unchecked :: Integer -> Literal
mkLitInt64Unchecked i = LitNumber LitNumInt64 i

-- | Creates a 'Literal' of type @Word64#@
mkLitWord64 :: Integer -> Literal
mkLitWord64 x = ASSERT2( inBoundedRange @Word64 x, integer x ) (mkLitWord64Unchecked x)

-- | Creates a 'Literal' of type @Word64#@.
--   If the argument is out of the range, it is wrapped.
mkLitWord64Wrap :: Integer -> Literal
mkLitWord64Wrap i = mkLitWord64Unchecked (toInteger (fromIntegral i :: Word64))

-- | Creates a 'Literal' of type @Word64#@ without checking its range.
mkLitWord64Unchecked :: Integer -> Literal
mkLitWord64Unchecked i = LitNumber LitNumWord64 i

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
mkLitString s = LitString (bytesFS $ mkFastString s)

mkLitInteger :: Integer -> Literal
mkLitInteger x = LitNumber LitNumInteger x

mkLitNatural :: Integer -> Literal
mkLitNatural x = ASSERT2( inNaturalRange x,  integer x )
                    (LitNumber LitNumNatural x)

inNaturalRange :: Integer -> Bool
inNaturalRange x = x >= 0

inBoundedRange :: forall a. (Bounded a, Integral a) => Integer -> Bool
inBoundedRange x  = x >= toInteger (minBound :: a) &&
                    x <= toInteger (maxBound :: a)

isMinBound :: Platform -> Literal -> Bool
isMinBound _        (LitChar c)        = c == minBound
isMinBound platform (LitNumber nt i)   = case nt of
   LitNumInt     -> i == platformMinInt platform
   LitNumInt8    -> i == toInteger (minBound :: Int8)
   LitNumInt16   -> i == toInteger (minBound :: Int16)
   LitNumInt32   -> i == toInteger (minBound :: Int32)
   LitNumInt64   -> i == toInteger (minBound :: Int64)
   LitNumWord    -> i == 0
   LitNumWord8   -> i == 0
   LitNumWord16  -> i == 0
   LitNumWord32  -> i == 0
   LitNumWord64  -> i == 0
   LitNumNatural -> i == 0
   LitNumInteger -> False
isMinBound _        _                  = False

isMaxBound :: Platform -> Literal -> Bool
isMaxBound _        (LitChar c)        = c == maxBound
isMaxBound platform (LitNumber nt i)   = case nt of
   LitNumInt     -> i == platformMaxInt platform
   LitNumInt8    -> i == toInteger (maxBound :: Int8)
   LitNumInt16   -> i == toInteger (maxBound :: Int16)
   LitNumInt32   -> i == toInteger (maxBound :: Int32)
   LitNumInt64   -> i == toInteger (maxBound :: Int64)
   LitNumWord    -> i == platformMaxWord platform
   LitNumWord8   -> i == toInteger (maxBound :: Word8)
   LitNumWord16  -> i == toInteger (maxBound :: Word16)
   LitNumWord32  -> i == toInteger (maxBound :: Word32)
   LitNumWord64  -> i == toInteger (maxBound :: Word64)
   LitNumNatural -> False
   LitNumInteger -> False
isMaxBound _        _                  = False

inCharRange :: Char -> Bool
inCharRange c =  c >= '\0' && c <= chr tARGET_MAX_CHAR

-- | Tests whether the literal represents a zero of whatever type it is
isZeroLit :: Literal -> Bool
isZeroLit (LitNumber _ 0) = True
isZeroLit (LitFloat  0)   = True
isZeroLit (LitDouble 0)   = True
isZeroLit _               = False

-- | Tests whether the literal represents a one of whatever type it is
isOneLit :: Literal -> Bool
isOneLit (LitNumber _ 1) = True
isOneLit (LitFloat  1)   = True
isOneLit (LitDouble 1)   = True
isOneLit _               = False

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
isLitValue_maybe (LitNumber _ i)   = Just i
isLitValue_maybe _                 = Nothing

-- | Apply a function to the 'Integer' contained in the 'Literal', for when that
-- makes sense, e.g. for 'Char' and numbers.
-- For fixed-size integral literals, the result will be wrapped in accordance
-- with the semantics of the target type.
-- See Note [Word/Int underflow/overflow]
mapLitValue  :: Platform -> (Integer -> Integer) -> Literal -> Literal
mapLitValue _        f (LitChar   c)      = mkLitChar (fchar c)
   where fchar = chr . fromInteger . f . toInteger . ord
mapLitValue platform f (LitNumber nt i)   = mkLitNumberWrap platform nt (f i)
mapLitValue _        _ l                  = pprPanic "mapLitValue" (ppr l)

{-
        Coercions
        ~~~~~~~~~
-}

charToIntLit, intToCharLit,
  floatToIntLit, intToFloatLit,
  doubleToIntLit, intToDoubleLit,
  floatToDoubleLit, doubleToFloatLit
  :: Literal -> Literal

-- | Narrow a literal number (unchecked result range)
narrowLit' :: forall a. Integral a => LitNumType -> Literal -> Literal
narrowLit' nt' (LitNumber _ i)  = LitNumber nt' (toInteger (fromInteger i :: a))
narrowLit' _   l                = pprPanic "narrowLit" (ppr l)

narrowInt8Lit, narrowInt16Lit, narrowInt32Lit, narrowInt64Lit,
  narrowWord8Lit, narrowWord16Lit, narrowWord32Lit, narrowWord64Lit :: Literal -> Literal
narrowInt8Lit   = narrowLit' @Int8   LitNumInt8
narrowInt16Lit  = narrowLit' @Int16  LitNumInt16
narrowInt32Lit  = narrowLit' @Int32  LitNumInt32
narrowInt64Lit  = narrowLit' @Int64  LitNumInt64
narrowWord8Lit  = narrowLit' @Word8  LitNumWord8
narrowWord16Lit = narrowLit' @Word16 LitNumWord16
narrowWord32Lit = narrowLit' @Word32 LitNumWord32
narrowWord64Lit = narrowLit' @Word64 LitNumWord64

-- | Extend or narrow a fixed-width literal (e.g. 'Int16#') to a target
-- word-sized literal ('Int#' or 'Word#'). Narrowing can only happen on 32-bit
-- architectures when we convert a 64-bit literal into a 32-bit one.
convertToWordLit, convertToIntLit :: Platform -> Literal -> Literal
convertToWordLit platform (LitNumber _nt i)  = mkLitWordWrap platform i
convertToWordLit _platform l                 = pprPanic "convertToWordLit" (ppr l)
convertToIntLit  platform (LitNumber _nt i)  = mkLitIntWrap platform i
convertToIntLit  _platform l                 = pprPanic "convertToIntLit" (ppr l)

charToIntLit (LitChar c)       = mkLitIntUnchecked (toInteger (ord c))
charToIntLit l                 = pprPanic "charToIntLit" (ppr l)
intToCharLit (LitNumber _ i)   = LitChar (chr (fromInteger i))
intToCharLit l                 = pprPanic "intToCharLit" (ppr l)

floatToIntLit (LitFloat f)      = mkLitIntUnchecked (truncate f)
floatToIntLit l                 = pprPanic "floatToIntLit" (ppr l)
intToFloatLit (LitNumber _ i)   = LitFloat (fromInteger i)
intToFloatLit l                 = pprPanic "intToFloatLit" (ppr l)

doubleToIntLit (LitDouble f)     = mkLitIntUnchecked (truncate f)
doubleToIntLit l                 = pprPanic "doubleToIntLit" (ppr l)
intToDoubleLit (LitNumber _ i)   = LitDouble (fromInteger i)
intToDoubleLit l                 = pprPanic "intToDoubleLit" (ppr l)

floatToDoubleLit (LitFloat  f) = LitDouble f
floatToDoubleLit l             = pprPanic "floatToDoubleLit" (ppr l)
doubleToFloatLit (LitDouble d) = LitFloat  d
doubleToFloatLit l             = pprPanic "doubleToFloatLit" (ppr l)

nullAddrLit :: Literal
nullAddrLit = LitNullAddr

-- | A rubbish literal; see Note [Rubbish literals]
rubbishLit :: Bool -> Literal
rubbishLit is_lifted = LitRubbish is_lifted

isRubbishLit :: Literal -> Bool
isRubbishLit (LitRubbish {}) = True
isRubbishLit _               = False

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
--      c.f. GHC.Core.Utils.exprIsTrivial
litIsTrivial (LitString _)    = False
litIsTrivial (LitNumber nt _) = case nt of
  LitNumInteger -> False
  LitNumNatural -> False
  LitNumInt     -> True
  LitNumInt8    -> True
  LitNumInt16   -> True
  LitNumInt32   -> True
  LitNumInt64   -> True
  LitNumWord    -> True
  LitNumWord8   -> True
  LitNumWord16  -> True
  LitNumWord32  -> True
  LitNumWord64  -> True
litIsTrivial _                  = True

-- | True if code space does not go bad if we duplicate this literal
litIsDupable :: Platform -> Literal -> Bool
--      c.f. GHC.Core.Utils.exprIsDupable
litIsDupable platform x = case x of
   (LitNumber nt i) -> case nt of
      LitNumInteger -> platformInIntRange platform i
      LitNumNatural -> platformInWordRange platform i
      LitNumInt     -> True
      LitNumInt8    -> True
      LitNumInt16   -> True
      LitNumInt32   -> True
      LitNumInt64   -> True
      LitNumWord    -> True
      LitNumWord8   -> True
      LitNumWord16  -> True
      LitNumWord32  -> True
      LitNumWord64  -> True
   (LitString _) -> False
   _             -> True

litFitsInChar :: Literal -> Bool
litFitsInChar (LitNumber _ i) = i >= toInteger (ord minBound)
                              && i <= toInteger (ord maxBound)
litFitsInChar _               = False

litIsLifted :: Literal -> Bool
litIsLifted (LitNumber nt _) = case nt of
  LitNumInteger -> True
  LitNumNatural -> True
  LitNumInt     -> False
  LitNumInt8    -> False
  LitNumInt16   -> False
  LitNumInt32   -> False
  LitNumInt64   -> False
  LitNumWord    -> False
  LitNumWord8   -> False
  LitNumWord16  -> False
  LitNumWord32  -> False
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
literalType (LitNumber lt _)  = case lt of
   LitNumInteger -> integerTy
   LitNumNatural -> naturalTy
   LitNumInt     -> intPrimTy
   LitNumInt8    -> int8PrimTy
   LitNumInt16   -> int16PrimTy
   LitNumInt32   -> int32PrimTy
   LitNumInt64   -> int64PrimTy
   LitNumWord    -> wordPrimTy
   LitNumWord8   -> word8PrimTy
   LitNumWord16  -> word16PrimTy
   LitNumWord32  -> word32PrimTy
   LitNumWord64  -> word64PrimTy
literalType (LitRubbish is_lifted) = mkForAllTy a Inferred (mkTyVarTy a)
  where
    -- See Note [Rubbish literals]
    a | is_lifted = alphaTyVar
      | otherwise = alphaTyVarUnliftedRep

absentLiteralOf :: TyCon -> Maybe Literal
-- Return a literal of the appropriate primitive
-- TyCon, to use as a placeholder when it doesn't matter
-- Rubbish literals are handled in GHC.Core.Opt.WorkWrap.Utils, because
--  1. Looking at the TyCon is not enough, we need the actual type
--  2. This would need to return a type application to a literal
absentLiteralOf tc = lookupUFM absent_lits tc

-- We do not use TyConEnv here to avoid import cycles.
absent_lits :: UniqFM TyCon Literal
absent_lits = listToUFM_Directly
                        -- Explicitly construct the mape from the known
                        -- keys of these tyCons.
                        [ (addrPrimTyConKey,    LitNullAddr)
                        , (charPrimTyConKey,    LitChar 'x')
                        , (intPrimTyConKey,     mkLitIntUnchecked 0)
                        , (int8PrimTyConKey,    mkLitInt8Unchecked 0)
                        , (int16PrimTyConKey,   mkLitInt16Unchecked 0)
                        , (int32PrimTyConKey,   mkLitInt32Unchecked 0)
                        , (int64PrimTyConKey,   mkLitInt64Unchecked 0)
                        , (wordPrimTyConKey,    mkLitWordUnchecked 0)
                        , (word8PrimTyConKey,   mkLitWord8Unchecked 0)
                        , (word16PrimTyConKey,  mkLitWord16Unchecked 0)
                        , (word32PrimTyConKey,  mkLitWord32Unchecked 0)
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
cmpLit (LitLabel     a _ _) (LitLabel      b _ _) = a `lexicalCompareFS` b
cmpLit (LitNumber nt1 a)    (LitNumber nt2  b)
  = (nt1 `compare` nt2) `mappend` (a `compare` b)
cmpLit (LitRubbish b1)      (LitRubbish b2)       = b1 `compare` b2
cmpLit lit1 lit2
  | isTrue# (dataToTag# lit1 <# dataToTag# lit2) = LT
  | otherwise                                    = GT

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
pprLiteral add_par (LitNumber nt i)
   = case nt of
       LitNumInteger -> pprIntegerVal add_par i
       LitNumNatural -> pprIntegerVal add_par i
       LitNumInt     -> pprPrimInt i
       LitNumInt8    -> pprPrimInt8 i
       LitNumInt16   -> pprPrimInt16 i
       LitNumInt32   -> pprPrimInt32 i
       LitNumInt64   -> pprPrimInt64 i
       LitNumWord    -> pprPrimWord i
       LitNumWord8   -> pprPrimWord8 i
       LitNumWord16  -> pprPrimWord16 i
       LitNumWord32  -> pprPrimWord32 i
       LitNumWord64  -> pprPrimWord64 i
pprLiteral add_par (LitLabel l mb fod) =
    add_par (text "__label" <+> b <+> ppr fod)
    where b = case mb of
              Nothing -> pprHsString l
              Just x  -> doubleQuotes (text (unpackFS l ++ '@':show x))
pprLiteral _       (LitRubbish is_lifted)
  = text "__RUBBISH"
    <> parens (if is_lifted then text "lifted" else text "unlifted")

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
LitIntN         -1#N
LitWord          1##
LitWordN         1##N
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
  Array#?  Or /lifted/ but /strict/ values, such as a field of
  a strict data constructor.  For these we use LitRubbish.
  See Note [Absent errors] in GHC.Core.Opt.WorkWrap.Utils.hs

The literal (LitRubbish is_lifted)
has type
  LitRubbish :: forall (a :: TYPE LiftedRep). a     if is_lifted
  LitRubbish :: forall (a :: TYPE UnliftedRep). a   otherwise

So we might see a w/w split like
  $wf x z = let y :: Array# Int = (LitRubbish False) @(Array# Int)
            in e

Here are the moving parts, but see also Note [Absent errors] in
GHC.Core.Opt.WorkWrap.Utils

* We define LitRubbish as a constructor in GHC.Types.Literal.Literal

* It is given its polymorphic type by Literal.literalType

* GHC.Core.Opt.WorkWrap.Utils.mk_absent_let introduces a LitRubbish for absent
  arguments of boxed, unlifted type; or boxed, lifted arguments of strict data
  constructors.

* In CoreToSTG we convert (RubishLit @t) to just ().  STG is untyped, so this
  will work OK for both lifted and unlifted (but boxed) values. The important
  thing is that it is a heap pointer, which the garbage collector can follow if
  it encounters it.

  We considered maintaining LitRubbish in STG, and lowering it in the code
  generators, but it seems simpler to do it once and for all in CoreToSTG.

  In GHC.ByteCode.Asm we just lower it as a 0 literal, because it's all boxed to
  the host GC anyway.
-}
