{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Data.Aeson.Encoding.Internal
    (
    -- * Encoding
      Encoding' (..)
    , Encoding
    , encodingToLazyByteString
    , unsafeToEncoding
    , retagEncoding
    , Series (..)
    , pairs
    , pair
    , pairStr
    , pair'
    -- * Predicates
    , nullEncoding
    -- * Encoding constructors
    , emptyArray_
    , emptyObject_
    , wrapObject
    , wrapArray
    , null_
    , bool
    , text
    , lazyText
    , string
    , list
    , dict
    , tuple
    , (>*<)
    , InArray
    , empty
    , (><)
    , econcat
    -- ** Decimal numbers
    , int8, int16, int32, int64, int
    , word8, word16, word32, word64, word
    , integer, float, double, scientific
    -- ** Decimal numbers as Text
    , int8Text, int16Text, int32Text, int64Text, intText
    , word8Text, word16Text, word32Text, word64Text, wordText
    , integerText, floatText, doubleText, scientificText
    -- ** Time
    , day
    , month
    , quarter
    , localTime
    , utcTime
    , timeOfDay
    , zonedTime
    -- ** value
    , value
    -- ** JSON tokens
    , comma, colon, openBracket, closeBracket, openCurly, closeCurly
    ) where

import Prelude.Compat

import Data.Aeson.Types.Internal (Value)
import Data.ByteString.Builder (Builder, char7, toLazyByteString)
import Data.Int
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, UTCTime, ZonedTime)
import Data.Time.Calendar.Month.Compat (Month)
import Data.Time.Calendar.Quarter.Compat (Quarter)
import Data.Typeable (Typeable)
import Data.Word
import qualified Data.Aeson.Encoding.Builder as EB
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as LT

-- | An encoding of a JSON value.
--
-- @tag@ represents which kind of JSON the Encoding is encoding to,
-- we reuse 'Text' and 'Value' as tags here.
newtype Encoding' tag = Encoding {
      fromEncoding :: Builder
      -- ^ Acquire the underlying bytestring builder.
    } deriving (Typeable)

-- | Often used synonym for 'Encoding''.
type Encoding = Encoding' Value

-- | Make Encoding from Builder.
--
-- Use with care! You have to make sure that the passed Builder
-- is a valid JSON Encoding!
unsafeToEncoding :: Builder -> Encoding' a
unsafeToEncoding = Encoding

encodingToLazyByteString :: Encoding' a -> BSL.ByteString
encodingToLazyByteString = toLazyByteString . fromEncoding
{-# INLINE encodingToLazyByteString #-}

retagEncoding :: Encoding' a -> Encoding' b
retagEncoding = Encoding . fromEncoding

-------------------------------------------------------------------------------
-- Encoding instances
-------------------------------------------------------------------------------

instance Show (Encoding' a) where
    show (Encoding e) = show (toLazyByteString e)

instance Eq (Encoding' a) where
    Encoding a == Encoding b = toLazyByteString a == toLazyByteString b

instance Ord (Encoding' a) where
    compare (Encoding a) (Encoding b) =
      compare (toLazyByteString a) (toLazyByteString b)

-- | A series of values that, when encoded, should be separated by
-- commas. Since 0.11.0.0, the '.=' operator is overloaded to create
-- either @(Text, Value)@ or 'Series'. You can use Series when
-- encoding directly to a bytestring builder as in the following
-- example:
--
-- > toEncoding (Person name age) = pairs ("name" .= name <> "age" .= age)
data Series = Empty
            | Value (Encoding' Series)
            deriving (Typeable)

pair :: Text -> Encoding -> Series
pair name val = pair' (text name) val
{-# INLINE pair #-}

pairStr :: String -> Encoding -> Series
pairStr name val = pair' (string name) val
{-# INLINE pairStr #-}

pair' :: Encoding' Text -> Encoding -> Series
pair' name val = Value $ retagEncoding $ retagEncoding name >< colon >< val

instance Semigroup Series where
    Empty   <> a       = a
    a       <> Empty   = a
    Value a <> Value b = Value (a >< comma >< b)

instance Monoid Series where
    mempty  = Empty
    mappend = (<>)

nullEncoding :: Encoding' a -> Bool
nullEncoding = BSL.null . toLazyByteString . fromEncoding

emptyArray_ :: Encoding
emptyArray_ = Encoding EB.emptyArray_

emptyObject_ :: Encoding
emptyObject_ = Encoding EB.emptyObject_

wrapArray :: Encoding' a -> Encoding
wrapArray e = retagEncoding $ openBracket >< e >< closeBracket

wrapObject :: Encoding' a -> Encoding
wrapObject e = retagEncoding $ openCurly >< e >< closeCurly

null_ :: Encoding
null_ = Encoding EB.null_

bool :: Bool -> Encoding
bool True = Encoding "true"
bool False = Encoding "false"

-- | Encode a series of key/value pairs, separated by commas.
pairs :: Series -> Encoding
pairs (Value v) = openCurly >< retagEncoding v >< closeCurly
pairs Empty     = emptyObject_
{-# INLINE pairs #-}

list :: (a -> Encoding) -> [a] -> Encoding
list _  []     = emptyArray_
list to' (x:xs) = openBracket >< to' x >< commas xs >< closeBracket
  where
    commas = foldr (\v vs -> comma >< to' v >< vs) empty
{-# INLINE list #-}

-- | Encode as JSON object
dict
    :: (k -> Encoding' Text)                     -- ^ key encoding
    -> (v -> Encoding)                                -- ^ value encoding
    -> (forall a. (k -> v -> a -> a) -> a -> m -> a)  -- ^ @foldrWithKey@ - indexed fold
    -> m                                              -- ^ container
    -> Encoding
dict encodeKey encodeVal foldrWithKey = pairs . foldrWithKey go mempty
  where
    go k v c = Value (encodeKV k v) <> c
    encodeKV k v = retagEncoding (encodeKey k) >< colon >< retagEncoding (encodeVal v)
{-# INLINE dict #-}

-- | Type tag for tuples contents, see 'tuple'.
data InArray

infixr 6 >*<
-- | See 'tuple'.
(>*<) :: Encoding' a -> Encoding' b -> Encoding' InArray
a >*< b = retagEncoding a >< comma >< retagEncoding b
{-# INLINE (>*<) #-}

empty :: Encoding' a
empty = Encoding mempty

econcat :: [Encoding' a] -> Encoding' a
econcat = foldr (><) empty

infixr 6 ><
(><) :: Encoding' a -> Encoding' a -> Encoding' a
Encoding a >< Encoding b = Encoding (a <> b)
{-# INLINE (><) #-}

-- | Encode as a tuple.
--
-- @
-- toEncoding (X a b c) = tuple $
--     toEncoding a >*<
--     toEncoding b >*<
--     toEncoding c
tuple :: Encoding' InArray -> Encoding
tuple b = retagEncoding $ openBracket >< b >< closeBracket
{-# INLINE tuple #-}

text :: Text -> Encoding' a
text = Encoding . EB.text

lazyText :: LT.Text -> Encoding' a
lazyText t = Encoding $
    B.char7 '"' <>
    LT.foldrChunks (\x xs -> EB.unquoted x <> xs) (B.char7 '"') t

string :: String -> Encoding' a
string = Encoding . EB.string

-------------------------------------------------------------------------------
-- chars
-------------------------------------------------------------------------------

comma, colon, openBracket, closeBracket, openCurly, closeCurly :: Encoding' a
comma        = Encoding $ char7 ','
colon        = Encoding $ char7 ':'
openBracket  = Encoding $ char7 '['
closeBracket = Encoding $ char7 ']'
openCurly    = Encoding $ char7 '{'
closeCurly   = Encoding $ char7 '}'

-------------------------------------------------------------------------------
-- Decimal numbers
-------------------------------------------------------------------------------

int8 :: Int8 -> Encoding
int8 = Encoding . B.int8Dec

int16 :: Int16 -> Encoding
int16 = Encoding . B.int16Dec

int32 :: Int32 -> Encoding
int32 = Encoding . B.int32Dec

int64 :: Int64 -> Encoding
int64 = Encoding . B.int64Dec

int :: Int -> Encoding
int = Encoding . B.intDec

word8 :: Word8 -> Encoding
word8 = Encoding . B.word8Dec

word16 :: Word16 -> Encoding
word16 = Encoding . B.word16Dec

word32 :: Word32 -> Encoding
word32 = Encoding . B.word32Dec

word64 :: Word64 -> Encoding
word64 = Encoding . B.word64Dec

word :: Word -> Encoding
word = Encoding . B.wordDec

integer :: Integer -> Encoding
integer = Encoding . B.integerDec

float :: Float -> Encoding
float = realFloatToEncoding $ Encoding . B.floatDec

double :: Double -> Encoding
double = realFloatToEncoding $ Encoding . B.doubleDec

scientific :: Scientific -> Encoding
scientific = Encoding . EB.scientific

realFloatToEncoding :: RealFloat a => (a -> Encoding) -> a -> Encoding
realFloatToEncoding e d
    | isNaN d || isInfinite d = null_
    | otherwise               = e d
{-# INLINE realFloatToEncoding #-}

-------------------------------------------------------------------------------
-- Decimal numbers as Text
-------------------------------------------------------------------------------

int8Text :: Int8 -> Encoding' a
int8Text = Encoding . EB.quote . B.int8Dec

int16Text :: Int16 -> Encoding' a
int16Text = Encoding . EB.quote . B.int16Dec

int32Text :: Int32 -> Encoding' a
int32Text = Encoding . EB.quote . B.int32Dec

int64Text :: Int64 -> Encoding' a
int64Text = Encoding . EB.quote . B.int64Dec

intText :: Int -> Encoding' a
intText = Encoding . EB.quote . B.intDec

word8Text :: Word8 -> Encoding' a
word8Text = Encoding . EB.quote . B.word8Dec

word16Text :: Word16 -> Encoding' a
word16Text = Encoding . EB.quote . B.word16Dec

word32Text :: Word32 -> Encoding' a
word32Text = Encoding . EB.quote . B.word32Dec

word64Text :: Word64 -> Encoding' a
word64Text = Encoding . EB.quote . B.word64Dec

wordText :: Word -> Encoding' a
wordText = Encoding . EB.quote . B.wordDec

integerText :: Integer -> Encoding' a
integerText = Encoding . EB.quote . B.integerDec

floatText :: Float -> Encoding' a
floatText = Encoding . EB.quote . B.floatDec

doubleText :: Double -> Encoding' a
doubleText = Encoding . EB.quote . B.doubleDec

scientificText :: Scientific -> Encoding' a
scientificText = Encoding . EB.quote . EB.scientific

-------------------------------------------------------------------------------
-- time
-------------------------------------------------------------------------------

day :: Day -> Encoding' a
day = Encoding . EB.quote . EB.day

month :: Month -> Encoding' a
month = Encoding . EB.quote . EB.month

quarter :: Quarter -> Encoding' a
quarter = Encoding . EB.quote . EB.quarter

localTime :: LocalTime -> Encoding' a
localTime = Encoding . EB.quote . EB.localTime

utcTime :: UTCTime -> Encoding' a
utcTime = Encoding . EB.quote . EB.utcTime

timeOfDay :: TimeOfDay -> Encoding' a
timeOfDay = Encoding . EB.quote . EB.timeOfDay

zonedTime :: ZonedTime -> Encoding' a
zonedTime = Encoding . EB.quote . EB.zonedTime

-------------------------------------------------------------------------------
-- Value
-------------------------------------------------------------------------------

value :: Value -> Encoding
value = Encoding . EB.encodeToBuilder
