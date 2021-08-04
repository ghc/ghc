{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

#include "incoherent-compat.h"
#include "overlapping-compat.h"

-- TODO: Drop this when we remove support for Data.Attoparsec.Number
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Data.Aeson.Types.FromJSON
    (
    -- * Core JSON classes
      FromJSON(..)
    -- * Liftings to unary and binary type constructors
    , FromJSON1(..)
    , parseJSON1
    , FromJSON2(..)
    , parseJSON2
    -- * Generic JSON classes
    , GFromJSON(..)
    , FromArgs(..)
    , genericParseJSON
    , genericLiftParseJSON
    -- * Classes and types for map keys
    , FromJSONKey(..)
    , FromJSONKeyFunction(..)
    , fromJSONKeyCoerce
    , coerceFromJSONKeyFunction
    , mapFromJSONKeyFunction

    , GFromJSONKey()
    , genericFromJSONKey

    -- * List functions
    , listParser

    -- * Inspecting @'Value's@
    , withObject
    , withText
    , withArray
    , withScientific
    , withBool
    , withEmbeddedJSON

    -- * Functions
    , fromJSON
    , ifromJSON
    , typeMismatch
    , unexpected
    , parseField
    , parseFieldMaybe
    , parseFieldMaybe'
    , explicitParseField
    , explicitParseFieldMaybe
    , explicitParseFieldMaybe'
    , parseIndexedJSON
    -- ** Operators
    , (.:)
    , (.:?)
    , (.:!)
    , (.!=)

    -- * Internal
    , parseOptionalFieldWith
    ) where

import Prelude.Compat

import Control.Applicative ((<|>), Const(..), liftA2)
import Control.Monad (zipWithM)
import Data.Aeson.Internal.Functions (mapKey)
import Data.Aeson.Parser.Internal (eitherDecodeWith, jsonEOF)
import Data.Aeson.Types.Generic
import Data.Aeson.Types.Internal
import Data.Bits (unsafeShiftR)
import Data.Fixed (Fixed, HasResolution (resolution), Nano)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product (Product(..))
import Data.Functor.Sum (Sum(..))
import Data.Functor.These (These1 (..))
import Data.Hashable (Hashable(..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Ratio ((%), Ratio)
import Data.Scientific (Scientific, base10Exponent)
import Data.Tagged (Tagged(..))
import Data.Text (Text, pack, unpack)
import Data.These (These (..))
import Data.Time (Day, DiffTime, LocalTime, NominalDiffTime, TimeOfDay, UTCTime, ZonedTime)
import Data.Time.Calendar.Compat (CalendarDiffDays (..), DayOfWeek (..))
import Data.Time.Calendar.Month.Compat (Month)
import Data.Time.Calendar.Quarter.Compat (Quarter, QuarterOfYear (..))
import Data.Time.LocalTime.Compat (CalendarDiffTime (..))
import Data.Time.Clock.System.Compat (SystemTime (..))
import Data.Time.Format.Compat (parseTimeM, defaultTimeLocale)
import Data.Traversable as Tr (sequence)
import Data.Vector (Vector)
import Data.Version (Version, parseVersion)
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Storable (Storable)
import Foreign.C.Types (CTime (..))
import GHC.Generics
import Numeric.Natural (Natural)
import Text.ParserCombinators.ReadP (readP_to_S)
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Aeson.Parser.Time as Time
import qualified Data.Attoparsec.ByteString.Char8 as A (endOfInput, parseOnly, scientific)
import qualified Data.ByteString.Lazy as L
import qualified Data.DList as DList
#if MIN_VERSION_dlist(1,0,0) && __GLASGOW_HASKELL__ >=800
import qualified Data.DList.DNonEmpty as DNE
#endif
import qualified Data.Fix as F
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HashSet
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as M
import qualified Data.Monoid as Monoid
import qualified Data.Scientific as Scientific
import qualified Data.Semigroup as Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Strict as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Tree as Tree
import qualified Data.UUID.Types as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

import qualified GHC.Exts as Exts
import qualified Data.Primitive.Array as PM
import qualified Data.Primitive.SmallArray as PM
import qualified Data.Primitive.Types as PM
import qualified Data.Primitive.PrimArray as PM

import Data.Coerce (Coercible, coerce)

parseIndexedJSON :: (Value -> Parser a) -> Int -> Value -> Parser a
parseIndexedJSON p idx value = p value <?> Index idx
{-# INLINE parseIndexedJSON #-}

parseIndexedJSONPair :: (Value -> Parser a) -> (Value -> Parser b) -> Int -> Value -> Parser (a, b)
parseIndexedJSONPair keyParser valParser idx value = p value <?> Index idx
  where
    p = withArray "(k, v)" $ \ab ->
        let n = V.length ab
        in if n == 2
             then (,) <$> parseJSONElemAtIndex keyParser 0 ab
                      <*> parseJSONElemAtIndex valParser 1 ab
             else fail $ "cannot unpack array of length " ++
                         show n ++ " into a pair"
{-# INLINE parseIndexedJSONPair #-}

parseJSONElemAtIndex :: (Value -> Parser a) -> Int -> V.Vector Value -> Parser a
parseJSONElemAtIndex p idx ary = p (V.unsafeIndex ary idx) <?> Index idx

parseRealFloat :: RealFloat a => String -> Value -> Parser a
parseRealFloat _    (Number s) = pure $ Scientific.toRealFloat s
parseRealFloat _    Null       = pure (0/0)
parseRealFloat name v          = prependContext name (unexpected v)
{-# INLINE parseRealFloat #-}

parseIntegralFromScientific :: forall a. Integral a => Scientific -> Parser a
parseIntegralFromScientific s =
    case Scientific.floatingOrInteger s :: Either Double a of
        Right x -> pure x
        Left _  -> fail $ "unexpected floating number " ++ show s
{-# INLINE parseIntegralFromScientific #-}

parseIntegral :: Integral a => String -> Value -> Parser a
parseIntegral name =
    prependContext name . withBoundedScientific' parseIntegralFromScientific
{-# INLINE parseIntegral #-}

parseBoundedIntegralFromScientific :: (Bounded a, Integral a) => Scientific -> Parser a
parseBoundedIntegralFromScientific s = maybe
    (fail $ "value is either floating or will cause over or underflow " ++ show s)
    pure
    (Scientific.toBoundedInteger s)
{-# INLINE parseBoundedIntegralFromScientific #-}

parseBoundedIntegral :: (Bounded a, Integral a) => String -> Value -> Parser a
parseBoundedIntegral name =
    prependContext name . withScientific' parseBoundedIntegralFromScientific
{-# INLINE parseBoundedIntegral #-}

parseScientificText :: Text -> Parser Scientific
parseScientificText
    = either fail pure
    . A.parseOnly (A.scientific <* A.endOfInput)
    . T.encodeUtf8

parseIntegralText :: Integral a => String -> Text -> Parser a
parseIntegralText name t =
    prependContext name $
            parseScientificText t
        >>= rejectLargeExponent
        >>= parseIntegralFromScientific
  where
    rejectLargeExponent :: Scientific -> Parser Scientific
    rejectLargeExponent s = withBoundedScientific' pure (Number s)
{-# INLINE parseIntegralText #-}

parseBoundedIntegralText :: (Bounded a, Integral a) => String -> Text -> Parser a
parseBoundedIntegralText name t =
    prependContext name $
        parseScientificText t >>= parseBoundedIntegralFromScientific

parseOptionalFieldWith :: (Value -> Parser (Maybe a))
                       -> Object -> Text -> Parser (Maybe a)
parseOptionalFieldWith pj obj key =
    case H.lookup key obj of
     Nothing -> pure Nothing
     Just v  -> pj v <?> Key key
{-# INLINE parseOptionalFieldWith #-}

-------------------------------------------------------------------------------
-- Generics
-------------------------------------------------------------------------------

-- | Class of generic representation types that can be converted from JSON.
class GFromJSON arity f where
    -- | This method (applied to 'defaultOptions') is used as the
    -- default generic implementation of 'parseJSON' (if the @arity@ is 'Zero')
    -- or 'liftParseJSON' (if the @arity@ is 'One').
    gParseJSON :: Options -> FromArgs arity a -> Value -> Parser (f a)

-- | A 'FromArgs' value either stores nothing (for 'FromJSON') or it stores the
-- two function arguments that decode occurrences of the type parameter (for
-- 'FromJSON1').
data FromArgs arity a where
    NoFromArgs :: FromArgs Zero a
    From1Args  :: (Value -> Parser a) -> (Value -> Parser [a]) -> FromArgs One a

-- | A configurable generic JSON decoder. This function applied to
-- 'defaultOptions' is used as the default for 'parseJSON' when the
-- type is an instance of 'Generic'.
genericParseJSON :: (Generic a, GFromJSON Zero (Rep a))
                 => Options -> Value -> Parser a
genericParseJSON opts = fmap to . gParseJSON opts NoFromArgs

-- | A configurable generic JSON decoder. This function applied to
-- 'defaultOptions' is used as the default for 'liftParseJSON' when the
-- type is an instance of 'Generic1'.
genericLiftParseJSON :: (Generic1 f, GFromJSON One (Rep1 f))
                     => Options -> (Value -> Parser a) -> (Value -> Parser [a])
                     -> Value -> Parser (f a)
genericLiftParseJSON opts pj pjl = fmap to1 . gParseJSON opts (From1Args pj pjl)

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

-- | A type that can be converted from JSON, with the possibility of
-- failure.
--
-- In many cases, you can get the compiler to generate parsing code
-- for you (see below).  To begin, let's cover writing an instance by
-- hand.
--
-- There are various reasons a conversion could fail.  For example, an
-- 'Object' could be missing a required key, an 'Array' could be of
-- the wrong size, or a value could be of an incompatible type.
--
-- The basic ways to signal a failed conversion are as follows:
--
-- * 'fail' yields a custom error message: it is the recommended way of
-- reporting a failure;
--
-- * 'Control.Applicative.empty' (or 'Control.Monad.mzero') is uninformative:
-- use it when the error is meant to be caught by some @('<|>')@;
--
-- * 'typeMismatch' can be used to report a failure when the encountered value
-- is not of the expected JSON type; 'unexpected' is an appropriate alternative
-- when more than one type may be expected, or to keep the expected type
-- implicit.
--
-- 'prependFailure' (or 'modifyFailure') add more information to a parser's
-- error messages.
--
-- An example type and instance using 'typeMismatch' and 'prependFailure':
--
-- @
-- \-- Allow ourselves to write 'Text' literals.
-- {-\# LANGUAGE OverloadedStrings #-}
--
-- data Coord = Coord { x :: Double, y :: Double }
--
-- instance 'FromJSON' Coord where
--     'parseJSON' ('Object' v) = Coord
--         '<$>' v '.:' \"x\"
--         '<*>' v '.:' \"y\"
--
--     \-- We do not expect a non-'Object' value here.
--     \-- We could use 'Control.Applicative.empty' to fail, but 'typeMismatch'
--     \-- gives a much more informative error message.
--     'parseJSON' invalid    =
--         'prependFailure' "parsing Coord failed, "
--             ('typeMismatch' \"Object\" invalid)
-- @
--
-- For this common case of only being concerned with a single
-- type of JSON value, the functions 'withObject', 'withScientific', etc.
-- are provided. Their use is to be preferred when possible, since
-- they are more terse. Using 'withObject', we can rewrite the above instance
-- (assuming the same language extension and data type) as:
--
-- @
-- instance 'FromJSON' Coord where
--     'parseJSON' = 'withObject' \"Coord\" $ \\v -> Coord
--         '<$>' v '.:' \"x\"
--         '<*>' v '.:' \"y\"
-- @
--
-- Instead of manually writing your 'FromJSON' instance, there are two options
-- to do it automatically:
--
-- * "Data.Aeson.TH" provides Template Haskell functions which will derive an
-- instance at compile time. The generated instance is optimized for your type
-- so it will probably be more efficient than the following option.
--
-- * The compiler can provide a default generic implementation for
-- 'parseJSON'.
--
-- To use the second, simply add a @deriving 'Generic'@ clause to your
-- datatype and declare a 'FromJSON' instance for your datatype without giving
-- a definition for 'parseJSON'.
--
-- For example, the previous example can be simplified to just:
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
--
-- import "GHC.Generics"
--
-- data Coord = Coord { x :: Double, y :: Double } deriving 'Generic'
--
-- instance 'FromJSON' Coord
-- @
--
-- The default implementation will be equivalent to
-- @parseJSON = 'genericParseJSON' 'defaultOptions'@; if you need different
-- options, you can customize the generic decoding by defining:
--
-- @
-- customOptions = 'defaultOptions'
--                 { 'fieldLabelModifier' = 'map' 'Data.Char.toUpper'
--                 }
--
-- instance 'FromJSON' Coord where
--     'parseJSON' = 'genericParseJSON' customOptions
-- @
class FromJSON a where
    parseJSON :: Value -> Parser a

    default parseJSON :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
    parseJSON = genericParseJSON defaultOptions

    parseJSONList :: Value -> Parser [a]
    parseJSONList = withArray "[]" $ \a ->
          zipWithM (parseIndexedJSON parseJSON) [0..]
        . V.toList
        $ a

-------------------------------------------------------------------------------
--  Classes and types for map keys
-------------------------------------------------------------------------------

-- | Read the docs for 'ToJSONKey' first. This class is a conversion
--   in the opposite direction. If you have a newtype wrapper around 'Text',
--   the recommended way to define instances is with generalized newtype deriving:
--
--   > newtype SomeId = SomeId { getSomeId :: Text }
--   >   deriving (Eq,Ord,Hashable,FromJSONKey)
--
--   If you have a sum of nullary constructors, you may use the generic
--   implementation:
--
-- @
-- data Color = Red | Green | Blue
--   deriving Generic
--
-- instance 'FromJSONKey' Color where
--   'fromJSONKey' = 'genericFromJSONKey' 'defaultJSONKeyOptions'
-- @
class FromJSONKey a where
    -- | Strategy for parsing the key of a map-like container.
    fromJSONKey :: FromJSONKeyFunction a
    default fromJSONKey :: FromJSON a => FromJSONKeyFunction a
    fromJSONKey = FromJSONKeyValue parseJSON

    -- | This is similar in spirit to the 'readList' method of 'Read'.
    --   It makes it possible to give 'String' keys special treatment
    --   without using @OverlappingInstances@. End users should always
    --   be able to use the default implementation of this method.
    fromJSONKeyList :: FromJSONKeyFunction [a]
    default fromJSONKeyList :: FromJSON a => FromJSONKeyFunction [a]
    fromJSONKeyList = FromJSONKeyValue parseJSON

-- | This type is related to 'ToJSONKeyFunction'. If 'FromJSONKeyValue' is used in the
--   'FromJSONKey' instance, then 'ToJSONKeyValue' should be used in the 'ToJSONKey'
--   instance. The other three data constructors for this type all correspond to
--   'ToJSONKeyText'. Strictly speaking, 'FromJSONKeyTextParser' is more powerful than
--   'FromJSONKeyText', which is in turn more powerful than 'FromJSONKeyCoerce'.
--   For performance reasons, these exist as three options instead of one.
data FromJSONKeyFunction a where
    FromJSONKeyCoerce :: Coercible Text a => FromJSONKeyFunction a
      -- ^ uses 'coerce'
    FromJSONKeyText :: !(Text -> a) -> FromJSONKeyFunction a
      -- ^ conversion from 'Text' that always succeeds
    FromJSONKeyTextParser :: !(Text -> Parser a) -> FromJSONKeyFunction a
      -- ^ conversion from 'Text' that may fail
    FromJSONKeyValue :: !(Value -> Parser a) -> FromJSONKeyFunction a
      -- ^ conversion for non-textual keys

-- | Only law abiding up to interpretation
instance Functor FromJSONKeyFunction where
    fmap h FromJSONKeyCoerce         = FromJSONKeyText (h . coerce)
    fmap h (FromJSONKeyText f)       = FromJSONKeyText (h . f)
    fmap h (FromJSONKeyTextParser f) = FromJSONKeyTextParser (fmap h . f)
    fmap h (FromJSONKeyValue f)      = FromJSONKeyValue (fmap h . f)

-- | Construct 'FromJSONKeyFunction' for types coercible from 'Text'. This
-- conversion is still unsafe, as 'Hashable' and 'Eq' instances of @a@ should be
-- compatible with 'Text' i.e. hash values should be equal for wrapped values as well.
-- This property will always be maintained if the 'Hashable' and 'Eq' instances
-- are derived with generalized newtype deriving.
-- compatible with 'Text' i.e. hash values be equal for wrapped values as well.
--
-- On pre GHC 7.8 this is unconstrainted function.
fromJSONKeyCoerce ::
    Coercible Text a =>
    FromJSONKeyFunction a
fromJSONKeyCoerce = FromJSONKeyCoerce

-- | Semantically the same as @coerceFromJSONKeyFunction = fmap coerce = coerce@.
--
-- See note on 'fromJSONKeyCoerce'.
coerceFromJSONKeyFunction ::
    Coercible a b =>
    FromJSONKeyFunction a -> FromJSONKeyFunction b
coerceFromJSONKeyFunction = coerce

{-# RULES
  "FromJSONKeyCoerce: fmap coerce" forall x .
                                   fmap coerce x = coerceFromJSONKeyFunction x
  #-}

-- | Same as 'fmap'. Provided for the consistency with 'ToJSONKeyFunction'.
mapFromJSONKeyFunction :: (a -> b) -> FromJSONKeyFunction a -> FromJSONKeyFunction b
mapFromJSONKeyFunction = fmap

-- | 'fromJSONKey' for 'Generic' types.
-- These types must be sums of nullary constructors, whose names will be used
-- as keys for JSON objects.
--
-- See also 'genericToJSONKey'.
--
-- === __Example__
--
-- @
-- data Color = Red | Green | Blue
--   deriving 'Generic'
--
-- instance 'FromJSONKey' Color where
--   'fromJSONKey' = 'genericFromJSONKey' 'defaultJSONKeyOptions'
-- @
genericFromJSONKey :: forall a. (Generic a, GFromJSONKey (Rep a))
             => JSONKeyOptions
             -> FromJSONKeyFunction a
genericFromJSONKey opts = FromJSONKeyTextParser $ \t ->
    case parseSumFromString (keyModifier opts) t of
        Nothing -> fail $
            "invalid key " ++ show t ++ ", expected one of " ++ show cnames
        Just k -> pure (to k)
  where
    cnames = unTagged2 (constructorTags (keyModifier opts) :: Tagged2 (Rep a) [String])

class    (ConstructorNames f, SumFromString f) => GFromJSONKey f where
instance (ConstructorNames f, SumFromString f) => GFromJSONKey f where

-------------------------------------------------------------------------------
-- Functions needed for documentation
-------------------------------------------------------------------------------

-- | Fail parsing due to a type mismatch, with a descriptive message.
--
-- The following wrappers should generally be prefered:
-- 'withObject', 'withArray', 'withText', 'withBool'.
--
-- ==== Error message example
--
-- > typeMismatch "Object" (String "oops")
-- > -- Error: "expected Object, but encountered String"
typeMismatch :: String -- ^ The name of the JSON type being parsed
                       -- (@\"Object\"@, @\"Array\"@, @\"String\"@, @\"Number\"@,
                       -- @\"Boolean\"@, or @\"Null\"@).
             -> Value  -- ^ The actual value encountered.
             -> Parser a
typeMismatch expected actual =
    fail $ "expected " ++ expected ++ ", but encountered " ++ typeOf actual

-- | Fail parsing due to a type mismatch, when the expected types are implicit.
--
-- ==== Error message example
--
-- > unexpected (String "oops")
-- > -- Error: "unexpected String"
unexpected :: Value -> Parser a
unexpected actual = fail $ "unexpected " ++ typeOf actual

-- | JSON type of a value, name of the head constructor.
typeOf :: Value -> String
typeOf v = case v of
    Object _ -> "Object"
    Array _  -> "Array"
    String _ -> "String"
    Number _ -> "Number"
    Bool _   -> "Boolean"
    Null     -> "Null"

-------------------------------------------------------------------------------
-- Lifings of FromJSON and ToJSON to unary and binary type constructors
-------------------------------------------------------------------------------

-- | Lifting of the 'FromJSON' class to unary type constructors.
--
-- Instead of manually writing your 'FromJSON1' instance, there are two options
-- to do it automatically:
--
-- * "Data.Aeson.TH" provides Template Haskell functions which will derive an
-- instance at compile time. The generated instance is optimized for your type
-- so it will probably be more efficient than the following option.
--
-- * The compiler can provide a default generic implementation for
-- 'liftParseJSON'.
--
-- To use the second, simply add a @deriving 'Generic1'@ clause to your
-- datatype and declare a 'FromJSON1' instance for your datatype without giving
-- a definition for 'liftParseJSON'.
--
-- For example:
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
--
-- import "GHC.Generics"
--
-- data Pair a b = Pair { pairFst :: a, pairSnd :: b } deriving 'Generic1'
--
-- instance 'FromJSON' a => 'FromJSON1' (Pair a)
-- @
--
-- If the default implementation doesn't give exactly the results you want,
-- you can customize the generic decoding with only a tiny amount of
-- effort, using 'genericLiftParseJSON' with your preferred 'Options':
--
-- @
-- customOptions = 'defaultOptions'
--                 { 'fieldLabelModifier' = 'map' 'Data.Char.toUpper'
--                 }
--
-- instance 'FromJSON' a => 'FromJSON1' (Pair a) where
--     'liftParseJSON' = 'genericLiftParseJSON' customOptions
-- @
class FromJSON1 f where
    liftParseJSON :: (Value -> Parser a) -> (Value -> Parser [a]) -> Value -> Parser (f a)

    default liftParseJSON :: (Generic1 f, GFromJSON One (Rep1 f))
                          => (Value -> Parser a) -> (Value -> Parser [a]) -> Value -> Parser (f a)
    liftParseJSON = genericLiftParseJSON defaultOptions

    liftParseJSONList :: (Value -> Parser a) -> (Value -> Parser [a]) -> Value -> Parser [f a]
    liftParseJSONList f g v = listParser (liftParseJSON f g) v

-- | Lift the standard 'parseJSON' function through the type constructor.
parseJSON1 :: (FromJSON1 f, FromJSON a) => Value -> Parser (f a)
parseJSON1 = liftParseJSON parseJSON parseJSONList
{-# INLINE parseJSON1 #-}

-- | Lifting of the 'FromJSON' class to binary type constructors.
--
-- Instead of manually writing your 'FromJSON2' instance, "Data.Aeson.TH"
-- provides Template Haskell functions which will derive an instance at compile time.

-- The compiler cannot provide a default generic implementation for 'liftParseJSON2',
-- unlike 'parseJSON' and 'liftParseJSON'.
class FromJSON2 f where
    liftParseJSON2
        :: (Value -> Parser a)
        -> (Value -> Parser [a])
        -> (Value -> Parser b)
        -> (Value -> Parser [b])
        -> Value -> Parser (f a b)
    liftParseJSONList2
        :: (Value -> Parser a)
        -> (Value -> Parser [a])
        -> (Value -> Parser b)
        -> (Value -> Parser [b])
        -> Value -> Parser [f a b]
    liftParseJSONList2 fa ga fb gb = withArray "[]" $ \vals ->
        fmap V.toList (V.mapM (liftParseJSON2 fa ga fb gb) vals)

-- | Lift the standard 'parseJSON' function through the type constructor.
parseJSON2 :: (FromJSON2 f, FromJSON a, FromJSON b) => Value -> Parser (f a b)
parseJSON2 = liftParseJSON2 parseJSON parseJSONList parseJSON parseJSONList
{-# INLINE parseJSON2 #-}

-------------------------------------------------------------------------------
-- List functions
-------------------------------------------------------------------------------

-- | Helper function to use with 'liftParseJSON'. See 'Data.Aeson.ToJSON.listEncoding'.
listParser :: (Value -> Parser a) -> Value -> Parser [a]
listParser f (Array xs) = fmap V.toList (V.mapM f xs)
listParser _ v          = typeMismatch "Array" v
{-# INLINE listParser #-}

-------------------------------------------------------------------------------
-- [] instances
-------------------------------------------------------------------------------

instance FromJSON1 [] where
    liftParseJSON _ p' = p'
    {-# INLINE liftParseJSON #-}

instance (FromJSON a) => FromJSON [a] where
    parseJSON = parseJSON1

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

-- | Add context to a failure message, indicating the name of the structure
-- being parsed.
--
-- > prependContext "MyType" (fail "[error message]")
-- > -- Error: "parsing MyType failed, [error message]"
prependContext :: String -> Parser a -> Parser a
prependContext name = prependFailure ("parsing " ++ name ++ " failed, ")

-- | @'withObject' name f value@ applies @f@ to the 'Object' when @value@
-- is an 'Data.Aeson.Object' and fails otherwise.
--
-- ==== Error message example
--
-- > withObject "MyType" f (String "oops")
-- > -- Error: "parsing MyType failed, expected Object, but encountered String"
withObject :: String -> (Object -> Parser a) -> Value -> Parser a
withObject _    f (Object obj) = f obj
withObject name _ v            = prependContext name (typeMismatch "Object" v)
{-# INLINE withObject #-}

-- | @'withText' name f value@ applies @f@ to the 'Text' when @value@ is a
-- 'Data.Aeson.String' and fails otherwise.
--
-- ==== Error message example
--
-- > withText "MyType" f Null
-- > -- Error: "parsing MyType failed, expected String, but encountered Null"
withText :: String -> (Text -> Parser a) -> Value -> Parser a
withText _    f (String txt) = f txt
withText name _ v            = prependContext name (typeMismatch "String" v)
{-# INLINE withText #-}

-- | @'withArray' expected f value@ applies @f@ to the 'Array' when @value@ is
-- an 'Data.Aeson.Array' and fails otherwise.
--
-- ==== Error message example
--
-- > withArray "MyType" f (String "oops")
-- > -- Error: "parsing MyType failed, expected Array, but encountered String"
withArray :: String -> (Array -> Parser a) -> Value -> Parser a
withArray _    f (Array arr) = f arr
withArray name _ v           = prependContext name (typeMismatch "Array" v)
{-# INLINE withArray #-}

-- | @'withScientific' name f value@ applies @f@ to the 'Scientific' number
-- when @value@ is a 'Data.Aeson.Number' and fails using 'typeMismatch'
-- otherwise.
--
-- /Warning/: If you are converting from a scientific to an unbounded
-- type such as 'Integer' you may want to add a restriction on the
-- size of the exponent (see 'withBoundedScientific') to prevent
-- malicious input from filling up the memory of the target system.
--
-- ==== Error message example
--
-- > withScientific "MyType" f (String "oops")
-- > -- Error: "parsing MyType failed, expected Number, but encountered String"
withScientific :: String -> (Scientific -> Parser a) -> Value -> Parser a
withScientific _ f (Number scientific) = f scientific
withScientific name _ v = prependContext name (typeMismatch "Number" v)
{-# INLINE withScientific #-}

-- | A variant of 'withScientific' which doesn't use 'prependContext', so that
-- such context can be added separately in a way that also applies when the
-- continuation @f :: Scientific -> Parser a@ fails.
--
-- /Warning/: If you are converting from a scientific to an unbounded
-- type such as 'Integer' you may want to add a restriction on the
-- size of the exponent (see 'withBoundedScientific') to prevent
-- malicious input from filling up the memory of the target system.
--
-- ==== Error message examples
--
-- > withScientific' f (String "oops")
-- > -- Error: "unexpected String"
-- >
-- > prependContext "MyType" (withScientific' f (String "oops"))
-- > -- Error: "parsing MyType failed, unexpected String"
withScientific' :: (Scientific -> Parser a) -> Value -> Parser a
withScientific' f v = case v of
    Number n -> f n
    _ -> typeMismatch "Number" v
{-# INLINE withScientific' #-}

-- | @'withBoundedScientific' name f value@ applies @f@ to the 'Scientific' number
-- when @value@ is a 'Number' with exponent less than or equal to 1024.
withBoundedScientific :: String -> (Scientific -> Parser a) -> Value -> Parser a
withBoundedScientific name f v = withBoundedScientific_ (prependContext name) f v
{-# INLINE withBoundedScientific #-}

-- | A variant of 'withBoundedScientific' which doesn't use 'prependContext',
-- so that such context can be added separately in a way that also applies
-- when the continuation @f :: Scientific -> Parser a@ fails.
withBoundedScientific' :: (Scientific -> Parser a) -> Value -> Parser a
withBoundedScientific' f v = withBoundedScientific_ id f v
{-# INLINE withBoundedScientific' #-}

-- | A variant of 'withBoundedScientific_' parameterized by a function to apply
-- to the 'Parser' in case of failure.
withBoundedScientific_ :: (Parser a -> Parser a) -> (Scientific -> Parser a) -> Value -> Parser a
withBoundedScientific_ whenFail f (Number scientific) =
    if exp10 > 1024
    then whenFail (fail msg)
    else f scientific
  where
    exp10 = base10Exponent scientific
    msg = "found a number with exponent " ++ show exp10 ++ ", but it must not be greater than 1024"
withBoundedScientific_ whenFail _ v =
    whenFail (typeMismatch "Number" v)
{-# INLINE withBoundedScientific_ #-}

-- | @'withBool' expected f value@ applies @f@ to the 'Bool' when @value@ is a
-- 'Boolean' and fails otherwise.
--
-- ==== Error message example
--
-- > withBool "MyType" f (String "oops")
-- > -- Error: "parsing MyType failed, expected Boolean, but encountered String"
withBool :: String -> (Bool -> Parser a) -> Value -> Parser a
withBool _    f (Bool arr) = f arr
withBool name _ v          = prependContext name (typeMismatch "Boolean" v)
{-# INLINE withBool #-}

-- | Decode a nested JSON-encoded string.
withEmbeddedJSON :: String -> (Value -> Parser a) -> Value -> Parser a
withEmbeddedJSON _ innerParser (String txt) =
    either fail innerParser $ eitherDecode (L.fromStrict $ T.encodeUtf8 txt)
    where
        eitherDecode = eitherFormatError . eitherDecodeWith jsonEOF ifromJSON
        eitherFormatError = either (Left . uncurry formatError) Right
withEmbeddedJSON name _ v = prependContext name (typeMismatch "String" v)
{-# INLINE withEmbeddedJSON #-}

-- | Convert a value from JSON, failing if the types do not match.
fromJSON :: (FromJSON a) => Value -> Result a
fromJSON = parse parseJSON
{-# INLINE fromJSON #-}

-- | Convert a value from JSON, failing if the types do not match.
ifromJSON :: (FromJSON a) => Value -> IResult a
ifromJSON = iparse parseJSON
{-# INLINE ifromJSON #-}

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'empty' if the key is not present or the value cannot
-- be converted to the desired type.
--
-- This accessor is appropriate if the key and value /must/ be present
-- in an object for it to be valid.  If the key and value are
-- optional, use '.:?' instead.
(.:) :: (FromJSON a) => Object -> Text -> Parser a
(.:) = explicitParseField parseJSON
{-# INLINE (.:) #-}

-- | Retrieve the value associated with the given key of an 'Object'. The
-- result is 'Nothing' if the key is not present or if its value is 'Null',
-- or 'empty' if the value cannot be converted to the desired type.
--
-- This accessor is most useful if the key and value can be absent
-- from an object without affecting its validity.  If the key and
-- value are mandatory, use '.:' instead.
(.:?) :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
(.:?) = explicitParseFieldMaybe parseJSON
{-# INLINE (.:?) #-}

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'Nothing' if the key is not present or 'empty' if the
-- value cannot be converted to the desired type.
--
-- This differs from '.:?' by attempting to parse 'Null' the same as any
-- other JSON value, instead of interpreting it as 'Nothing'.
(.:!) :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
(.:!) = explicitParseFieldMaybe' parseJSON
{-# INLINE (.:!) #-}

-- | Function variant of '.:'.
parseField :: (FromJSON a) => Object -> Text -> Parser a
parseField = (.:)
{-# INLINE parseField #-}

-- | Function variant of '.:?'.
parseFieldMaybe :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
parseFieldMaybe = (.:?)
{-# INLINE parseFieldMaybe #-}

-- | Function variant of '.:!'.
parseFieldMaybe' :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
parseFieldMaybe' = (.:!)
{-# INLINE parseFieldMaybe' #-}

-- | Variant of '.:' with explicit parser function.
--
-- E.g. @'explicitParseField' 'parseJSON1' :: ('FromJSON1' f, 'FromJSON' a) -> 'Object' -> 'Text' -> 'Parser' (f a)@
explicitParseField :: (Value -> Parser a) -> Object -> Text -> Parser a
explicitParseField p obj key = case H.lookup key obj of
    Nothing -> fail $ "key " ++ show key ++ " not found"
    Just v  -> p v <?> Key key
{-# INLINE explicitParseField #-}

-- | Variant of '.:?' with explicit parser function.
explicitParseFieldMaybe :: (Value -> Parser a) -> Object -> Text -> Parser (Maybe a)
explicitParseFieldMaybe p obj key = case H.lookup key obj of
    Nothing -> pure Nothing
    Just v  -> liftParseJSON p (listParser p) v <?> Key key -- listParser isn't used by maybe instance.
{-# INLINE explicitParseFieldMaybe #-}

-- | Variant of '.:!' with explicit parser function.
explicitParseFieldMaybe' :: (Value -> Parser a) -> Object -> Text -> Parser (Maybe a)
explicitParseFieldMaybe' p obj key = case H.lookup key obj of
    Nothing -> pure Nothing
    Just v  -> Just <$> p v <?> Key key
{-# INLINE explicitParseFieldMaybe' #-}

-- | Helper for use in combination with '.:?' to provide default
-- values for optional JSON object fields.
--
-- This combinator is most useful if the key and value can be absent
-- from an object without affecting its validity and we know a default
-- value to assign in that case.  If the key and value are mandatory,
-- use '.:' instead.
--
-- Example usage:
--
-- @ v1 <- o '.:?' \"opt_field_with_dfl\" .!= \"default_val\"
-- v2 <- o '.:'  \"mandatory_field\"
-- v3 <- o '.:?' \"opt_field2\"
-- @
(.!=) :: Parser (Maybe a) -> a -> Parser a
pmval .!= val = fromMaybe val <$> pmval
{-# INLINE (.!=) #-}

--------------------------------------------------------------------------------
-- Generic parseJSON
-------------------------------------------------------------------------------

instance GFromJSON arity V1 where
    -- Whereof we cannot format, thereof we cannot parse:
    gParseJSON _ _ _ = fail "Attempted to parse empty type"


instance OVERLAPPABLE_ (GFromJSON arity a) => GFromJSON arity (M1 i c a) where
    -- Meta-information, which is not handled elsewhere, is just added to the
    -- parsed value:
    gParseJSON opts fargs = fmap M1 . gParseJSON opts fargs

-- Information for error messages

type TypeName = String
type ConName = String

-- | Add the name of the type being parsed to a parser's error messages.
contextType :: TypeName -> Parser a -> Parser a
contextType = prependContext

-- | Add the tagKey that will be looked up while building an ADT
-- | Produce the error equivalent to
-- | Left "Error in $: parsing T failed, expected an object with keys "tag" and
-- | "contents", where "tag" i-- |s associated to one of ["Foo", "Bar"],
-- | The parser returned error was: could not find key "tag"
contextTag :: Text -> [String] -> Parser a -> Parser a
contextTag tagKey cnames = prependFailure
  ("expected Object with key \"" ++ unpack tagKey ++ "\"" ++
  " containing one of " ++ show cnames ++ ", ")

-- | Add the name of the constructor being parsed to a parser's error messages.
contextCons :: ConName -> TypeName -> Parser a -> Parser a
contextCons cname tname = prependContext (showCons cname tname)

-- | Render a constructor as @\"MyType(MyConstructor)\"@.
showCons :: ConName -> TypeName -> String
showCons cname tname = tname ++ "(" ++ cname ++ ")"

--------------------------------------------------------------------------------

-- Parsing single fields

instance (FromJSON a) => GFromJSON arity (K1 i a) where
    -- Constant values are decoded using their FromJSON instance:
    gParseJSON _opts _ = fmap K1 . parseJSON

instance GFromJSON One Par1 where
    -- Direct occurrences of the last type parameter are decoded with the
    -- function passed in as an argument:
    gParseJSON _opts (From1Args pj _) = fmap Par1 . pj

instance (FromJSON1 f) => GFromJSON One (Rec1 f) where
    -- Recursive occurrences of the last type parameter are decoded using their
    -- FromJSON1 instance:
    gParseJSON _opts (From1Args pj pjl) = fmap Rec1 . liftParseJSON pj pjl

instance (FromJSON1 f, GFromJSON One g) => GFromJSON One (f :.: g) where
    -- If an occurrence of the last type parameter is nested inside two
    -- composed types, it is decoded by using the outermost type's FromJSON1
    -- instance to generically decode the innermost type:
    gParseJSON opts fargs =
        let gpj = gParseJSON opts fargs in
        fmap Comp1 . liftParseJSON gpj (listParser gpj)

--------------------------------------------------------------------------------

instance (GFromJSON' arity a, Datatype d) => GFromJSON arity (D1 d a) where
    -- Meta-information, which is not handled elsewhere, is just added to the
    -- parsed value:
    gParseJSON opts fargs = fmap M1 . gParseJSON' (tname :* opts :* fargs)
      where
        tname = moduleName proxy ++ "." ++ datatypeName proxy
        proxy = undefined :: M1 _i d _f _p

-- | 'GFromJSON', after unwrapping the 'D1' constructor, now carrying the data
-- type's name.
class GFromJSON' arity f where
    gParseJSON' :: TypeName :* Options :* FromArgs arity a
                -> Value
                -> Parser (f a)

-- | Single constructor.
instance ( ConsFromJSON arity a
         , AllNullary         (C1 c a) allNullary
         , ParseSum     arity (C1 c a) allNullary
         , Constructor c
         ) => GFromJSON' arity (C1 c a) where
    -- The option 'tagSingleConstructors' determines whether to wrap
    -- a single-constructor type.
    gParseJSON' p@(_ :* opts :* _)
        | tagSingleConstructors opts
            = (unTagged :: Tagged allNullary (Parser (C1 c a p)) -> Parser (C1 c a p))
            . parseSum p
        | otherwise = fmap M1 . consParseJSON (cname :* p)
      where
        cname = conName (undefined :: M1 _i c _f _p)

-- | Multiple constructors.
instance ( AllNullary          (a :+: b) allNullary
         , ParseSum      arity (a :+: b) allNullary
         ) => GFromJSON' arity (a :+: b) where
    -- If all constructors of a sum datatype are nullary and the
    -- 'allNullaryToStringTag' option is set they are expected to be
    -- encoded as strings.  This distinction is made by 'parseSum':
    gParseJSON' p =
        (unTagged :: Tagged allNullary (Parser ((a :+: b) _d)) ->
                                        Parser ((a :+: b) _d))
                   . parseSum p

--------------------------------------------------------------------------------

class ParseSum arity f allNullary where
    parseSum :: TypeName :* Options :* FromArgs arity a
             -> Value
             -> Tagged allNullary (Parser (f a))

instance ( ConstructorNames        f
         , SumFromString           f
         , FromPair          arity f
         , FromTaggedObject  arity f
         , FromUntaggedValue arity f
         ) => ParseSum       arity f True where
    parseSum p@(tname :* opts :* _)
        | allNullaryToStringTag opts = Tagged . parseAllNullarySum tname opts
        | otherwise                  = Tagged . parseNonAllNullarySum p

instance ( ConstructorNames        f
         , FromPair          arity f
         , FromTaggedObject  arity f
         , FromUntaggedValue arity f
         ) => ParseSum       arity f False where
    parseSum p = Tagged . parseNonAllNullarySum p

--------------------------------------------------------------------------------

parseAllNullarySum :: (SumFromString f, ConstructorNames f)
                   => TypeName -> Options -> Value -> Parser (f a)
parseAllNullarySum tname opts =
    withText tname $ \tag ->
        maybe (badTag tag) return $
            parseSumFromString modifier tag
  where
    badTag tag = failWithCTags tname modifier $ \cnames ->
        "expected one of the tags " ++ show cnames ++
        ", but found tag " ++ show tag
    modifier = constructorTagModifier opts

-- | Fail with an informative error message about a mismatched tag.
-- The error message is parameterized by the list of expected tags,
-- to be inferred from the result type of the parser.
failWithCTags
  :: forall f a t. ConstructorNames f
  => TypeName -> (String -> t) -> ([t] -> String) -> Parser (f a)
failWithCTags tname modifier f =
    contextType tname . fail $ f cnames
  where
    cnames = unTagged2 (constructorTags modifier :: Tagged2 f [t])

class SumFromString f where
    parseSumFromString :: (String -> String) -> Text -> Maybe (f a)

instance (SumFromString a, SumFromString b) => SumFromString (a :+: b) where
    parseSumFromString opts key = (L1 <$> parseSumFromString opts key) <|>
                                  (R1 <$> parseSumFromString opts key)

instance (Constructor c) => SumFromString (C1 c U1) where
    parseSumFromString modifier key
        | key == name = Just $ M1 U1
        | otherwise   = Nothing
      where
        name = pack $ modifier $ conName (undefined :: M1 _i c _f _p)

-- For genericFromJSONKey
instance SumFromString a => SumFromString (D1 d a) where
    parseSumFromString modifier key = M1 <$> parseSumFromString modifier key

-- | List of all constructor tags.
constructorTags :: ConstructorNames a => (String -> t) -> Tagged2 a [t]
constructorTags modifier =
    fmap DList.toList (constructorNames' modifier)

-- | List of all constructor names of an ADT, after a given conversion
-- function. (Better inlining.)
class ConstructorNames a where
    constructorNames' :: (String -> t) -> Tagged2 a (DList.DList t)

instance (ConstructorNames a, ConstructorNames b) => ConstructorNames (a :+: b) where
    constructorNames' = liftA2 append constructorNames' constructorNames'
      where
        append
          :: Tagged2 a (DList.DList t)
          -> Tagged2 b (DList.DList t)
          -> Tagged2 (a :+: b) (DList.DList t)
        append (Tagged2 xs) (Tagged2 ys) = Tagged2 (DList.append xs ys)

instance Constructor c => ConstructorNames (C1 c a) where
    constructorNames' f = Tagged2 (pure (f cname))
      where
        cname = conName (undefined :: M1 _i c _f _p)

-- For genericFromJSONKey
instance ConstructorNames a => ConstructorNames (D1 d a) where
    constructorNames' = retag . constructorNames'
      where
        retag :: Tagged2 a u -> Tagged2 (D1 d a) u
        retag (Tagged2 x) = Tagged2 x

--------------------------------------------------------------------------------
parseNonAllNullarySum :: forall f c arity.
                         ( FromPair          arity f
                         , FromTaggedObject  arity f
                         , FromUntaggedValue arity f
                         , ConstructorNames        f
                         ) => TypeName :* Options :* FromArgs arity c
                           -> Value -> Parser (f c)
parseNonAllNullarySum p@(tname :* opts :* _) =
    case sumEncoding opts of
      TaggedObject{..} ->
          withObject tname $ \obj -> do
              tag <- contextType tname . contextTag tagKey cnames_ $ obj .: tagKey
              fromMaybe (badTag tag <?> Key tagKey) $
                  parseFromTaggedObject (tag :* contentsFieldName :* p) obj
        where
          tagKey = pack tagFieldName
          badTag tag = failWith_ $ \cnames ->
              "expected tag field to be one of " ++ show cnames ++
              ", but found tag " ++ show tag
          cnames_ = unTagged2 (constructorTags (constructorTagModifier opts) :: Tagged2 f [String])

      ObjectWithSingleField ->
          withObject tname $ \obj -> case H.toList obj of
              [(tag, v)] -> maybe (badTag tag) (<?> Key tag) $
                  parsePair (tag :* p) v
              _ -> contextType tname . fail $
                  "expected an Object with a single pair, but found " ++
                  show (H.size obj) ++ " pairs"
        where
          badTag tag = failWith_ $ \cnames ->
              "expected an Object with a single pair where the tag is one of " ++
              show cnames ++ ", but found tag " ++ show tag

      TwoElemArray ->
          withArray tname $ \arr -> case V.length arr of
              2 | String tag <- V.unsafeIndex arr 0 ->
                  maybe (badTag tag <?> Index 0) (<?> Index 1) $
                      parsePair (tag :* p) (V.unsafeIndex arr 1)
                | otherwise ->
                  contextType tname $
                      fail "tag element is not a String" <?> Index 0
              len -> contextType tname . fail $
                  "expected a 2-element Array, but encountered an Array of length " ++
                  show len
        where
          badTag tag = failWith_ $ \cnames ->
              "expected tag of the 2-element Array to be one of " ++
              show cnames ++ ", but found tag " ++ show tag

      UntaggedValue -> parseUntaggedValue p
  where
    failWith_ = failWithCTags tname (constructorTagModifier opts)

--------------------------------------------------------------------------------

class FromTaggedObject arity f where
    -- The first two components of the parameter tuple are: the constructor tag
    -- to match against, and the contents field name.
    parseFromTaggedObject
        :: Text :* String :* TypeName :* Options :* FromArgs arity a
        -> Object
        -> Maybe (Parser (f a))

instance ( FromTaggedObject arity a, FromTaggedObject arity b) =>
    FromTaggedObject arity (a :+: b) where
        parseFromTaggedObject p obj =
            (fmap L1 <$> parseFromTaggedObject p obj) <|>
            (fmap R1 <$> parseFromTaggedObject p obj)

instance ( IsRecord                f isRecord
         , FromTaggedObject' arity f isRecord
         , Constructor c
         ) => FromTaggedObject arity (C1 c f) where
    parseFromTaggedObject (tag :* contentsFieldName :* p@(_ :* opts :* _))
        | tag == tag'
        = Just . fmap M1 .
            (unTagged :: Tagged isRecord (Parser (f a)) -> Parser (f a)) .
            parseFromTaggedObject' (contentsFieldName :* cname :* p)
        | otherwise = const Nothing
      where
        tag' = pack $ constructorTagModifier opts cname
        cname = conName (undefined :: M1 _i c _f _p)

--------------------------------------------------------------------------------

class FromTaggedObject' arity f isRecord where
    -- The first component of the parameter tuple is the contents field name.
    parseFromTaggedObject'
        :: String :* ConName :* TypeName :* Options :* FromArgs arity a
        -> Object -> Tagged isRecord (Parser (f a))

instance (RecordFromJSON arity f, FieldNames f) => FromTaggedObject' arity f True where
    -- Records are unpacked in the tagged object
    parseFromTaggedObject' (_ :* p) = Tagged . recordParseJSON (True :* p)

instance (ConsFromJSON arity f) => FromTaggedObject' arity f False where
    -- Nonnullary nonrecords are encoded in the contents field
    parseFromTaggedObject' p obj = Tagged $ do
        contents <- contextCons cname tname (obj .: key)
        consParseJSON p' contents <?> Key key
      where
        key = pack contentsFieldName
        contentsFieldName :* p'@(cname :* tname :* _) = p

instance OVERLAPPING_ FromTaggedObject' arity U1 False where
    -- Nullary constructors don't need a contents field
    parseFromTaggedObject' _ _ = Tagged (pure U1)

--------------------------------------------------------------------------------

-- | Constructors need to be decoded differently depending on whether they're
-- a record or not. This distinction is made by 'ConsParseJSON'.
class ConsFromJSON arity f where
    consParseJSON
        :: ConName :* TypeName :* Options :* FromArgs arity a
        -> Value -> Parser (f a)

class ConsFromJSON' arity f isRecord where
    consParseJSON'
        :: ConName :* TypeName :* Options :* FromArgs arity a
        -> Value -> Tagged isRecord (Parser (f a))

instance ( IsRecord            f isRecord
         , ConsFromJSON' arity f isRecord
         ) => ConsFromJSON arity f where
    consParseJSON p =
      (unTagged :: Tagged isRecord (Parser (f a)) -> Parser (f a))
          . consParseJSON' p

instance OVERLAPPING_
         ( GFromJSON arity a, RecordFromJSON arity (S1 s a)
         ) => ConsFromJSON' arity (S1 s a) True where
    consParseJSON' p@(cname :* tname :* opts :* fargs)
        | unwrapUnaryRecords opts = Tagged . fmap M1 . gParseJSON opts fargs
        | otherwise = Tagged . withObject (showCons cname tname) (recordParseJSON (False :* p))

instance RecordFromJSON arity f => ConsFromJSON' arity f True where
    consParseJSON' p@(cname :* tname :* _) =
        Tagged . withObject (showCons cname tname) (recordParseJSON (False :* p))

instance OVERLAPPING_
         ConsFromJSON' arity U1 False where
    -- Empty constructors are expected to be encoded as an empty array:
    consParseJSON' (cname :* tname :* _) v =
        Tagged . contextCons cname tname $ case v of
            Array a | V.null a -> pure U1
                    | otherwise -> fail_ a
            _ -> typeMismatch "Array" v
      where
        fail_ a = fail $
            "expected an empty Array, but encountered an Array of length " ++
            show (V.length a)

instance OVERLAPPING_
         GFromJSON arity f => ConsFromJSON' arity (S1 s f) False where
    consParseJSON' (_ :* _ :* opts :* fargs) =
        Tagged . fmap M1 . gParseJSON opts fargs

instance (ProductFromJSON arity f, ProductSize f
         ) => ConsFromJSON' arity f False where
    consParseJSON' p = Tagged . productParseJSON0 p

--------------------------------------------------------------------------------

class FieldNames f where
    fieldNames :: f a -> [String] -> [String]

instance (FieldNames a, FieldNames b) => FieldNames (a :*: b) where
    fieldNames _ =
      fieldNames (undefined :: a x) .
      fieldNames (undefined :: b y)

instance (Selector s) => FieldNames (S1 s f) where
    fieldNames _ = (selName (undefined :: M1 _i s _f _p) :)

class RecordFromJSON arity f where
    recordParseJSON
        :: Bool :* ConName :* TypeName :* Options :* FromArgs arity a
        -> Object -> Parser (f a)

instance ( FieldNames f
         , RecordFromJSON' arity f
         ) => RecordFromJSON arity f where
    recordParseJSON (fromTaggedSum :* p@(cname :* tname :* opts :* _)) =
        \obj -> checkUnknown obj >> recordParseJSON' p obj
        where
            knownFields :: H.HashMap Text ()
            knownFields = H.fromList $ map ((,()) . pack) $
                [tagFieldName (sumEncoding opts) | fromTaggedSum] <>
                (fieldLabelModifier opts <$> fieldNames (undefined :: f a) [])

            checkUnknown =
                if not (rejectUnknownFields opts)
                then \_ -> return ()
                else \obj -> case H.keys (H.difference obj knownFields) of
                    [] -> return ()
                    unknownFields -> contextCons cname tname $
                        fail ("unknown fields: " ++ show unknownFields)

class RecordFromJSON' arity f where
    recordParseJSON'
        :: ConName :* TypeName :* Options :* FromArgs arity a
        -> Object -> Parser (f a)

instance ( RecordFromJSON' arity a
         , RecordFromJSON' arity b
         ) => RecordFromJSON' arity (a :*: b) where
    recordParseJSON' p obj =
        (:*:) <$> recordParseJSON' p obj
              <*> recordParseJSON' p obj

instance OVERLAPPABLE_ (Selector s, GFromJSON arity a) =>
         RecordFromJSON' arity (S1 s a) where
    recordParseJSON' (cname :* tname :* opts :* fargs) obj = do
        fv <- contextCons cname tname (obj .: label)
        M1 <$> gParseJSON opts fargs fv <?> Key label
      where
        label = pack $ fieldLabelModifier opts sname
        sname = selName (undefined :: M1 _i s _f _p)

instance INCOHERENT_ (Selector s, FromJSON a) =>
         RecordFromJSON' arity (S1 s (K1 i (Maybe a))) where
    recordParseJSON' (_ :* _ :* opts :* _) obj = M1 . K1 <$> obj .:? pack label
      where
        label = fieldLabelModifier opts sname
        sname = selName (undefined :: M1 _i s _f _p)

-- Parse an Option like a Maybe.
instance INCOHERENT_ (Selector s, FromJSON a) =>
         RecordFromJSON' arity (S1 s (K1 i (Semigroup.Option a))) where
    recordParseJSON' p obj = wrap <$> recordParseJSON' p obj
      where
        wrap :: S1 s (K1 i (Maybe a)) p -> S1 s (K1 i (Semigroup.Option a)) p
        wrap (M1 (K1 a)) = M1 (K1 (Semigroup.Option a))

--------------------------------------------------------------------------------

productParseJSON0
    :: forall f arity a. (ProductFromJSON arity f, ProductSize f)
    => ConName :* TypeName :* Options :* FromArgs arity a
    -> Value -> Parser (f a)
    -- Products are expected to be encoded to an array. Here we check whether we
    -- got an array of the same size as the product, then parse each of the
    -- product's elements using productParseJSON:
productParseJSON0 p@(cname :* tname :* _ :* _) =
    withArray (showCons cname tname) $ \arr ->
        let lenArray = V.length arr
            lenProduct = (unTagged2 :: Tagged2 f Int -> Int)
                         productSize in
        if lenArray == lenProduct
        then productParseJSON p arr 0 lenProduct
        else contextCons cname tname $
             fail $ "expected an Array of length " ++ show lenProduct ++
                    ", but encountered an Array of length " ++ show lenArray

--

class ProductFromJSON arity f where
    productParseJSON :: ConName :* TypeName :* Options :* FromArgs arity a
                 -> Array -> Int -> Int
                 -> Parser (f a)

instance ( ProductFromJSON    arity a
         , ProductFromJSON    arity b
         ) => ProductFromJSON arity (a :*: b) where
    productParseJSON p arr ix len =
        (:*:) <$> productParseJSON p arr ix  lenL
              <*> productParseJSON p arr ixR lenR
        where
          lenL = len `unsafeShiftR` 1
          ixR  = ix + lenL
          lenR = len - lenL

instance (GFromJSON arity a) => ProductFromJSON arity (S1 s a) where
    productParseJSON (_ :* _ :* opts :* fargs) arr ix _ =
        M1 <$> gParseJSON opts fargs (V.unsafeIndex arr ix) <?> Index ix

--------------------------------------------------------------------------------

class FromPair arity f where
    -- The first component of the parameter tuple is the tag to match.
    parsePair :: Text :* TypeName :* Options :* FromArgs arity a
              -> Value
              -> Maybe (Parser (f a))

instance ( FromPair arity a
         , FromPair arity b
         ) => FromPair arity (a :+: b) where
    parsePair p pair =
        (fmap L1 <$> parsePair p pair) <|>
        (fmap R1 <$> parsePair p pair)

instance ( Constructor c
         , ConsFromJSON arity a
         ) => FromPair arity (C1 c a) where
    parsePair (tag :* p@(_ :* opts :* _)) v
        | tag == tag' = Just $ M1 <$> consParseJSON (cname :* p) v
        | otherwise   = Nothing
      where
        tag' = pack $ constructorTagModifier opts cname
        cname = conName (undefined :: M1 _i c _a _p)

--------------------------------------------------------------------------------

class FromUntaggedValue arity f where
    parseUntaggedValue :: TypeName :* Options :* FromArgs arity a
                       -> Value
                       -> Parser (f a)

instance
    ( FromUntaggedValue    arity a
    , FromUntaggedValue    arity b
    ) => FromUntaggedValue arity (a :+: b)
  where
    parseUntaggedValue p value =
        L1 <$> parseUntaggedValue p value <|>
        R1 <$> parseUntaggedValue p value

instance OVERLAPPABLE_
    ( ConsFromJSON arity a
    , Constructor c
    ) => FromUntaggedValue arity (C1 c a)
  where
    parseUntaggedValue p = fmap M1 . consParseJSON (cname :* p)
      where
        cname = conName (undefined :: M1 _i c _f _p)

instance OVERLAPPING_
    ( Constructor c )
    => FromUntaggedValue arity (C1 c U1)
  where
    parseUntaggedValue (tname :* opts :* _) v =
        contextCons cname tname $ case v of
            String tag
                | tag == tag' -> pure $ M1 U1
                | otherwise -> fail_ tag
            _ -> typeMismatch "String" v
      where
        tag' = pack $ constructorTagModifier opts cname
        cname = conName (undefined :: M1 _i c _f _p)
        fail_ tag = fail $
          "expected tag " ++ show tag' ++ ", but found tag " ++ show tag

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- base
-------------------------------------------------------------------------------


instance FromJSON2 Const where
    liftParseJSON2 p _ _ _ = fmap Const . p
    {-# INLINE liftParseJSON2 #-}

instance FromJSON a => FromJSON1 (Const a) where
    liftParseJSON _ _ = fmap Const . parseJSON
    {-# INLINE liftParseJSON #-}

instance FromJSON a => FromJSON (Const a b) where
    {-# INLINE parseJSON #-}
    parseJSON = fmap Const . parseJSON

instance (FromJSON a, FromJSONKey a) => FromJSONKey (Const a b) where
    fromJSONKey = fmap Const fromJSONKey


instance FromJSON1 Maybe where
    liftParseJSON _ _ Null = pure Nothing
    liftParseJSON p _ a    = Just <$> p a
    {-# INLINE liftParseJSON #-}

instance (FromJSON a) => FromJSON (Maybe a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}



instance FromJSON2 Either where
    liftParseJSON2 pA _ pB _ (Object (H.toList -> [(key, value)]))
        | key == left  = Left  <$> pA value <?> Key left
        | key == right = Right <$> pB value <?> Key right
      where
        left, right :: Text
        left  = "Left"
        right = "Right"

    liftParseJSON2 _ _ _ _ _ = fail $
        "expected an object with a single property " ++
        "where the property key should be either " ++
        "\"Left\" or \"Right\""
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a) => FromJSON1 (Either a) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b) => FromJSON (Either a b) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}

instance FromJSON Void where
    parseJSON _ = fail "Cannot parse Void"
    {-# INLINE parseJSON #-}

instance FromJSON Bool where
    parseJSON (Bool b) = pure b
    parseJSON v = typeMismatch "Bool" v
    {-# INLINE parseJSON #-}

instance FromJSONKey Bool where
    fromJSONKey = FromJSONKeyTextParser $ \t -> case t of
        "true"  -> pure True
        "false" -> pure False
        _       -> fail $ "cannot parse key " ++ show t ++ " into Bool"

instance FromJSON Ordering where
  parseJSON = withText "Ordering" $ \s ->
    case s of
      "LT" -> return LT
      "EQ" -> return EQ
      "GT" -> return GT
      _ -> fail $ "parsing Ordering failed, unexpected " ++ show s ++
                  " (expected \"LT\", \"EQ\", or \"GT\")"

instance FromJSON () where
    parseJSON = withArray "()" $ \v ->
                  if V.null v
                    then pure ()
                    else prependContext "()" $ fail "expected an empty array"
    {-# INLINE parseJSON #-}

instance FromJSON Char where
    parseJSON = withText "Char" parseChar
    {-# INLINE parseJSON #-}

    parseJSONList (String s) = pure (T.unpack s)
    parseJSONList v = typeMismatch "String" v
    {-# INLINE parseJSONList #-}

parseChar :: Text -> Parser Char
parseChar t =
    if T.compareLength t 1 == EQ
      then pure $ T.head t
      else prependContext "Char" $ fail "expected a string of length 1"

instance FromJSON Double where
    parseJSON = parseRealFloat "Double"
    {-# INLINE parseJSON #-}

instance FromJSONKey Double where
    fromJSONKey = FromJSONKeyTextParser $ \t -> case t of
        "NaN"       -> pure (0/0)
        "Infinity"  -> pure (1/0)
        "-Infinity" -> pure (negate 1/0)
        _           -> Scientific.toRealFloat <$> parseScientificText t

instance FromJSON Float where
    parseJSON = parseRealFloat "Float"
    {-# INLINE parseJSON #-}

instance FromJSONKey Float where
    fromJSONKey = FromJSONKeyTextParser $ \t -> case t of
        "NaN"       -> pure (0/0)
        "Infinity"  -> pure (1/0)
        "-Infinity" -> pure (negate 1/0)
        _           -> Scientific.toRealFloat <$> parseScientificText t

instance (FromJSON a, Integral a) => FromJSON (Ratio a) where
    parseJSON (Number x)
      | exp10 <= 1024
      , exp10 >= -1024 = return $! realToFrac x
      | otherwise      = prependContext "Ratio" $ fail msg
      where
        exp10 = base10Exponent x
        msg = "found a number with exponent " ++ show exp10
           ++ ", but it must not be greater than 1024 or less than -1024"
    parseJSON o = objParser o
      where
        objParser = withObject "Rational" $ \obj -> do
            numerator <- obj .: "numerator"
            denominator <- obj .: "denominator"
            if denominator == 0
            then fail "Ratio denominator was 0"
            else pure $ numerator % denominator
    {-# INLINE parseJSON #-}

-- | This instance includes a bounds check to prevent maliciously
-- large inputs to fill up the memory of the target system. You can
-- newtype 'Scientific' and provide your own instance using
-- 'withScientific' if you want to allow larger inputs.
instance HasResolution a => FromJSON (Fixed a) where
    parseJSON = prependContext "Fixed" . withBoundedScientific' (pure . realToFrac)
    {-# INLINE parseJSON #-}

instance FromJSON Int where
    parseJSON = parseBoundedIntegral "Int"
    {-# INLINE parseJSON #-}

instance FromJSONKey Int where
    fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Int"

-- | This instance includes a bounds check to prevent maliciously
-- large inputs to fill up the memory of the target system. You can
-- newtype 'Scientific' and provide your own instance using
-- 'withScientific' if you want to allow larger inputs.
instance FromJSON Integer where
    parseJSON = parseIntegral "Integer"
    {-# INLINE parseJSON #-}

instance FromJSONKey Integer where
    fromJSONKey = FromJSONKeyTextParser $ parseIntegralText "Integer"

instance FromJSON Natural where
    parseJSON value = do
        integer <- parseIntegral "Natural" value
        parseNatural integer

instance FromJSONKey Natural where
    fromJSONKey = FromJSONKeyTextParser $ \text -> do
        integer <- parseIntegralText "Natural" text
        parseNatural integer

parseNatural :: Integer -> Parser Natural
parseNatural integer =
    if integer < 0 then
        fail $ "parsing Natural failed, unexpected negative number " <> show integer
    else
        pure $ fromIntegral integer

instance FromJSON Int8 where
    parseJSON = parseBoundedIntegral "Int8"
    {-# INLINE parseJSON #-}

instance FromJSONKey Int8 where
    fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Int8"

instance FromJSON Int16 where
    parseJSON = parseBoundedIntegral "Int16"
    {-# INLINE parseJSON #-}

instance FromJSONKey Int16 where
    fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Int16"

instance FromJSON Int32 where
    parseJSON = parseBoundedIntegral "Int32"
    {-# INLINE parseJSON #-}

instance FromJSONKey Int32 where
    fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Int32"

instance FromJSON Int64 where
    parseJSON = parseBoundedIntegral "Int64"
    {-# INLINE parseJSON #-}

instance FromJSONKey Int64 where
    fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Int64"

instance FromJSON Word where
    parseJSON = parseBoundedIntegral "Word"
    {-# INLINE parseJSON #-}

instance FromJSONKey Word where
    fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Word"

instance FromJSON Word8 where
    parseJSON = parseBoundedIntegral "Word8"
    {-# INLINE parseJSON #-}

instance FromJSONKey Word8 where
    fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Word8"

instance FromJSON Word16 where
    parseJSON = parseBoundedIntegral "Word16"
    {-# INLINE parseJSON #-}

instance FromJSONKey Word16 where
    fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Word16"

instance FromJSON Word32 where
    parseJSON = parseBoundedIntegral "Word32"
    {-# INLINE parseJSON #-}

instance FromJSONKey Word32 where
    fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Word32"

instance FromJSON Word64 where
    parseJSON = parseBoundedIntegral "Word64"
    {-# INLINE parseJSON #-}

instance FromJSONKey Word64 where
    fromJSONKey = FromJSONKeyTextParser $ parseBoundedIntegralText "Word64"

instance FromJSON CTime where
    parseJSON = fmap CTime . parseJSON
    {-# INLINE parseJSON #-}

instance FromJSON Text where
    parseJSON = withText "Text" pure
    {-# INLINE parseJSON #-}

instance FromJSONKey Text where
    fromJSONKey = fromJSONKeyCoerce


instance FromJSON LT.Text where
    parseJSON = withText "Lazy Text" $ pure . LT.fromStrict
    {-# INLINE parseJSON #-}

instance FromJSONKey LT.Text where
    fromJSONKey = FromJSONKeyText LT.fromStrict


instance FromJSON Version where
    parseJSON = withText "Version" parseVersionText
    {-# INLINE parseJSON #-}

instance FromJSONKey Version where
    fromJSONKey = FromJSONKeyTextParser parseVersionText

parseVersionText :: Text -> Parser Version
parseVersionText = go . readP_to_S parseVersion . unpack
  where
    go [(v,[])] = return v
    go (_ : xs) = go xs
    go _        = fail "parsing Version failed"

-------------------------------------------------------------------------------
-- semigroups NonEmpty
-------------------------------------------------------------------------------

instance FromJSON1 NonEmpty where
    liftParseJSON p _ = withArray "NonEmpty" $
        (>>= ne) . Tr.sequence . zipWith (parseIndexedJSON p) [0..] . V.toList
      where
        ne []     = fail "parsing NonEmpty failed, unexpected empty list"
        ne (x:xs) = pure (x :| xs)
    {-# INLINE liftParseJSON #-}

instance (FromJSON a) => FromJSON (NonEmpty a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

-------------------------------------------------------------------------------
-- scientific
-------------------------------------------------------------------------------

instance FromJSON Scientific where
    parseJSON = withScientific "Scientific" pure
    {-# INLINE parseJSON #-}

-------------------------------------------------------------------------------
-- DList
-------------------------------------------------------------------------------

instance FromJSON1 DList.DList where
    liftParseJSON p _ = withArray "DList" $
      fmap DList.fromList .
      Tr.sequence . zipWith (parseIndexedJSON p) [0..] . V.toList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a) => FromJSON (DList.DList a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

#if MIN_VERSION_dlist(1,0,0) && __GLASGOW_HASKELL__ >=800
-- | @since 1.5.3.0
instance FromJSON1 DNE.DNonEmpty where
    liftParseJSON p _ = withArray "DNonEmpty" $
        (>>= ne) . Tr.sequence . zipWith (parseIndexedJSON p) [0..] . V.toList
      where
        ne []     = fail "parsing DNonEmpty failed, unexpected empty list"
        ne (x:xs) = pure (DNE.fromNonEmpty (x :| xs))
    {-# INLINE liftParseJSON #-}

-- | @since 1.5.3.0
instance (FromJSON a) => FromJSON (DNE.DNonEmpty a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}
#endif

-------------------------------------------------------------------------------
-- transformers - Functors
-------------------------------------------------------------------------------

instance FromJSON1 Identity where
    liftParseJSON p _ a = Identity <$> p a
    {-# INLINE liftParseJSON #-}

    liftParseJSONList _ p a = fmap Identity <$> p a
    {-# INLINE liftParseJSONList #-}

instance (FromJSON a) => FromJSON (Identity a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

    parseJSONList = liftParseJSONList parseJSON parseJSONList
    {-# INLINE parseJSONList #-}

instance (FromJSONKey a) => FromJSONKey (Identity a) where
    fromJSONKey = coerceFromJSONKeyFunction (fromJSONKey :: FromJSONKeyFunction a)
    fromJSONKeyList = coerceFromJSONKeyFunction (fromJSONKeyList :: FromJSONKeyFunction [a])


instance (FromJSON1 f, FromJSON1 g) => FromJSON1 (Compose f g) where
    liftParseJSON p pl a = Compose <$> liftParseJSON g gl a
      where
        g  = liftParseJSON p pl
        gl = liftParseJSONList p pl
    {-# INLINE liftParseJSON #-}

    liftParseJSONList p pl a = map Compose <$> liftParseJSONList g gl a
      where
        g  = liftParseJSON p pl
        gl = liftParseJSONList p pl
    {-# INLINE liftParseJSONList #-}

instance (FromJSON1 f, FromJSON1 g, FromJSON a) => FromJSON (Compose f g a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

    parseJSONList = liftParseJSONList parseJSON parseJSONList
    {-# INLINE parseJSONList #-}


instance (FromJSON1 f, FromJSON1 g) => FromJSON1 (Product f g) where
    liftParseJSON p pl a = uncurry Pair <$> liftParseJSON2 px pxl py pyl a
      where
        px  = liftParseJSON p pl
        pxl = liftParseJSONList p pl
        py  = liftParseJSON p pl
        pyl = liftParseJSONList p pl
    {-# INLINE liftParseJSON #-}

instance (FromJSON1 f, FromJSON1 g, FromJSON a) => FromJSON (Product f g a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}


instance (FromJSON1 f, FromJSON1 g) => FromJSON1 (Sum f g) where
    liftParseJSON p pl (Object (H.toList -> [(key, value)]))
        | key == inl = InL <$> liftParseJSON p pl value <?> Key inl
        | key == inr = InR <$> liftParseJSON p pl value <?> Key inl
      where
        inl, inr :: Text
        inl = "InL"
        inr = "InR"

    liftParseJSON _ _ _ = fail $
        "parsing Sum failed, expected an object with a single property " ++
        "where the property key should be either " ++
        "\"InL\" or \"InR\""
    {-# INLINE liftParseJSON #-}

instance (FromJSON1 f, FromJSON1 g, FromJSON a) => FromJSON (Sum f g a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

-------------------------------------------------------------------------------
-- containers
-------------------------------------------------------------------------------

instance FromJSON1 Seq.Seq where
    liftParseJSON p _ = withArray "Seq" $
      fmap Seq.fromList .
      Tr.sequence . zipWith (parseIndexedJSON p) [0..] . V.toList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a) => FromJSON (Seq.Seq a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}


instance (Ord a, FromJSON a) => FromJSON (Set.Set a) where
    parseJSON = fmap Set.fromList . parseJSON
    {-# INLINE parseJSON #-}


instance FromJSON IntSet.IntSet where
    parseJSON = fmap IntSet.fromList . parseJSON
    {-# INLINE parseJSON #-}


instance FromJSON1 IntMap.IntMap where
    liftParseJSON p pl = fmap IntMap.fromList . liftParseJSON p' pl'
      where
        p'  = liftParseJSON2     parseJSON parseJSONList p pl
        pl' = liftParseJSONList2 parseJSON parseJSONList p pl
    {-# INLINE liftParseJSON #-}

instance FromJSON a => FromJSON (IntMap.IntMap a) where
    parseJSON = fmap IntMap.fromList . parseJSON
    {-# INLINE parseJSON #-}


instance (FromJSONKey k, Ord k) => FromJSON1 (M.Map k) where
    liftParseJSON p _ = case fromJSONKey of
        FromJSONKeyCoerce -> withObject "Map" $
            fmap (H.foldrWithKey (M.insert . unsafeCoerce) M.empty) . H.traverseWithKey (\k v -> p v <?> Key k)
        FromJSONKeyText f -> withObject "Map" $
            fmap (H.foldrWithKey (M.insert . f) M.empty) . H.traverseWithKey (\k v -> p v <?> Key k)
        FromJSONKeyTextParser f -> withObject "Map" $
            H.foldrWithKey (\k v m -> M.insert <$> f k <?> Key k <*> p v <?> Key k <*> m) (pure M.empty)
        FromJSONKeyValue f -> withArray "Map" $ \arr ->
            fmap M.fromList . Tr.sequence .
                zipWith (parseIndexedJSONPair f p) [0..] . V.toList $ arr
    {-# INLINE liftParseJSON #-}

instance (FromJSONKey k, Ord k, FromJSON v) => FromJSON (M.Map k v) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}


instance FromJSON1 Tree.Tree where
    liftParseJSON p pl = go
      where
        go v = uncurry Tree.Node <$> liftParseJSON2 p pl p' pl' v

        p' = liftParseJSON go (listParser go)
        pl'= liftParseJSONList go (listParser go)

instance (FromJSON v) => FromJSON (Tree.Tree v) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

-------------------------------------------------------------------------------
-- uuid
-------------------------------------------------------------------------------

instance FromJSON UUID.UUID where
    parseJSON = withText "UUID" $
        maybe (fail "invalid UUID") pure . UUID.fromText

instance FromJSONKey UUID.UUID where
    fromJSONKey = FromJSONKeyTextParser $
        maybe (fail "invalid UUID") pure . UUID.fromText

-------------------------------------------------------------------------------
-- vector
-------------------------------------------------------------------------------

instance FromJSON1 Vector where
    liftParseJSON p _ = withArray "Vector" $
        V.mapM (uncurry $ parseIndexedJSON p) . V.indexed
    {-# INLINE liftParseJSON #-}

instance (FromJSON a) => FromJSON (Vector a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}


vectorParseJSON :: (FromJSON a, VG.Vector w a) => String -> Value -> Parser (w a)
vectorParseJSON s = withArray s $ fmap V.convert . V.mapM (uncurry $ parseIndexedJSON parseJSON) . V.indexed
{-# INLINE vectorParseJSON #-}

instance (Storable a, FromJSON a) => FromJSON (VS.Vector a) where
    parseJSON = vectorParseJSON "Data.Vector.Storable.Vector"

instance (VP.Prim a, FromJSON a) => FromJSON (VP.Vector a) where
    parseJSON = vectorParseJSON "Data.Vector.Primitive.Vector"
    {-# INLINE parseJSON #-}

instance (VG.Vector VU.Vector a, FromJSON a) => FromJSON (VU.Vector a) where
    parseJSON = vectorParseJSON "Data.Vector.Unboxed.Vector"
    {-# INLINE parseJSON #-}

-------------------------------------------------------------------------------
-- unordered-containers
-------------------------------------------------------------------------------

instance (Eq a, Hashable a, FromJSON a) => FromJSON (HashSet.HashSet a) where
    parseJSON = fmap HashSet.fromList . parseJSON
    {-# INLINE parseJSON #-}


instance (FromJSONKey k, Eq k, Hashable k) => FromJSON1 (H.HashMap k) where
    liftParseJSON p _ = case fromJSONKey of
        FromJSONKeyCoerce -> withObject "HashMap ~Text" $
            uc . H.traverseWithKey (\k v -> p v <?> Key k)
        FromJSONKeyText f -> withObject "HashMap" $
            fmap (mapKey f) . H.traverseWithKey (\k v -> p v <?> Key k)
        FromJSONKeyTextParser f -> withObject "HashMap" $
            H.foldrWithKey (\k v m -> H.insert <$> f k <?> Key k <*> p v <?> Key k <*> m) (pure H.empty)
        FromJSONKeyValue f -> withArray "Map" $ \arr ->
            fmap H.fromList . Tr.sequence .
                zipWith (parseIndexedJSONPair f p) [0..] . V.toList $ arr
      where
        uc :: Parser (H.HashMap Text v) -> Parser (H.HashMap k v)
        uc = unsafeCoerce

instance (FromJSON v, FromJSONKey k, Eq k, Hashable k) => FromJSON (H.HashMap k v) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

-------------------------------------------------------------------------------
-- aeson
-------------------------------------------------------------------------------

instance FromJSON Value where
    parseJSON = pure
    {-# INLINE parseJSON #-}

instance FromJSON DotNetTime where
    parseJSON = withText "DotNetTime" $ \t ->
        let (s,m) = T.splitAt (T.length t - 5) t
            t'    = T.concat [s,".",m]
        in case parseTimeM True defaultTimeLocale "/Date(%s%Q)/" (unpack t') of
             Just d -> pure (DotNetTime d)
             _      -> fail "could not parse .NET time"
    {-# INLINE parseJSON #-}

-------------------------------------------------------------------------------
-- primitive
-------------------------------------------------------------------------------

instance FromJSON a => FromJSON (PM.Array a) where
  -- note: we could do better than this if vector exposed the data
  -- constructor in Data.Vector.
  parseJSON = fmap Exts.fromList . parseJSON

instance FromJSON a => FromJSON (PM.SmallArray a) where
  parseJSON = fmap Exts.fromList . parseJSON

instance (PM.Prim a,FromJSON a) => FromJSON (PM.PrimArray a) where
  parseJSON = fmap Exts.fromList . parseJSON

-------------------------------------------------------------------------------
-- time
-------------------------------------------------------------------------------

instance FromJSON Day where
    parseJSON = withText "Day" (Time.run Time.day)

instance FromJSONKey Day where
    fromJSONKey = FromJSONKeyTextParser (Time.run Time.day)


instance FromJSON TimeOfDay where
    parseJSON = withText "TimeOfDay" (Time.run Time.timeOfDay)

instance FromJSONKey TimeOfDay where
    fromJSONKey = FromJSONKeyTextParser (Time.run Time.timeOfDay)


instance FromJSON LocalTime where
    parseJSON = withText "LocalTime" (Time.run Time.localTime)

instance FromJSONKey LocalTime where
    fromJSONKey = FromJSONKeyTextParser (Time.run Time.localTime)


-- | Supported string formats:
--
-- @YYYY-MM-DD HH:MM Z@
-- @YYYY-MM-DD HH:MM:SS Z@
-- @YYYY-MM-DD HH:MM:SS.SSS Z@
--
-- The first space may instead be a @T@, and the second space is
-- optional.  The @Z@ represents UTC.  The @Z@ may be replaced with a
-- time zone offset of the form @+0000@ or @-08:00@, where the first
-- two digits are hours, the @:@ is optional and the second two digits
-- (also optional) are minutes.
instance FromJSON ZonedTime where
    parseJSON = withText "ZonedTime" (Time.run Time.zonedTime)

instance FromJSONKey ZonedTime where
    fromJSONKey = FromJSONKeyTextParser (Time.run Time.zonedTime)


instance FromJSON UTCTime where
    parseJSON = withText "UTCTime" (Time.run Time.utcTime)

instance FromJSONKey UTCTime where
    fromJSONKey = FromJSONKeyTextParser (Time.run Time.utcTime)


-- | This instance includes a bounds check to prevent maliciously
-- large inputs to fill up the memory of the target system. You can
-- newtype 'Scientific' and provide your own instance using
-- 'withScientific' if you want to allow larger inputs.
instance FromJSON NominalDiffTime where
    parseJSON = withBoundedScientific "NominalDiffTime" $ pure . realToFrac
    {-# INLINE parseJSON #-}


-- | This instance includes a bounds check to prevent maliciously
-- large inputs to fill up the memory of the target system. You can
-- newtype 'Scientific' and provide your own instance using
-- 'withScientific' if you want to allow larger inputs.
instance FromJSON DiffTime where
    parseJSON = withBoundedScientific "DiffTime" $ pure . realToFrac
    {-# INLINE parseJSON #-}

instance FromJSON SystemTime where
    parseJSON v = prependContext "SystemTime" $ do
        n <- parseJSON v
        let n' = floor (n * fromInteger (resolution n) :: Nano)
        let (secs, nano) = n' `divMod` resolution n
        return (MkSystemTime (fromInteger secs) (fromInteger nano))

instance FromJSON CalendarDiffTime where
    parseJSON = withObject "CalendarDiffTime" $ \obj -> CalendarDiffTime
        <$> obj .: "months"
        <*> obj .: "time"

instance FromJSON CalendarDiffDays where
    parseJSON = withObject "CalendarDiffDays" $ \obj -> CalendarDiffDays
        <$> obj .: "months"
        <*> obj .: "days"

instance FromJSON DayOfWeek where
    parseJSON = withText "DaysOfWeek" parseDayOfWeek

parseDayOfWeek :: T.Text -> Parser DayOfWeek
parseDayOfWeek t = case T.toLower t of
    "monday"    -> return Monday
    "tuesday"   -> return Tuesday
    "wednesday" -> return Wednesday
    "thursday"  -> return Thursday
    "friday"    -> return Friday
    "saturday"  -> return Saturday
    "sunday"    -> return Sunday
    _           -> fail "Invalid week day"

instance FromJSONKey DayOfWeek where
    fromJSONKey = FromJSONKeyTextParser parseDayOfWeek

instance FromJSON QuarterOfYear where
    parseJSON = withText "DaysOfWeek" parseQuarterOfYear

parseQuarterOfYear :: T.Text -> Parser QuarterOfYear
parseQuarterOfYear t = case T.toLower t of
    "q1" -> return Q1
    "q2" -> return Q2
    "q3" -> return Q3
    "q4" -> return Q4
    _    -> fail "Invalid quarter of year"

instance FromJSONKey QuarterOfYear where
    fromJSONKey = FromJSONKeyTextParser parseQuarterOfYear

instance FromJSON Quarter where
    parseJSON = withText "Quarter" (Time.run Time.quarter)

instance FromJSONKey Quarter where
    fromJSONKey = FromJSONKeyTextParser (Time.run Time.quarter)

instance FromJSON Month where
    parseJSON = withText "Month" (Time.run Time.month)

instance FromJSONKey Month where
    fromJSONKey = FromJSONKeyTextParser (Time.run Time.month)

-------------------------------------------------------------------------------
-- base Monoid/Semigroup
-------------------------------------------------------------------------------

instance FromJSON1 Monoid.Dual where
    liftParseJSON p _ = fmap Monoid.Dual . p
    {-# INLINE liftParseJSON #-}

instance FromJSON a => FromJSON (Monoid.Dual a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}


instance FromJSON1 Monoid.First where
    liftParseJSON p p' = fmap Monoid.First . liftParseJSON p p'
    {-# INLINE liftParseJSON #-}

instance FromJSON a => FromJSON (Monoid.First a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}


instance FromJSON1 Monoid.Last where
    liftParseJSON p p' = fmap Monoid.Last . liftParseJSON p p'
    {-# INLINE liftParseJSON #-}

instance FromJSON a => FromJSON (Monoid.Last a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}


instance FromJSON1 Semigroup.Min where
    liftParseJSON p _ a = Semigroup.Min <$> p a
    {-# INLINE liftParseJSON #-}

    liftParseJSONList _ p a = fmap Semigroup.Min <$> p a
    {-# INLINE liftParseJSONList #-}

instance (FromJSON a) => FromJSON (Semigroup.Min a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

    parseJSONList = liftParseJSONList parseJSON parseJSONList
    {-# INLINE parseJSONList #-}


instance FromJSON1 Semigroup.Max where
    liftParseJSON p _ a = Semigroup.Max <$> p a
    {-# INLINE liftParseJSON #-}

    liftParseJSONList _ p a = fmap Semigroup.Max <$> p a
    {-# INLINE liftParseJSONList #-}

instance (FromJSON a) => FromJSON (Semigroup.Max a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

    parseJSONList = liftParseJSONList parseJSON parseJSONList
    {-# INLINE parseJSONList #-}


instance FromJSON1 Semigroup.First where
    liftParseJSON p _ a = Semigroup.First <$> p a
    {-# INLINE liftParseJSON #-}

    liftParseJSONList _ p a = fmap Semigroup.First <$> p a
    {-# INLINE liftParseJSONList #-}

instance (FromJSON a) => FromJSON (Semigroup.First a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

    parseJSONList = liftParseJSONList parseJSON parseJSONList
    {-# INLINE parseJSONList #-}


instance FromJSON1 Semigroup.Last where
    liftParseJSON p _ a = Semigroup.Last <$> p a
    {-# INLINE liftParseJSON #-}

    liftParseJSONList _ p a = fmap Semigroup.Last <$> p a
    {-# INLINE liftParseJSONList #-}

instance (FromJSON a) => FromJSON (Semigroup.Last a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

    parseJSONList = liftParseJSONList parseJSON parseJSONList
    {-# INLINE parseJSONList #-}


instance FromJSON1 Semigroup.WrappedMonoid where
    liftParseJSON p _ a = Semigroup.WrapMonoid <$> p a
    {-# INLINE liftParseJSON #-}

    liftParseJSONList _ p a = fmap Semigroup.WrapMonoid <$> p a
    {-# INLINE liftParseJSONList #-}

instance (FromJSON a) => FromJSON (Semigroup.WrappedMonoid a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

    parseJSONList = liftParseJSONList parseJSON parseJSONList
    {-# INLINE parseJSONList #-}


instance FromJSON1 Semigroup.Option where
    liftParseJSON p p' = fmap Semigroup.Option . liftParseJSON p p'
    {-# INLINE liftParseJSON #-}

instance FromJSON a => FromJSON (Semigroup.Option a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

-------------------------------------------------------------------------------
-- data-fix
-------------------------------------------------------------------------------

-- | @since 1.5.3.0
instance FromJSON1 f => FromJSON (F.Fix f) where
    parseJSON = go where go = fmap F.Fix . liftParseJSON go parseJSONList

-- | @since 1.5.3.0
instance (FromJSON1 f, Functor f) => FromJSON (F.Mu f) where
    parseJSON = fmap (F.unfoldMu F.unFix) . parseJSON

-- | @since 1.5.3.0
instance (FromJSON1 f, Functor f) => FromJSON (F.Nu f) where
    parseJSON = fmap (F.unfoldNu F.unFix) . parseJSON

-------------------------------------------------------------------------------
-- strict
-------------------------------------------------------------------------------

-- | @since 1.5.3.0
instance (FromJSON a, FromJSON b) => FromJSON (S.These a b) where
    parseJSON = fmap S.toStrict . parseJSON

-- | @since 1.5.3.0
instance FromJSON2 S.These where
    liftParseJSON2 pa pas pb pbs = fmap S.toStrict . liftParseJSON2 pa pas pb pbs

-- | @since 1.5.3.0
instance FromJSON a => FromJSON1 (S.These a) where
    liftParseJSON pa pas = fmap S.toStrict . liftParseJSON pa pas

-- | @since 1.5.3.0
instance (FromJSON a, FromJSON b) => FromJSON (S.Pair a b) where
    parseJSON = fmap S.toStrict . parseJSON

-- | @since 1.5.3.0
instance FromJSON2 S.Pair where
    liftParseJSON2 pa pas pb pbs = fmap S.toStrict . liftParseJSON2 pa pas pb pbs

-- | @since 1.5.3.0
instance FromJSON a => FromJSON1 (S.Pair a) where
    liftParseJSON pa pas = fmap S.toStrict . liftParseJSON pa pas

-- | @since 1.5.3.0
instance (FromJSON a, FromJSON b) => FromJSON (S.Either a b) where
    parseJSON = fmap S.toStrict . parseJSON

-- | @since 1.5.3.0
instance FromJSON2 S.Either where
    liftParseJSON2 pa pas pb pbs = fmap S.toStrict . liftParseJSON2 pa pas pb pbs

-- | @since 1.5.3.0
instance FromJSON a => FromJSON1 (S.Either a) where
    liftParseJSON pa pas = fmap S.toStrict . liftParseJSON pa pas

-- | @since 1.5.3.0
instance FromJSON a => FromJSON (S.Maybe a) where
    parseJSON = fmap S.toStrict . parseJSON

-- | @since 1.5.3.0
instance FromJSON1 S.Maybe where
    liftParseJSON pa pas = fmap S.toStrict . liftParseJSON pa pas

-------------------------------------------------------------------------------
-- tagged
-------------------------------------------------------------------------------

instance FromJSON1 Proxy where
    {-# INLINE liftParseJSON #-}
    liftParseJSON _ _ = fromNull "Proxy" Proxy

instance FromJSON (Proxy a) where
    {-# INLINE parseJSON #-}
    parseJSON = fromNull "Proxy" Proxy

fromNull :: String -> a -> Value -> Parser a
fromNull _ a Null = pure a
fromNull c _ v    = prependContext c (typeMismatch "Null" v)

instance FromJSON2 Tagged where
    liftParseJSON2 _ _ p _ = fmap Tagged . p
    {-# INLINE liftParseJSON2 #-}

instance FromJSON1 (Tagged a) where
    liftParseJSON p _ = fmap Tagged . p
    {-# INLINE liftParseJSON #-}

instance FromJSON b => FromJSON (Tagged a b) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

instance FromJSONKey b => FromJSONKey (Tagged a b) where
    fromJSONKey = coerceFromJSONKeyFunction (fromJSONKey :: FromJSONKeyFunction b)
    fromJSONKeyList = (fmap . fmap) Tagged fromJSONKeyList

-------------------------------------------------------------------------------
-- these
-------------------------------------------------------------------------------

-- | @since 1.5.1.0
instance (FromJSON a, FromJSON b) => FromJSON (These a b) where
    parseJSON = withObject "These a b" (p . H.toList)
      where
        p [("This", a), ("That", b)] = These <$> parseJSON a <*> parseJSON b
        p [("That", b), ("This", a)] = These <$> parseJSON a <*> parseJSON b
        p [("This", a)] = This <$> parseJSON a
        p [("That", b)] = That <$> parseJSON b
        p _  = fail "Expected object with 'This' and 'That' keys only"

-- | @since 1.5.1.0
instance FromJSON a => FromJSON1 (These a) where
    liftParseJSON pb _ = withObject "These a b" (p . H.toList)
      where
        p [("This", a), ("That", b)] = These <$> parseJSON a <*> pb b
        p [("That", b), ("This", a)] = These <$> parseJSON a <*> pb b
        p [("This", a)] = This <$> parseJSON a
        p [("That", b)] = That <$> pb b
        p _  = fail "Expected object with 'This' and 'That' keys only"

-- | @since 1.5.1.0
instance FromJSON2 These where
    liftParseJSON2 pa _ pb _ = withObject "These a b" (p . H.toList)
      where
        p [("This", a), ("That", b)] = These <$> pa a <*> pb b
        p [("That", b), ("This", a)] = These <$> pa a <*> pb b
        p [("This", a)] = This <$> pa a
        p [("That", b)] = That <$> pb b
        p _  = fail "Expected object with 'This' and 'That' keys only"

-- | @since 1.5.1.0
instance (FromJSON1 f, FromJSON1 g) => FromJSON1 (These1 f g) where
    liftParseJSON px pl = withObject "These1" (p . H.toList)
      where
        p [("This", a), ("That", b)] = These1 <$> liftParseJSON px pl a <*> liftParseJSON px pl b
        p [("That", b), ("This", a)] = These1 <$> liftParseJSON px pl a <*> liftParseJSON px pl b
        p [("This", a)] = This1 <$> liftParseJSON px pl a
        p [("That", b)] = That1 <$> liftParseJSON px pl b
        p _  = fail "Expected object with 'This' and 'That' keys only"

-- | @since 1.5.1.0
instance (FromJSON1 f, FromJSON1 g, FromJSON a) => FromJSON (These1 f g a) where
    parseJSON = parseJSON1

-------------------------------------------------------------------------------
-- Instances for converting from map keys
-------------------------------------------------------------------------------

instance (FromJSON a, FromJSON b) => FromJSONKey (a,b)
instance (FromJSON a, FromJSON b, FromJSON c) => FromJSONKey (a,b,c)
instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d) => FromJSONKey (a,b,c,d)

instance FromJSONKey Char where
    fromJSONKey = FromJSONKeyTextParser parseChar
    fromJSONKeyList = FromJSONKeyText T.unpack

instance (FromJSONKey a, FromJSON a) => FromJSONKey [a] where
    fromJSONKey = fromJSONKeyList

-------------------------------------------------------------------------------
-- Tuple instances, see tuple-instances-from.hs
-------------------------------------------------------------------------------

instance FromJSON2 (,) where
    liftParseJSON2 pA _ pB _ = withArray "(a, b)" $ \t ->
        let n = V.length t
        in if n == 2
            then (,)
                <$> parseJSONElemAtIndex pA 0 t
                <*> parseJSONElemAtIndex pB 1 t
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 2"
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a) => FromJSON1 ((,) a) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b) => FromJSON (a, b) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}


instance (FromJSON a) => FromJSON2 ((,,) a) where
    liftParseJSON2 pB _ pC _ = withArray "(a, b, c)" $ \t ->
        let n = V.length t
        in if n == 3
            then (,,)
                <$> parseJSONElemAtIndex parseJSON 0 t
                <*> parseJSONElemAtIndex pB 1 t
                <*> parseJSONElemAtIndex pC 2 t
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 3"
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a, FromJSON b) => FromJSON1 ((,,) a b) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (a, b, c) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}


instance (FromJSON a, FromJSON b) => FromJSON2 ((,,,) a b) where
    liftParseJSON2 pC _ pD _ = withArray "(a, b, c, d)" $ \t ->
        let n = V.length t
        in if n == 4
            then (,,,)
                <$> parseJSONElemAtIndex parseJSON 0 t
                <*> parseJSONElemAtIndex parseJSON 1 t
                <*> parseJSONElemAtIndex pC 2 t
                <*> parseJSONElemAtIndex pD 3 t
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 4"
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON1 ((,,,) a b c) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d) => FromJSON (a, b, c, d) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}


instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON2 ((,,,,) a b c) where
    liftParseJSON2 pD _ pE _ = withArray "(a, b, c, d, e)" $ \t ->
        let n = V.length t
        in if n == 5
            then (,,,,)
                <$> parseJSONElemAtIndex parseJSON 0 t
                <*> parseJSONElemAtIndex parseJSON 1 t
                <*> parseJSONElemAtIndex parseJSON 2 t
                <*> parseJSONElemAtIndex pD 3 t
                <*> parseJSONElemAtIndex pE 4 t
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 5"
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d) => FromJSON1 ((,,,,) a b c d) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e) => FromJSON (a, b, c, d, e) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}


instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d) => FromJSON2 ((,,,,,) a b c d) where
    liftParseJSON2 pE _ pF _ = withArray "(a, b, c, d, e, f)" $ \t ->
        let n = V.length t
        in if n == 6
            then (,,,,,)
                <$> parseJSONElemAtIndex parseJSON 0 t
                <*> parseJSONElemAtIndex parseJSON 1 t
                <*> parseJSONElemAtIndex parseJSON 2 t
                <*> parseJSONElemAtIndex parseJSON 3 t
                <*> parseJSONElemAtIndex pE 4 t
                <*> parseJSONElemAtIndex pF 5 t
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 6"
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e) => FromJSON1 ((,,,,,) a b c d e) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f) => FromJSON (a, b, c, d, e, f) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}


instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e) => FromJSON2 ((,,,,,,) a b c d e) where
    liftParseJSON2 pF _ pG _ = withArray "(a, b, c, d, e, f, g)" $ \t ->
        let n = V.length t
        in if n == 7
            then (,,,,,,)
                <$> parseJSONElemAtIndex parseJSON 0 t
                <*> parseJSONElemAtIndex parseJSON 1 t
                <*> parseJSONElemAtIndex parseJSON 2 t
                <*> parseJSONElemAtIndex parseJSON 3 t
                <*> parseJSONElemAtIndex parseJSON 4 t
                <*> parseJSONElemAtIndex pF 5 t
                <*> parseJSONElemAtIndex pG 6 t
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 7"
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f) => FromJSON1 ((,,,,,,) a b c d e f) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g) => FromJSON (a, b, c, d, e, f, g) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}


instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f) => FromJSON2 ((,,,,,,,) a b c d e f) where
    liftParseJSON2 pG _ pH _ = withArray "(a, b, c, d, e, f, g, h)" $ \t ->
        let n = V.length t
        in if n == 8
            then (,,,,,,,)
                <$> parseJSONElemAtIndex parseJSON 0 t
                <*> parseJSONElemAtIndex parseJSON 1 t
                <*> parseJSONElemAtIndex parseJSON 2 t
                <*> parseJSONElemAtIndex parseJSON 3 t
                <*> parseJSONElemAtIndex parseJSON 4 t
                <*> parseJSONElemAtIndex parseJSON 5 t
                <*> parseJSONElemAtIndex pG 6 t
                <*> parseJSONElemAtIndex pH 7 t
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 8"
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g) => FromJSON1 ((,,,,,,,) a b c d e f g) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h) => FromJSON (a, b, c, d, e, f, g, h) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}


instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g) => FromJSON2 ((,,,,,,,,) a b c d e f g) where
    liftParseJSON2 pH _ pI _ = withArray "(a, b, c, d, e, f, g, h, i)" $ \t ->
        let n = V.length t
        in if n == 9
            then (,,,,,,,,)
                <$> parseJSONElemAtIndex parseJSON 0 t
                <*> parseJSONElemAtIndex parseJSON 1 t
                <*> parseJSONElemAtIndex parseJSON 2 t
                <*> parseJSONElemAtIndex parseJSON 3 t
                <*> parseJSONElemAtIndex parseJSON 4 t
                <*> parseJSONElemAtIndex parseJSON 5 t
                <*> parseJSONElemAtIndex parseJSON 6 t
                <*> parseJSONElemAtIndex pH 7 t
                <*> parseJSONElemAtIndex pI 8 t
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 9"
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h) => FromJSON1 ((,,,,,,,,) a b c d e f g h) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i) => FromJSON (a, b, c, d, e, f, g, h, i) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}


instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h) => FromJSON2 ((,,,,,,,,,) a b c d e f g h) where
    liftParseJSON2 pI _ pJ _ = withArray "(a, b, c, d, e, f, g, h, i, j)" $ \t ->
        let n = V.length t
        in if n == 10
            then (,,,,,,,,,)
                <$> parseJSONElemAtIndex parseJSON 0 t
                <*> parseJSONElemAtIndex parseJSON 1 t
                <*> parseJSONElemAtIndex parseJSON 2 t
                <*> parseJSONElemAtIndex parseJSON 3 t
                <*> parseJSONElemAtIndex parseJSON 4 t
                <*> parseJSONElemAtIndex parseJSON 5 t
                <*> parseJSONElemAtIndex parseJSON 6 t
                <*> parseJSONElemAtIndex parseJSON 7 t
                <*> parseJSONElemAtIndex pI 8 t
                <*> parseJSONElemAtIndex pJ 9 t
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 10"
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i) => FromJSON1 ((,,,,,,,,,) a b c d e f g h i) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j) => FromJSON (a, b, c, d, e, f, g, h, i, j) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}


instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i) => FromJSON2 ((,,,,,,,,,,) a b c d e f g h i) where
    liftParseJSON2 pJ _ pK _ = withArray "(a, b, c, d, e, f, g, h, i, j, k)" $ \t ->
        let n = V.length t
        in if n == 11
            then (,,,,,,,,,,)
                <$> parseJSONElemAtIndex parseJSON 0 t
                <*> parseJSONElemAtIndex parseJSON 1 t
                <*> parseJSONElemAtIndex parseJSON 2 t
                <*> parseJSONElemAtIndex parseJSON 3 t
                <*> parseJSONElemAtIndex parseJSON 4 t
                <*> parseJSONElemAtIndex parseJSON 5 t
                <*> parseJSONElemAtIndex parseJSON 6 t
                <*> parseJSONElemAtIndex parseJSON 7 t
                <*> parseJSONElemAtIndex parseJSON 8 t
                <*> parseJSONElemAtIndex pJ 9 t
                <*> parseJSONElemAtIndex pK 10 t
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 11"
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j) => FromJSON1 ((,,,,,,,,,,) a b c d e f g h i j) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j, FromJSON k) => FromJSON (a, b, c, d, e, f, g, h, i, j, k) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}


instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j) => FromJSON2 ((,,,,,,,,,,,) a b c d e f g h i j) where
    liftParseJSON2 pK _ pL _ = withArray "(a, b, c, d, e, f, g, h, i, j, k, l)" $ \t ->
        let n = V.length t
        in if n == 12
            then (,,,,,,,,,,,)
                <$> parseJSONElemAtIndex parseJSON 0 t
                <*> parseJSONElemAtIndex parseJSON 1 t
                <*> parseJSONElemAtIndex parseJSON 2 t
                <*> parseJSONElemAtIndex parseJSON 3 t
                <*> parseJSONElemAtIndex parseJSON 4 t
                <*> parseJSONElemAtIndex parseJSON 5 t
                <*> parseJSONElemAtIndex parseJSON 6 t
                <*> parseJSONElemAtIndex parseJSON 7 t
                <*> parseJSONElemAtIndex parseJSON 8 t
                <*> parseJSONElemAtIndex parseJSON 9 t
                <*> parseJSONElemAtIndex pK 10 t
                <*> parseJSONElemAtIndex pL 11 t
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 12"
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j, FromJSON k) => FromJSON1 ((,,,,,,,,,,,) a b c d e f g h i j k) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j, FromJSON k, FromJSON l) => FromJSON (a, b, c, d, e, f, g, h, i, j, k, l) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}


instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j, FromJSON k) => FromJSON2 ((,,,,,,,,,,,,) a b c d e f g h i j k) where
    liftParseJSON2 pL _ pM _ = withArray "(a, b, c, d, e, f, g, h, i, j, k, l, m)" $ \t ->
        let n = V.length t
        in if n == 13
            then (,,,,,,,,,,,,)
                <$> parseJSONElemAtIndex parseJSON 0 t
                <*> parseJSONElemAtIndex parseJSON 1 t
                <*> parseJSONElemAtIndex parseJSON 2 t
                <*> parseJSONElemAtIndex parseJSON 3 t
                <*> parseJSONElemAtIndex parseJSON 4 t
                <*> parseJSONElemAtIndex parseJSON 5 t
                <*> parseJSONElemAtIndex parseJSON 6 t
                <*> parseJSONElemAtIndex parseJSON 7 t
                <*> parseJSONElemAtIndex parseJSON 8 t
                <*> parseJSONElemAtIndex parseJSON 9 t
                <*> parseJSONElemAtIndex parseJSON 10 t
                <*> parseJSONElemAtIndex pL 11 t
                <*> parseJSONElemAtIndex pM 12 t
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 13"
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j, FromJSON k, FromJSON l) => FromJSON1 ((,,,,,,,,,,,,) a b c d e f g h i j k l) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j, FromJSON k, FromJSON l, FromJSON m) => FromJSON (a, b, c, d, e, f, g, h, i, j, k, l, m) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}


instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j, FromJSON k, FromJSON l) => FromJSON2 ((,,,,,,,,,,,,,) a b c d e f g h i j k l) where
    liftParseJSON2 pM _ pN _ = withArray "(a, b, c, d, e, f, g, h, i, j, k, l, m, n)" $ \t ->
        let n = V.length t
        in if n == 14
            then (,,,,,,,,,,,,,)
                <$> parseJSONElemAtIndex parseJSON 0 t
                <*> parseJSONElemAtIndex parseJSON 1 t
                <*> parseJSONElemAtIndex parseJSON 2 t
                <*> parseJSONElemAtIndex parseJSON 3 t
                <*> parseJSONElemAtIndex parseJSON 4 t
                <*> parseJSONElemAtIndex parseJSON 5 t
                <*> parseJSONElemAtIndex parseJSON 6 t
                <*> parseJSONElemAtIndex parseJSON 7 t
                <*> parseJSONElemAtIndex parseJSON 8 t
                <*> parseJSONElemAtIndex parseJSON 9 t
                <*> parseJSONElemAtIndex parseJSON 10 t
                <*> parseJSONElemAtIndex parseJSON 11 t
                <*> parseJSONElemAtIndex pM 12 t
                <*> parseJSONElemAtIndex pN 13 t
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 14"
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j, FromJSON k, FromJSON l, FromJSON m) => FromJSON1 ((,,,,,,,,,,,,,) a b c d e f g h i j k l m) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j, FromJSON k, FromJSON l, FromJSON m, FromJSON n) => FromJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}


instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j, FromJSON k, FromJSON l, FromJSON m) => FromJSON2 ((,,,,,,,,,,,,,,) a b c d e f g h i j k l m) where
    liftParseJSON2 pN _ pO _ = withArray "(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)" $ \t ->
        let n = V.length t
        in if n == 15
            then (,,,,,,,,,,,,,,)
                <$> parseJSONElemAtIndex parseJSON 0 t
                <*> parseJSONElemAtIndex parseJSON 1 t
                <*> parseJSONElemAtIndex parseJSON 2 t
                <*> parseJSONElemAtIndex parseJSON 3 t
                <*> parseJSONElemAtIndex parseJSON 4 t
                <*> parseJSONElemAtIndex parseJSON 5 t
                <*> parseJSONElemAtIndex parseJSON 6 t
                <*> parseJSONElemAtIndex parseJSON 7 t
                <*> parseJSONElemAtIndex parseJSON 8 t
                <*> parseJSONElemAtIndex parseJSON 9 t
                <*> parseJSONElemAtIndex parseJSON 10 t
                <*> parseJSONElemAtIndex parseJSON 11 t
                <*> parseJSONElemAtIndex parseJSON 12 t
                <*> parseJSONElemAtIndex pN 13 t
                <*> parseJSONElemAtIndex pO 14 t
            else fail $ "cannot unpack array of length " ++ show n ++ " into a tuple of length 15"
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j, FromJSON k, FromJSON l, FromJSON m, FromJSON n) => FromJSON1 ((,,,,,,,,,,,,,,) a b c d e f g h i j k l m n) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j, FromJSON k, FromJSON l, FromJSON m, FromJSON n, FromJSON o) => FromJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}
