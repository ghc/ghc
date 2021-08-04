{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#include "overlapping-compat.h"
#include "incoherent-compat.h"

-- TODO: Drop this when we remove support for Data.Attoparsec.Number
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Data.Aeson.Types.ToJSON
    (
    -- * Core JSON classes
      ToJSON(..)
    -- * Liftings to unary and binary type constructors
    , ToJSON1(..)
    , toJSON1
    , toEncoding1
    , ToJSON2(..)
    , toJSON2
    , toEncoding2
    -- * Generic JSON classes
    , GToJSON'(..)
    , ToArgs(..)
    , genericToJSON
    , genericToEncoding
    , genericLiftToJSON
    , genericLiftToEncoding
    -- * Classes and types for map keys
    , ToJSONKey(..)
    , ToJSONKeyFunction(..)
    , toJSONKeyText
    , contramapToJSONKeyFunction

    , GToJSONKey()
    , genericToJSONKey

    -- * Object key-value pairs
    , KeyValue(..)
    , KeyValuePair(..)
    , FromPairs(..)
    -- * Functions needed for documentation
    -- * Encoding functions
    , listEncoding
    , listValue
    ) where

import Prelude.Compat

import Control.Applicative (Const(..))
import Control.Monad.ST (ST)
import Data.Aeson.Encoding (Encoding, Encoding', Series, dict, emptyArray_)
import Data.Aeson.Encoding.Internal ((>*<))
import Data.Aeson.Internal.Functions (mapHashKeyVal, mapKeyVal)
import Data.Aeson.Types.Generic (AllNullary, False, IsRecord, One, ProductSize, Tagged2(..), True, Zero, productSize)
import Data.Aeson.Types.Internal
import Data.Attoparsec.Number (Number(..))
import Data.Bits (unsafeShiftR)
import Data.DList (DList)
import Data.Fixed (Fixed, HasResolution, Nano)
import Data.Foldable (toList)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Contravariant (Contravariant (..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product (Product(..))
import Data.Functor.Sum (Sum(..))
import Data.Functor.These (These1 (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy (Proxy(..))
import Data.Ratio (Ratio, denominator, numerator)
import Data.Scientific (Scientific)
import Data.Tagged (Tagged(..))
import Data.Text (Text, pack)
import Data.These (These (..))
import Data.Time (Day, DiffTime, LocalTime, NominalDiffTime, TimeOfDay, UTCTime, ZonedTime)
import Data.Time.Calendar.Month.Compat (Month)
import Data.Time.Calendar.Quarter.Compat (Quarter, QuarterOfYear (..))
import Data.Time.Calendar.Compat (CalendarDiffDays (..), DayOfWeek (..))
import Data.Time.LocalTime.Compat (CalendarDiffTime (..))
import Data.Time.Clock.System.Compat (SystemTime (..))
import Data.Time.Format.Compat (FormatTime, formatTime, defaultTimeLocale)
import Data.Vector (Vector)
import Data.Version (Version, showVersion)
import Data.Void (Void, absurd)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Storable (Storable)
import Foreign.C.Types (CTime (..))
import GHC.Generics
import Numeric.Natural (Natural)
import qualified Data.Aeson.Encoding as E
import qualified Data.Aeson.Encoding.Internal as E (InArray, comma, econcat, retagEncoding)
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
import qualified Data.List.NonEmpty as NE
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
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

import qualified Data.Aeson.Encoding.Builder as EB
import qualified Data.ByteString.Builder as B

import qualified GHC.Exts as Exts
import qualified Data.Primitive.Array as PM
import qualified Data.Primitive.SmallArray as PM
import qualified Data.Primitive.Types as PM
import qualified Data.Primitive.PrimArray as PM

toJSONPair :: (a -> Value) -> (b -> Value) -> (a, b) -> Value
toJSONPair a b = liftToJSON2 a (listValue a) b (listValue b)
{-# INLINE toJSONPair #-}

realFloatToJSON :: RealFloat a => a -> Value
realFloatToJSON d
    | isNaN d || isInfinite d = Null
    | otherwise = Number $ Scientific.fromFloatDigits d
{-# INLINE realFloatToJSON #-}

-------------------------------------------------------------------------------
-- Generics
-------------------------------------------------------------------------------

-- | Class of generic representation types that can be converted to
-- JSON.
class GToJSON' enc arity f where
    -- | This method (applied to 'defaultOptions') is used as the
    -- default generic implementation of 'toJSON'
    -- (with @enc ~ 'Value'@ and @arity ~ 'Zero'@)
    -- and 'liftToJSON' (if the @arity@ is 'One').
    --
    -- It also provides a generic implementation of 'toEncoding'
    -- (with @enc ~ 'Encoding'@ and @arity ~ 'Zero'@)
    -- and 'liftToEncoding' (if the @arity@ is 'One').
    gToJSON :: Options -> ToArgs enc arity a -> f a -> enc

-- | A 'ToArgs' value either stores nothing (for 'ToJSON') or it stores the two
-- function arguments that encode occurrences of the type parameter (for
-- 'ToJSON1').
data ToArgs res arity a where
    NoToArgs :: ToArgs res Zero a
    To1Args  :: (a -> res) -> ([a] -> res) -> ToArgs res One a

-- | A configurable generic JSON creator. This function applied to
-- 'defaultOptions' is used as the default for 'toJSON' when the type
-- is an instance of 'Generic'.
genericToJSON :: (Generic a, GToJSON' Value Zero (Rep a))
              => Options -> a -> Value
genericToJSON opts = gToJSON opts NoToArgs . from

-- | A configurable generic JSON creator. This function applied to
-- 'defaultOptions' is used as the default for 'liftToJSON' when the type
-- is an instance of 'Generic1'.
genericLiftToJSON :: (Generic1 f, GToJSON' Value One (Rep1 f))
                  => Options -> (a -> Value) -> ([a] -> Value)
                  -> f a -> Value
genericLiftToJSON opts tj tjl = gToJSON opts (To1Args tj tjl) . from1

-- | A configurable generic JSON encoder. This function applied to
-- 'defaultOptions' is used as the default for 'toEncoding' when the type
-- is an instance of 'Generic'.
genericToEncoding :: (Generic a, GToJSON' Encoding Zero (Rep a))
                  => Options -> a -> Encoding
genericToEncoding opts = gToJSON opts NoToArgs . from

-- | A configurable generic JSON encoder. This function applied to
-- 'defaultOptions' is used as the default for 'liftToEncoding' when the type
-- is an instance of 'Generic1'.
genericLiftToEncoding :: (Generic1 f, GToJSON' Encoding One (Rep1 f))
                      => Options -> (a -> Encoding) -> ([a] -> Encoding)
                      -> f a -> Encoding
genericLiftToEncoding opts te tel = gToJSON opts (To1Args te tel) . from1

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

-- | A type that can be converted to JSON.
--
-- Instances in general /must/ specify 'toJSON' and /should/ (but don't need
-- to) specify 'toEncoding'.
--
-- An example type and instance:
--
-- @
-- \-- Allow ourselves to write 'Text' literals.
-- {-\# LANGUAGE OverloadedStrings #-}
--
-- data Coord = Coord { x :: Double, y :: Double }
--
-- instance 'ToJSON' Coord where
--   'toJSON' (Coord x y) = 'object' [\"x\" '.=' x, \"y\" '.=' y]
--
--   'toEncoding' (Coord x y) = 'pairs' (\"x\" '.=' x '<>' \"y\" '.=' y)
-- @
--
-- Instead of manually writing your 'ToJSON' instance, there are two options
-- to do it automatically:
--
-- * "Data.Aeson.TH" provides Template Haskell functions which will derive an
-- instance at compile time. The generated instance is optimized for your type
-- so it will probably be more efficient than the following option.
--
-- * The compiler can provide a default generic implementation for
-- 'toJSON'.
--
-- To use the second, simply add a @deriving 'Generic'@ clause to your
-- datatype and declare a 'ToJSON' instance. If you require nothing other than
-- 'defaultOptions', it is sufficient to write (and this is the only
-- alternative where the default 'toJSON' implementation is sufficient):
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
--
-- import "GHC.Generics"
--
-- data Coord = Coord { x :: Double, y :: Double } deriving 'Generic'
--
-- instance 'ToJSON' Coord where
--     'toEncoding' = 'genericToEncoding' 'defaultOptions'
-- @
--
-- If on the other hand you wish to customize the generic decoding, you have
-- to implement both methods:
--
-- @
-- customOptions = 'defaultOptions'
--                 { 'fieldLabelModifier' = 'map' 'Data.Char.toUpper'
--                 }
--
-- instance 'ToJSON' Coord where
--     'toJSON'     = 'genericToJSON' customOptions
--     'toEncoding' = 'genericToEncoding' customOptions
-- @
--
-- Previous versions of this library only had the 'toJSON' method. Adding
-- 'toEncoding' had two reasons:
--
-- 1. toEncoding is more efficient for the common case that the output of
-- 'toJSON' is directly serialized to a @ByteString@.
-- Further, expressing either method in terms of the other would be
-- non-optimal.
--
-- 2. The choice of defaults allows a smooth transition for existing users:
-- Existing instances that do not define 'toEncoding' still
-- compile and have the correct semantics. This is ensured by making
-- the default implementation of 'toEncoding' use 'toJSON'. This produces
-- correct results, but since it performs an intermediate conversion to a
-- 'Value', it will be less efficient than directly emitting an 'Encoding'.
-- (this also means that specifying nothing more than
-- @instance ToJSON Coord@ would be sufficient as a generically decoding
-- instance, but there probably exists no good reason to not specify
-- 'toEncoding' in new instances.)
class ToJSON a where
    -- | Convert a Haskell value to a JSON-friendly intermediate type.
    toJSON     :: a -> Value

    default toJSON :: (Generic a, GToJSON' Value Zero (Rep a)) => a -> Value
    toJSON = genericToJSON defaultOptions

    -- | Encode a Haskell value as JSON.
    --
    -- The default implementation of this method creates an
    -- intermediate 'Value' using 'toJSON'.  This provides
    -- source-level compatibility for people upgrading from older
    -- versions of this library, but obviously offers no performance
    -- advantage.
    --
    -- To benefit from direct encoding, you /must/ provide an
    -- implementation for this method.  The easiest way to do so is by
    -- having your types implement 'Generic' using the @DeriveGeneric@
    -- extension, and then have GHC generate a method body as follows.
    --
    -- @
    -- instance 'ToJSON' Coord where
    --     'toEncoding' = 'genericToEncoding' 'defaultOptions'
    -- @

    toEncoding :: a -> Encoding
    toEncoding = E.value . toJSON
    {-# INLINE toEncoding #-}

    toJSONList :: [a] -> Value
    toJSONList = listValue toJSON
    {-# INLINE toJSONList #-}

    toEncodingList :: [a] -> Encoding
    toEncodingList = listEncoding toEncoding
    {-# INLINE toEncodingList #-}

-------------------------------------------------------------------------------
-- Object key-value pairs
-------------------------------------------------------------------------------

-- | A key-value pair for encoding a JSON object.
class KeyValue kv where
    (.=) :: ToJSON v => Text -> v -> kv
    infixr 8 .=

instance KeyValue Series where
    name .= value = E.pair name (toEncoding value)
    {-# INLINE (.=) #-}

instance KeyValue Pair where
    name .= value = (name, toJSON value)
    {-# INLINE (.=) #-}

-- | Constructs a singleton 'H.HashMap'. For calling functions that
--   demand an 'Object' for constructing objects. To be used in
--   conjunction with 'mconcat'. Prefer to use 'object' where possible.
instance KeyValue Object where
    name .= value = H.singleton name (toJSON value)
    {-# INLINE (.=) #-}

-------------------------------------------------------------------------------
--  Classes and types for map keys
-------------------------------------------------------------------------------

-- | Typeclass for types that can be used as the key of a map-like container
--   (like 'Map' or 'HashMap'). For example, since 'Text' has a 'ToJSONKey'
--   instance and 'Char' has a 'ToJSON' instance, we can encode a value of
--   type 'Map' 'Text' 'Char':
--
--   >>> LBC8.putStrLn $ encode $ Map.fromList [("foo" :: Text, 'a')]
--   {"foo":"a"}
--
--   Since 'Int' also has a 'ToJSONKey' instance, we can similarly write:
--
--   >>> LBC8.putStrLn $ encode $ Map.fromList [(5 :: Int, 'a')]
--   {"5":"a"}
--
--   JSON documents only accept strings as object keys. For any type
--   from @base@ that has a natural textual representation, it can be
--   expected that its 'ToJSONKey' instance will choose that representation.
--
--   For data types that lack a natural textual representation, an alternative
--   is provided. The map-like container is represented as a JSON array
--   instead of a JSON object. Each value in the array is an array with
--   exactly two values. The first is the key and the second is the value.
--
--   For example, values of type '[Text]' cannot be encoded to a
--   string, so a 'Map' with keys of type '[Text]' is encoded as follows:
--
--   >>> LBC8.putStrLn $ encode $ Map.fromList [(["foo","bar","baz" :: Text], 'a')]
--   [[["foo","bar","baz"],"a"]]
--
--   The default implementation of 'ToJSONKey' chooses this method of
--   encoding a key, using the 'ToJSON' instance of the type.
--
--   To use your own data type as the key in a map, all that is needed
--   is to write a 'ToJSONKey' (and possibly a 'FromJSONKey') instance
--   for it. If the type cannot be trivially converted to and from 'Text',
--   it is recommended that 'ToJSONKeyValue' is used. Since the default
--   implementations of the typeclass methods can build this from a
--   'ToJSON' instance, there is nothing that needs to be written:
--
--   > data Foo = Foo { fooAge :: Int, fooName :: Text }
--   >   deriving (Eq,Ord,Generic)
--   > instance ToJSON Foo
--   > instance ToJSONKey Foo
--
--   That's it. We can now write:
--
--   >>> let m = Map.fromList [(Foo 4 "bar",'a'),(Foo 6 "arg",'b')]
--   >>> LBC8.putStrLn $ encode m
--   [[{"fooName":"bar","fooAge":4},"a"],[{"fooName":"arg","fooAge":6},"b"]]
--
--   The next case to consider is if we have a type that is a
--   newtype wrapper around 'Text'. The recommended approach is to use
--   generalized newtype deriving:
--
--   > newtype RecordId = RecordId { getRecordId :: Text }
--   >   deriving (Eq,Ord,ToJSONKey)
--
--   Then we may write:
--
--   >>> LBC8.putStrLn $ encode $ Map.fromList [(RecordId "abc",'a')]
--   {"abc":"a"}
--
--   Simple sum types are a final case worth considering. Suppose we have:
--
--   > data Color = Red | Green | Blue
--   >   deriving (Show,Read,Eq,Ord)
--
--   It is possible to get the 'ToJSONKey' instance for free as we did
--   with 'Foo'. However, in this case, we have a natural way to go to
--   and from 'Text' that does not require any escape sequences. So
--   'ToJSONKeyText' can be used instead of 'ToJSONKeyValue' to encode maps
--   as objects instead of arrays of pairs. This instance may be
--   implemented using generics as follows:
--
-- @
-- instance 'ToJSONKey' Color where
--   'toJSONKey' = 'genericToJSONKey' 'defaultJSONKeyOptions'
-- @
--
--   === __Low-level implementations__
--
--   The 'Show' instance can be used to help write 'ToJSONKey':
--
--   > instance ToJSONKey Color where
--   >   toJSONKey = ToJSONKeyText f g
--   >     where f = Text.pack . show
--   >           g = text . Text.pack . show
--   >           -- text function is from Data.Aeson.Encoding
--
--   The situation of needing to turning function @a -> Text@ into
--   a 'ToJSONKeyFunction' is common enough that a special combinator
--   is provided for it. The above instance can be rewritten as:
--
--   > instance ToJSONKey Color where
--   >   toJSONKey = toJSONKeyText (Text.pack . show)
--
--   The performance of the above instance can be improved by
--   not using 'String' as an intermediate step when converting to
--   'Text'. One option for improving performance would be to use
--   template haskell machinery from the @text-show@ package. However,
--   even with the approach, the 'Encoding' (a wrapper around a bytestring
--   builder) is generated by encoding the 'Text' to a 'ByteString',
--   an intermediate step that could be avoided. The fastest possible
--   implementation would be:
--
--   > -- Assuming that OverloadedStrings is enabled
--   > instance ToJSONKey Color where
--   >   toJSONKey = ToJSONKeyText f g
--   >     where f x = case x of {Red -> "Red";Green ->"Green";Blue -> "Blue"}
--   >           g x = case x of {Red -> text "Red";Green -> text "Green";Blue -> text "Blue"}
--   >           -- text function is from Data.Aeson.Encoding
--
--   This works because GHC can lift the encoded values out of the case
--   statements, which means that they are only evaluated once. This
--   approach should only be used when there is a serious need to
--   maximize performance.

class ToJSONKey a where
    -- | Strategy for rendering the key for a map-like container.
    toJSONKey :: ToJSONKeyFunction a
    default toJSONKey :: ToJSON a => ToJSONKeyFunction a
    toJSONKey = ToJSONKeyValue toJSON toEncoding

    -- | This is similar in spirit to the 'showsList' method of 'Show'.
    --   It makes it possible to give 'String' keys special treatment
    --   without using @OverlappingInstances@. End users should always
    --   be able to use the default implementation of this method.
    toJSONKeyList :: ToJSONKeyFunction [a]
    default toJSONKeyList :: ToJSON a => ToJSONKeyFunction [a]
    toJSONKeyList = ToJSONKeyValue toJSON toEncoding

data ToJSONKeyFunction a
    = ToJSONKeyText !(a -> Text) !(a -> Encoding' Text)
      -- ^ key is encoded to string, produces object
    | ToJSONKeyValue !(a -> Value) !(a -> Encoding)
      -- ^ key is encoded to value, produces array

-- | Helper for creating textual keys.
--
-- @
-- instance 'ToJSONKey' MyKey where
--     'toJSONKey' = 'toJSONKeyText' myKeyToText
--       where
--         myKeyToText = Text.pack . show -- or showt from text-show
-- @
toJSONKeyText :: (a -> Text) -> ToJSONKeyFunction a
toJSONKeyText f = ToJSONKeyText f (E.text . f)

-- | TODO: should this be exported?
toJSONKeyTextEnc :: (a -> Encoding' Text) -> ToJSONKeyFunction a
toJSONKeyTextEnc e = ToJSONKeyText tot e
 where
    -- TODO: dropAround is also used in stringEncoding, which is unfortunate atm
    tot = T.dropAround (== '"')
        . T.decodeLatin1
        . L.toStrict
        . E.encodingToLazyByteString
        . e

instance Contravariant ToJSONKeyFunction where
    contramap = contramapToJSONKeyFunction

-- | Contravariant map, as 'ToJSONKeyFunction' is a contravariant functor.
contramapToJSONKeyFunction :: (b -> a) -> ToJSONKeyFunction a -> ToJSONKeyFunction b
contramapToJSONKeyFunction h x = case x of
    ToJSONKeyText  f g -> ToJSONKeyText (f . h) (g . h)
    ToJSONKeyValue f g -> ToJSONKeyValue (f . h) (g . h)

-- 'toJSONKey' for 'Generic' types.
-- Deriving is supported for enumeration types, i.e. the sums of nullary
-- constructors. The names of constructors will be used as keys for JSON
-- objects.
--
-- See also 'genericFromJSONKey'.
--
-- === __Example__
--
-- @
-- data Color = Red | Green | Blue
--   deriving 'Generic'
--
-- instance 'ToJSONKey' Color where
--   'toJSONKey' = 'genericToJSONKey' 'defaultJSONKeyOptions'
-- @
genericToJSONKey :: (Generic a, GToJSONKey (Rep a))
           => JSONKeyOptions -> ToJSONKeyFunction a
genericToJSONKey opts = toJSONKeyText (pack . keyModifier opts . getConName . from)

class    GetConName f => GToJSONKey f
instance GetConName f => GToJSONKey f

-------------------------------------------------------------------------------
-- Lifings of FromJSON and ToJSON to unary and binary type constructors
-------------------------------------------------------------------------------


-- | Lifting of the 'ToJSON' class to unary type constructors.
--
-- Instead of manually writing your 'ToJSON1' instance, there are two options
-- to do it automatically:
--
-- * "Data.Aeson.TH" provides Template Haskell functions which will derive an
-- instance at compile time. The generated instance is optimized for your type
-- so it will probably be more efficient than the following option.
--
-- * The compiler can provide a default generic implementation for
-- 'toJSON1'.
--
-- To use the second, simply add a @deriving 'Generic1'@ clause to your
-- datatype and declare a 'ToJSON1' instance for your datatype without giving
-- definitions for 'liftToJSON' or 'liftToEncoding'.
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
-- instance 'ToJSON' a => 'ToJSON1' (Pair a)
-- @
--
-- If the default implementation doesn't give exactly the results you want,
-- you can customize the generic encoding with only a tiny amount of
-- effort, using 'genericLiftToJSON' and 'genericLiftToEncoding' with
-- your preferred 'Options':
--
-- @
-- customOptions = 'defaultOptions'
--                 { 'fieldLabelModifier' = 'map' 'Data.Char.toUpper'
--                 }
--
-- instance 'ToJSON' a => 'ToJSON1' (Pair a) where
--     'liftToJSON'     = 'genericLiftToJSON' customOptions
--     'liftToEncoding' = 'genericLiftToEncoding' customOptions
-- @
--
-- See also 'ToJSON'.
class ToJSON1 f where
    liftToJSON :: (a -> Value) -> ([a] -> Value) -> f a -> Value

    default liftToJSON :: (Generic1 f, GToJSON' Value One (Rep1 f))
                       => (a -> Value) -> ([a] -> Value) -> f a -> Value
    liftToJSON = genericLiftToJSON defaultOptions

    liftToJSONList :: (a -> Value) -> ([a] -> Value) -> [f a] -> Value
    liftToJSONList f g = listValue (liftToJSON f g)

    liftToEncoding :: (a -> Encoding) -> ([a] -> Encoding) -> f a -> Encoding

    default liftToEncoding :: (Generic1 f, GToJSON' Encoding One (Rep1 f))
                           => (a -> Encoding) -> ([a] -> Encoding)
                           -> f a -> Encoding
    liftToEncoding = genericLiftToEncoding defaultOptions

    liftToEncodingList :: (a -> Encoding) -> ([a] -> Encoding) -> [f a] -> Encoding
    liftToEncodingList f g = listEncoding (liftToEncoding f g)

-- | Lift the standard 'toJSON' function through the type constructor.
toJSON1 :: (ToJSON1 f, ToJSON a) => f a -> Value
toJSON1 = liftToJSON toJSON toJSONList
{-# INLINE toJSON1 #-}

-- | Lift the standard 'toEncoding' function through the type constructor.
toEncoding1 :: (ToJSON1 f, ToJSON a) => f a -> Encoding
toEncoding1 = liftToEncoding toEncoding toEncodingList
{-# INLINE toEncoding1 #-}

-- | Lifting of the 'ToJSON' class to binary type constructors.
--
-- Instead of manually writing your 'ToJSON2' instance, "Data.Aeson.TH"
-- provides Template Haskell functions which will derive an instance at compile time.
--
-- The compiler cannot provide a default generic implementation for 'liftToJSON2',
-- unlike 'toJSON' and 'liftToJSON'.
class ToJSON2 f where
    liftToJSON2 :: (a -> Value) -> ([a] -> Value) -> (b -> Value) -> ([b] -> Value) -> f a b -> Value
    liftToJSONList2 :: (a -> Value) -> ([a] -> Value) -> (b -> Value) -> ([b] -> Value) -> [f a b] -> Value
    liftToJSONList2 fa ga fb gb = listValue (liftToJSON2 fa ga fb gb)

    liftToEncoding2 :: (a -> Encoding) -> ([a] -> Encoding) -> (b -> Encoding) -> ([b] -> Encoding) -> f a b -> Encoding
    liftToEncodingList2 :: (a -> Encoding) -> ([a] -> Encoding) -> (b -> Encoding) -> ([b] -> Encoding) -> [f a b] -> Encoding
    liftToEncodingList2 fa ga fb gb = listEncoding (liftToEncoding2 fa ga fb gb)

-- | Lift the standard 'toJSON' function through the type constructor.
toJSON2 :: (ToJSON2 f, ToJSON a, ToJSON b) => f a b -> Value
toJSON2 = liftToJSON2 toJSON toJSONList toJSON toJSONList
{-# INLINE toJSON2 #-}

-- | Lift the standard 'toEncoding' function through the type constructor.
toEncoding2 :: (ToJSON2 f, ToJSON a, ToJSON b) => f a b -> Encoding
toEncoding2 = liftToEncoding2 toEncoding toEncodingList toEncoding toEncodingList
{-# INLINE toEncoding2 #-}

-------------------------------------------------------------------------------
-- Encoding functions
-------------------------------------------------------------------------------

-- | Helper function to use with 'liftToEncoding'.
-- Useful when writing own 'ToJSON1' instances.
--
-- @
-- newtype F a = F [a]
--
-- -- This instance encodes 'String' as an array of chars
-- instance 'ToJSON1' F where
--     'liftToJSON'     tj _ (F xs) = 'liftToJSON'     tj ('listValue'    tj) xs
--     'liftToEncoding' te _ (F xs) = 'liftToEncoding' te ('listEncoding' te) xs
--
-- instance 'Data.Aeson.FromJSON.FromJSON1' F where
--     'Data.Aeson.FromJSON.liftParseJSON' p _ v = F \<$\> 'Data.Aeson.FromJSON.liftParseJSON' p ('Data.Aeson.FromJSON.listParser' p) v
-- @
listEncoding :: (a -> Encoding) -> [a] -> Encoding
listEncoding = E.list
{-# INLINE listEncoding #-}

-- | Helper function to use with 'liftToJSON', see 'listEncoding'.
listValue :: (a -> Value) -> [a] -> Value
listValue f = Array . V.fromList . map f
{-# INLINE listValue #-}

-------------------------------------------------------------------------------
-- [] instances
-------------------------------------------------------------------------------

-- These are needed for key-class default definitions

instance ToJSON1 [] where
    liftToJSON _ to' = to'
    {-# INLINE liftToJSON #-}

    liftToEncoding _ to' = to'
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON [a] where
    {-# SPECIALIZE instance ToJSON String #-}
    {-# SPECIALIZE instance ToJSON [String] #-}
    {-# SPECIALIZE instance ToJSON [Array] #-}
    {-# SPECIALIZE instance ToJSON [Object] #-}

    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

-------------------------------------------------------------------------------
-- Generic toJSON / toEncoding
-------------------------------------------------------------------------------

instance OVERLAPPABLE_ (GToJSON' enc arity a) => GToJSON' enc arity (M1 i c a) where
    -- Meta-information, which is not handled elsewhere, is ignored:
    gToJSON opts targs = gToJSON opts targs . unM1
    {-# INLINE gToJSON #-}

instance GToJSON' enc One Par1 where
    -- Direct occurrences of the last type parameter are encoded with the
    -- function passed in as an argument:
    gToJSON _opts (To1Args tj _) = tj . unPar1
    {-# INLINE gToJSON #-}

instance ( ConsToJSON enc arity a
         , AllNullary          (C1 c a) allNullary
         , SumToJSON enc arity (C1 c a) allNullary
         ) => GToJSON' enc arity (D1 d (C1 c a)) where
    -- The option 'tagSingleConstructors' determines whether to wrap
    -- a single-constructor type.
    gToJSON opts targs
        | tagSingleConstructors opts = (unTagged :: Tagged allNullary enc -> enc)
                                     . sumToJSON opts targs
                                     . unM1
        | otherwise = consToJSON opts targs . unM1 . unM1
    {-# INLINE gToJSON #-}

instance (ConsToJSON enc arity a) => GToJSON' enc arity (C1 c a) where
    -- Constructors need to be encoded differently depending on whether they're
    -- a record or not. This distinction is made by 'consToJSON':
    gToJSON opts targs = consToJSON opts targs . unM1
    {-# INLINE gToJSON #-}

instance ( AllNullary       (a :+: b) allNullary
         , SumToJSON  enc arity (a :+: b) allNullary
         ) => GToJSON' enc arity (a :+: b)
  where
    -- If all constructors of a sum datatype are nullary and the
    -- 'allNullaryToStringTag' option is set they are encoded to
    -- strings.  This distinction is made by 'sumToJSON':
    gToJSON opts targs = (unTagged :: Tagged allNullary enc -> enc)
                       . sumToJSON opts targs
    {-# INLINE gToJSON #-}

--------------------------------------------------------------------------------
-- Generic toJSON

-- Note: Refactoring 'ToJSON a' to 'ToJSON enc a' (and 'ToJSON1' similarly) is
-- possible but makes error messages a bit harder to understand for missing
-- instances.

instance GToJSON' Value arity V1 where
    -- Empty values do not exist, which makes the job of formatting them
    -- rather easy:
    gToJSON _ _ x = x `seq` error "case: V1"
    {-# INLINE gToJSON #-}

instance ToJSON a => GToJSON' Value arity (K1 i a) where
    -- Constant values are encoded using their ToJSON instance:
    gToJSON _opts _ = toJSON . unK1
    {-# INLINE gToJSON #-}

instance ToJSON1 f => GToJSON' Value One (Rec1 f) where
    -- Recursive occurrences of the last type parameter are encoded using their
    -- ToJSON1 instance:
    gToJSON _opts (To1Args tj tjl) = liftToJSON tj tjl . unRec1
    {-# INLINE gToJSON #-}

instance GToJSON' Value arity U1 where
    -- Empty constructors are encoded to an empty array:
    gToJSON _opts _ _ = emptyArray
    {-# INLINE gToJSON #-}

instance ( WriteProduct arity a, WriteProduct arity b
         , ProductSize        a, ProductSize        b
         ) => GToJSON' Value arity (a :*: b)
  where
    -- Products are encoded to an array. Here we allocate a mutable vector of
    -- the same size as the product and write the product's elements to it using
    -- 'writeProduct':
    gToJSON opts targs p =
        Array $ V.create $ do
          mv <- VM.unsafeNew lenProduct
          writeProduct opts targs mv 0 lenProduct p
          return mv
        where
          lenProduct = (unTagged2 :: Tagged2 (a :*: b) Int -> Int)
                       productSize
    {-# INLINE gToJSON #-}

instance ( ToJSON1 f
         , GToJSON' Value One g
         ) => GToJSON' Value One (f :.: g)
  where
    -- If an occurrence of the last type parameter is nested inside two
    -- composed types, it is encoded by using the outermost type's ToJSON1
    -- instance to generically encode the innermost type:
    gToJSON opts targs =
      let gtj = gToJSON opts targs in
      liftToJSON gtj (listValue gtj) . unComp1
    {-# INLINE gToJSON #-}

--------------------------------------------------------------------------------
-- Generic toEncoding

instance ToJSON a => GToJSON' Encoding arity (K1 i a) where
    -- Constant values are encoded using their ToJSON instance:
    gToJSON _opts _ = toEncoding . unK1
    {-# INLINE gToJSON #-}

instance ToJSON1 f => GToJSON' Encoding One (Rec1 f) where
    -- Recursive occurrences of the last type parameter are encoded using their
    -- ToEncoding1 instance:
    gToJSON _opts (To1Args te tel) = liftToEncoding te tel . unRec1
    {-# INLINE gToJSON #-}

instance GToJSON' Encoding arity U1 where
    -- Empty constructors are encoded to an empty array:
    gToJSON _opts _ _ = E.emptyArray_
    {-# INLINE gToJSON #-}

instance ( EncodeProduct  arity a
         , EncodeProduct  arity b
         ) => GToJSON' Encoding arity (a :*: b)
  where
    -- Products are encoded to an array. Here we allocate a mutable vector of
    -- the same size as the product and write the product's elements to it using
    -- 'encodeProduct':
    gToJSON opts targs p = E.list E.retagEncoding [encodeProduct opts targs p]
    {-# INLINE gToJSON #-}

instance ( ToJSON1 f
         , GToJSON' Encoding One g
         ) => GToJSON' Encoding One (f :.: g)
  where
    -- If an occurrence of the last type parameter is nested inside two
    -- composed types, it is encoded by using the outermost type's ToJSON1
    -- instance to generically encode the innermost type:
    gToJSON opts targs =
      let gte = gToJSON opts targs in
      liftToEncoding gte (listEncoding gte) . unComp1
    {-# INLINE gToJSON #-}

--------------------------------------------------------------------------------

class SumToJSON enc arity f allNullary where
    sumToJSON :: Options -> ToArgs enc arity a
              -> f a -> Tagged allNullary enc

instance ( GetConName f
         , FromString enc
         , TaggedObject                     enc arity f
         , SumToJSON' ObjectWithSingleField enc arity f
         , SumToJSON' TwoElemArray          enc arity f
         , SumToJSON' UntaggedValue         enc arity f
         ) => SumToJSON enc arity f True
  where
    sumToJSON opts targs
        | allNullaryToStringTag opts = Tagged . fromString
                                     . constructorTagModifier opts . getConName
        | otherwise = Tagged . nonAllNullarySumToJSON opts targs

instance ( TaggedObject                     enc arity f
         , SumToJSON' ObjectWithSingleField enc arity f
         , SumToJSON' TwoElemArray          enc arity f
         , SumToJSON' UntaggedValue         enc arity f
         ) => SumToJSON enc arity f False
  where
    sumToJSON opts targs = Tagged . nonAllNullarySumToJSON opts targs

nonAllNullarySumToJSON :: ( TaggedObject                     enc arity f
                          , SumToJSON' ObjectWithSingleField enc arity f
                          , SumToJSON' TwoElemArray          enc arity f
                          , SumToJSON' UntaggedValue         enc arity f
                          ) => Options -> ToArgs enc arity a
                            -> f a -> enc
nonAllNullarySumToJSON opts targs =
    case sumEncoding opts of

      TaggedObject{..}      ->
        taggedObject opts targs tagFieldName contentsFieldName

      ObjectWithSingleField ->
        (unTagged :: Tagged ObjectWithSingleField enc -> enc)
          . sumToJSON' opts targs

      TwoElemArray          ->
        (unTagged :: Tagged TwoElemArray enc -> enc)
          . sumToJSON' opts targs

      UntaggedValue         ->
        (unTagged :: Tagged UntaggedValue enc -> enc)
          . sumToJSON' opts targs

--------------------------------------------------------------------------------

class FromString enc where
  fromString :: String -> enc

instance FromString Encoding where
  fromString = toEncoding

instance FromString Value where
  fromString = String . pack

--------------------------------------------------------------------------------

class TaggedObject enc arity f where
    taggedObject :: Options -> ToArgs enc arity a
                 -> String -> String
                 -> f a -> enc

instance ( TaggedObject enc arity a
         , TaggedObject enc arity b
         ) => TaggedObject enc arity (a :+: b)
  where
    taggedObject opts targs tagFieldName contentsFieldName (L1 x) =
        taggedObject opts targs tagFieldName contentsFieldName x
    taggedObject opts targs tagFieldName contentsFieldName (R1 x) =
        taggedObject opts targs tagFieldName contentsFieldName x

instance ( IsRecord                      a isRecord
         , TaggedObject' enc pairs arity a isRecord
         , FromPairs enc pairs
         , FromString enc
         , KeyValuePair enc pairs
         , Constructor c
         ) => TaggedObject enc arity (C1 c a)
  where
    taggedObject opts targs tagFieldName contentsFieldName =
      fromPairs . mappend tag . contents
      where
        tag = tagFieldName `pair`
          (fromString (constructorTagModifier opts (conName (undefined :: t c a p)))
            :: enc)
        contents =
          (unTagged :: Tagged isRecord pairs -> pairs) .
            taggedObject' opts targs contentsFieldName . unM1

class TaggedObject' enc pairs arity f isRecord where
    taggedObject' :: Options -> ToArgs enc arity a
                  -> String -> f a -> Tagged isRecord pairs

instance ( GToJSON' enc arity f
         , KeyValuePair enc pairs
         ) => TaggedObject' enc pairs arity f False
  where
    taggedObject' opts targs contentsFieldName =
        Tagged . (contentsFieldName `pair`) . gToJSON opts targs

instance OVERLAPPING_ Monoid pairs => TaggedObject' enc pairs arity U1 False where
    taggedObject' _ _ _ _ = Tagged mempty

instance ( RecordToPairs enc pairs arity f
         ) => TaggedObject' enc pairs arity f True
  where
    taggedObject' opts targs _ = Tagged . recordToPairs opts targs

--------------------------------------------------------------------------------

-- | Get the name of the constructor of a sum datatype.
class GetConName f where
    getConName :: f a -> String

instance (GetConName a, GetConName b) => GetConName (a :+: b) where
    getConName (L1 x) = getConName x
    getConName (R1 x) = getConName x

instance (Constructor c) => GetConName (C1 c a) where
    getConName = conName

-- For genericToJSONKey
instance GetConName a => GetConName (D1 d a) where
    getConName (M1 x) = getConName x

--------------------------------------------------------------------------------

-- Reflection of SumEncoding variants

data ObjectWithSingleField
data TwoElemArray
data UntaggedValue

--------------------------------------------------------------------------------

class SumToJSON' s enc arity f where
    sumToJSON' :: Options -> ToArgs enc arity a
                    -> f a -> Tagged s enc

instance ( SumToJSON' s enc arity a
         , SumToJSON' s enc arity b
         ) => SumToJSON' s enc arity (a :+: b)
  where
    sumToJSON' opts targs (L1 x) = sumToJSON' opts targs x
    sumToJSON' opts targs (R1 x) = sumToJSON' opts targs x

--------------------------------------------------------------------------------

instance ( GToJSON'    Value arity a
         , ConsToJSON Value arity a
         , Constructor c
         ) => SumToJSON' TwoElemArray Value arity (C1 c a) where
    sumToJSON' opts targs x = Tagged $ Array $ V.create $ do
      mv <- VM.unsafeNew 2
      VM.unsafeWrite mv 0 $ String $ pack $ constructorTagModifier opts
                                   $ conName (undefined :: t c a p)
      VM.unsafeWrite mv 1 $ gToJSON opts targs x
      return mv

--------------------------------------------------------------------------------

instance ( GToJSON'    Encoding arity a
         , ConsToJSON Encoding arity a
         , Constructor c
         ) => SumToJSON' TwoElemArray Encoding arity (C1 c a)
  where
    sumToJSON' opts targs x = Tagged $ E.list id
      [ toEncoding (constructorTagModifier opts (conName (undefined :: t c a p)))
      , gToJSON opts targs x
      ]

--------------------------------------------------------------------------------

class ConsToJSON enc arity f where
    consToJSON :: Options -> ToArgs enc arity a
               -> f a -> enc

class ConsToJSON' enc arity f isRecord where
    consToJSON'     :: Options -> ToArgs enc arity a
                    -> f a -> Tagged isRecord enc

instance ( IsRecord                f isRecord
         , ConsToJSON'   enc arity f isRecord
         ) => ConsToJSON enc arity f
  where
    consToJSON opts targs =
        (unTagged :: Tagged isRecord enc -> enc)
      . consToJSON' opts targs
    {-# INLINE consToJSON #-}

instance OVERLAPPING_
         ( RecordToPairs enc pairs arity (S1 s f)
         , FromPairs enc pairs
         , GToJSON' enc arity f
         ) => ConsToJSON' enc arity (S1 s f) True
  where
    consToJSON' opts targs
      | unwrapUnaryRecords opts = Tagged . gToJSON opts targs
      | otherwise = Tagged . fromPairs . recordToPairs opts targs
    {-# INLINE consToJSON' #-}

instance ( RecordToPairs enc pairs arity f
         , FromPairs enc pairs
         ) => ConsToJSON' enc arity f True
  where
    consToJSON' opts targs = Tagged . fromPairs . recordToPairs opts targs
    {-# INLINE consToJSON' #-}

instance GToJSON' enc arity f => ConsToJSON' enc arity f False where
    consToJSON' opts targs = Tagged . gToJSON opts targs
    {-# INLINE consToJSON' #-}

--------------------------------------------------------------------------------

class RecordToPairs enc pairs arity f where
    -- 1st element: whole thing
    -- 2nd element: in case the record has only 1 field, just the value
    --              of the field (without the key); 'Nothing' otherwise
    recordToPairs :: Options -> ToArgs enc arity a
                  -> f a -> pairs

instance ( Monoid pairs
         , RecordToPairs enc pairs arity a
         , RecordToPairs enc pairs arity b
         ) => RecordToPairs enc pairs arity (a :*: b)
  where
    recordToPairs opts (targs :: ToArgs enc arity p) (a :*: b) =
        pairsOf a `mappend` pairsOf b
      where
        pairsOf :: (RecordToPairs enc pairs arity f) => f p -> pairs
        pairsOf = recordToPairs opts targs
    {-# INLINE recordToPairs #-}

instance ( Selector s
         , GToJSON' enc arity a
         , KeyValuePair enc pairs
         ) => RecordToPairs enc pairs arity (S1 s a)
  where
    recordToPairs = fieldToPair
    {-# INLINE recordToPairs #-}

instance INCOHERENT_
    ( Selector s
    , GToJSON' enc arity (K1 i (Maybe a))
    , KeyValuePair enc pairs
    , Monoid pairs
    ) => RecordToPairs enc pairs arity (S1 s (K1 i (Maybe a)))
  where
    recordToPairs opts _ (M1 k1) | omitNothingFields opts
                                 , K1 Nothing <- k1 = mempty
    recordToPairs opts targs m1 = fieldToPair opts targs m1
    {-# INLINE recordToPairs #-}

instance INCOHERENT_
    ( Selector s
    , GToJSON' enc arity (K1 i (Maybe a))
    , KeyValuePair enc pairs
    , Monoid pairs
    ) => RecordToPairs enc pairs arity (S1 s (K1 i (Semigroup.Option a)))
  where
    recordToPairs opts targs = recordToPairs opts targs . unwrap
      where
        unwrap :: S1 s (K1 i (Semigroup.Option a)) p -> S1 s (K1 i (Maybe a)) p
        unwrap (M1 (K1 (Semigroup.Option a))) = M1 (K1 a)
    {-# INLINE recordToPairs #-}

fieldToPair :: (Selector s
               , GToJSON' enc arity a
               , KeyValuePair enc pairs)
            => Options -> ToArgs enc arity p
            -> S1 s a p -> pairs
fieldToPair opts targs m1 =
  let key   = fieldLabelModifier opts (selName m1)
      value = gToJSON opts targs (unM1 m1)
  in key `pair` value
{-# INLINE fieldToPair #-}

--------------------------------------------------------------------------------

class WriteProduct arity f where
    writeProduct :: Options
                 -> ToArgs Value arity a
                 -> VM.MVector s Value
                 -> Int -- ^ index
                 -> Int -- ^ length
                 -> f a
                 -> ST s ()

instance ( WriteProduct arity a
         , WriteProduct arity b
         ) => WriteProduct arity (a :*: b) where
    writeProduct opts targs mv ix len (a :*: b) = do
      writeProduct opts targs mv ix  lenL a
      writeProduct opts targs mv ixR lenR b
        where
          lenL = len `unsafeShiftR` 1
          lenR = len - lenL
          ixR  = ix  + lenL
    {-# INLINE writeProduct #-}

instance OVERLAPPABLE_ (GToJSON' Value arity a) => WriteProduct arity a where
    writeProduct opts targs mv ix _ =
      VM.unsafeWrite mv ix . gToJSON opts targs
    {-# INLINE writeProduct #-}

--------------------------------------------------------------------------------

class EncodeProduct arity f where
    encodeProduct :: Options -> ToArgs Encoding arity a
                  -> f a -> Encoding' E.InArray

instance ( EncodeProduct    arity a
         , EncodeProduct    arity b
         ) => EncodeProduct arity (a :*: b) where
    encodeProduct opts targs (a :*: b) | omitNothingFields opts =
        E.econcat $ intersperse E.comma $
        filter (not . E.nullEncoding)
        [encodeProduct opts targs a, encodeProduct opts targs b]
    encodeProduct opts targs (a :*: b) =
      encodeProduct opts targs a >*<
      encodeProduct opts targs b
    {-# INLINE encodeProduct #-}

instance OVERLAPPABLE_ (GToJSON' Encoding arity a) => EncodeProduct arity a where
    encodeProduct opts targs a = E.retagEncoding $ gToJSON opts targs a
    {-# INLINE encodeProduct #-}

--------------------------------------------------------------------------------

instance ( GToJSON'   enc arity a
         , ConsToJSON enc arity a
         , FromPairs  enc pairs
         , KeyValuePair  enc pairs
         , Constructor c
         ) => SumToJSON' ObjectWithSingleField enc arity (C1 c a)
  where
    sumToJSON' opts targs =
      Tagged . fromPairs . (typ `pair`) . gToJSON opts targs
        where
          typ = constructorTagModifier opts $
                         conName (undefined :: t c a p)

--------------------------------------------------------------------------------

instance OVERLAPPABLE_
    ( ConsToJSON enc arity a
    ) => SumToJSON' UntaggedValue enc arity (C1 c a)
  where
    sumToJSON' opts targs = Tagged . gToJSON opts targs

instance OVERLAPPING_
    ( Constructor c
    , FromString enc
    ) => SumToJSON' UntaggedValue enc arity (C1 c U1)
  where
    sumToJSON' opts _ _ = Tagged . fromString $
        constructorTagModifier opts $ conName (undefined :: t c U1 p)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- base
-------------------------------------------------------------------------------

instance ToJSON2 Const where
    liftToJSON2 t _ _ _ (Const x) = t x
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 t _ _ _ (Const x) = t x
    {-# INLINE liftToEncoding2 #-}

instance ToJSON a => ToJSON1 (Const a) where
    liftToJSON _ _ (Const x) = toJSON x
    {-# INLINE liftToJSON #-}

    liftToEncoding _ _ (Const x) = toEncoding x
    {-# INLINE liftToEncoding #-}

instance ToJSON a => ToJSON (Const a b) where
    toJSON (Const x) = toJSON x
    {-# INLINE toJSON #-}

    toEncoding (Const x) = toEncoding x
    {-# INLINE toEncoding #-}

instance (ToJSON a, ToJSONKey a) => ToJSONKey (Const a b) where
    toJSONKey = contramap getConst toJSONKey


instance ToJSON1 Maybe where
    liftToJSON t _ (Just a) = t a
    liftToJSON _  _ Nothing  = Null
    {-# INLINE liftToJSON #-}

    liftToEncoding t _ (Just a) = t a
    liftToEncoding _  _ Nothing  = E.null_
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON (Maybe a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}


instance ToJSON2 Either where
    liftToJSON2  toA _ _toB _ (Left a)  = Object $ H.singleton "Left"  (toA a)
    liftToJSON2 _toA _  toB _ (Right b) = Object $ H.singleton "Right" (toB b)
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2  toA _ _toB _ (Left a) = E.pairs $ E.pair "Left" $ toA a

    liftToEncoding2 _toA _ toB _ (Right b) = E.pairs $ E.pair "Right" $ toB b
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a) => ToJSON1 (Either a) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}

    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b) => ToJSON (Either a b) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}

    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance ToJSON Void where
    toJSON = absurd
    {-# INLINE toJSON #-}

    toEncoding = absurd
    {-# INLINE toEncoding #-}


instance ToJSON Bool where
    toJSON = Bool
    {-# INLINE toJSON #-}

    toEncoding = E.bool
    {-# INLINE toEncoding #-}

instance ToJSONKey Bool where
    toJSONKey = toJSONKeyText $ \x -> if x then "true" else "false"


instance ToJSON Ordering where
  toJSON     = toJSON     . orderingToText
  toEncoding = toEncoding . orderingToText

orderingToText :: Ordering -> T.Text
orderingToText o = case o of
                     LT -> "LT"
                     EQ -> "EQ"
                     GT -> "GT"

instance ToJSON () where
    toJSON _ = emptyArray
    {-# INLINE toJSON #-}

    toEncoding _ = emptyArray_
    {-# INLINE toEncoding #-}


instance ToJSON Char where
    toJSON = String . T.singleton
    {-# INLINE toJSON #-}

    toJSONList = String . T.pack
    {-# INLINE toJSONList #-}

    toEncoding = E.string . (:[])
    {-# INLINE toEncoding #-}

    toEncodingList = E.string
    {-# INLINE toEncodingList #-}


instance ToJSON Double where
    toJSON = realFloatToJSON
    {-# INLINE toJSON #-}

    toEncoding = E.double
    {-# INLINE toEncoding #-}

instance ToJSONKey Double where
    toJSONKey = toJSONKeyTextEnc E.doubleText
    {-# INLINE toJSONKey #-}


instance ToJSON Number where
    toJSON (D d) = toJSON d
    toJSON (I i) = toJSON i
    {-# INLINE toJSON #-}

    toEncoding (D d) = toEncoding d
    toEncoding (I i) = toEncoding i
    {-# INLINE toEncoding #-}


instance ToJSON Float where
    toJSON = realFloatToJSON
    {-# INLINE toJSON #-}

    toEncoding = E.float
    {-# INLINE toEncoding #-}

instance ToJSONKey Float where
    toJSONKey = toJSONKeyTextEnc E.floatText
    {-# INLINE toJSONKey #-}


instance (ToJSON a, Integral a) => ToJSON (Ratio a) where
    toJSON r = object [ "numerator"   .= numerator   r
                      , "denominator" .= denominator r
                      ]
    {-# INLINE toJSON #-}

    toEncoding r = E.pairs $
        "numerator" .= numerator r <>
        "denominator" .= denominator r
    {-# INLINE toEncoding #-}


instance HasResolution a => ToJSON (Fixed a) where
    toJSON = Number . realToFrac
    {-# INLINE toJSON #-}

    toEncoding = E.scientific . realToFrac
    {-# INLINE toEncoding #-}

instance HasResolution a => ToJSONKey (Fixed a) where
    toJSONKey = toJSONKeyTextEnc (E.scientificText . realToFrac)
    {-# INLINE toJSONKey #-}


instance ToJSON Int where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = E.int
    {-# INLINE toEncoding #-}

instance ToJSONKey Int where
    toJSONKey = toJSONKeyTextEnc E.intText
    {-# INLINE toJSONKey #-}


instance ToJSON Integer where
    toJSON = Number . fromInteger
    {-# INLINE toJSON #-}

    toEncoding = E.integer
    {-# INLINE toEncoding #-}

instance ToJSONKey Integer where
    toJSONKey = toJSONKeyTextEnc E.integerText
    {-# INLINE toJSONKey #-}


instance ToJSON Natural where
    toJSON = toJSON . toInteger
    {-# INLINE toJSON #-}

    toEncoding = toEncoding . toInteger
    {-# INLINE toEncoding #-}

instance ToJSONKey Natural where
    toJSONKey = toJSONKeyTextEnc (E.integerText . toInteger)
    {-# INLINE toJSONKey #-}


instance ToJSON Int8 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = E.int8
    {-# INLINE toEncoding #-}

instance ToJSONKey Int8 where
    toJSONKey = toJSONKeyTextEnc E.int8Text
    {-# INLINE toJSONKey #-}


instance ToJSON Int16 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = E.int16
    {-# INLINE toEncoding #-}

instance ToJSONKey Int16 where
    toJSONKey = toJSONKeyTextEnc E.int16Text
    {-# INLINE toJSONKey #-}


instance ToJSON Int32 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = E.int32
    {-# INLINE toEncoding #-}

instance ToJSONKey Int32 where
    toJSONKey = toJSONKeyTextEnc E.int32Text
    {-# INLINE toJSONKey #-}


instance ToJSON Int64 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = E.int64
    {-# INLINE toEncoding #-}

instance ToJSONKey Int64 where
    toJSONKey = toJSONKeyTextEnc E.int64Text
    {-# INLINE toJSONKey #-}

instance ToJSON Word where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = E.word
    {-# INLINE toEncoding #-}

instance ToJSONKey Word where
    toJSONKey = toJSONKeyTextEnc E.wordText
    {-# INLINE toJSONKey #-}


instance ToJSON Word8 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = E.word8
    {-# INLINE toEncoding #-}

instance ToJSONKey Word8 where
    toJSONKey = toJSONKeyTextEnc E.word8Text
    {-# INLINE toJSONKey #-}


instance ToJSON Word16 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = E.word16
    {-# INLINE toEncoding #-}

instance ToJSONKey Word16 where
    toJSONKey = toJSONKeyTextEnc E.word16Text
    {-# INLINE toJSONKey #-}


instance ToJSON Word32 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = E.word32
    {-# INLINE toEncoding #-}

instance ToJSONKey Word32 where
    toJSONKey = toJSONKeyTextEnc E.word32Text
    {-# INLINE toJSONKey #-}


instance ToJSON Word64 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = E.word64
    {-# INLINE toEncoding #-}

instance ToJSONKey Word64 where
    toJSONKey = toJSONKeyTextEnc E.word64Text
    {-# INLINE toJSONKey #-}

instance ToJSON CTime where
    toJSON (CTime i) = toJSON i
    {-# INLINE toJSON #-}

    toEncoding (CTime i) = toEncoding i
    {-# INLINE toEncoding #-}

instance ToJSON Text where
    toJSON = String
    {-# INLINE toJSON #-}

    toEncoding = E.text
    {-# INLINE toEncoding #-}

instance ToJSONKey Text where
    toJSONKey = toJSONKeyText id
    {-# INLINE toJSONKey #-}


instance ToJSON LT.Text where
    toJSON = String . LT.toStrict
    {-# INLINE toJSON #-}

    toEncoding = E.lazyText
    {-# INLINE toEncoding #-}

instance ToJSONKey LT.Text where
    toJSONKey = toJSONKeyText LT.toStrict


instance ToJSON Version where
    toJSON = toJSON . showVersion
    {-# INLINE toJSON #-}

    toEncoding = toEncoding . showVersion
    {-# INLINE toEncoding #-}

instance ToJSONKey Version where
    toJSONKey = toJSONKeyText (T.pack . showVersion)

-------------------------------------------------------------------------------
-- semigroups NonEmpty
-------------------------------------------------------------------------------

instance ToJSON1 NonEmpty where
    liftToJSON t _ = listValue t . NE.toList
    {-# INLINE liftToJSON #-}

    liftToEncoding t _ = listEncoding t . NE.toList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON (NonEmpty a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

-------------------------------------------------------------------------------
-- scientific
-------------------------------------------------------------------------------

instance ToJSON Scientific where
    toJSON = Number
    {-# INLINE toJSON #-}

    toEncoding = E.scientific
    {-# INLINE toEncoding #-}

instance ToJSONKey Scientific where
    toJSONKey = toJSONKeyTextEnc E.scientificText

-------------------------------------------------------------------------------
-- DList
-------------------------------------------------------------------------------

instance ToJSON1 DList.DList where
    liftToJSON t _ = listValue t . toList
    {-# INLINE liftToJSON #-}

    liftToEncoding t _ = listEncoding t . toList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON (DList.DList a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

#if MIN_VERSION_dlist(1,0,0) && __GLASGOW_HASKELL__ >=800
-- | @since 1.5.3.0
instance ToJSON1 DNE.DNonEmpty where
    liftToJSON t _ = listValue t . DNE.toList
    {-# INLINE liftToJSON #-}

    liftToEncoding t _ = listEncoding t . DNE.toList
    {-# INLINE liftToEncoding #-}

-- | @since 1.5.3.0
instance (ToJSON a) => ToJSON (DNE.DNonEmpty a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}
#endif

-------------------------------------------------------------------------------
-- transformers - Functors
-------------------------------------------------------------------------------

instance ToJSON1 Identity where
    liftToJSON t _ (Identity a) = t a
    {-# INLINE liftToJSON #-}

    liftToJSONList _ tl xs = tl (map runIdentity xs)
    {-# INLINE liftToJSONList #-}

    liftToEncoding t _ (Identity a) = t a
    {-# INLINE liftToEncoding #-}

    liftToEncodingList _ tl xs = tl (map runIdentity xs)
    {-# INLINE liftToEncodingList #-}

instance (ToJSON a) => ToJSON (Identity a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toJSONList = liftToJSONList toJSON toJSONList
    {-# INLINE toJSONList #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

    toEncodingList = liftToEncodingList toEncoding toEncodingList
    {-# INLINE toEncodingList #-}

instance (ToJSONKey a) => ToJSONKey (Identity a) where
    toJSONKey = contramapToJSONKeyFunction runIdentity toJSONKey
    toJSONKeyList = contramapToJSONKeyFunction (map runIdentity) toJSONKeyList


instance (ToJSON1 f, ToJSON1 g) => ToJSON1 (Compose f g) where
    liftToJSON tv tvl (Compose x) = liftToJSON g gl x
      where
        g = liftToJSON tv tvl
        gl = liftToJSONList tv tvl
    {-# INLINE liftToJSON #-}

    liftToJSONList te tel xs = liftToJSONList g gl (map getCompose xs)
      where
        g = liftToJSON te tel
        gl = liftToJSONList te tel
    {-# INLINE liftToJSONList #-}

    liftToEncoding te tel (Compose x) = liftToEncoding g gl x
      where
        g = liftToEncoding te tel
        gl = liftToEncodingList te tel
    {-# INLINE liftToEncoding #-}

    liftToEncodingList te tel xs = liftToEncodingList g gl (map getCompose xs)
      where
        g = liftToEncoding te tel
        gl = liftToEncodingList te tel
    {-# INLINE liftToEncodingList #-}

instance (ToJSON1 f, ToJSON1 g, ToJSON a) => ToJSON (Compose f g a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toJSONList = liftToJSONList toJSON toJSONList
    {-# INLINE toJSONList #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

    toEncodingList = liftToEncodingList toEncoding toEncodingList
    {-# INLINE toEncodingList #-}


instance (ToJSON1 f, ToJSON1 g) => ToJSON1 (Product f g) where
    liftToJSON tv tvl (Pair x y) = liftToJSON2 tx txl ty tyl (x, y)
      where
        tx = liftToJSON tv tvl
        txl = liftToJSONList tv tvl
        ty = liftToJSON tv tvl
        tyl = liftToJSONList tv tvl

    liftToEncoding te tel (Pair x y) = liftToEncoding2 tx txl ty tyl (x, y)
      where
        tx = liftToEncoding te tel
        txl = liftToEncodingList te tel
        ty = liftToEncoding te tel
        tyl = liftToEncodingList te tel

instance (ToJSON1 f, ToJSON1 g, ToJSON a) => ToJSON (Product f g a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

instance (ToJSON1 f, ToJSON1 g) => ToJSON1 (Sum f g) where
    liftToJSON tv tvl (InL x) = Object $ H.singleton "InL" (liftToJSON tv tvl x)
    liftToJSON tv tvl (InR y) = Object $ H.singleton "InR" (liftToJSON tv tvl y)

    liftToEncoding te tel (InL x) = E.pairs $ E.pair "InL" $ liftToEncoding te tel x
    liftToEncoding te tel (InR y) = E.pairs $ E.pair "InR" $ liftToEncoding te tel y

instance (ToJSON1 f, ToJSON1 g, ToJSON a) => ToJSON (Sum f g a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

-------------------------------------------------------------------------------
-- containers
-------------------------------------------------------------------------------

instance ToJSON1 Seq.Seq where
    liftToJSON t _ = listValue t . toList
    {-# INLINE liftToJSON #-}

    liftToEncoding t _ = listEncoding t . toList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON (Seq.Seq a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}


instance ToJSON1 Set.Set where
    liftToJSON t _ = listValue t . Set.toList
    {-# INLINE liftToJSON #-}

    liftToEncoding t _ = listEncoding t . Set.toList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON (Set.Set a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}


instance ToJSON IntSet.IntSet where
    toJSON = toJSON . IntSet.toList
    {-# INLINE toJSON #-}

    toEncoding = toEncoding . IntSet.toList
    {-# INLINE toEncoding #-}


instance ToJSON1 IntMap.IntMap where
    liftToJSON t tol = liftToJSON to' tol' . IntMap.toList
      where
        to'  = liftToJSON2     toJSON toJSONList t tol
        tol' = liftToJSONList2 toJSON toJSONList t tol
    {-# INLINE liftToJSON #-}

    liftToEncoding t tol = liftToEncoding to' tol' . IntMap.toList
      where
        to'  = liftToEncoding2     toEncoding toEncodingList t tol
        tol' = liftToEncodingList2 toEncoding toEncodingList t tol
    {-# INLINE liftToEncoding #-}

instance ToJSON a => ToJSON (IntMap.IntMap a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}


instance ToJSONKey k => ToJSON1 (M.Map k) where
    liftToJSON g _ = case toJSONKey of
        ToJSONKeyText f _ -> Object . mapHashKeyVal f g
        ToJSONKeyValue  f _ -> Array . V.fromList . map (toJSONPair f g) . M.toList
    {-# INLINE liftToJSON #-}

    liftToEncoding g _ = case toJSONKey of
        ToJSONKeyText _ f -> dict f g M.foldrWithKey
        ToJSONKeyValue _ f -> listEncoding (pairEncoding f) . M.toList
      where
        pairEncoding f (a, b) = E.list id [f a, g b]
    {-# INLINE liftToEncoding #-}


instance (ToJSON v, ToJSONKey k) => ToJSON (M.Map k v) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}


instance ToJSON1 Tree.Tree where
    liftToJSON t tol = go
      where
        go (Tree.Node root branches) =
            liftToJSON2 t tol to' tol' (root, branches)

        to' = liftToJSON go (listValue go)
        tol' = liftToJSONList go (listValue go)
    {-# INLINE liftToJSON #-}

    liftToEncoding t tol = go
      where
        go (Tree.Node root branches) =
            liftToEncoding2 t tol to' tol' (root, branches)

        to' = liftToEncoding go (listEncoding go)
        tol' = liftToEncodingList go (listEncoding go)
    {-# INLINE liftToEncoding #-}

instance (ToJSON v) => ToJSON (Tree.Tree v) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

-------------------------------------------------------------------------------
-- uuid
-------------------------------------------------------------------------------

instance ToJSON UUID.UUID where
    toJSON = toJSON . UUID.toText
    toEncoding = E.unsafeToEncoding . EB.quote . B.byteString . UUID.toASCIIBytes

instance ToJSONKey UUID.UUID where
    toJSONKey = ToJSONKeyText UUID.toText $
        E.unsafeToEncoding . EB.quote . B.byteString . UUID.toASCIIBytes

-------------------------------------------------------------------------------
-- vector
-------------------------------------------------------------------------------

instance ToJSON1 Vector where
    liftToJSON t _ = Array . V.map t
    {-# INLINE liftToJSON #-}

    liftToEncoding t _ =  listEncoding t . V.toList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON (Vector a) where
    {-# SPECIALIZE instance ToJSON Array #-}

    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

encodeVector :: (ToJSON a, VG.Vector v a) => v a -> Encoding
encodeVector = listEncoding toEncoding . VG.toList
{-# INLINE encodeVector #-}

vectorToJSON :: (VG.Vector v a, ToJSON a) => v a -> Value
vectorToJSON = Array . V.map toJSON . V.convert
{-# INLINE vectorToJSON #-}

instance (Storable a, ToJSON a) => ToJSON (VS.Vector a) where
    toJSON = vectorToJSON
    {-# INLINE toJSON #-}

    toEncoding = encodeVector
    {-# INLINE toEncoding #-}


instance (VP.Prim a, ToJSON a) => ToJSON (VP.Vector a) where
    toJSON = vectorToJSON
    {-# INLINE toJSON #-}

    toEncoding = encodeVector
    {-# INLINE toEncoding #-}


instance (VG.Vector VU.Vector a, ToJSON a) => ToJSON (VU.Vector a) where
    toJSON = vectorToJSON
    {-# INLINE toJSON #-}

    toEncoding = encodeVector
    {-# INLINE toEncoding #-}

-------------------------------------------------------------------------------
-- unordered-containers
-------------------------------------------------------------------------------

instance ToJSON1 HashSet.HashSet where
    liftToJSON t _ = listValue t . HashSet.toList
    {-# INLINE liftToJSON #-}

    liftToEncoding t _ = listEncoding t . HashSet.toList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON (HashSet.HashSet a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}


instance ToJSONKey k => ToJSON1 (H.HashMap k) where
    liftToJSON g _ = case toJSONKey of
        ToJSONKeyText f _ -> Object . mapKeyVal f g
        ToJSONKeyValue f _ -> Array . V.fromList . map (toJSONPair f g) . H.toList
    {-# INLINE liftToJSON #-}

    -- liftToEncoding :: forall a. (a -> Encoding) -> ([a] -> Encoding) -> H.HashMap k a -> Encoding
    liftToEncoding g _ = case toJSONKey of
        ToJSONKeyText _ f -> dict f g H.foldrWithKey
        ToJSONKeyValue _ f -> listEncoding (pairEncoding f) . H.toList
      where
        pairEncoding f (a, b) = E.list id [f a, g b]
    {-# INLINE liftToEncoding #-}

instance (ToJSON v, ToJSONKey k) => ToJSON (H.HashMap k v) where
    {-# SPECIALIZE instance ToJSON Object #-}

    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

-------------------------------------------------------------------------------
-- aeson
-------------------------------------------------------------------------------

instance ToJSON Value where
    toJSON a = a
    {-# INLINE toJSON #-}

    toEncoding = E.value
    {-# INLINE toEncoding #-}

instance ToJSON DotNetTime where
    toJSON = toJSON . dotNetTime

    toEncoding = toEncoding . dotNetTime

dotNetTime :: DotNetTime -> String
dotNetTime (DotNetTime t) = secs ++ formatMillis t ++ ")/"
  where secs  = formatTime defaultTimeLocale "/Date(%s" t

formatMillis :: (FormatTime t) => t -> String
formatMillis = take 3 . formatTime defaultTimeLocale "%q"

-------------------------------------------------------------------------------
-- primitive
-------------------------------------------------------------------------------

instance ToJSON a => ToJSON (PM.Array a) where
  -- note: we could do better than this if vector exposed the data
  -- constructor in Data.Vector.
  toJSON = toJSON . Exts.toList
  toEncoding = toEncoding . Exts.toList

instance ToJSON a => ToJSON (PM.SmallArray a) where
  toJSON = toJSON . Exts.toList
  toEncoding = toEncoding . Exts.toList

instance (PM.Prim a,ToJSON a) => ToJSON (PM.PrimArray a) where
  toJSON = toJSON . Exts.toList
  toEncoding = toEncoding . Exts.toList

-------------------------------------------------------------------------------
-- time
-------------------------------------------------------------------------------

instance ToJSON Day where
    toJSON     = stringEncoding . E.day
    toEncoding = E.day

instance ToJSONKey Day where
    toJSONKey = toJSONKeyTextEnc E.day

instance ToJSON Month where
    toJSON     = stringEncoding . E.month
    toEncoding = E.month

instance ToJSONKey Month where
    toJSONKey = toJSONKeyTextEnc E.month

instance ToJSON Quarter where
    toJSON     = stringEncoding . E.quarter
    toEncoding = E.quarter

instance ToJSONKey Quarter where
    toJSONKey = toJSONKeyTextEnc E.quarter

instance ToJSON TimeOfDay where
    toJSON     = stringEncoding . E.timeOfDay
    toEncoding = E.timeOfDay

instance ToJSONKey TimeOfDay where
    toJSONKey = toJSONKeyTextEnc E.timeOfDay


instance ToJSON LocalTime where
    toJSON     = stringEncoding . E.localTime
    toEncoding = E.localTime

instance ToJSONKey LocalTime where
    toJSONKey = toJSONKeyTextEnc E.localTime


instance ToJSON ZonedTime where
    toJSON     = stringEncoding . E.zonedTime
    toEncoding = E.zonedTime

instance ToJSONKey ZonedTime where
    toJSONKey = toJSONKeyTextEnc E.zonedTime


instance ToJSON UTCTime where
    toJSON     = stringEncoding . E.utcTime
    toEncoding = E.utcTime

instance ToJSONKey UTCTime where
    toJSONKey = toJSONKeyTextEnc E.utcTime

-- | Encode something t a JSON string.
stringEncoding :: Encoding' Text -> Value
stringEncoding = String
    . T.dropAround (== '"')
    . T.decodeLatin1
    . L.toStrict
    . E.encodingToLazyByteString
{-# INLINE stringEncoding #-}


instance ToJSON NominalDiffTime where
    toJSON = Number . realToFrac
    {-# INLINE toJSON #-}

    toEncoding = E.scientific . realToFrac
    {-# INLINE toEncoding #-}


instance ToJSON DiffTime where
    toJSON = Number . realToFrac
    {-# INLINE toJSON #-}

    toEncoding = E.scientific . realToFrac
    {-# INLINE toEncoding #-}

-- | Encoded as number
instance ToJSON SystemTime where
    toJSON (MkSystemTime secs nsecs) =
        toJSON (fromIntegral secs + fromIntegral nsecs / 1000000000 :: Nano)
    toEncoding (MkSystemTime secs nsecs) =
        toEncoding (fromIntegral secs + fromIntegral nsecs / 1000000000 :: Nano)

instance ToJSON CalendarDiffTime where
    toJSON (CalendarDiffTime m nt) = object
        [ "months" .= m
        , "time" .= nt
        ]
    toEncoding (CalendarDiffTime m nt) = E.pairs
        ("months" .= m <> "time" .= nt)

instance ToJSON CalendarDiffDays where
    toJSON (CalendarDiffDays m d) = object
        [ "months" .= m
        , "days" .= d
        ]
    toEncoding (CalendarDiffDays m d) = E.pairs
        ("months" .= m <> "days" .= d)

instance ToJSON DayOfWeek where
    toJSON Monday    = "monday"
    toJSON Tuesday   = "tuesday"
    toJSON Wednesday = "wednesday"
    toJSON Thursday  = "thursday"
    toJSON Friday    = "friday"
    toJSON Saturday  = "saturday"
    toJSON Sunday    = "sunday"

    toEncoding = toEncodingDayOfWeek

toEncodingDayOfWeek :: DayOfWeek -> E.Encoding' a
toEncodingDayOfWeek Monday    = E.unsafeToEncoding "\"monday\""
toEncodingDayOfWeek Tuesday   = E.unsafeToEncoding "\"tuesday\""
toEncodingDayOfWeek Wednesday = E.unsafeToEncoding "\"wednesday\""
toEncodingDayOfWeek Thursday  = E.unsafeToEncoding "\"thursday\""
toEncodingDayOfWeek Friday    = E.unsafeToEncoding "\"friday\""
toEncodingDayOfWeek Saturday  = E.unsafeToEncoding "\"saturday\""
toEncodingDayOfWeek Sunday    = E.unsafeToEncoding "\"sunday\""

instance ToJSONKey DayOfWeek where
    toJSONKey = toJSONKeyTextEnc toEncodingDayOfWeek

instance ToJSON QuarterOfYear where
    toJSON Q1 = "q1"
    toJSON Q2 = "q2"
    toJSON Q3 = "q3"
    toJSON Q4 = "q4"

toEncodingQuarterOfYear :: QuarterOfYear -> E.Encoding' a
toEncodingQuarterOfYear Q1 = E.unsafeToEncoding "\"q1\""
toEncodingQuarterOfYear Q2 = E.unsafeToEncoding "\"q2\""
toEncodingQuarterOfYear Q3 = E.unsafeToEncoding "\"q3\""
toEncodingQuarterOfYear Q4 = E.unsafeToEncoding "\"q4\""

instance ToJSONKey QuarterOfYear where
    toJSONKey = toJSONKeyTextEnc toEncodingQuarterOfYear

-------------------------------------------------------------------------------
-- base Monoid/Semigroup
-------------------------------------------------------------------------------

instance ToJSON1 Monoid.Dual where
    liftToJSON t _ = t . Monoid.getDual
    {-# INLINE liftToJSON #-}

    liftToEncoding t _ = t . Monoid.getDual
    {-# INLINE liftToEncoding #-}

instance ToJSON a => ToJSON (Monoid.Dual a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}


instance ToJSON1 Monoid.First where
    liftToJSON t to' = liftToJSON t to' . Monoid.getFirst
    {-# INLINE liftToJSON #-}

    liftToEncoding t to' = liftToEncoding t to' . Monoid.getFirst
    {-# INLINE liftToEncoding #-}

instance ToJSON a => ToJSON (Monoid.First a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}


instance ToJSON1 Monoid.Last where
    liftToJSON t to' = liftToJSON t to' . Monoid.getLast
    {-# INLINE liftToJSON #-}

    liftToEncoding t to' = liftToEncoding t to' . Monoid.getLast
    {-# INLINE liftToEncoding #-}

instance ToJSON a => ToJSON (Monoid.Last a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}


instance ToJSON1 Semigroup.Min where
    liftToJSON t _ (Semigroup.Min x) = t x
    {-# INLINE liftToJSON #-}

    liftToEncoding t _ (Semigroup.Min x) = t x
    {-# INLINE liftToEncoding #-}

instance ToJSON a => ToJSON (Semigroup.Min a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}


instance ToJSON1 Semigroup.Max where
    liftToJSON t _ (Semigroup.Max x) = t x
    {-# INLINE liftToJSON #-}

    liftToEncoding t _ (Semigroup.Max x) = t x
    {-# INLINE liftToEncoding #-}

instance ToJSON a => ToJSON (Semigroup.Max a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

instance ToJSON1 Semigroup.First where
    liftToJSON t _ (Semigroup.First x) = t x
    {-# INLINE liftToJSON #-}

    liftToEncoding t _ (Semigroup.First x) = t x
    {-# INLINE liftToEncoding #-}

instance ToJSON a => ToJSON (Semigroup.First a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}


instance ToJSON1 Semigroup.Last where
    liftToJSON t _ (Semigroup.Last x) = t x
    {-# INLINE liftToJSON #-}

    liftToEncoding t _ (Semigroup.Last x) = t x
    {-# INLINE liftToEncoding #-}

instance ToJSON a => ToJSON (Semigroup.Last a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}


instance ToJSON1 Semigroup.WrappedMonoid where
    liftToJSON t _ (Semigroup.WrapMonoid x) = t x
    {-# INLINE liftToJSON #-}

    liftToEncoding t _ (Semigroup.WrapMonoid x) = t x
    {-# INLINE liftToEncoding #-}

instance ToJSON a => ToJSON (Semigroup.WrappedMonoid a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}


instance ToJSON1 Semigroup.Option where
    liftToJSON t to' = liftToJSON t to' . Semigroup.getOption
    {-# INLINE liftToJSON #-}

    liftToEncoding t to' = liftToEncoding t to' . Semigroup.getOption
    {-# INLINE liftToEncoding #-}

instance ToJSON a => ToJSON (Semigroup.Option a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

-------------------------------------------------------------------------------
-- data-fix
-------------------------------------------------------------------------------

-- | @since 1.5.3.0
instance ToJSON1 f => ToJSON (F.Fix f) where
    toJSON     = go where go (F.Fix f) = liftToJSON go toJSONList f
    toEncoding = go where go (F.Fix f) = liftToEncoding go toEncodingList f

-- | @since 1.5.3.0
instance (ToJSON1 f, Functor f) => ToJSON (F.Mu f) where
    toJSON     = F.foldMu (liftToJSON id (listValue id))
    toEncoding = F.foldMu (liftToEncoding id (listEncoding id))

-- | @since 1.5.3.0
instance (ToJSON1 f, Functor f) => ToJSON (F.Nu f) where
    toJSON     = F.foldNu (liftToJSON id (listValue id))
    toEncoding = F.foldNu (liftToEncoding id (listEncoding id))

-------------------------------------------------------------------------------
-- strict
-------------------------------------------------------------------------------

-- | @since 1.5.3.0
instance (ToJSON a, ToJSON b) => ToJSON (S.These a b) where
    toJSON = toJSON . S.toLazy
    toEncoding = toEncoding . S.toLazy

-- | @since 1.5.3.0
instance ToJSON2 S.These where
    liftToJSON2 toa toas tob tobs = liftToJSON2 toa toas tob tobs . S.toLazy
    liftToEncoding2 toa toas tob tobs = liftToEncoding2 toa toas tob tobs . S.toLazy

-- | @since 1.5.3.0
instance ToJSON a => ToJSON1 (S.These a) where
    liftToJSON toa tos = liftToJSON toa tos . S.toLazy
    liftToEncoding toa tos = liftToEncoding toa tos . S.toLazy

-- | @since 1.5.3.0
instance (ToJSON a, ToJSON b) => ToJSON (S.Pair a b) where
    toJSON = toJSON . S.toLazy
    toEncoding = toEncoding . S.toLazy

-- | @since 1.5.3.0
instance ToJSON2 S.Pair where
    liftToJSON2 toa toas tob tobs = liftToJSON2 toa toas tob tobs . S.toLazy
    liftToEncoding2 toa toas tob tobs = liftToEncoding2 toa toas tob tobs . S.toLazy

-- | @since 1.5.3.0
instance ToJSON a => ToJSON1 (S.Pair a) where
    liftToJSON toa tos = liftToJSON toa tos . S.toLazy
    liftToEncoding toa tos = liftToEncoding toa tos . S.toLazy

-- | @since 1.5.3.0
instance (ToJSON a, ToJSON b) => ToJSON (S.Either a b) where
    toJSON = toJSON . S.toLazy
    toEncoding = toEncoding . S.toLazy

-- | @since 1.5.3.0
instance ToJSON2 S.Either where
    liftToJSON2 toa toas tob tobs = liftToJSON2 toa toas tob tobs . S.toLazy
    liftToEncoding2 toa toas tob tobs = liftToEncoding2 toa toas tob tobs . S.toLazy

-- | @since 1.5.3.0
instance ToJSON a => ToJSON1 (S.Either a) where
    liftToJSON toa tos = liftToJSON toa tos . S.toLazy
    liftToEncoding toa tos = liftToEncoding toa tos . S.toLazy

-- | @since 1.5.3.0
instance ToJSON a => ToJSON (S.Maybe a) where
    toJSON = toJSON . S.toLazy
    toEncoding = toEncoding . S.toLazy

-- | @since 1.5.3.0
instance ToJSON1 S.Maybe where
    liftToJSON toa tos = liftToJSON toa tos . S.toLazy
    liftToEncoding toa tos = liftToEncoding toa tos . S.toLazy

-------------------------------------------------------------------------------
-- tagged
-------------------------------------------------------------------------------

instance ToJSON1 Proxy where
    liftToJSON _ _ _ = Null
    {-# INLINE liftToJSON #-}

    liftToEncoding _ _ _ = E.null_
    {-# INLINE liftToEncoding #-}

instance ToJSON (Proxy a) where
    toJSON _ = Null
    {-# INLINE toJSON #-}

    toEncoding _ = E.null_
    {-# INLINE toEncoding #-}


instance ToJSON2 Tagged where
    liftToJSON2 _ _ t _ (Tagged x) = t x
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 _ _ t _ (Tagged x) = t x
    {-# INLINE liftToEncoding2 #-}

instance ToJSON1 (Tagged a) where
    liftToJSON t _ (Tagged x) = t x
    {-# INLINE liftToJSON #-}

    liftToEncoding t _ (Tagged x) = t x
    {-# INLINE liftToEncoding #-}

instance ToJSON b => ToJSON (Tagged a b) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

instance ToJSONKey b => ToJSONKey (Tagged a b) where
    toJSONKey = contramapToJSONKeyFunction unTagged toJSONKey
    toJSONKeyList = contramapToJSONKeyFunction (fmap unTagged) toJSONKeyList

-------------------------------------------------------------------------------
-- these
-------------------------------------------------------------------------------

-- | @since 1.5.1.0
instance (ToJSON a, ToJSON b) => ToJSON (These a b) where
    toJSON (This a)    = object [ "This" .= a ]
    toJSON (That b)    = object [ "That" .= b ]
    toJSON (These a b) = object [ "This" .= a, "That" .= b ]

    toEncoding (This a)    = E.pairs $ "This" .= a
    toEncoding (That b)    = E.pairs $ "That" .= b
    toEncoding (These a b) = E.pairs $ "This" .= a <> "That" .= b

-- | @since 1.5.1.0
instance ToJSON2 These where
    liftToJSON2  toa _ _tob _ (This a)    = object [ "This" .= toa a ]
    liftToJSON2 _toa _  tob _ (That b)    = object [ "That" .= tob b ]
    liftToJSON2  toa _  tob _ (These a b) = object [ "This" .= toa a, "That" .= tob b ]

    liftToEncoding2  toa _ _tob _ (This a)    = E.pairs $ E.pair "This" (toa a)
    liftToEncoding2 _toa _  tob _ (That b)    = E.pairs $ E.pair "That" (tob b)
    liftToEncoding2  toa _  tob _ (These a b) = E.pairs $ E.pair "This" (toa a) <> E.pair "That" (tob b)

-- | @since 1.5.1.0
instance ToJSON a => ToJSON1 (These a) where
    liftToJSON _tob _ (This a)    = object [ "This" .= a ]
    liftToJSON  tob _ (That b)    = object [ "That" .= tob b ]
    liftToJSON  tob _ (These a b) = object [ "This" .= a, "That" .= tob b ]

    liftToEncoding _tob _ (This a)    = E.pairs $ "This" .= a
    liftToEncoding  tob _ (That b)    = E.pairs $ E.pair "That" (tob b)
    liftToEncoding  tob _ (These a b) = E.pairs $ "This" .= a <> E.pair "That" (tob b)

-- | @since 1.5.1.0
instance (ToJSON1 f, ToJSON1 g) => ToJSON1 (These1 f g) where
    liftToJSON tx tl (This1 a)    = object [ "This" .= liftToJSON tx tl a ]
    liftToJSON tx tl (That1 b)    = object [ "That" .= liftToJSON tx tl b ]
    liftToJSON tx tl (These1 a b) = object [ "This" .= liftToJSON tx tl a, "That" .= liftToJSON tx tl b ]

    liftToEncoding tx tl (This1 a)    = E.pairs $ E.pair "This" (liftToEncoding tx tl a)
    liftToEncoding tx tl (That1 b)    = E.pairs $ E.pair "That" (liftToEncoding tx tl b)
    liftToEncoding tx tl (These1 a b) = E.pairs $
        pair "This" (liftToEncoding tx tl a) `mappend`
        pair "That" (liftToEncoding tx tl b)

-- | @since 1.5.1.0
instance (ToJSON1 f, ToJSON1 g, ToJSON a) => ToJSON (These1 f g a) where
    toJSON     = toJSON1
    toEncoding = toEncoding1

-------------------------------------------------------------------------------
-- Instances for converting t map keys
-------------------------------------------------------------------------------

instance (ToJSON a, ToJSON b) => ToJSONKey (a,b)
instance (ToJSON a, ToJSON b, ToJSON c) => ToJSONKey (a,b,c)
instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSONKey (a,b,c,d)

instance ToJSONKey Char where
    toJSONKey = ToJSONKeyText T.singleton (E.string . (:[]))
    toJSONKeyList = toJSONKeyText T.pack

instance (ToJSONKey a, ToJSON a) => ToJSONKey [a] where
    toJSONKey = toJSONKeyList

-------------------------------------------------------------------------------
-- Tuple instances
-------------------------------------------------------------------------------

instance ToJSON2 (,) where
    liftToJSON2 toA _ toB _ (a, b) = Array $ V.create $ do
        mv <- VM.unsafeNew 2
        VM.unsafeWrite mv 0 (toA a)
        VM.unsafeWrite mv 1 (toB b)
        return mv
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 toA _ toB _ (a, b) = E.list id [toA a, toB b]
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a) => ToJSON1 ((,) a) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}
    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b) => ToJSON (a, b) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}
    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance (ToJSON a) => ToJSON2 ((,,) a) where
    liftToJSON2 toB _ toC _ (a, b, c) = Array $ V.create $ do
        mv <- VM.unsafeNew 3
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toB b)
        VM.unsafeWrite mv 2 (toC c)
        return mv
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 toB _ toC _ (a, b, c) = E.list id
      [ toEncoding a
      , toB b
      , toC c
      ]
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a, ToJSON b) => ToJSON1 ((,,) a b) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}
    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (a, b, c) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}
    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance (ToJSON a, ToJSON b) => ToJSON2 ((,,,) a b) where
    liftToJSON2 toC _ toD _ (a, b, c, d) = Array $ V.create $ do
        mv <- VM.unsafeNew 4
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toC c)
        VM.unsafeWrite mv 3 (toD d)
        return mv
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 toC _ toD _ (a, b, c, d) = E.list id
      [ toEncoding a
      , toEncoding b
      , toC c
      , toD d
      ]
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON1 ((,,,) a b c) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}
    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSON (a, b, c, d) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}
    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON2 ((,,,,) a b c) where
    liftToJSON2 toD _ toE _ (a, b, c, d, e) = Array $ V.create $ do
        mv <- VM.unsafeNew 5
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toJSON c)
        VM.unsafeWrite mv 3 (toD d)
        VM.unsafeWrite mv 4 (toE e)
        return mv
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 toD _ toE _ (a, b, c, d, e) = E.list id
      [ toEncoding a
      , toEncoding b
      , toEncoding c
      , toD d
      , toE e
      ]
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSON1 ((,,,,) a b c d) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}
    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e) => ToJSON (a, b, c, d, e) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}
    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSON2 ((,,,,,) a b c d) where
    liftToJSON2 toE _ toF _ (a, b, c, d, e, f) = Array $ V.create $ do
        mv <- VM.unsafeNew 6
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toJSON c)
        VM.unsafeWrite mv 3 (toJSON d)
        VM.unsafeWrite mv 4 (toE e)
        VM.unsafeWrite mv 5 (toF f)
        return mv
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 toE _ toF _ (a, b, c, d, e, f) = E.list id
      [ toEncoding a
      , toEncoding b
      , toEncoding c
      , toEncoding d
      , toE e
      , toF f
      ]
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e) => ToJSON1 ((,,,,,) a b c d e) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}
    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f) => ToJSON (a, b, c, d, e, f) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}
    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e) => ToJSON2 ((,,,,,,) a b c d e) where
    liftToJSON2 toF _ toG _ (a, b, c, d, e, f, g) = Array $ V.create $ do
        mv <- VM.unsafeNew 7
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toJSON c)
        VM.unsafeWrite mv 3 (toJSON d)
        VM.unsafeWrite mv 4 (toJSON e)
        VM.unsafeWrite mv 5 (toF f)
        VM.unsafeWrite mv 6 (toG g)
        return mv
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 toF _ toG _ (a, b, c, d, e, f, g) = E.list id
        [ toEncoding a
        , toEncoding b
        , toEncoding c
        , toEncoding d
        , toEncoding e
        , toF f
        , toG g
        ]
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f) => ToJSON1 ((,,,,,,) a b c d e f) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}
    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g) => ToJSON (a, b, c, d, e, f, g) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}
    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f) => ToJSON2 ((,,,,,,,) a b c d e f) where
    liftToJSON2 toG _ toH _ (a, b, c, d, e, f, g, h) = Array $ V.create $ do
        mv <- VM.unsafeNew 8
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toJSON c)
        VM.unsafeWrite mv 3 (toJSON d)
        VM.unsafeWrite mv 4 (toJSON e)
        VM.unsafeWrite mv 5 (toJSON f)
        VM.unsafeWrite mv 6 (toG g)
        VM.unsafeWrite mv 7 (toH h)
        return mv
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 toG _ toH _ (a, b, c, d, e, f, g, h) = E.list id
        [ toEncoding a
        , toEncoding b
        , toEncoding c
        , toEncoding d
        , toEncoding e
        , toEncoding f
        , toG g
        , toH h
        ]
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g) => ToJSON1 ((,,,,,,,) a b c d e f g) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}
    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h) => ToJSON (a, b, c, d, e, f, g, h) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}
    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g) => ToJSON2 ((,,,,,,,,) a b c d e f g) where
    liftToJSON2 toH _ toI _ (a, b, c, d, e, f, g, h, i) = Array $ V.create $ do
        mv <- VM.unsafeNew 9
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toJSON c)
        VM.unsafeWrite mv 3 (toJSON d)
        VM.unsafeWrite mv 4 (toJSON e)
        VM.unsafeWrite mv 5 (toJSON f)
        VM.unsafeWrite mv 6 (toJSON g)
        VM.unsafeWrite mv 7 (toH h)
        VM.unsafeWrite mv 8 (toI i)
        return mv
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 toH _ toI _ (a, b, c, d, e, f, g, h, i) = E.list id
        [ toEncoding a
        , toEncoding b
        , toEncoding c
        , toEncoding d
        , toEncoding e
        , toEncoding f
        , toEncoding g
        , toH h
        , toI i
        ]
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h) => ToJSON1 ((,,,,,,,,) a b c d e f g h) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}
    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i) => ToJSON (a, b, c, d, e, f, g, h, i) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}
    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h) => ToJSON2 ((,,,,,,,,,) a b c d e f g h) where
    liftToJSON2 toI _ toJ _ (a, b, c, d, e, f, g, h, i, j) = Array $ V.create $ do
        mv <- VM.unsafeNew 10
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toJSON c)
        VM.unsafeWrite mv 3 (toJSON d)
        VM.unsafeWrite mv 4 (toJSON e)
        VM.unsafeWrite mv 5 (toJSON f)
        VM.unsafeWrite mv 6 (toJSON g)
        VM.unsafeWrite mv 7 (toJSON h)
        VM.unsafeWrite mv 8 (toI i)
        VM.unsafeWrite mv 9 (toJ j)
        return mv
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 toI _ toJ _ (a, b, c, d, e, f, g, h, i, j) = E.list id
        [ toEncoding a
        , toEncoding b
        , toEncoding c
        , toEncoding d
        , toEncoding e
        , toEncoding f
        , toEncoding g
        , toEncoding h
        , toI i
        , toJ j
        ]
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i) => ToJSON1 ((,,,,,,,,,) a b c d e f g h i) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}
    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j) => ToJSON (a, b, c, d, e, f, g, h, i, j) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}
    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i) => ToJSON2 ((,,,,,,,,,,) a b c d e f g h i) where
    liftToJSON2 toJ _ toK _ (a, b, c, d, e, f, g, h, i, j, k) = Array $ V.create $ do
        mv <- VM.unsafeNew 11
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toJSON c)
        VM.unsafeWrite mv 3 (toJSON d)
        VM.unsafeWrite mv 4 (toJSON e)
        VM.unsafeWrite mv 5 (toJSON f)
        VM.unsafeWrite mv 6 (toJSON g)
        VM.unsafeWrite mv 7 (toJSON h)
        VM.unsafeWrite mv 8 (toJSON i)
        VM.unsafeWrite mv 9 (toJ j)
        VM.unsafeWrite mv 10 (toK k)
        return mv
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 toJ _ toK _ (a, b, c, d, e, f, g, h, i, j, k) = E.list id
        [ toEncoding a
        , toEncoding b
        , toEncoding c
        , toEncoding d
        , toEncoding e
        , toEncoding f
        , toEncoding g
        , toEncoding h
        , toEncoding i
        , toJ j
        , toK k
        ]
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j) => ToJSON1 ((,,,,,,,,,,) a b c d e f g h i j) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}
    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k) => ToJSON (a, b, c, d, e, f, g, h, i, j, k) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}
    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j) => ToJSON2 ((,,,,,,,,,,,) a b c d e f g h i j) where
    liftToJSON2 toK _ toL _ (a, b, c, d, e, f, g, h, i, j, k, l) = Array $ V.create $ do
        mv <- VM.unsafeNew 12
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toJSON c)
        VM.unsafeWrite mv 3 (toJSON d)
        VM.unsafeWrite mv 4 (toJSON e)
        VM.unsafeWrite mv 5 (toJSON f)
        VM.unsafeWrite mv 6 (toJSON g)
        VM.unsafeWrite mv 7 (toJSON h)
        VM.unsafeWrite mv 8 (toJSON i)
        VM.unsafeWrite mv 9 (toJSON j)
        VM.unsafeWrite mv 10 (toK k)
        VM.unsafeWrite mv 11 (toL l)
        return mv
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 toK _ toL _ (a, b, c, d, e, f, g, h, i, j, k, l) = E.list id
        [ toEncoding a
        , toEncoding b
        , toEncoding c
        , toEncoding d
        , toEncoding e
        , toEncoding f
        , toEncoding g
        , toEncoding h
        , toEncoding i
        , toEncoding j
        , toK k
        , toL l
        ]
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k) => ToJSON1 ((,,,,,,,,,,,) a b c d e f g h i j k) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}
    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l) => ToJSON (a, b, c, d, e, f, g, h, i, j, k, l) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}
    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k) => ToJSON2 ((,,,,,,,,,,,,) a b c d e f g h i j k) where
    liftToJSON2 toL _ toM _ (a, b, c, d, e, f, g, h, i, j, k, l, m) = Array $ V.create $ do
        mv <- VM.unsafeNew 13
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toJSON c)
        VM.unsafeWrite mv 3 (toJSON d)
        VM.unsafeWrite mv 4 (toJSON e)
        VM.unsafeWrite mv 5 (toJSON f)
        VM.unsafeWrite mv 6 (toJSON g)
        VM.unsafeWrite mv 7 (toJSON h)
        VM.unsafeWrite mv 8 (toJSON i)
        VM.unsafeWrite mv 9 (toJSON j)
        VM.unsafeWrite mv 10 (toJSON k)
        VM.unsafeWrite mv 11 (toL l)
        VM.unsafeWrite mv 12 (toM m)
        return mv
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 toL _ toM _ (a, b, c, d, e, f, g, h, i, j, k, l, m) = E.list id
        [ toEncoding a
        , toEncoding b
        , toEncoding c
        , toEncoding d
        , toEncoding e
        , toEncoding f
        , toEncoding g
        , toEncoding h
        , toEncoding i
        , toEncoding j
        , toEncoding k
        , toL l
        , toM m
        ]
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l) => ToJSON1 ((,,,,,,,,,,,,) a b c d e f g h i j k l) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}
    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l, ToJSON m) => ToJSON (a, b, c, d, e, f, g, h, i, j, k, l, m) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}
    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l) => ToJSON2 ((,,,,,,,,,,,,,) a b c d e f g h i j k l) where
    liftToJSON2 toM _ toN _ (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = Array $ V.create $ do
        mv <- VM.unsafeNew 14
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toJSON c)
        VM.unsafeWrite mv 3 (toJSON d)
        VM.unsafeWrite mv 4 (toJSON e)
        VM.unsafeWrite mv 5 (toJSON f)
        VM.unsafeWrite mv 6 (toJSON g)
        VM.unsafeWrite mv 7 (toJSON h)
        VM.unsafeWrite mv 8 (toJSON i)
        VM.unsafeWrite mv 9 (toJSON j)
        VM.unsafeWrite mv 10 (toJSON k)
        VM.unsafeWrite mv 11 (toJSON l)
        VM.unsafeWrite mv 12 (toM m)
        VM.unsafeWrite mv 13 (toN n)
        return mv
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 toM _ toN _ (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = E.list id
        [ toEncoding a
        , toEncoding b
        , toEncoding c
        , toEncoding d
        , toEncoding e
        , toEncoding f
        , toEncoding g
        , toEncoding h
        , toEncoding i
        , toEncoding j
        , toEncoding k
        , toEncoding l
        , toM m
        , toN n
        ]
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l, ToJSON m) => ToJSON1 ((,,,,,,,,,,,,,) a b c d e f g h i j k l m) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}
    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l, ToJSON m, ToJSON n) => ToJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}
    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l, ToJSON m) => ToJSON2 ((,,,,,,,,,,,,,,) a b c d e f g h i j k l m) where
    liftToJSON2 toN _ toO _ (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = Array $ V.create $ do
        mv <- VM.unsafeNew 15
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toJSON c)
        VM.unsafeWrite mv 3 (toJSON d)
        VM.unsafeWrite mv 4 (toJSON e)
        VM.unsafeWrite mv 5 (toJSON f)
        VM.unsafeWrite mv 6 (toJSON g)
        VM.unsafeWrite mv 7 (toJSON h)
        VM.unsafeWrite mv 8 (toJSON i)
        VM.unsafeWrite mv 9 (toJSON j)
        VM.unsafeWrite mv 10 (toJSON k)
        VM.unsafeWrite mv 11 (toJSON l)
        VM.unsafeWrite mv 12 (toJSON m)
        VM.unsafeWrite mv 13 (toN n)
        VM.unsafeWrite mv 14 (toO o)
        return mv
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 toN _ toO _ (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = E.list id
        [ toEncoding a
        , toEncoding b
        , toEncoding c
        , toEncoding d
        , toEncoding e
        , toEncoding f
        , toEncoding g
        , toEncoding h
        , toEncoding i
        , toEncoding j
        , toEncoding k
        , toEncoding l
        , toEncoding m
        , toN n
        , toO o
        ]
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l, ToJSON m, ToJSON n) => ToJSON1 ((,,,,,,,,,,,,,,) a b c d e f g h i j k l m n) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}
    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l, ToJSON m, ToJSON n, ToJSON o) => ToJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}
    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

--------------------------------------------------------------------------------

-- | Wrap a list of pairs as an object.
class Monoid pairs => FromPairs enc pairs | enc -> pairs where
  fromPairs :: pairs -> enc

instance (a ~ Value) => FromPairs (Encoding' a) Series where
  fromPairs = E.pairs

instance FromPairs Value (DList Pair) where
  fromPairs = object . toList

-- | Like 'KeyValue' but the value is already converted to JSON
-- ('Value' or 'Encoding'), and the result actually represents lists of pairs
-- so it can be readily concatenated.
class Monoid kv => KeyValuePair v kv where
    pair :: String -> v -> kv

instance (v ~ Value) => KeyValuePair v (DList Pair) where
    pair k v = DList.singleton (pack k .= v)

instance (e ~ Encoding) => KeyValuePair e Series where
    pair = E.pairStr
