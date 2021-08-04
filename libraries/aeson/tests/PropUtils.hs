{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module PropUtils (module PropUtils) where

import Prelude.Compat

import Data.Aeson (eitherDecode, encode)
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.Internal (IResult(..), formatError, ifromJSON, iparse)
import qualified Data.Aeson.Internal as I
import Data.Aeson.Parser (value)
import Data.Aeson.Types
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Int (Int8)
import Data.Map (Map)
import Data.Time (ZonedTime)
import Encoders
import Instances ()
import Test.QuickCheck (Arbitrary(..), Property, Testable, (===), (.&&.), counterexample)
import Types
import Text.Read (readMaybe)
import qualified Data.Attoparsec.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict as H
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V


encodeDouble :: Double -> Double -> Property
encodeDouble num denom
    | isInfinite d || isNaN d = encode d === "null"
    | otherwise               = (read . L.unpack . encode) d === d
  where d = num / denom

encodeInteger :: Integer -> Property
encodeInteger i = encode i === L.pack (show i)

toParseJSON :: (Eq a, Show a) =>
               (Value -> Parser a) -> (a -> Value) -> a -> Property
toParseJSON parsejson tojson x =
    case iparse parsejson . tojson $ x of
      IError path msg -> failure "parse" (formatError path msg) x
      ISuccess x'     -> x === x'

toParseJSON1
    :: (Eq (f Int), Show (f Int))
    => (forall a. LiftParseJSON f a)
    -> (forall a. LiftToJSON f a)
    -> f Int
    -> Property
toParseJSON1 parsejson1 tojson1 = toParseJSON parsejson tojson
  where
    parsejson = parsejson1 parseJSON (listParser parseJSON)
    tojson    = tojson1 toJSON (listValue toJSON)

roundTripEnc :: (FromJSON a, ToJSON a, Show a) =>
             (a -> a -> Property) -> a -> a -> Property
roundTripEnc eq _ i =
    case fmap ifromJSON . L.parse value . encode $ i of
      L.Done _ (ISuccess v)      -> v `eq` i
      L.Done _ (IError path err) -> failure "fromJSON" (formatError path err) i
      L.Fail _ _ err             -> failure "parse" err i

roundTripNoEnc :: (FromJSON a, ToJSON a, Show a) =>
             (a -> a -> Property) -> a -> a -> Property
roundTripNoEnc eq _ i =
    case ifromJSON . toJSON $ i of
      (ISuccess v)      -> v `eq` i
      (IError path err) -> failure "fromJSON" (formatError path err) i

roundTripEq :: (Eq a, FromJSON a, ToJSON a, Show a) => a -> a -> Property
roundTripEq x y = roundTripEnc (===) x y .&&. roundTripNoEnc (===) x y

roundtripReadShow :: Value -> Property
roundtripReadShow v = readMaybe (show v) === Just v

-- We test keys by encoding HashMap and Map with it
roundTripKey
    :: (Ord a, Hashable a, FromJSONKey a, ToJSONKey a, Show a)
    => a -> HashMap a Int -> Map a Int -> Property
roundTripKey _ h m = roundTripEq h h .&&. roundTripEq m m

infix 4 ==~
(==~) :: (ApproxEq a, Show a) => a -> a -> Property
x ==~ y =
  counterexample (show x ++ " /= " ++ show y) (x =~ y)

toFromJSON :: (Arbitrary a, Eq a, FromJSON a, ToJSON a, Show a) => a -> Property
toFromJSON x = case ifromJSON (toJSON x) of
                IError path err -> failure "fromJSON" (formatError path err) x
                ISuccess x'     -> x === x'

modifyFailureProp :: String -> String -> Bool
modifyFailureProp orig added =
    result == Error (added ++ orig)
  where
    parser = const $ modifyFailure (added ++) $ fail orig
    result :: Result ()
    result = parse parser ()

parserThrowErrorProp :: String -> Property
parserThrowErrorProp msg =
    result === Error msg
  where
    parser = const $ parserThrowError [] msg
    result :: Result ()
    result = parse parser ()

-- | Tests (also) that we catch the JSONPath and it has elements in the right order.
parserCatchErrorProp :: [String] -> String -> Property
parserCatchErrorProp path msg =
    result === Success ([I.Key "outer", I.Key "inner"] ++ jsonPath, msg)
  where
    parser = parserCatchError outer (curry pure)

    outer = inner I.<?> I.Key "outer"
    inner = parserThrowError jsonPath msg I.<?> I.Key "inner"

    result :: Result (I.JSONPath, String)
    result = parse (const parser) ()

    jsonPath = map (I.Key . T.pack) path

-- | Perform a structural comparison of the results of two encoding
-- methods. Compares decoded values to account for HashMap-driven
-- variation in JSON object key ordering.
sameAs :: (a -> Value) -> (a -> Encoding) -> a -> Property
sameAs toVal toEnc v =
  counterexample (show s) $
    eitherDecode s === Right (toVal v)
  where
    s = encodingToLazyByteString (toEnc v)

sameAs1
    :: (forall a. LiftToJSON f a)
    -> (forall a. LiftToEncoding f a)
    -> f Int
    -> Property
sameAs1 toVal1 toEnc1 v = lhs === rhs
  where
    rhs = Right $ toVal1 toJSON (listValue toJSON) v
    lhs = eitherDecode . encodingToLazyByteString $
        toEnc1 toEncoding (listEncoding toEncoding) v

sameAs1Agree
    :: ToJSON a
    => (f a -> Encoding)
    -> (forall b. LiftToEncoding f b)
    -> f a
    -> Property
sameAs1Agree toEnc toEnc1 v = rhs === lhs
  where
    rhs = encodingToLazyByteString $ toEnc v
    lhs = encodingToLazyByteString $ toEnc1 toEncoding (listEncoding toEncoding) v

type P6 = Product6 Int Bool String (Approx Double) (Int, Approx Double) ()
type S4 = Sum4 Int8 ZonedTime T.Text (Map.Map String Int)

--------------------------------------------------------------------------------
-- Value properties
--------------------------------------------------------------------------------

-- | Add the formatted @Value@ to the printed counterexample when the property
-- fails.
checkValue :: Testable a => (Value -> a) -> Value -> Property
checkValue prop v = counterexample (L.unpack (encode v)) (prop v)

isString :: Value -> Bool
isString (String _) = True
isString _          = False

is2ElemArray :: Value -> Bool
is2ElemArray (Array v) = V.length v == 2 && isString (V.head v)
is2ElemArray _         = False

isTaggedObjectValue :: Value -> Bool
isTaggedObjectValue (Object obj) = "tag"      `H.member` obj &&
                                   "contents" `H.member` obj
isTaggedObjectValue _            = False

isNullaryTaggedObject :: Value -> Bool
isNullaryTaggedObject obj = isTaggedObject' obj && isObjectWithSingleField obj

isTaggedObject :: Value -> Property
isTaggedObject = checkValue isTaggedObject'

isTaggedObject' :: Value -> Bool
isTaggedObject' (Object obj) = "tag" `H.member` obj
isTaggedObject' _            = False

isObjectWithSingleField :: Value -> Bool
isObjectWithSingleField (Object obj) = H.size obj == 1
isObjectWithSingleField _            = False

-- | is untaggedValue of EitherTextInt
isUntaggedValueETI :: Value -> Bool
isUntaggedValueETI (String s)
    | s == "nonenullary"   = True
isUntaggedValueETI (Bool _)   = True
isUntaggedValueETI (Number _) = True
isUntaggedValueETI (Array a)  = length a == 2
isUntaggedValueETI _          = False

isEmptyArray :: Value -> Property
isEmptyArray = checkValue isEmptyArray'

isEmptyArray' :: Value -> Bool
isEmptyArray' = (Array mempty ==)
