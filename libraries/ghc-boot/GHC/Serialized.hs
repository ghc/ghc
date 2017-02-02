{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

--
-- (c) The University of Glasgow 2002-2006
--
-- Serialized values

module GHC.Serialized (
    -- * Main Serialized data type
    Serialized(..),

    -- * Going into and out of 'Serialized'
    toSerialized, fromSerialized,

    -- * Handy serialization functions
    serializeWithData, deserializeWithData,
  ) where

import Data.Bits
import Data.Word        ( Word8 )
import Data.Data


-- | Represents a serialized value of a particular type. Attempts can be made to deserialize it at certain types
data Serialized = Serialized TypeRep [Word8]

-- | Put a Typeable value that we are able to actually turn into bytes into a 'Serialized' value ready for deserialization later
toSerialized :: forall a. Typeable a => (a -> [Word8]) -> a -> Serialized
toSerialized serialize what = Serialized rep (serialize what)
  where
    rep = typeOf what

-- | If the 'Serialized' value contains something of the given type, then use the specified deserializer to return @Just@ that.
-- Otherwise return @Nothing@.
fromSerialized :: forall a. Typeable a => ([Word8] -> a) -> Serialized -> Maybe a
#if MIN_VERSION_base(4,10,0)
fromSerialized deserialize (Serialized the_type bytes)
  | the_type == rep = Just (deserialize bytes)
  | otherwise       = Nothing
  where rep = typeRep (Proxy :: Proxy a)
#else
fromSerialized deserialize (Serialized the_type bytes)
  | the_type == typeOf (undefined :: a) = Just (deserialize bytes)
  | otherwise                           = Nothing
#endif

-- | Use a 'Data' instance to implement a serialization scheme dual to that of 'deserializeWithData'
serializeWithData :: Data a => a -> [Word8]
serializeWithData what = serializeWithData' what []

serializeWithData' :: Data a => a -> [Word8] -> [Word8]
serializeWithData' what = fst $ gfoldl (\(before, a_to_b) a -> (before . serializeWithData' a, a_to_b a))
                                       (\x -> (serializeConstr (constrRep (toConstr what)), x))
                                       what

-- | Use a 'Data' instance to implement a deserialization scheme dual to that of 'serializeWithData'
deserializeWithData :: Data a => [Word8] -> a
deserializeWithData = snd . deserializeWithData'

deserializeWithData' :: forall a. Data a => [Word8] -> ([Word8], a)
deserializeWithData' bytes = deserializeConstr bytes $ \constr_rep bytes ->
                             gunfold (\(bytes, b_to_r) -> let (bytes', b) = deserializeWithData' bytes in (bytes', b_to_r b))
                                     (\x -> (bytes, x))
                                     (repConstr (dataTypeOf (undefined :: a)) constr_rep)


serializeConstr :: ConstrRep -> [Word8] -> [Word8]
serializeConstr (AlgConstr ix)   = serializeWord8 1 . serializeInt ix
serializeConstr (IntConstr i)    = serializeWord8 2 . serializeInteger i
serializeConstr (FloatConstr r)  = serializeWord8 3 . serializeRational r
serializeConstr (CharConstr c)   = serializeWord8 4 . serializeChar c


deserializeConstr :: [Word8] -> (ConstrRep -> [Word8] -> a) -> a
deserializeConstr bytes k = deserializeWord8 bytes $ \constr_ix bytes ->
                            case constr_ix of
                                1 -> deserializeInt      bytes $ \ix -> k (AlgConstr ix)
                                2 -> deserializeInteger  bytes $ \i  -> k (IntConstr i)
                                3 -> deserializeRational bytes $ \r  -> k (FloatConstr r)
                                4 -> deserializeChar     bytes $ \c  -> k (CharConstr c)
                                x -> error $ "deserializeConstr: unrecognised serialized constructor type " ++ show x ++ " in context " ++ show bytes


serializeFixedWidthNum :: forall a. (Integral a, FiniteBits a) => a -> [Word8] -> [Word8]
serializeFixedWidthNum what = go (finiteBitSize what) what
  where
    go :: Int -> a -> [Word8] -> [Word8]
    go size current rest
      | size <= 0 = rest
      | otherwise = fromIntegral (current .&. 255) : go (size - 8) (current `shiftR` 8) rest

deserializeFixedWidthNum :: forall a b. (Integral a, FiniteBits a) => [Word8] -> (a -> [Word8] -> b) -> b
deserializeFixedWidthNum bytes k = go (finiteBitSize (undefined :: a)) bytes k
  where
    go :: Int -> [Word8] -> (a -> [Word8] -> b) -> b
    go size bytes k
      | size <= 0 = k 0 bytes
      | otherwise = case bytes of
                        (byte:bytes) -> go (size - 8) bytes (\x -> k ((x `shiftL` 8) .|. fromIntegral byte))
                        []           -> error "deserializeFixedWidthNum: unexpected end of stream"


serializeEnum :: (Enum a) => a -> [Word8] -> [Word8]
serializeEnum = serializeInt . fromEnum

deserializeEnum :: Enum a => [Word8] -> (a -> [Word8] -> b) -> b
deserializeEnum bytes k = deserializeInt bytes (k . toEnum)


serializeWord8 :: Word8 -> [Word8] -> [Word8]
serializeWord8 x = (x:)

deserializeWord8 :: [Word8] -> (Word8 -> [Word8] -> a) -> a
deserializeWord8 (byte:bytes) k = k byte bytes
deserializeWord8 []           _ = error "deserializeWord8: unexpected end of stream"


serializeInt :: Int -> [Word8] -> [Word8]
serializeInt = serializeFixedWidthNum

deserializeInt :: [Word8] -> (Int -> [Word8] -> a) -> a
deserializeInt = deserializeFixedWidthNum


serializeRational :: (Real a) => a -> [Word8] -> [Word8]
serializeRational = serializeString . show . toRational

deserializeRational :: (Fractional a) => [Word8] -> (a -> [Word8] -> b) -> b
deserializeRational bytes k = deserializeString bytes (k . fromRational . read)


serializeInteger :: Integer -> [Word8] -> [Word8]
serializeInteger = serializeString . show

deserializeInteger :: [Word8] -> (Integer -> [Word8] -> a) -> a
deserializeInteger bytes k = deserializeString bytes (k . read)


serializeChar :: Char -> [Word8] -> [Word8]
serializeChar = serializeString . show

deserializeChar :: [Word8] -> (Char -> [Word8] -> a) -> a
deserializeChar bytes k = deserializeString bytes (k . read)


serializeString :: String -> [Word8] -> [Word8]
serializeString = serializeList serializeEnum

deserializeString :: [Word8] -> (String -> [Word8] -> a) -> a
deserializeString = deserializeList deserializeEnum


serializeList :: (a -> [Word8] -> [Word8]) -> [a] -> [Word8] -> [Word8]
serializeList serialize_element xs = serializeInt (length xs) . foldr (.) id (map serialize_element xs)

deserializeList :: forall a b. (forall c. [Word8] -> (a -> [Word8] -> c) -> c)
                -> [Word8] -> ([a] -> [Word8] -> b) -> b
deserializeList deserialize_element bytes k = deserializeInt bytes $ \len bytes -> go len bytes k
  where
    go :: Int -> [Word8] -> ([a] -> [Word8] -> b) -> b
    go len bytes k
      | len <= 0  = k [] bytes
      | otherwise = deserialize_element bytes (\elt bytes -> go (len - 1) bytes (k . (elt:)))
