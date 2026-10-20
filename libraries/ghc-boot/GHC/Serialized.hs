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

import Prelude -- See note [Why do we import Prelude here?]
import Control.DeepSeq
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Data


-- | Represents a serialized value of a particular type. Attempts can be made to deserialize it at certain types
data Serialized = Serialized TypeRep ShortByteString

instance NFData Serialized where
  rnf (Serialized tr ws) = rnf tr `seq` rnf ws

-- | Put a Typeable value that we are able to actually turn into bytes into a 'Serialized' value ready for deserialization later
toSerialized :: forall a. Typeable a => (a -> ShortByteString) -> a -> Serialized
toSerialized serialize what = Serialized (typeOf what) (serialize what)

-- | If the 'Serialized' value contains something of the given type, then use the specified deserializer to return @Just@ that.
-- Otherwise return @Nothing@.
fromSerialized :: forall a. Typeable a => (ShortByteString -> a) -> Serialized -> Maybe a
fromSerialized deserialize (Serialized the_type bytes)
  | the_type == rep = Just (deserialize bytes)
  | otherwise       = Nothing
  where rep = typeRep (Proxy :: Proxy a)

-- | Use a 'Data' instance to implement a serialization scheme dual to that of 'deserializeWithData'
serializeWithData :: Data a => a -> ShortByteString
serializeWithData = Short.toShort . ByteString.Lazy.toStrict . runPut . putData

-- | Use a 'Data' instance to implement a deserialization scheme dual to that of 'serializeWithData'
deserializeWithData :: Data a => ShortByteString -> a
deserializeWithData bs =
  case runGetOrFail getData (ByteString.Lazy.fromStrict (Short.fromShort bs)) of
    Right (_, _, x)    -> x
    Left (_, off, msg) ->
      error ("deserializeWithData: " ++ msg ++ " at offset " ++ show off)

putData :: Data a => a -> Put
putData x = do
  putConstrRep (constrRep (toConstr x))
  sequence_ (gmapQ putData x)

getData :: forall a. Data a => Get a
getData = do
  rep <- getConstrRep
  fromConstrM getData (repConstr (dataTypeOf (undefined :: a)) rep)

putConstrRep :: ConstrRep -> Put
putConstrRep (AlgConstr ix)  = putWord8 1 >> put ix
putConstrRep (IntConstr i)   = putWord8 2 >> put i
putConstrRep (FloatConstr r) = putWord8 3 >> put r
putConstrRep (CharConstr c)  = putWord8 4 >> put c

getConstrRep :: Get ConstrRep
getConstrRep = do
  tag <- getWord8
  case tag of
    1 -> AlgConstr   <$> get
    2 -> IntConstr   <$> get
    3 -> FloatConstr <$> get
    4 -> CharConstr  <$> get
    _ -> fail ("unrecognised constructor tag " ++ show tag)
