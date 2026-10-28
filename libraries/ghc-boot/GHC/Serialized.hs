{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiWayIf #-}
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
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Char (chr, ord)
import Data.Data
import Data.Ratio (numerator, denominator, (%))
import Data.Word (Word8)


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

-- Note [Serialized encoding]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- serializeWithData walks a value with its Data instance. For each
-- constructor it writes the ConstrRep, then the fields in order. The
-- ConstrRep is encoded as one unsigned LEB128 number (the "code"),
-- followed by its payload:
--
--   code      ConstrRep        payload
--   0         IntConstr i      i as signed LEB128
--   1         FloatConstr r    numerator as signed LEB128,
--                              denominator as unsigned LEB128
--   2         CharConstr c     ord c as unsigned LEB128
--   2 + ix    AlgConstr ix     none (ix >= 1 is the constructor index)
--
-- The code of an algebraic constructor contains its index, so most
-- constructors take one byte. A small number takes two bytes: one for the
-- code and one for the LEB128 value. For example:
--
--   Just True        ==> [4, 4]                     (2 bytes)
--   [1,2] :: [Int]   ==> [4, 0,1, 4, 0,2, 3]        (7 bytes)
--   3.25 :: Double   ==> [1, 13, 4]                 (3 bytes)
--
-- We do not use the Binary instances of Data.Binary for the numbers:
-- they have a fixed width (8 bytes for an Int, 5 for a small Integer)
-- and would make e.g. each element of a [Int] cost 15 bytes.
--
-- The encoding is internal. It is only read back by the same version of
-- this module (via interface files, or via the external interpreter),
-- so it can be changed freely.

-- | Use a 'Data' instance to implement a serialization scheme dual to that of 'deserializeWithData'
--
-- See Note [Serialized encoding]
serializeWithData :: Data a => a -> ShortByteString
serializeWithData = Short.toShort . ByteString.Lazy.toStrict . runPut . putData

-- | Use a 'Data' instance to implement a deserialization scheme dual to that of 'serializeWithData'
--
-- See Note [Serialized encoding]
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

-- Codes of the non-algebraic constructor representations.
-- See Note [Serialized encoding]
int_code, float_code, char_code :: Int
int_code   = 0
float_code = 1
char_code  = 2

-- | Code of an algebraic constructor with the given (1-based) index.
alg_code :: ConIndex -> Int
alg_code ix = char_code + ix

putConstrRep :: ConstrRep -> Put
putConstrRep (AlgConstr ix)  = putULEB128 (alg_code ix)
putConstrRep (IntConstr i)   = putULEB128 int_code >> putSLEB128 i
putConstrRep (FloatConstr r) = putULEB128 float_code
                               >> putSLEB128 (numerator r)
                               >> putULEB128 (denominator r)
putConstrRep (CharConstr c)  = putULEB128 char_code >> putULEB128 (ord c)

getConstrRep :: Get ConstrRep
getConstrRep = do
  code <- getULEB128
  if | code == int_code   -> IntConstr <$> getSLEB128
     | code == float_code -> do n <- getSLEB128
                                d <- getULEB128
                                pure (FloatConstr (n % d))
     | code == char_code  -> CharConstr . chr <$> getULEB128
     | otherwise          -> pure (AlgConstr (code - char_code))

-- LEB128 encoding: 7 bits of data per byte, least significant group
-- first. Bit 7 of a byte is set when another byte follows.
--
-- Signed LEB128 (SLEB128) encodes the two's complement value. Bit 6 of
-- the last byte is the sign bit: the decoder sign-extends from it. So
-- the encoder stops when the remaining bits are all equal to the sign
-- bit of the last byte written: when the value is 0 and bit 6 is clear,
-- or when the value is -1 and bit 6 is set. For example:
--
--   63  ==> [0x3f]              -64  ==> [0x40]
--   64  ==> [0xc0, 0x00]        -65  ==> [0xbf, 0x7f]
--
-- The unsigned functions are polymorphic: they are used at Int (codes,
-- Char ordinals) and at Integer (denominators). The signed functions
-- work on Integer only.

putULEB128 :: (Integral a, Bits a) => a -> Put
putULEB128 = go
  where
    go w
      | w < 0x80  = putWord8 (fromIntegral w)
      | otherwise = do putWord8 (fromIntegral (w .&. 0x7f) .|. 0x80)
                       go (w `shiftR` 7)

getULEB128 :: (Num a, Bits a) => Get a
getULEB128 = go 0 0
  where
    go shift acc = do
      b <- getWord8
      let acc' = acc .|. (fromIntegral (b .&. 0x7f) `shiftL` shift)
      if testBit b 7
        then go (shift + 7) acc'
        else pure acc'

putSLEB128 :: Integer -> Put
putSLEB128 v
  | done      = putWord8 byte
  | otherwise = putWord8 (byte .|. 0x80) >> putSLEB128 v'
  where
    byte     = fromIntegral (v .&. 0x7f) :: Word8
    v'       = v `shiftR` 7  -- arithmetic shift: keeps the sign
    sign_bit = testBit byte 6
    done     = (v' == 0 && not sign_bit) || (v' == -1 && sign_bit)

getSLEB128 :: Get Integer
getSLEB128 = go 0 0
  where
    go :: Int -> Integer -> Get Integer
    go shift acc = do
      b <- getWord8
      let acc'   = acc .|. (fromIntegral (b .&. 0x7f) `shiftL` shift)
          shift' = shift + 7
      if | testBit b 7 -> go shift' acc'
         | testBit b 6 -> pure (acc' - (1 `shiftL` shift'))  -- sign-extend
         | otherwise   -> pure acc'
