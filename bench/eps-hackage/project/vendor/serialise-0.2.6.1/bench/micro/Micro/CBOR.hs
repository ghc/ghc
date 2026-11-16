{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Micro.CBOR (serialise, deserialise) where

import Micro.Types

import Codec.Serialise.Class
import Codec.Serialise.Encoding
import Codec.Serialise.Decoding hiding (DecodeAction(Done, Fail))
import qualified Codec.Serialise as Serialise

#if ! MIN_VERSION_base(4,11,0)
import           Data.Monoid
#endif

import qualified Data.ByteString.Lazy as BS

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Word
#endif

serialise :: Tree -> BS.ByteString
serialise = Serialise.serialise

deserialise :: BS.ByteString -> Tree
deserialise = Serialise.deserialise

encodeCtr0 :: Word -> Encoding
encodeCtr2 :: (Serialise a, Serialise b) => Word -> a -> b -> Encoding

encodeCtr0 n     = encodeListLen 1 <> encode (n :: Word)
encodeCtr2 n a b = encodeListLen 3 <> encode (n :: Word) <> encode a <> encode b

{-# INLINE encodeCtr0 #-}
{-# INLINE encodeCtr2 #-}

{-# INLINE decodeCtrTag #-}
{-# INLINE decodeCtrBody0 #-}
{-# INLINE decodeCtrBody2 #-}

decodeCtrTag :: Decoder s (Word, Int)
decodeCtrTag = (\len tag -> (tag, len)) <$> decodeListLen <*> decodeWord

decodeCtrBody0 :: Int -> a -> Decoder s a
decodeCtrBody0 1 f = pure f
decodeCtrBody0 x _ = error $ "decodeCtrBody0: impossible tag " ++ show x

decodeCtrBody2
  :: (Serialise a, Serialise b) => Int -> (a -> b -> c) -> Decoder s c
decodeCtrBody2 3 f = do x1 <- decode
                        x2 <- decode
                        return (f x1 x2)
decodeCtrBody2 x _ = error $ "decodeCtrBody2: impossible tag " ++ show x

instance Serialise Tree where
  encode Leaf       = encodeCtr0 1
  encode (Fork a b) = encodeCtr2 2 a b

  decode = do
    (t,l) <- decodeCtrTag
    case t of
      1 -> decodeCtrBody0 l Leaf
      2 -> decodeCtrBody2 l Fork
      x -> error $ "Serialise Tree: decode: impossible tag " ++ show x
