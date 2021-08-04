{-# LANGUAGE CPP #-}
module Utils where

import           Data.Word
import           Data.ByteArray               (Bytes, ScrubbedBytes)

#ifdef WITH_BASEMENT_SUPPORT
import           Basement.Block (Block)
import           Basement.UArray (UArray)
#endif

unS :: String -> [Word8]
unS = map (fromIntegral . fromEnum)

ascii :: [Word8] -> String
ascii = map (toEnum . fromIntegral)

-- | similar to proxy
data Witness a = Witness

withWitness :: Witness a -> a -> a
withWitness _ a = a

withBytesWitness :: Bytes -> Bytes
withBytesWitness = withWitness (Witness :: Witness Bytes)

withScrubbedBytesWitness :: ScrubbedBytes -> ScrubbedBytes
withScrubbedBytesWitness = id

#ifdef WITH_BASEMENT_SUPPORT
withBlockWitness :: Block Word8 -> Block Word8
withBlockWitness = withWitness (Witness :: Witness (Block Word8))

withUArrayWitness :: UArray Word8 -> UArray Word8
withUArrayWitness = withWitness (Witness :: Witness (UArray Word8))
#endif

numberedList :: [a] -> [(Int, a)]
numberedList = zip [1..]
