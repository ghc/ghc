{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -Wall #-}

module Parser
  ( byteParserBadOnce
  ) where

import Control.Monad.ST (runST)
import Data.Word (Word8)
import Packed.Bytes (Bytes)
import Packed.Bytes.Parser (Parser)
import Packed.Bytes.Stream.ST (ByteStream)
import qualified Data.Char
import qualified Packed.Bytes as B
import qualified Packed.Bytes.Parser as P
import qualified Packed.Bytes.Stream.ST as Stream

-- from common directory
import qualified Data.Trie.Naive as Naive

snmptrapdNaive :: Naive.Trie (Parser Word)
snmptrapdNaive = Naive.fromStringList
  [ ("STRING: ", P.any >>= \_ -> return 5)
  ]

runExampleParser :: Parser a -> (forall s. ByteStream s) -> (Maybe a, Maybe String)
runExampleParser parser stream = runST $ do
  P.Result mleftovers r <- P.parseStreamST stream parser
  mextra <- case mleftovers of
    Nothing -> return Nothing
    Just (P.Leftovers chunk remainingStream) -> do
      bs <- Stream.unpack remainingStream
      return (Just (map word8ToChar (B.unpack chunk ++ bs)))
  return (r,mextra)

byteParserBadOnce :: Int -> Int
byteParserBadOnce x = do
  let sample = ("STRING: _6_ " ++ show x)
      stream = Stream.fromBytes (s2b sample)
      expected = 6
      (r,mextra) = runExampleParser (Naive.parser snmptrapdNaive) stream
      a1 = if Nothing == mextra then 1 else 0
      a2 = if Just expected == r then 1 else 0
   in a1 + (a2 + x)

s2b :: String -> Bytes
s2b = B.pack . map charToWord8

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . Data.Char.ord

word8ToChar :: Word8 -> Char
word8ToChar = Data.Char.chr . fromIntegral
