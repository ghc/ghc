{-# LANGUAGE CPP, OverloadedStrings, ExistentialQuantification, BangPatterns #-}

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

module Main where

import Control.DeepSeq
import Control.Exception (evaluate)
import Criterion.Main
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import Data.Bits
import Data.Char (ord)
import Data.List (foldl')

import Control.Applicative
import Data.Binary
import Data.Binary.Get

import qualified Data.Serialize.Get as Cereal

import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Lazy as AL

#if !MIN_VERSION_bytestring(0,10,0)
instance NFData S.ByteString
instance NFData L.ByteString where
  rnf = rnf . L.toChunks
#endif

main :: IO ()
main = do
  evaluate $ rnf [
    rnf brackets,
    rnf bracketsInChunks,
    rnf bracketCount,
    rnf oneMegabyte,
    rnf oneMegabyteLBS,
    rnf manyBytes,
    rnf encodedBigInteger
     ]
  defaultMain
    [ bgroup "brackets"
        [ bench "Binary 100kb, one chunk" $
            whnf (checkBracket . runTest bracketParser) brackets
        , bench "Binary 100kb, 100 byte chunks" $
            whnf (checkBracket . runTest bracketParser) bracketsInChunks
        , bench "Attoparsec lazy-bs 100kb, one chunk" $
            whnf (checkBracket . runAttoL bracketParser_atto) brackets
        , bench "Attoparsec lazy-bs 100kb, 100 byte chunks" $
            whnf (checkBracket . runAttoL bracketParser_atto) bracketsInChunks
        , bench "Attoparsec strict-bs 100kb" $
            whnf (checkBracket . runAtto bracketParser_atto) $ S.concat (L.toChunks brackets)
        , bench "Cereal strict-bs 100kb" $
            whnf (checkBracket . runCereal bracketParser_cereal) $ S.concat (L.toChunks brackets)
        ]
    , bgroup "comparison getStruct4, 1MB of struct of 4 Word8s"
      [ bench "Attoparsec" $
          whnf (runAtto (getStruct4_atto mega)) oneMegabyte
      , bench "Binary" $
          whnf (runTest (getStruct4 mega)) oneMegabyteLBS
      , bench "Cereal" $
          whnf (runCereal (getStruct4_cereal mega)) oneMegabyte
      ]
    , bgroup "comparison getWord8, 1MB"
        [ bench "Attoparsec" $
            whnf (runAtto (getWord8N1_atto mega)) oneMegabyte
        , bench "Binary" $
            whnf (runTest (getWord8N1 mega)) oneMegabyteLBS
        , bench "Cereal" $
            whnf (runCereal (getWord8N1_cereal mega)) oneMegabyte
        ]
    , bgroup "getWord8 1MB"
        [ bench "chunk size 2 bytes" $
            whnf (runTest (getWord8N2 mega)) oneMegabyteLBS
        , bench "chunk size 4 bytes" $
            whnf (runTest (getWord8N4 mega)) oneMegabyteLBS
        , bench "chunk size 8 bytes" $
            whnf (runTest (getWord8N8 mega)) oneMegabyteLBS
        , bench "chunk size 16 bytes" $
            whnf (runTest (getWord8N16 mega)) oneMegabyteLBS
        ]
    , bgroup "getWord8 1MB Applicative"
        [ bench "chunk size 2 bytes" $
            whnf (runTest (getWord8N2A mega)) oneMegabyteLBS
        , bench "chunk size 4 bytes" $
            whnf (runTest (getWord8N4A mega)) oneMegabyteLBS
        , bench "chunk size 8 bytes" $
            whnf (runTest (getWord8N8A mega)) oneMegabyteLBS
        , bench "chunk size 16 bytes" $
            whnf (runTest (getWord8N16A mega)) oneMegabyteLBS
        ]
    , bgroup "roll"
        [ bench "foldr"  $ nf (roll_foldr  :: [Word8] -> Integer) manyBytes
        , bench "foldl'" $ nf (roll_foldl' :: [Word8] -> Integer) manyBytes
        ]
    , bgroup "Integer"
        [ bench "decode" $ nf (decode :: L.ByteString -> Integer) encodedBigInteger
        ]
    ]

checkBracket :: Int -> Int
checkBracket x | x == bracketCount = x
               | otherwise = error "argh!"

runTest :: Get a -> L.ByteString -> a
runTest decoder inp = runGet decoder inp

runCereal :: Cereal.Get a -> C8.ByteString -> a
runCereal decoder inp = case Cereal.runGet decoder inp of
                          Right a -> a
                          Left err -> error err

runAtto :: AL.Parser a -> C8.ByteString -> a
runAtto decoder inp = case A.parseOnly decoder inp of
                        Right a -> a
                        Left err -> error err

runAttoL :: Show a => AL.Parser a -> L.ByteString -> a
runAttoL decoder inp = case AL.parse decoder inp of
                        AL.Done _ r -> r
                        a -> error (show a)

-- Defs.

oneMegabyte :: S.ByteString
oneMegabyte = S.replicate mega $ fromIntegral $ ord 'a'

oneMegabyteLBS :: L.ByteString
oneMegabyteLBS = L.fromChunks [oneMegabyte]

mega :: Int
mega = 1024 * 1024

-- 100k of brackets
bracketTest :: L.ByteString -> Int
bracketTest inp = runTest bracketParser inp

bracketCount :: Int
bracketCount = fromIntegral $ L.length brackets `div` 2

brackets :: L.ByteString
brackets = L.fromChunks [C8.concat (L.toChunks bracketsInChunks)]

bracketsInChunks :: L.ByteString
bracketsInChunks = L.fromChunks (replicate chunksOfBrackets oneChunk)
  where
    oneChunk = "((()((()()))((()(()()()()()()()(((()()()()(()()(()(()())))))()((())())))()())(((())())(()))))()(()))"
    chunksOfBrackets = 102400 `div` S.length oneChunk

bracketParser :: Get Int
bracketParser = cont <|> return 0
  where
  cont = do v <- some ( do 40 <- getWord8
                           n <- many cont
                           41 <- getWord8
                           return $! sum n + 1)
            return $! sum v

bracketParser_cereal :: Cereal.Get Int
bracketParser_cereal = cont <|> return 0
  where
  cont = do v <- some ( do 40 <- Cereal.getWord8
                           n <- many cont
                           41 <- Cereal.getWord8
                           return $! sum n + 1)
            return $! sum v

bracketParser_atto :: A.Parser Int
bracketParser_atto = cont <|> return 0
  where
  cont = do v <- some ( do _ <- A.word8 40
                           n <- bracketParser_atto
                           _ <- A.word8 41
                           return $! n + 1)
            return $! sum v

-- Strict struct of 4 Word8s
data S2 = S2 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
data S4 = S4 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
data S8 = S8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
             {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
data S16 = S16 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
               {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
               {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
               {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8

getStruct4 :: Int -> Get [S4]
getStruct4 = loop []
  where loop acc 0 = return acc
        loop acc n = do
          !w0 <- getWord8
          !w1 <- getWord8
          !w2 <- getWord8
          !w3 <- getWord8
          let !s = S4 w0 w1 w2 w3
          loop (s : acc) (n - 4)

getStruct4_cereal :: Int -> Cereal.Get [S4]
getStruct4_cereal = loop []
  where loop acc 0 = return acc
        loop acc n = do
          !w0 <- Cereal.getWord8
          !w1 <- Cereal.getWord8
          !w2 <- Cereal.getWord8
          !w3 <- Cereal.getWord8
          let !s = S4 w0 w1 w2 w3
          loop (s : acc) (n - 4)

getStruct4_atto :: Int -> A.Parser [S4]
getStruct4_atto = loop []
  where loop acc 0 = return acc
        loop acc n = do
          !w0 <- A.anyWord8
          !w1 <- A.anyWord8
          !w2 <- A.anyWord8
          !w3 <- A.anyWord8
          let !s = S4 w0 w1 w2 w3
          loop (s : acc) (n - 4)

getWord8N1 :: Int -> Get [Word8]
getWord8N1 = loop []
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord8
          loop (s0:s) (n-1)

getWord8N1_cereal :: Int -> Cereal.Get [Word8]
getWord8N1_cereal = loop []
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- Cereal.getWord8
          loop (s0:s) (n-1)

getWord8N1_atto :: Int -> A.Parser [Word8]
getWord8N1_atto = loop []
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- A.anyWord8
          loop (s0:s) (n-1)

getWord8N2 :: Int -> Get [S2]
getWord8N2 = loop []
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord8
          s1 <- getWord8
          let !v = S2 s0 s1
          loop (v:s) (n-2)

getWord8N2A :: Int -> Get [S2]
getWord8N2A = loop []
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          !v <- S2 <$> getWord8 <*> getWord8
          loop (v:s) (n-2)

getWord8N4 :: Int -> Get [S4]
getWord8N4 = loop []
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord8
          s1 <- getWord8
          s2 <- getWord8
          s3 <- getWord8
          let !v = S4 s0 s1 s2 s3
          loop (v:s) (n-4)

getWord8N4A :: Int -> Get [S4]
getWord8N4A = loop []
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          !v <- S4 <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8
          loop (v:s) (n-4)

getWord8N8 :: Int -> Get [S8]
getWord8N8 = loop []
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord8
          s1 <- getWord8
          s2 <- getWord8
          s3 <- getWord8
          s4 <- getWord8
          s5 <- getWord8
          s6 <- getWord8
          s7 <- getWord8
          let !v = S8 s0 s1 s2 s3 s4 s5 s6 s7
          loop (v:s) (n-8)

getWord8N8A :: Int -> Get [S8]
getWord8N8A = loop []
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          !v <- S8 <$> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
          loop (v:s) (n-8)

getWord8N16 :: Int -> Get [S16]
getWord8N16 = loop []
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord8
          s1 <- getWord8
          s2 <- getWord8
          s3 <- getWord8
          s4 <- getWord8
          s5 <- getWord8
          s6 <- getWord8
          s7 <- getWord8
          s8 <- getWord8
          s9 <- getWord8
          s10 <- getWord8
          s11 <- getWord8
          s12 <- getWord8
          s13 <- getWord8
          s14 <- getWord8
          s15 <- getWord8
          let !v = S16 s0 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15
          loop (v:s) (n-16)

getWord8N16A :: Int -> Get [S16]
getWord8N16A = loop []
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          !v <- S16 <$> getWord8
                    <*> getWord8
                    <*> getWord8
                    <*> getWord8
                    <*> getWord8
                    <*> getWord8
                    <*> getWord8
                    <*> getWord8
                    <*> getWord8
                    <*> getWord8
                    <*> getWord8
                    <*> getWord8
                    <*> getWord8
                    <*> getWord8
                    <*> getWord8
                    <*> getWord8
          loop (v:s) (n-16)

manyBytes :: [Word8]
manyBytes = concat $ replicate 256 [0..255]

bigInteger :: Integer
bigInteger = roll_foldl' manyBytes

encodedBigInteger :: L.ByteString
encodedBigInteger = encode bigInteger

roll_foldr :: (Integral a, Bits a) => [Word8] -> a
roll_foldr   = foldr unstep 0
  where
    unstep b a = a `shiftL` 8 .|. fromIntegral b

roll_foldl' :: (Integral a, Bits a) => [Word8] -> a
roll_foldl'   = foldl' unstep 0 . reverse
  where
    unstep a b = a `shiftL` 8 .|. fromIntegral b
