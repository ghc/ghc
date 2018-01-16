{-# LANGUAGE CPP, ExistentialQuantification #-}

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

module Main (main) where

#if ! MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid(mappend, mempty))
#endif

import Control.DeepSeq
import Control.Exception (evaluate)
import Criterion.Main
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Char (ord)
import Data.Word (Word8)

import Data.Binary.Builder

main :: IO ()
main = do
  evaluate $ rnf
    [ rnf word8s
    , rnf smallByteString
    , rnf largeByteString
    ]
  defaultMain
    [ -- Test GHC loop optimization of continuation based code.
      bench "[Word8]" $ whnf (run . fromWord8s) word8s

      -- Test bounds check merging
    , bench "bounds/[Word8]" $ whnf (run . from4Word8s) word8s

    , bench "small ByteString" $ whnf (run . fromByteString) smallByteString
    , bench "large ByteString" $ whnf (run . fromByteString) largeByteString
    , bench "length-prefixed ByteString" $ whnf (run . lengthPrefixedBS)
      smallByteString

    , bgroup "Host endian"
      [ bench "1MB of Word8 in chunks of 16" $ whnf (run . putWord8N16) n
      , bench "1MB of Word16 in chunks of 16" $ whnf (run . putWord16N16Host)
        (n `div` 2)
      , bench "1MB of Word32 in chunks of 16" $ whnf (run . putWord32N16Host)
        (n `div` 4)
      , bench "1MB of Word64 in chunks of 16" $ whnf (run . putWord64N16Host)
        (n `div` 8)
      ]
    ]
  where
    run = L.length . toLazyByteString
    n = 1 * (2 ^ (20 :: Int))  -- one MB

-- Input data

word8s :: [Word8]
word8s = replicate 10000 $ fromIntegral $ ord 'a'
{-# NOINLINE word8s #-}

smallByteString :: S.ByteString
smallByteString = C.pack "abcdefghi"

largeByteString :: S.ByteString
largeByteString = S.pack word8s

------------------------------------------------------------------------
-- Benchmarks

fromWord8s :: [Word8] -> Builder
fromWord8s [] = mempty
fromWord8s (x:xs) = singleton x <> fromWord8s xs

from4Word8s :: [Word8] -> Builder
from4Word8s [] = mempty
from4Word8s (x:xs) = singleton x <> singleton x <> singleton x <> singleton x <>
                     from4Word8s xs

-- Write 100 short, length-prefixed ByteStrings.
lengthPrefixedBS :: S.ByteString -> Builder
lengthPrefixedBS bs = loop (100 :: Int)
  where loop n | n `seq` False = undefined
        loop 0 = mempty
        loop n =
#if WORD_SIZE_IN_BITS == 32
            putWord32be (fromIntegral $ S.length bs) <>
#elif WORD_SIZE_IN_BITS == 64
            putWord64be (fromIntegral $ S.length bs) <>
#else
# error Unsupported platform
#endif
            fromByteString bs <>
            loop (n-1)

putWord8N16 :: Int -> Builder
putWord8N16 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n =
          singleton (s+0) <>
          singleton (s+1) <>
          singleton (s+2) <>
          singleton (s+3) <>
          singleton (s+4) <>
          singleton (s+5) <>
          singleton (s+6) <>
          singleton (s+7) <>
          singleton (s+8) <>
          singleton (s+9) <>
          singleton (s+10) <>
          singleton (s+11) <>
          singleton (s+12) <>
          singleton (s+13) <>
          singleton (s+14) <>
          singleton (s+15) <>
          loop (s+16) (n-16)

putWord16N16Host :: Int -> Builder
putWord16N16Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n =
          putWord16host (s+0) <>
          putWord16host (s+1) <>
          putWord16host (s+2) <>
          putWord16host (s+3) <>
          putWord16host (s+4) <>
          putWord16host (s+5) <>
          putWord16host (s+6) <>
          putWord16host (s+7) <>
          putWord16host (s+8) <>
          putWord16host (s+9) <>
          putWord16host (s+10) <>
          putWord16host (s+11) <>
          putWord16host (s+12) <>
          putWord16host (s+13) <>
          putWord16host (s+14) <>
          putWord16host (s+15) <>
          loop (s+16) (n-16)

putWord32N16Host :: Int -> Builder
putWord32N16Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n =
          putWord32host (s+0) <>
          putWord32host (s+1) <>
          putWord32host (s+2) <>
          putWord32host (s+3) <>
          putWord32host (s+4) <>
          putWord32host (s+5) <>
          putWord32host (s+6) <>
          putWord32host (s+7) <>
          putWord32host (s+8) <>
          putWord32host (s+9) <>
          putWord32host (s+10) <>
          putWord32host (s+11) <>
          putWord32host (s+12) <>
          putWord32host (s+13) <>
          putWord32host (s+14) <>
          putWord32host (s+15) <>
          loop (s+16) (n-16)

putWord64N16Host :: Int -> Builder
putWord64N16Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n =
          putWord64host (s+0) <>
          putWord64host (s+1) <>
          putWord64host (s+2) <>
          putWord64host (s+3) <>
          putWord64host (s+4) <>
          putWord64host (s+5) <>
          putWord64host (s+6) <>
          putWord64host (s+7) <>
          putWord64host (s+8) <>
          putWord64host (s+9) <>
          putWord64host (s+10) <>
          putWord64host (s+11) <>
          putWord64host (s+12) <>
          putWord64host (s+13) <>
          putWord64host (s+14) <>
          putWord64host (s+15) <>
          loop (s+16) (n-16)

------------------------------------------------------------------------
-- Utilities

infixr 6 <>

(<>) :: Monoid m => m -> m -> m
(<>) = mappend
