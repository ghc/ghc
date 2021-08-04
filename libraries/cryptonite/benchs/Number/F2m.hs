{-# LANGUAGE PackageImports #-}

module Number.F2m (benchF2m) where

import Gauge.Main
import System.Random

import Crypto.Number.Basic (log2)
import Crypto.Number.F2m

genInteger :: Int -> Int -> Integer
genInteger salt bits
    = head
    . dropWhile ((< bits) . log2)
    . scanl (\a r -> a * 2^(31 :: Int) + abs r) 0
    . randoms
    . mkStdGen
    $ salt + bits

benchMod :: Int -> Benchmark
benchMod bits = bench (show bits) $ nf (modF2m m) a
  where
    m = genInteger 0 bits
    a = genInteger 1 (2 * bits)

benchMul :: Int -> Benchmark
benchMul bits = bench (show bits) $ nf (mulF2m m a) b
  where
    m = genInteger 0 bits
    a = genInteger 1 bits
    b = genInteger 2 bits

benchSquare :: Int -> Benchmark
benchSquare bits = bench (show bits) $ nf (squareF2m m) a
  where
    m = genInteger 0 bits
    a = genInteger 1 bits

benchInv :: Int -> Benchmark
benchInv bits = bench (show bits) $ nf (invF2m m) a
  where
    m = genInteger 0 bits
    a = genInteger 1 bits

bitsList :: [Int]
bitsList = [64, 128, 256, 512, 1024, 2048]

benchF2m =
    [ bgroup    "modF2m" $ map benchMod    bitsList
    , bgroup    "mulF2m" $ map benchMul    bitsList
    , bgroup "squareF2m" $ map benchSquare bitsList
    , bgroup    "invF2m" $ map benchInv    bitsList
    ]
