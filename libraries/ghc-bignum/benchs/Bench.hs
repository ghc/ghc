{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

import Criterion.Main

import Data.Bits
import qualified GHC.Num.Integer as I

#include "MachDeps.h"

main :: IO ()
main = defaultMain benchmarks

benchmarks :: [Benchmark]
benchmarks =
   let
      lblNew   = "New Integer"
      lblStock = "Stock Integer"
      bn = bench lblNew
      bs = bench lblStock

      integerFromWordList True ws  = negate (integerFromWordList False ws)
      integerFromWordList False ws = go 0 ws
         where
            go c []     = c
            go c (x:xs) = go ((c `shiftL` WORD_SIZE_IN_BITS) + fromIntegral x) xs

      rws = cycle
            [ 0x127894564, 0x547ABCD, 0x178454, 0x488888, 0xABBAB, 0x77878, 0x78894560, 0x78475254
            , 0x127894564, 0x547A897, 0x178454, 0x488FFF, 0xABA878, 0x787978, 0x78894560, 0x78475254
            , 0x145674564, 0x5568BCD, 0x1ABD54, 0x48DEF8, 0xABAB54AB, 0x787999, 0x78AAAAA0, 0xCCCCCC54
            , 0x127898794, 0x547ABCD, 0x17D4D4, 0x488FE8, 0xA125ABAB, 0x787878, 0x7BCBCBC0, 0x78475254
            ]

      sizeA, sizeB :: Int
      sizeA = 8
      sizeB = 1

      a, b :: [Word]
      a = take sizeA rws
      b = take sizeB (drop 7 rws)

      na,nb :: I.Integer
      na = I.integerFromWordList False a
      nb = I.integerFromWordList False b

      sa,sb :: Integer
      sa = integerFromWordList False a
      sb = integerFromWordList False b

      groupBinOp op = bgroup $ mconcat
                     [ show sizeA, "-word "
                     , op
                     , " ", show sizeB, "-word"
                     ]

   in
   [ groupBinOp "+"
      [ bn $ whnf (uncurry I.integerAdd) $! (na,nb)
      , bs $ whnf (uncurry (+))          $! (sa,sb)
      ]
   , groupBinOp "-"
      [ bn $ whnf (uncurry I.integerSub) $! (na,nb)
      , bs $ whnf (uncurry (-))          $! (sa,sb)
      ]
   , groupBinOp "*"
      [ bn $ whnf (uncurry I.integerMul) $! (na,nb)
      , bs $ whnf (uncurry (*))          $! (sa,sb)
      ]
   , groupBinOp "`quotRem`"
      [ bn $ whnf (uncurry I.integerQuotRem) $! (na,nb)
      , bs $ whnf (uncurry quotRem)          $! (sa,sb)
      ]
   , groupBinOp "`gcd`"
      [ bn $ whnf (uncurry I.integerGcd)     $! (na,nb)
      , bs $ whnf (uncurry gcd)              $! (sa,sb)
      ]
   ]
