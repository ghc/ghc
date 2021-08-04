{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Numbers (numbers) where

import Criterion.Main (bench, bgroup, nf)
import Criterion.Types (Benchmark)
import Data.Scientific (Scientific(..))
import Text.Parsec.Text ()
import Text.Parsec.Text.Lazy ()
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.Attoparsec.Text as AT
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T

strN, strNePos, strNeNeg :: String
strN     = "1234.56789"
strNePos = "1234.56789e3"
strNeNeg = "1234.56789e-3"

numbers :: Benchmark
numbers = bgroup "numbers" [
  let !tN     = T.pack strN
      !tNePos = T.pack strNePos
      !tNeNeg = T.pack strNeNeg
  in bgroup "Text"
  [
    bgroup "no power"
    [ bench "double" $ nf (AT.parseOnly AT.double) tN
    , bench "number" $ nf (AT.parseOnly AT.number) tN
    , bench "rational" $
      nf (AT.parseOnly (AT.rational :: AT.Parser Rational)) tN
    , bench "scientific" $
      nf (AT.parseOnly (AT.rational :: AT.Parser Scientific)) tN
    ]
  , bgroup "positive power"
    [ bench "double" $ nf (AT.parseOnly AT.double) tNePos
    , bench "number" $ nf (AT.parseOnly AT.number) tNePos
    , bench "rational" $
      nf (AT.parseOnly (AT.rational :: AT.Parser Rational)) tNePos
    , bench "scientific" $
      nf (AT.parseOnly (AT.rational :: AT.Parser Scientific)) tNePos
    ]
  , bgroup "negative power"
    [ bench "double" $ nf (AT.parseOnly AT.double) tNeNeg
    , bench "number" $ nf (AT.parseOnly AT.number) tNeNeg
    , bench "rational" $
      nf (AT.parseOnly (AT.rational :: AT.Parser Rational))  tNeNeg
    , bench "scientific" $
      nf (AT.parseOnly (AT.rational :: AT.Parser Scientific)) tNeNeg
    ]
  ]
  , let !bN     = BC.pack strN
        !bNePos = BC.pack strNePos
        !bNeNeg = BC.pack strNeNeg
  in bgroup "ByteString"
  [ bgroup "no power"
    [ bench "double" $ nf (AC.parseOnly AC.double) bN
    , bench "number" $ nf (AC.parseOnly AC.number) bN
    , bench "rational" $
      nf (AC.parseOnly (AC.rational :: AC.Parser Rational))   bN
    , bench "scientific" $
      nf (AC.parseOnly (AC.rational :: AC.Parser Scientific)) bN
    ]
  , bgroup "positive power"
    [ bench "double" $ nf (AC.parseOnly AC.double) bNePos
    , bench "number" $ nf (AC.parseOnly AC.number) bNePos
    , bench "rational" $
      nf (AC.parseOnly (AC.rational :: AC.Parser Rational)) bNePos
    , bench "scientific" $
      nf (AC.parseOnly (AC.rational :: AC.Parser Scientific)) bNePos
    ]
  , bgroup "negative power"
    [ bench "double" $ nf (AC.parseOnly AC.double) bNeNeg
    , bench "number" $ nf (AC.parseOnly AC.number) bNeNeg
    , bench "rational" $
      nf (AC.parseOnly (AC.rational :: AC.Parser Rational)) bNeNeg
    , bench "scientific" $
      nf (AC.parseOnly (AC.rational :: AC.Parser Scientific)) bNeNeg
    ]
  ]
 ]
