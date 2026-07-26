{-# LANGUAGE TemplateHaskell, RebindableSyntax #-}

import Prelude
import T18102b_aux

x1 :: Int
x1 = $$(intQuote_TTH)

z1 :: Int
z1 = $(intQuote_TH)

x2 :: Char
x2 = $$(charQuote_TTH)

z2 :: Char
z2 = $(charQuote_TH)

x3 :: [Int]
x3 = $$(seqQuote_TTH)

z3 :: [Int]
z3 = $(seqQuote_TH)

x4 :: [Int]
x4 = $$(listQuote_TTH)

z4 :: [Int]
z4 = $(listQuote_TH)


main :: IO ()
main = do
  print t1
  print x1
  print z1

  print t2
  print x2
  print z2

  print t3
  print x3
  print z3

  print t4
  print x4
  print z4
