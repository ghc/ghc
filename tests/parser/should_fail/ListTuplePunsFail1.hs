{-# language UnboxedTuples #-}

module ListTuplePunsFail1 where

type T = '(Int, Double)

type U = '()

type L = '[Int, Double]

type L0 = '[]

type L1 = '[Int]

type Uu = '(##)

type Bc = '(,,,)

type Uud = (##)

type S2du = (# | #)

type S2da = (# Int | Int #)
