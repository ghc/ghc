{-# OPTIONS_GHC -O2 #-}

module Test where

import GHC.Num.Integer

neg_neg :: Integer -> Integer
neg_neg x = integerNegate (integerNegate x)
