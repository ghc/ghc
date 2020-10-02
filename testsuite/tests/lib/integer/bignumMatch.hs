{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -ddump-rule-firings -O0 -v0 #-}

module Test where

import GHC.Num.Integer

foo :: Integer
foo = IS 45# `integerAdd` (IS 0# `integerMul` IS 18#)
