{-# LANGUAGE DatatypeContexts #-}
module ShouldFail where

import Data.Ratio

data Integral a => P a = P { p :: a }

f :: Integral a => P (Ratio a) -> P (Ratio a)
f x = x { p = p x }
