module Test where

-- !!! caused compiler to generate bogus HC code, fixed in
-- basicTypes/Literal.lhs rev. 1.36.

f :: Double -> Int
f x = round (x - (-5.0))
