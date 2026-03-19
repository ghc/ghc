module Main where

main = do

  let
    -- A floating point value that squares to infinity (NB: DBL_MAX ≈ 1.8e308)
    big :: Double
    big = 1e200

  -- Should compute NaN, even with constant folding.
  print $ big * big - big * big

  -- Should compute Infinity, and not fromRational (toRational (1/0) / 2)).
  print $ (big * big) / 2
