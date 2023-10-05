{-# LANGUAGE ViewPatterns, GADTs #-}

module ExPat where

data T where
  MkT :: Integral a => a -> Int -> T

-- c.f. T11700

-- Succeeds because y::Int
f x = let MkT _ y  = x
      in y

-- Remarkablly, this succeeds because
--   (toInteger (v::a)) is an Integer
g x = let MkT (toInteger -> y) _ = x
      in y
