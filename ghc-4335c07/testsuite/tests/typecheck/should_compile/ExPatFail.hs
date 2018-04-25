{-# LANGUAGE GADTs #-}

module ExPatFail where

data T where
  MkT :: Integral a => a -> Int -> T

-- Fails because y is bound to an existential type
-- Mind you, the error message is pretty terrible
-- c.f. T11700

f x = let MkT y _ = x
      in y
