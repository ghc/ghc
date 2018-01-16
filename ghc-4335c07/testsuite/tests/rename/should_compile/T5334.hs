{-# LANGUAGE RecordWildCards #-}

module T5334 where

data T = T { a, b :: Int }

t = T {..}
  where
     a = 1
     -- b = 2

data S = S { x, y :: Int }

s = S { x = 1 }
