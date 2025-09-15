{-# LANGUAGE TypeFamilies #-}
module Simple17 where

foo :: Int -> Int
foo n = bar n
  where
    bar :: t ~ Int => Int -> t
    bar n = n

