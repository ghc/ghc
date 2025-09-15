{-# LANGUAGE TypeFamilies, GADTs, UndecidableInstances #-}

module T8550 where

type family F a
type instance F () = F ()
data A where
  A :: F () ~ () => A
x :: A
x = A

main :: IO ()
main = seq A (return ())

-- Note: This worked in GHC 7.8, but I (Richard E) think this regression
-- is acceptable.
