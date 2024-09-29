{-# LANGUAGE GADTs #-}

module T25266b where

data T a where { T1 :: T Int; T2 :: a -> T a }

h :: Int -> (Int,Int)
-- Binding for `f` is accepted; we do not generalise it
--     f :: forall a. alpha -> beta -> T a -> Int
-- We figure out alpha/beta from the call sites
h p = let f x y t = (case t of
                      T1   -> length [x,y]
                      T2 _ -> 2)  :: Int
      in (f p (4::Int) (T2 'c'), f 4 5 (T2 "oooh"))

