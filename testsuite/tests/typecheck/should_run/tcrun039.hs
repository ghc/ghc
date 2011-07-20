{-# LANGUAGE GADTs, ExplicitForAll #-}

-- Test for GADTs and implication constraints

module Main where

data T a where
  MkT :: Num a => a -> T a

f :: Read a => T a -> String -> a
f (MkT n) s = n + read s

----------------
data GADT a where
  MkG :: Num a => a -> GADT [a]

g :: forall b. Read b => GADT b -> String -> b
g (MkG n) s = -- Here we know Read [b]
	      n : (read s)

main = do print (f (MkT (3::Int)) "4")
          print (g (MkG (3::Int)) "[4,5]")
