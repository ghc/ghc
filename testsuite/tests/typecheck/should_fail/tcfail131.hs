-- Error message in monomorphic case

module ShouldFail where

  f = (*)	-- Monomorphic
  g :: Num b => b -> b
  g x = f x x
