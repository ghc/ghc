{-# OPTIONS -fglasgow-exts #-}

-- Test the handling of conditionals in rank-n stuff
-- Should fail, regardless of branch ordering

module ShouldFail where

f v = (if v then
	  (\ (x :: forall a. a->a) -> x) 
	else
	  (\ x -> x) 
      ) id 'c'

g v = (if v then
	  (\ (x :: forall a. a->a) -> x) 
	else
	  (\ x -> x) 
      ) id 'c'
