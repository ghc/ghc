{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

-- Test the handling of conditionals in rank-n stuff
-- Should fail, regardless of branch ordering

module ShouldFail where

-- These two are ok
f1 = (\ (x :: forall a. a->a) -> x) 
f2 = (\ (x :: forall a. a->a) -> x) id 'c'

-- These fail
f3 v = (if v then
	  (\ (x :: forall a. a->a) -> x) 
	else
	  (\ x -> x) 
      ) id 'c'

f4 v = (if v then
	  (\ x -> x) 
	else
	  (\ (x :: forall a. a->a) -> x) 
       ) id 'c'
