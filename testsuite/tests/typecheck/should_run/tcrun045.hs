{-# LANGUAGE ImplicitParams, FlexibleContexts #-}

-- !!! Implicit parameter superclass constraint
-- These examples actually crashed GHC 4.08.2

module Main where

class C t where
    methodC :: t -> t

instance (?imp :: Int) => C Int where
    methodC x = ?imp + x

-- Check implicit parameter constraints in instance heads
test1 :: Int
test1 = methodC 10
  where ?imp = 2

test2 :: (?imp :: Int) => Int
test2 = methodC 20
  where ?imp = 2


class (?imp :: Int) => D t where
    methodD :: t -> t

instance (?imp :: Int) => D Int where
    methodD x = x + ?imp

-- Check implicit parameter constraints in *superclass*, not just in instances
test3 :: Int
test3 = methodD 10
  where ?imp = 2

test4 :: (?imp :: Int) => Int
test4 = methodD 20
  where ?imp = 2

test5 :: D Int => Int -- Requires FlexibleContexts
test5 = methodD ?imp

main = do
    print test1
    print $ let ?imp = 3 in test2
    print test3
    print $ let ?imp = 3 in test4
    print $ let ?imp = 3 in test5
