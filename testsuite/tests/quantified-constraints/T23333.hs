{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE GADTs, DataKinds #-}
module T23333 where

import Data.Kind
import Data.Coerce

foo1 :: (forall y. Bool ~ y) => z -> Bool
foo1 x = not x

foo2 :: (forall y. y ~ Bool) => z -> Bool
foo2 x = not x

-- Testcases from #16432
t1 :: forall f b. (forall a. Coercible (f a) a) => b -> f b
t1 = coerce

data U :: () -> Type where
 MkU :: Int -> U '()

t2 :: forall n res. (('()~n) => (Int~res)) => U n -> res
t2 (MkU n) = n

t3 :: ((Bool~Bool) => (Char~res)) => res
t3 = 'a'
