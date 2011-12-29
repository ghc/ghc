{-# OPTIONS_GHC -fcontext-stack=1000 #-} 
{-# LANGUAGE 
     FlexibleContexts, FlexibleInstances, FunctionalDependencies, 
     MultiParamTypeClasses, OverlappingInstances, TypeSynonymInstances, 
     TypeOperators, UndecidableInstances, TypeFamilies #-} 
module T5321Fun where

-- As the below code demonstrates, the same issues demonstrated with
-- Functional Dependencies also appear with Type Families, although less
--horribly, as their code-path seems more optimized in the current
-- constraint solver:

-- Our running example, for simplicity's sake, is a type-level map of a
-- single function. For reference, here is the code for a simple
-- value-level map of a single function.

-- > vfoo = id
-- > mapfoo (x : xs) = vfoo x : mapfoo xs
-- > mapfoo [] = []

-- Because Haskell is a lazy language, this runs in O(n) time and constant stack.

-- We now lift map to the type level, to operate over HLists.

-- First, the basic HList types

infixr 3 :*
data x :* xs = x :* xs deriving Show
data HNil = HNil deriving Show

-- Next, a large boring HList

-- Adds ten cells
addData x = i :* i :* d :* d :* s :* 
            i :* i :* d :* d :* s :* 
            x 
    where i = 1 :: Int 
          d = 1 :: Double 
          s = "" 

-- Has 70 cells. 
sampleData = addData $ addData $ addData $ addData $ addData $ 
             addData $ addData $ 
             HNil


class TFoo x where 
     type TFooFun x 
     tfoo :: x -> TFooFun x 
     tfoo = undefined

instance TFoo Int where 
    type TFooFun Int = Double
instance TFoo Double where 
    type TFooFun Double = Int
instance TFoo String where 
    type TFooFun String = String

class THMapFoo1 as where 
      type THMapFoo1Res as 
      thMapFoo1 :: as -> THMapFoo1Res as

instance (TFoo a, THMapFoo1 as) => THMapFoo1 (a :* as) where 
    type THMapFoo1Res (a :* as) = TFooFun a :* THMapFoo1Res as 
    thMapFoo1 (x :* xs) = tfoo x :* thMapFoo1 xs

instance THMapFoo1 HNil where 
    type THMapFoo1Res HNil = HNil 
    thMapFoo1 _ = HNil

-- The following, when enabled, takes ~3.5s. This demonstrates that slowdown occurs with type families as well.

testTHMapFoo1 = thMapFoo1 sampleData 

class THMapFoo2 acc as where 
      type THMapFoo2Res acc as 
      thMapFoo2 :: acc -> as -> THMapFoo2Res acc as

instance (TFoo a, THMapFoo2 (TFooFun a :* acc) as) => THMapFoo2 acc (a :* as) where 
    type THMapFoo2Res acc (a :* as) = THMapFoo2Res (TFooFun a :* acc) as 
    thMapFoo2 acc (x :* xs) = thMapFoo2 (tfoo x :* acc) xs

instance THMapFoo2 acc HNil where 
    type THMapFoo2Res acc HNil = acc 
    thMapFoo2 acc _ = acc

-- The following, when enabled, takes ~0.6s. This demonstrates that the
-- tail recursive transform fixes the slowdown with type families just as
-- with fundeps.

testTHMapFoo2 = thMapFoo2 HNil sampleData 

class THMapFoo3 acc as where 
      type THMapFoo3Res acc as 
      thMapFoo3 :: acc -> as -> THMapFoo3Res acc as

instance (THMapFoo3 (TFooFun a :* acc) as, TFoo a) => THMapFoo3 acc (a :* as) where 
    type THMapFoo3Res acc (a :* as) = THMapFoo3Res (TFooFun a :* acc) as 
    thMapFoo3 acc (x :* xs) = thMapFoo3 (tfoo x :* acc) xs

instance THMapFoo3 acc HNil where 
    type THMapFoo3Res acc HNil = acc 
    thMapFoo3 acc _ = acc

-- The following, when enabled, also takes ~0.6s. This demonstrates that,
-- unlike the fundep case, the order of type class constraints does not,
-- in this instance, affect the performance of type families.

testTHMapFoo3 = thMapFoo3 HNil sampleData 
