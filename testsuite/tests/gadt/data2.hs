{-# LANGUAGE GADTs, ExistentialQuantification #-}

-- Trac #289

module ShouldCompile where

class Foo a where 
  foo :: a -> Int 

data T = forall a. T (G a) 
data G a where
  A :: G a
  B :: Foo a => a -> G a

doFoo :: T -> Int 
doFoo (T A) = 2 
doFoo (T (B x)) = foo x 


