{-# LANGUAGE TypeFamilies, EmptyDataDecls, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, OverlappingInstances, UndecidableInstances #-}

module NonLinearLHS where

type family E a b
type instance E a a = [a]

foo :: E [Int] (E Int Int) -> Int
foo = sum . concat

data family F a b
data instance F a a = MkF [a] 

goo :: F Int Int -> F Bool Bool
goo (MkF xs) = MkF $ map odd xs


-- HList-like type equality

data True; data False;

type family EqTy a b
type instance EqTy a a = True

class EqTyP a b result
instance (EqTy a b ~ isEq, Proxy isEq result) => EqTyP a b result

class Proxy inp out
instance (result ~ True) => Proxy True result
instance (result ~ False) => Proxy notTrue result

testTrue :: EqTyP Int Int r => r
testTrue = undefined

testFalse :: EqTyP Int Bool r => r
testFalse = undefined