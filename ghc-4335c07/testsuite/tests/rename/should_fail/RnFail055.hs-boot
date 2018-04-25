{-# LANGUAGE MultiParamTypeClasses,RankNTypes,ExistentialQuantification,DatatypeContexts #-}
module RnFail055 where

f1 :: Float -> Int

type S1 a b c = (a,b)

type S2 a b = forall b. (a,b)

type S3 t = [t]

data T1 a b = T1 [a] [b]

data (Eq a) => T2 a b = T2 a

data T3 = T3
data T3' = T3'

data T4 b = T4 (forall a. b -> a)

data T5 a = T5 a

data T6 = T6 !Int

data T7 a = forall b . T7 a

class C1 a b where { m1 :: a -> b }
class C2 a b where { m2 :: a -> b }
class (Ord a, Eq a) => C3 a where { }
