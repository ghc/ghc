{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module M where

class (a ~ b) => Aggregate a b where
instance Aggregate a a where

liftM :: (Aggregate ae am) => (forall r. am -> r) -> ae
liftM _ = undefined

class Positive a

mytake :: (Positive n) => n -> r
mytake = undefined

x :: (Positive n) => n
x = liftM mytake
