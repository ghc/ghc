{-# LANGUAGE FlexibleInstances, UndecidableInstances,
             MultiParamTypeClasses, FunctionalDependencies #-}
module T4401 where

class Mul x y z | x y -> z
class IsType a
class IsType a => IsSized a s | a -> s

data Array n a = Array
instance IsSized a s => IsType (Array n a)
instance (IsSized a s, Mul n s ns) => IsSized (Array n a) ns
