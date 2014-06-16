{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module T3460 where

class Nat n where
    toInt :: n -> Int

class (Nat (Arity f)) => Model f where
    type Arity f

ok :: Model f => f -> Arity f -> Int
ok _ n = toInt n

bug :: (Model f, Arity f ~ n) => f -> n -> Int
bug _ n = toInt n
