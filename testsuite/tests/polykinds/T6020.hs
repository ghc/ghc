{-# LANGUAGE DataKinds, FunctionalDependencies, FlexibleInstances,
             UndecidableInstances, PolyKinds, KindSignatures,
             ConstraintKinds, FlexibleContexts #-}

module T6020 where

class Id (a :: k) (b :: k) | a -> b
instance Id a a

class Test (x :: a) (y :: a) | x -> y
instance (Id x y, Id y z) => Test x z

test :: Test True True => ()
test = ()

