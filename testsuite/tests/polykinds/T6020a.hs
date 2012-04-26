{-# LANGUAGE DataKinds, FunctionalDependencies, FlexibleInstances,
              UndecidableInstances, PolyKinds, KindSignatures,
              ConstraintKinds, FlexibleContexts, GADTs #-}

module T6020a where

class Id (a :: k) (b :: k) | b -> a
instance a ~ b => Id a b

class Test (x :: a) (y :: a)
instance (Id x y, Id y z) => Test x z

test :: Test True True => ()
test = ()


