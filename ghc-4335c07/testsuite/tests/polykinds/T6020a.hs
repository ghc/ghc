{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE DataKinds, FunctionalDependencies, FlexibleInstances,
              UndecidableInstances, PolyKinds, KindSignatures,
              ConstraintKinds, FlexibleContexts, GADTs #-}

module T6020a where

class Id (a :: k) (b :: k) | b -> a
instance a ~ b => Id a b

class Test (x :: a) (y :: a)
instance (Id x y, Id y z) => Test x z
-- Weird test case: (Id x y) and (Id y z) are both simplifiable

test :: Test True True => ()
-- Weird test case: (Test True True) is simplifiable
test = ()


