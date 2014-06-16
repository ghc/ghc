{-# LANGUAGE DataKinds, FunctionalDependencies, FlexibleInstances,
             UndecidableInstances, PolyKinds, KindSignatures,
             ConstraintKinds, FlexibleContexts #-}

module T6020 where

class Id (a :: k) (b :: k) | a -> b
instance Id a a

f :: Id x y => x -> y
f = f

--class Test (x :: a) (y :: a) | x -> y
--instance (Id x y, Id y z) => Test x z

-- (Id x0 y0, Id y0 z0, x~x0, z~z0)
-- (Id x y0, Id y0 z, y0~z, y0~y)

--test :: Test True True => ()
--test = ()

