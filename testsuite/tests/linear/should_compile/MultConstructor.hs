{-# LANGUAGE GADTs, DataKinds, LinearTypes, KindSignatures, ExplicitForAll, TypeApplications #-}
module MultConstructor where

import GHC.Types

data T (p :: Multiplicity) a where
  MkT :: a %p -> T p a

data Existential a where  -- #18790
  MkE :: a %(p :: Multiplicity) -> Existential a

f1 :: forall (a :: Type). T 'Many a %1 -> (a,a)
f1 (MkT x) = (x,x)

f2 :: forall (a :: Type) m. T 'Many a %1 -> T m a
f2 (MkT x) = MkT x

f3 :: forall (a :: Type). a %1 -> T 'One a
f3 = MkT

g1 :: forall (a :: Type). a %1 -> Existential a
g1 x = MkE x

g2 :: forall (a :: Type). Existential a -> a
g2 (MkE x) = x

vta :: Int %1 -> Existential Int
vta x = MkE @Int @'One x

h :: a %(m :: Multiplicity) -> b
h = h

vta2 :: Int %1 -> Bool  -- see #23764
vta2 = h @Int @One @Bool
