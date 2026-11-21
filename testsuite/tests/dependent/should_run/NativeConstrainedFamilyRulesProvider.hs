{-# LANGUAGE BangPatterns, DataKinds, ExperimentalDependentTypes, FlexibleInstances, GADTs, LinearTypes, MonoLocalBinds, ScopedTypeVariables, TypeApplications, TypeFamilies #-}
module NativeConstrainedFamilyRulesProvider (Collects(..), zero, make, consume) where
import FinitePosition
import ParameterizedIndexLibrary (Fin(..))
import PackedVecLibrary (Nat(..))
class Allowed a
instance Allowed Bool
class Collects c where
    type Elem c
instance Allowed a => Collects [a] where
    type Elem [a] = a
{-# NOINLINE inspect #-}
inspect :: forall n. foreach (i :: Fin n) -> FinView i
inspect i = viewFin i
{-# INLINE zero #-}
zero :: forall n. FinView ('FZ @n)
zero = inspect @('S n) FZ
type LinearPayload = exists a. (a, a %1 -> Int)

make :: Bool -> LinearPayload
make flag = (flag, readFlag)
  where
    readFlag :: Bool %1 -> Int
    readFlag True = 101
    readFlag False = 102

{-# NOINLINE consume #-}
consume :: Bool -> LinearPayload %1 -> Int
consume _ p = case p of (v, f) -> f v

{-# RULES "linear/unpack" forall (flag :: Bool).
    consume flag = (\p -> let !(v, f) = p in f v) #-}
