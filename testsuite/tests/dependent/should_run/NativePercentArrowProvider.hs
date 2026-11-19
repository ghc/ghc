{-# LANGUAGE DataKinds, GADTs, LinearTypes, Modifiers, NoExperimentalDependentTypes #-}
{-# LANGUAGE PolyKinds, StandaloneKindSignatures, TypeApplications, TypeFamilies, TypeOperators, UnicodeSyntax #-}
module NativePercentArrowProvider where
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (FUN, Matchability, M, U, Multiplicity(Many, One))

type Callback :: (Type %U -> Type) -> Type
data Callback f where
  Callback :: f Bool %M -> Callback f

type LeftOrder a b = a %U %One -> b
type RightOrder a b = a %One %U → b
type Unicode a b = a %U ⊸ b
type Variable (m :: Matchability) (p :: Multiplicity) a b = a %m %p -> b

both :: LeftOrder Int Bool :~: RightOrder Int Bool
both = Refl

linear :: Unicode Int Bool :~: FUN @U 'One Int Bool
linear = Refl

scoped :: Variable M 'Many Int Bool :~: FUN @M 'Many Int Bool
scoped = Refl
