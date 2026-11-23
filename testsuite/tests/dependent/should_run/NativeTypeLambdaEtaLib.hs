{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers #-}
{-# LANGUAGE RoleAnnotations, StandaloneKindSignatures, TypeFamilies, TypeOperators #-}
module NativeTypeLambdaEtaLib where

import Data.Kind (Type)
import GHC.Matchability (U)

type Stamp :: Type %U -> Type %U -> Type
type family Stamp tag cell
type instance Stamp Int Bool = Either Int Bool

type Discard :: Type %U -> Type %U -> Type
type family Discard tag cell where
  Discard tag _ = tag

type Map :: (Type %U -> Type) %U -> [Type] %U -> [Type]
type family Map f xs where
  Map _ '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

type role Batch nominal
data Batch (xs :: [Type]) = Batch Int
