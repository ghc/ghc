{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers, PolyKinds, RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures, TypeAbstractions, TypeApplications, TypeFamilies #-}
module InvisibleFamily (Test, Observed, Inferred, PickVisible, ObservedVisible, IndexedVisible, wrappedValues) where

import Data.Kind (Type)
import GHC.Exts (M, U)

-- | A family indexed by the kind of its result.
type Select :: forall k. @U k
type family Select @k
type instance Select @Bool = 'True

-- | Store a family that still accepts an invisible argument.
type Carrier :: (forall k. @U k) -> Type
data Carrier f
-- | Pass Select before supplying its kind argument.
type Test = Carrier Select

-- | Apply the family argument at Bool.
type Run :: (forall k. @U k) -> Bool
type family Run f where
  forall (f :: forall k. @U k). Run f = f @Bool

-- | The consumer can reduce this alias to True.
type Observed = Run Select

-- | The monomorphic result kind supplies the missing argument.
type Pick :: Bool
type Pick = Select

-- | Expose how GHC saturates Select when it infers the result kind.
type Inferred = Select

-- | Supply a visible argument and leave the kind argument open.
type PickVisible :: Bool -> forall (k :: Type). @U Type
type family PickVisible flag @k
type instance PickVisible 'True @Bool = Int
type instance PickVisible 'False @Bool = Int

-- | Infer the result kind from the polymorphic argument.
type RunVisible :: forall r. (forall (k :: Type). @U r) -> r
type family RunVisible @r f where
  forall r (f :: forall (k :: Type). @U r). RunVisible @r f = f @Bool

type ObservedVisible = RunVisible (PickVisible 'True)
type IndexedVisible flag = RunVisible (PickVisible flag)

-- | Select the kind of the type function returned by Wrap.
type family Arrow (list :: Bool) where
  Arrow False = Type %M -> Type
  Arrow True  = Type %U -> Type

-- | Return either an ordinary constructor or a type family.
type family Wrap (list :: Bool) :: Arrow list
type instance Wrap False = Maybe
type instance Wrap True  = AsList

-- | Wrap the element type in a list.
type family AsList a where
  AsList a = [a]

-- | Apply both functions through their family-defined arrow kinds.
wrappedValues :: (Wrap False Int, Wrap True Int)
wrappedValues = (Just 7, [7])
