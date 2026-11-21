{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers, PolyKinds, RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures, TypeAbstractions, TypeApplications, TypeFamilies #-}
module InvisibleFamily
  (Test, Observed, Inferred, PickVisible, ObservedVisible, IndexedVisible,
   ReadAt, ReadFlag, ReadType, Required, RequiredPair, requiredArguments, wrappedValues) where

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

-- | Choose a result using the result kind and a later input type.
type ReadAt :: Bool -> forall (result :: Type). @U forall (input :: Type). @U result
type family ReadAt flag @result @input
type instance ReadAt 'True @Bool @Bool = 'True
type instance ReadAt 'True @Type @Bool = Int

-- | Let the receiver determine the result kind and supply the input later.
type ReadFlag = RunVisible @Bool (ReadAt 'True)
type ReadType = RunVisible @Type (ReadAt 'True)

-- | Take the argument kind as the first required parameter.
type Required :: forall k -> @U k %U -> Type
type family Required k a where
  Required Bool 'True = Int
  Required Type Bool = Ordering

-- | Use the same type family with arguments at two different kinds.
type ApplyRequired :: (forall k -> @U k %U -> Type) -> Type
type family ApplyRequired f where
  forall (f :: forall k -> @U k %U -> Type).
    ApplyRequired f = (f Bool 'True, f Type Bool)

type RequiredPair = ApplyRequired Required

-- | Return one result for each use of the required kind argument.
requiredArguments :: RequiredPair
requiredArguments = (3, LT)

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
