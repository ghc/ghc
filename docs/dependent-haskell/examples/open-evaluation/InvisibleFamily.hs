{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures, TypeAbstractions, TypeApplications, TypeFamilies #-}
module InvisibleFamily (Test, Observed, Inferred) where

import Data.Kind (Type)
import GHC.Exts (U)

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
