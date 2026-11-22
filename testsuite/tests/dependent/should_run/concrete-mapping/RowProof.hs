{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeOperators #-}
module RowProof where
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import Row

-- | The integer-label instance maps a flagged value.
mapped :: forall (a :: Type). foreach (p :: a -> Bool)
       -> foreach (flag :: Bool) -> foreach (x :: a)
       -> fmap p ('Flagged flag x :: Row Int Bool a) :~: 'Flagged flag (p x)
mapped _ _ _ = Refl

-- | The Boolean-label instance discards a flagged value.
discarded :: forall (a :: Type). foreach (p :: a -> Bool)
          -> foreach (flag :: Bool) -> foreach (x :: a)
          -> fmap p ('Flagged flag x :: Row Bool Bool a) :~: 'Empty
discarded _ _ _ = Refl

-- | Mapping the element preserves both fixed fields.
both :: forall (a :: Type). foreach (p :: a -> Bool)
     -> foreach (n :: Int) -> foreach (flag :: Bool) -> foreach (x :: a)
     -> fmap p ('Both n flag x) :~: 'Both n flag (p x)
both _ _ _ _ = Refl
