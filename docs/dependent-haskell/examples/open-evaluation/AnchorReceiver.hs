{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, Modifiers, PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures, TypeAbstractions, TypeApplications, TypeFamilies #-}
module AnchorReceiver where

import Data.Kind (Type)
import GHC.Matchability (M, U, Matchability)

type Select :: forall (m :: Matchability). Bool -> (Type %m -> Type) -> Type
type family Select @m flag f where
    Select @m 'True f = f Bool

data Anchor (m :: Matchability) where
    Match :: Anchor M
    Unmatch :: Anchor U

type Identity :: Type -> Type
type family Identity a where
    Identity a = a

type Receiver :: forall (m :: Matchability). ((Type %m -> Type) %U -> Type) -> Anchor m -> Type
type family Receiver @m f anchor where
    Receiver @M f 'Match = f Maybe
    Receiver @U f 'Unmatch = f Identity

-- | Select the container separately from the element type.
type family WrapOpen (list :: Bool) (element :: Type) :: Type
type instance WrapOpen 'True element = [element]
