{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TypeFamilies, TypeOperators #-}
module LocalDerivedAssociatedClient (reading) where

import Data.Type.Equality ((:~:) (Refl))
import LocalDerivedAssociated (Flipped, Sample(..), Signal(..))

-- | Use the checked equation and the derived instance through the interface.
reading :: (Flipped 'Stop :~: 'Go, Reading (), String)
reading = (Refl, sample (), show (sample ()))
