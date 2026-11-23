{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}

module NativeSizeListNoDescent where

import Data.Type.Equality ((:~:)(Refl))
import PreludeMapLengthSize (Size)
import NativeSizeListNoDescentSource (withoutDescent)

-- | An open call must be rejected before the equality witness is checked.
unchecked :: forall a. foreach (bound :: Size) -> foreach (xs :: [a])
          -> withoutDescent bound xs :~: withoutDescent bound xs
unchecked _ _ = Refl
