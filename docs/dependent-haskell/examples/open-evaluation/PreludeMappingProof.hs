{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, Modifiers, PolyKinds, TypeOperators #-}
module PreludeMappingProof (mapNothing, example) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import Prelude

-- | Use the installed Functor Maybe instance with an abstract callback.
mapNothing :: forall a b (f :: a %U -> b). Proxy f
           -> Prelude.fmap f ('Nothing :: Maybe a) :~: ('Nothing :: Maybe b)
mapNothing _ = Refl

type Negated :: Bool %U -> Bool
type Negated = Prelude.not

-- | Show the proof and evaluate the method with a divergent callback.
example :: String
example = show (mapNothing (Proxy :: Proxy Negated), run (error "forced callback") Nothing)
  where
    {-# NOINLINE run #-}
    run :: (Bool -> Bool) -> Maybe Bool -> Maybe Bool
    run = Prelude.fmap
