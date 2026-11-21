{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, Modifiers, PolyKinds, TypeOperators #-}
module PreludeMappingProof (mapNothing, mapJust, mapTwice, example, presentExample) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import Prelude

-- | Use the installed Functor Maybe instance with an abstract callback.
mapNothing :: forall a b (f :: a %U -> b). Proxy f
           -> Prelude.fmap f ('Nothing :: Maybe a) :~: ('Nothing :: Maybe b)
mapNothing _ = Refl

-- | Preserve the callback application under the lazy constructor.
mapJust :: forall a b (f :: a %U -> b) (x :: a). Proxy f -> Proxy x
        -> Prelude.fmap f ('Just x) :~: 'Just (f x)
mapJust _ _ = Refl

-- | Feed the first method's symbolic result to the second method.
mapTwice :: forall a b c (f :: a %U -> b) (g :: b %U -> c) (x :: a).
            Proxy f -> Proxy g -> Proxy x
         -> Prelude.fmap g (Prelude.fmap f ('Just x)) :~: 'Just (g (f x))
mapTwice _ _ _ = Refl

type Negated :: Bool %U -> Bool
type Negated = Prelude.not

-- | Show the proof and evaluate the method with a divergent callback.
example :: String
example = show (mapNothing (Proxy :: Proxy Negated), run (error "forced callback") Nothing)

-- | Compare the proofs with the runtime results and demand on the field.
presentExample :: String
presentExample = show ( mapJust f x, mapTwice f f x
                      , run not (Just True), run not (run not (Just True))
                      , case run (error "forced callback") (Just (error "forced payload")) of
                            Just _ -> True
                            Nothing -> False )
  where
    f = Proxy :: Proxy Negated
    x = Proxy :: Proxy 'True

-- | Keep a call to the installed method in the runtime checks.
{-# NOINLINE run #-}
run :: (Bool -> Bool) -> Maybe Bool -> Maybe Bool
run = Prelude.fmap
