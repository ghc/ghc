{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, FlexibleContexts, GADTs #-}
{-# LANGUAGE Modifiers, PolyKinds, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing -Wno-simplifiable-class-constraints #-}
module DirectMappingControl where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import Prelude
#ifdef ALTERNATE
import DirectMappingProvider (Alternate(..))
#endif

#ifdef CHANGED_RULE
{-# RULES "map-bool-nothing" forall f. Prelude.fmap @Maybe f (Nothing @Bool) = Just (f True) #-}
#endif

#ifdef PRESENT
control :: forall a b (f :: a %U -> b) (x :: a). Proxy f -> Proxy x
        -> Prelude.fmap f ('Just x) :~: 'Just (f x)
control _ _ = Refl
#else
control :: forall a b (f :: a %U -> b) (x :: a).
#ifdef GIVEN
           Functor Maybe =>
#endif
           Proxy f -> Proxy x
#ifdef ALTERNATE
        -> alternate f ('Just x) :~: ('Nothing :: Maybe b)
#else
        -> Prelude.fmap f ('Nothing :: Maybe a) :~: ('Nothing :: Maybe b)
#endif
control _ _ = Refl
#endif
