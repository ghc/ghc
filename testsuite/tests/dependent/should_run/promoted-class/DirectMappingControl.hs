{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, FlexibleContexts, GADTs #-}
{-# LANGUAGE Modifiers, PolyKinds, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing -Wno-simplifiable-class-constraints #-}
module DirectMappingControl where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import Prelude
#if defined(ALTERNATE) || defined(STRICT_FIELD)
import DirectMappingProvider (Alternate(..))
#endif

#ifdef CHANGED_RULE
{-# RULES "map-bool-nothing" forall f. Prelude.fmap @Maybe f (Nothing @Bool) = Just (f True) #-}
#endif

#if defined(OTHER_HEAD) || defined(OTHER_INPUT)
control :: forall a b (f :: a %U -> b) (g :: a %U -> b) (x :: a) (y :: a).
           Proxy f -> Proxy g -> Proxy x -> Proxy y -> Prelude.fmap f ('Just x) :~:
#ifdef OTHER_HEAD
           'Just (g x)
#else
           'Just (f y)
#endif
control _ _ _ _ = Refl
#elif defined(CANCEL)
control :: forall a b (f :: a %U -> b) (x :: a) (y :: a). Proxy f
        -> Prelude.fmap f ('Just x) :~: Prelude.fmap f ('Just y)
        -> x :~: y
control _ Refl = Refl
#elif defined(STRICT_FIELD)
control :: forall a b (f :: a %U -> b) (x :: a). Proxy f -> Proxy x
        -> strictMap f ('Just x) :~: 'Just (f x)
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
