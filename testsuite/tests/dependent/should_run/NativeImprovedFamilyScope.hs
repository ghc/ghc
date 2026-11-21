{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, Modifiers #-}
{-# LANGUAGE NoExperimentalDependentTypes, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators #-}
module NativeImprovedFamilyScope where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (U)
import NativeConstrainedFamilyProvider

instance {-# OVERLAPPING #-} Allowed a => Output (Key [a]) [a] where
    type Project (Key [a]) = Bool
    type Explicit (Key [a]) [a] = Bool

data Witness c e where
    Witness :: Output c e => Witness c e

data Hold (f :: Type %U -> Type)

#if defined(ALIAS_GUARD)
type Item = Int
#else
type Item = Bool
#endif

-- The cached alias requires different owner arguments at its two uses.
aliased :: Proxy (Projected [Item], Projected Char)
aliased = Proxy

#if defined(MISSING_OWNER)
abstract :: forall c. Proxy c -> Proxy (Project c)
#elif defined(WRONG_EXPLICIT)
abstract :: Proxy c -> Proxy (Explicit (Key Bool) Char)
#elif defined(EXPLICIT_PREFIX)
abstract :: Proxy c -> Proxy (Hold (Explicit (Key Bool)))
#else
abstract :: forall c e. Output c e => Proxy c -> Proxy (Project c)
#endif
abstract _ = Proxy

inBranch :: forall c e. Witness c e -> ()
inBranch Witness = Proxy @(Project c) `seq` ()

transitive :: forall a b c. Chain a b c => Proxy a -> Proxy (Target a)
transitive _ = Proxy

ground :: (Target Bool :~: Int, FromFixed Bool :~: Char)
ground = (Refl, Refl)
