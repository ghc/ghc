{-# LANGUAGE CPP, ConstraintKinds, FlexibleContexts, MultiParamTypeClasses, QuantifiedConstraints, TypeApplications #-}
{-# LANGUAGE GADTs, NoExperimentalDependentTypes, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module NativeConstrainedFamilyClient where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import NativeConstrainedFamilyProvider (Allowed, Collects(..), Display, Defaulted(..), Pair(..))

-- The superclass of Self cannot justify a family in Self's own arguments.
class Collects c => Self c x

#ifdef CIRCULAR_QUALIFIER
withSelf :: Self c (Elem c) => Proxy c -> Bool
#else
withSelf :: (Self c (Elem c), Collects c) => Proxy c -> Bool
#endif
withSelf _ = True

#ifdef LATE_QUALIFIER
acrossForall :: forall (c :: Type). Show (Elem c) =>
    forall (x :: Type). Collects c => Proxy x -> Proxy c -> Bool
#else
acrossForall :: forall (c :: Type). Collects c =>
    forall (x :: Type). Show (Elem c) => Proxy x -> Proxy c -> Bool
#endif
acrossForall _ _ = True

-- An ordinary reader can use both components of the imported constraint.
displayElement :: (Display c, Eq c) => Proxy c -> Elem c -> String
displayElement _ = show

#ifdef MENTION_TUPLE
mentioned :: Proxy c -> Proxy (Display c)
#else
mentioned :: Collects c => Proxy c -> Proxy (Display c)
#endif
mentioned _ = Proxy

assumed :: forall c. Collects c => Proxy c -> Proxy (Elem c)
assumed _ =
    let local :: Proxy (Elem c)
        local = Proxy
    in local

assumedInPattern :: forall c. Witness c -> ()
assumedInPattern (Witness _) =
    let local :: Proxy (Elem c)
        local = Proxy
    in (local :: Proxy (Elem c)) `seq` Proxy @(Elem c) `seq` ()
assumedInPattern (ByEquality _) = ()

-- The outer signature supplies both the domain rule and its premise.
fromRule :: forall f a.
#ifdef OMIT_DOMAIN_PREMISE
    (forall x. Allowed x => Collects (f x)) =>
#else
    (Allowed a, forall x. Allowed x => Collects (f x)) =>
#endif
    Proxy (f a) -> ()
fromRule _ =
    let local :: Proxy (Elem (f a))
        local = Proxy
    in (local :: Proxy (Elem (f a))) `seq` ()

localContext :: forall c. Proxy c -> ()
localContext _ =
    let local :: Collects c => Proxy (Elem c)
        local = Proxy
    in ()

resolved :: Allowed a => Elem [a] :~: a
resolved = Refl

ground :: Elem [Bool] :~: Bool
ground = Refl

defaulted :: Def Int :~: Char
defaulted = Refl

permuted :: Swapped Int Bool :~: Char
permuted = Refl

data Witness c where
    Witness :: Collects c => Proxy (Elem c) -> Witness c
    ByEquality :: c ~ [Bool] => Proxy (Elem c) -> Witness c

newtype Known = Known (Proxy (Elem [Bool]))
