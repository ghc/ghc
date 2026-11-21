{-# LANGUAGE CPP, FlexibleContexts, GADTs, PartialTypeSignatures, QuantifiedConstraints #-}
{-# LANGUAGE NoExperimentalDependentTypes, ScopedTypeVariables, StaticPointers, TypeApplications, TypeFamilies #-}
module NativePartialFamilyScope where

import Data.Proxy (Proxy(..))
import GHC.StaticPtr (StaticPtr)
import NativeConstrainedFamilyProvider (Allowed, Collects(..))

#if defined(CONTEXT_WILDCARD)
check :: forall c. _ => Proxy c -> _
check _ = Proxy @(Elem c) `seq` ()
#elif defined(SIBLING)
check :: forall c. Proxy c -> ()
check _ = bad
  where
    good :: Collects c => Proxy c -> _
    good _ = Proxy @(Elem c) `seq` ()
    bad :: ()
    bad = Proxy @(Elem c) `seq` ()
#elif defined(STATIC)
check :: forall c. Collects c => Proxy c -> _
check _ = (static (Proxy @(Elem c) `seq` ()) :: StaticPtr ())
#else
forward :: forall f a.
#if !defined(MISSING_BINDING)
           (Allowed a, forall x. Allowed x => Collects (f x))
#else
           (forall x. Allowed x => Collects (f x))
#endif
        => Proxy (f a) -> _
forward _ = Proxy @(Elem (f a)) `seq` True

ascribed :: Bool
ascribed =
    ((\_ -> Proxy @(Elem (f a)) `seq` True)
      :: forall f a.
#if !defined(MISSING_ANNOTATION)
         (Allowed a, forall x. Allowed x => Collects (f x))
#else
         (forall x. Allowed x => Collects (f x))
#endif
      => Proxy (f a) -> _) (Proxy :: Proxy [Bool])
#endif
