{-# LANGUAGE AllowAmbiguousTypes, Arrows, CPP, FlexibleContexts, GADTs, NamedWildCards #-}
{-# LANGUAGE NoExperimentalDependentTypes, PartialTypeSignatures, QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables, StaticPointers, TemplateHaskell, TypeApplications, TypeFamilies #-}
module NativePartialFamilyScope where

import Control.Arrow (arr)
import Data.Proxy (Proxy(..))
import GHC.StaticPtr (StaticPtr)
import Language.Haskell.TH.Syntax (Code, Q)
import NativeConstrainedFamilyProvider (Allowed, Collects(..))

data Witness c where
    Witness :: Collects c => Witness c

quoted :: forall t. Code Q ()
quoted = [|| () ||]

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
#elif defined(SPLICE_ESCAPE)
check :: Proxy Int -> _
check _ = ([|| \(w :: Witness Int) -> case w of
                   Witness -> $$(quoted @(Elem Int)) ||] :: Code Q (Witness Int -> ()))
#elif defined(OTHER_INDEX)
check :: Collects _index => Proxy _index -> Proxy other -> _
check _ (_ :: Proxy other) = Proxy @(Elem other) `seq` ()
#elif defined(INFERRED_ESCAPE)
check :: forall c. Witness c -> _
check Witness = Proxy @(Elem c)
#else
proxyElement :: forall c. Collects c => Proxy c -> Proxy (Elem c)
proxyElement _ = Proxy

-- Each call to proxyElement requires the class in the generalised type.
inferred p = proxyElement p

inferredContext :: forall c. _ => Proxy c -> _
inferredContext p = proxyElement p

arrow :: forall c. Collects c => Proxy c -> _
arrow = proc _ -> arr (\() -> Proxy @(Elem c) `seq` ()) -< ()

splice :: forall c. Collects c => Proxy c -> _
splice _ = ([|| $$(quoted @(Elem c)) ||] :: Code Q ())

sameIndex :: Collects _index => Proxy _index -> _
sameIndex (_ :: Proxy c) = Proxy @(Elem c) `seq` ()

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
