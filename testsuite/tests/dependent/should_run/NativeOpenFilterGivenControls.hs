{-# LANGUAGE AllowAmbiguousTypes, DataKinds, GADTs, Modifiers, NoExperimentalDependentTypes #-}
{-# LANGUAGE PolyKinds, ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module NativeOpenFilterGivenControls where

import Data.Coerce (Coercible)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import PreludeFilterSource (FilterWith)

withoutResult :: forall a (p :: a %U -> Bool) (x :: a) (xs :: [a]).
                 Proxy p -> Proxy x -> Proxy xs
              -> FilterWith p (x ': xs) :~: (x ': FilterWith p xs)
withoutResult _ _ _ = Refl

wrongResult :: forall a (p :: a %U -> Bool) (x :: a) (xs :: [a]). (p x ~ 'False)
            => Proxy p -> Proxy x -> Proxy xs
            -> FilterWith p (x ': xs) :~: (x ': FilterWith p xs)
wrongResult _ _ _ = Refl

otherPredicate :: forall a (p :: a %U -> Bool) (q :: a %U -> Bool) (x :: a) (xs :: [a]).
                  (p x ~ 'True) => Proxy p -> Proxy '(q, x) -> Proxy xs
               -> FilterWith q (x ': xs) :~: (x ': FilterWith q xs)
otherPredicate _ _ _ = Refl

otherHead :: forall a (p :: a %U -> Bool) (x :: a) (y :: a) (xs :: [a]). (p y ~ 'True)
          => Proxy p -> Proxy '(x, y) -> Proxy xs
          -> FilterWith p (x ': xs) :~: (x ': FilterWith p xs)
otherHead _ _ _ = Refl

representationalResult :: forall a (p :: a %U -> Bool) (x :: a) (xs :: [a]).
                          Coercible (p x) 'True => Proxy p -> Proxy x -> Proxy xs
                       -> FilterWith p (x ': xs) :~: (x ': FilterWith p xs)
representationalResult _ _ _ = Refl

invertHead :: forall a (p :: a %U -> Bool) (x :: a) (y :: a). (p x ~ p y)
           => Proxy p -> Proxy x -> Proxy y -> x :~: y
invertHead _ _ _ = Refl

invertPredicate :: forall a (p :: a %U -> Bool) (q :: a %U -> Bool) (x :: a). (p x ~ q x)
                => Proxy p -> Proxy q -> Proxy x -> p :~: q
invertPredicate _ _ _ = Refl

data HeadResult b where
    KnownTrue :: HeadResult 'True
    Unknown :: HeadResult b

-- Both branches have the same type indices. Only the first has a Given.
staleGiven :: forall a (p :: a %U -> Bool) (x :: a) (xs :: [a]).
              Proxy p -> Proxy x -> Proxy xs -> HeadResult (p x)
           -> FilterWith p (x ': xs) :~: (x ': FilterWith p xs)
staleGiven _ _ _ KnownTrue = Refl
staleGiven _ _ _ Unknown = Refl
