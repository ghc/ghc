{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module PreludeFilterProof
  (keepFalse, skipTrue, keepTrue, skipFalse, lazyHead, lazyIdentityHead, showPreludeFilterProofs) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import PreludeFilterSource (FilterIdentity, FilterNegated)

-- | Keep a False head and leave the recursive call on the unknown tail.
keepFalse :: Proxy xs
          -> FilterNegated ('False ': xs) :~: ('False ': FilterNegated xs)
keepFalse _ = Refl

-- | Skip a True head and keep the recursive call on the unknown tail.
skipTrue :: Proxy xs -> FilterNegated ('True ': xs) :~: FilterNegated xs
skipTrue _ = Refl

-- | Keep a True head with the identity predicate and leave the tail open.
keepTrue :: Proxy xs -> FilterIdentity ('True ': xs) :~: ('True ': FilterIdentity xs)
keepTrue _ = Refl

-- | Skip a False head and recurse with the original predicate.
skipFalse :: Proxy xs -> FilterIdentity ('False ': xs) :~: FilterIdentity xs
skipFalse _ = Refl

-- | Observe the kept head without evaluating the tail.
{-# NOINLINE lazyHead #-}
lazyHead :: [Bool] -> Bool
lazyHead xs = case Prelude.filter not (False : xs) of
    False : _ -> True
    _ -> False

-- | Observe a True head through the identity predicate without forcing the tail.
{-# NOINLINE lazyIdentityHead #-}
lazyIdentityHead :: [Bool] -> Bool
lazyIdentityHead xs = case Prelude.filter id (True : xs) of
    True : _ -> True
    _ -> False

-- | Show both branches and the demand on the tail for each predicate.
showPreludeFilterProofs :: String
showPreludeFilterProofs =
    show ((keepFalse nil, skipTrue nil, lazyHead undefined),
          (keepTrue nil, skipFalse nil, lazyIdentityHead undefined))
  where
    nil = Proxy :: Proxy '[]
