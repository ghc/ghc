{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module PreludeFilterProof (keepFalse, skipTrue, lazyHead, showPreludeFilterProofs) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import PreludeFilterSource (FilterNegated)

-- | Keep a False head and leave the recursive call on the unknown tail.
keepFalse :: Proxy xs
          -> FilterNegated ('False ': xs) :~: ('False ': FilterNegated xs)
keepFalse _ = Refl

-- | Skip a True head and keep the recursive call on the unknown tail.
skipTrue :: Proxy xs -> FilterNegated ('True ': xs) :~: FilterNegated xs
skipTrue _ = Refl

-- | Observe the kept head without evaluating the tail.
{-# NOINLINE lazyHead #-}
lazyHead :: [Bool] -> Bool
lazyHead xs = case Prelude.filter not (False : xs) of
    False : _ -> True
    _ -> False

-- | Observe both proofs under the rewrite policy that permits the equations.
showPreludeFilterProofs :: String
showPreludeFilterProofs =
    show (keepFalse (Proxy :: Proxy '[]), skipTrue (Proxy :: Proxy '[]), lazyHead undefined)
