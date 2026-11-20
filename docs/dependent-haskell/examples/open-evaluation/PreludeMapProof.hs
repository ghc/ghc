{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, PolyKinds, TypeOperators #-}
module PreludeMapProof (preludeNil, preludeNegateHead, preludeIdentityHead, showPreludeMapProofs) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import PreludeMapSource (PreludeNegateAll, PreludeIdentityMap)

-- | Rewriting must be disabled in the module that checks these equations.
preludeNil :: PreludeNegateAll '[] :~: '[]
preludeNil = Refl

-- | The equation needs the first constructor, and leaves the tail abstract.
preludeNegateHead :: Proxy xs
                  -> PreludeNegateAll ('True ': xs) :~: ('False ': PreludeNegateAll xs)
preludeNegateHead _ = Refl

-- | Keep the original element and leave the recursive call on the tail.
preludeIdentityHead :: Proxy x -> Proxy xs
                   -> PreludeIdentityMap (x ': xs) :~: (x ': PreludeIdentityMap xs)
preludeIdentityHead _ _ = Refl

-- | Inspect both proofs under the rewrite policy that certifies them.
showPreludeMapProofs :: String
showPreludeMapProofs = show (preludeNil, preludeNegateHead (Proxy :: Proxy '[]))
