{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module PreludeMapProof (preludeNil, preludeNegateHead, showPreludeMapProofs) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import PreludeMapSource (PreludeNegateAll)

-- | Rewriting must be disabled in the module that checks these equations.
preludeNil :: PreludeNegateAll '[] :~: '[]
preludeNil = Refl

-- | The equation needs the first constructor, and leaves the tail abstract.
preludeNegateHead :: Proxy xs
                  -> PreludeNegateAll ('True ': xs) :~: ('False ': PreludeNegateAll xs)
preludeNegateHead _ = Refl

-- | Inspect both proofs under the rewrite policy that certifies them.
showPreludeMapProofs :: String
showPreludeMapProofs = show (preludeNil, preludeNegateHead (Proxy :: Proxy '[]))
