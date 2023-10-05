{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module T22793 where

import Data.Kind

type Foo :: forall k. k -> k -> Constraint

class Foo s a

bob :: forall {k1} {ks} {ka} q (p :: k1 -> q -> Type)
              (f :: ka -> q) (s :: ks) (t :: ks)
              (a :: ka) (b :: ka). Foo s a
     => p a (f b) -> p s (f t)
bob f = undefined
