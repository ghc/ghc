{-# LANGUAGE RequiredTypeArguments, GADTs #-}

module T25127_fail_arity where

data KindVal a where
  K :: forall k.
       forall (a::k) ->
       k ->
       KindVal a

data ProxyVis a where
  V :: forall k.
       forall (a::k) ->
       ProxyVis a

f0 K         = ()   -- too few
f1 (K a)     = ()   -- too few
f2 (K a b)   = ()   -- just right
f3 (K a b c) = ()   -- too many

g0 V       = ()  -- too few
g1 (V a)   = ()  -- just right
g2 (V a b) = ()  -- too many
