{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module T16418 where

import Data.Kind

data SameKind :: forall k. k -> k -> Type

f :: forall a k (b :: k). SameKind a b -> ()
f = g
  where
    g :: SameKind a b -> ()
    g _ = ()
