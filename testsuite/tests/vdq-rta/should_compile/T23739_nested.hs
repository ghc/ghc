{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RequiredTypeArguments #-}

module T23739_nested where

import Data.Kind

f :: forall a -> a -> a
f ((type t) :: Type) x = x :: t