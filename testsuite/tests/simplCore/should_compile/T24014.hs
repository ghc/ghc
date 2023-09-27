{-# LANGUAGE ExplicitNamespaces, ScopedTypeVariables, RequiredTypeArguments #-}
module T24014 where

visId :: forall a -> a -> a
visId (type a) x = x

f :: forall a -> a -> a
f (type x) = visId (type x)

g :: forall a. a -> a
g = visId (type a)
