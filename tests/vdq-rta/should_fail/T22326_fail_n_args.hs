{-# LANGUAGE RequiredTypeArguments, ExplicitNamespaces #-}

module T22326_fail_n_args where

f :: a -> forall b -> b
f x (type y) z = undefined