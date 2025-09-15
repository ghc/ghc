{-# LANGUAGE RequiredTypeArguments #-}

module T23740a where

a = 10

f :: a -> a
f = id