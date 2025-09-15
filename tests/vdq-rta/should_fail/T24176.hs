{-# LANGUAGE QuantifiedConstraints, RequiredTypeArguments #-}
module T24176 where

f :: (forall a -> Eq a) => a
f = f
