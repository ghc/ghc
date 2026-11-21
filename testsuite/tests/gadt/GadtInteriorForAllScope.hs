{-# LANGUAGE GADTs, RequiredTypeArguments, ScopedTypeVariables, TypeAbstractions #-}
module GadtInteriorForAllScope where

data Box a where
  Box :: Int -> forall a. a -> Box a

wrong :: Box a -> Bool
wrong (Box @Int _ _) = True

data Required a where
  Required :: Int -> forall a -> a -> Required a

data Pack where
  Pack :: Int -> forall a -> a -> Pack

early :: Required a -> (Int, a)
early (Required (type b) n x) = (n, x)

escape :: Pack -> a
escape (Pack _ (type b) (x :: b)) = x
