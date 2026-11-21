{-# LANGUAGE GADTs, TypeAbstractions #-}
module GadtInteriorForAllScope where

data Box a where
  Box :: Int -> forall a. a -> Box a

wrong :: Box a -> Bool
wrong (Box @Int _ _) = True
