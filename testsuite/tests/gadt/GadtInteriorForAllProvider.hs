{-# LANGUAGE GADTs, RequiredTypeArguments #-}
module GadtInteriorForAllProvider where

data Box a where
  Box :: !Int -> forall a. a -> Box a

data Pack where
  Pack :: !Int -> forall a. a -> Pack

data First a where
  IsInt :: First Int
  Any :: First a

data Later b a where
  Later :: forall b. First b -> forall a. a -> Later b a

data Shadow where
  Shadow :: forall a. a -> forall a. a -> Shadow

data Required a where
  Required :: !Int -> forall a -> a -> Required a

data RequiredPack where
  RequiredPack :: Int -> forall a -> a -> RequiredPack

data RequiredLater b a where
  RequiredLater :: forall b. First b -> forall a -> a -> RequiredLater b a
