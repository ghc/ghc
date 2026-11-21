{-# LANGUAGE GADTs, RequiredTypeArguments #-}
module GadtInteriorContextProvider (Tag(..), Two(..)) where

data Tag a where
  Tag :: forall a -> Tag a

data Two a where
  Two :: forall a. Tag a -> (a ~ Int) => Tag a -> Two a
  Strict :: forall a. !(Tag a) -> (a ~ Int) => Tag a -> Two a
