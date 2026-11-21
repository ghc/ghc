{-# LANGUAGE GADTs, RequiredTypeArguments #-}
module GadtInteriorContextProvider (Tag(..), Two(..), Hidden(..)) where

data Tag a where
  Tag :: forall a -> Tag a

data Two a where
  Two :: forall a. Tag a -> (a ~ Int) => Tag a -> Two a
  Strict :: forall a. !(Tag a) -> (a ~ Int) => Tag a -> Two a

data Hidden where
  Hidden :: forall a. a -> Eq a => a -> Hidden
