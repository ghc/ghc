{-# LANGUAGE GADTs, RequiredTypeArguments #-}
module GadtTrailingContextProvider (T(..), Witness(..), Checked(..)) where

data T a where
  MkT :: Int -> forall a. Eq a => T a
  Force :: !Int -> forall a. Eq a => T a

data Witness a where
  Witness :: forall a -> Witness a

data Checked a b where
  Checked :: forall a. Witness a -> forall b. (a ~ Int, b ~ Bool) => Checked a b
