{-# LANGUAGE GADTs, RequiredTypeArguments #-}
module GadtTrailingContextProvider (T(..), Witness(..), Checked(..), Evidence(..), Combined(..), F(..), Box(..)) where

data T a where
  MkT :: Int -> forall a. Eq a => T a
  Force :: !Int -> forall a. Eq a => T a

data Witness a where
  Witness :: forall a -> Witness a

data Checked a b where
  Checked :: forall a. Witness a -> forall b. (a ~ Int, b ~ Bool) => Checked a b

data Evidence a where
  Evidence :: Ord a => Evidence a

data Combined a b where
  Combined :: forall a. Evidence a -> forall b. Eq b => Combined a b

data F where
  K :: a -> Eq a => F

data Box a where
  Box :: forall a. a -> Eq a => Box a
