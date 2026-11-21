{-# LANGUAGE GADTs, RequiredTypeArguments #-}
module GadtInteriorContextScope where

data Tag a where
  Tag :: forall a -> Tag a

data Two a where
  Two :: forall a. Tag a -> (a ~ Int) => Tag a -> Two a

wrong :: Two a -> Bool
wrong (Two (Tag (type Int)) _) = True
