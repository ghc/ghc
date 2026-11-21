{-# LANGUAGE GADTs, RequiredTypeArguments #-}

module TerminalGadtForAllProvider (G(..), P(..), Promotable(..)) where

data P a where
  P :: P Int

data G a b where
  After :: forall a. Int -> forall b -> G a b
  Force :: forall a. !Int -> forall b -> G a b
  Refine :: forall a. P a -> forall b -> G a b

data Promotable a b where
  Payload :: forall a. Bool -> forall b -> Promotable a b
