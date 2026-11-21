{-# LANGUAGE GADTs, RequiredTypeArguments #-}
module GadtLateFieldProvider where

data Late where
  Late :: Int -> forall a -> !a -> Late
