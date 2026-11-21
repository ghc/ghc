{-# LANGUAGE GADTs, RequiredTypeArguments #-}
module TerminalGadtContextScope where

data G a b where
  Scope :: forall a. Show b => a -> forall b -> G a b
