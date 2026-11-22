{-# LANGUAGE GADTs, ScopedTypeVariables, TemplateHaskell #-}
module GadtPunScope where
import PunTypeSplice (a)

plain :: forall a. $(a) -> a -> a
plain _ x = x

data G where
  MkG :: forall a. $(a) -> a -> G

-- The data header's binder does not scope over a GADT constructor.
data Phantom a where
  NoOuter :: $(a) -> Phantom Bool
