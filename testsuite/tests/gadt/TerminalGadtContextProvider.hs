{-# LANGUAGE FlexibleContexts, GADTs, KindSignatures, RequiredTypeArguments #-}
module TerminalGadtContextProvider (G(..), Observe(..), Recursive(..), IndexedTag(..)) where

import Data.Kind (Type)
import Type.Reflection (Typeable)

data G a b where
  Store :: forall a. Eq a => (Show a => a -> forall b -> G a b)
  Narrow :: forall a. (a ~ Int) => Int -> forall b -> G a b

class Observe a where
  observe :: a -> Bool

data Recursive a b where
  Recur :: forall a. Observe (Recursive a Bool) => a -> forall b -> Recursive a b

data IndexedTag a where
  IndexedTag :: Int -> forall (a :: Type) -> Typeable a => IndexedTag a
