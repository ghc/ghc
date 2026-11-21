{-# LANGUAGE ExplicitNamespaces, GADTs, RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, ViewPatterns #-}
module GadtTrailingFieldScope where

import Data.Proxy
import Type.Reflection

data N where
  N :: a -> (Eq a, Num a) => N

wrong :: N -> Bool
wrong (N 0) = True
wrong (N _) = False

data Later a b where
  Later :: forall a. Proxy a -> forall b -> Typeable b => Later a b

inspect :: forall a. Typeable a => Proxy a -> String
inspect _ = show (typeRep @a)

early :: Later a a -> String
early (Later (inspect -> name) (type _)) = name
