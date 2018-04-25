{-# LANGUAGE TypeInType, ScopedTypeVariables, TypeOperators, GADTs #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}  -- don't want erroneous warning in test output
                                               -- if removing this doesn't change output, then
                                               -- remove it!

module T13337 where

import Data.Typeable
import Data.Kind

f :: forall k (a :: k). (Typeable k, Typeable a) => Proxy a -> Proxy Int
f p = case eqT :: Maybe (k :~: Type) of
  Nothing -> Proxy
  Just Refl -> case eqT :: Maybe (a :~: Int) of
    Nothing -> Proxy
    Just Refl -> p
