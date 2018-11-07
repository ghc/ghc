{-# LANGUAGE TemplateHaskell, RankNTypes, TypeOperators, DataKinds, PolyKinds,
             GADTs #-}

module T8031 where

import Data.Proxy
import Data.Kind

data SList :: [k] -> Type where
  SCons :: Proxy h -> Proxy t -> SList (h ': t)

$( [d| foo :: forall k (a :: k). Proxy a
           -> forall (b :: [k]). Proxy b
           -> SList (a ': b)
       foo a b = SCons a b |] )
