{-# LANGUAGE TemplateHaskell, RankNTypes, TypeOperators, TypeInType,
             GADTs #-}

module T8031 where

import Data.Proxy
import Data.Kind

data SList :: [k] -> * where
  SCons :: Proxy h -> Proxy t -> SList (h ': t)

$( [d| foo :: forall (a :: k). Proxy a
           -> forall (b :: [k]). Proxy b
           -> SList (a ': b)
       foo a b = SCons a b |] )
