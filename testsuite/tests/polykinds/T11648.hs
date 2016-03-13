{-# LANGUAGE PolyKinds, TypeOperators, TypeFamilies,
             MultiParamTypeClasses #-}

module T11648 where

class Monoidy (to :: k0 -> k1 -> *) (m :: k1)  where
  type MComp to m :: k1 -> k1 -> k0
  mjoin :: MComp to m m m `to` m
