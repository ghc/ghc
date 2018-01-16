{-# LANGUAGE TypeFamilies, PolyKinds, DataKinds #-}

module T9200b where

---------
--- test CUSK on closed type families
type family F (a :: k) where
  F True = False
  F False = True
  F x = x
