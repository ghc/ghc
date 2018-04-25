{-# LANGUAGE PolyKinds, GADTs #-}

module T13391 where

data G (a :: k) where
  GInt   :: G Int
  GMaybe :: G Maybe
