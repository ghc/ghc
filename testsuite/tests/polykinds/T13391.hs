{-# LANGUAGE PolyKinds, GADTs #-}

module T13391 where

-- This should fail as it ought to require TypeInType
data G (a :: k) where
  GInt   :: G Int
  GMaybe :: G Maybe
