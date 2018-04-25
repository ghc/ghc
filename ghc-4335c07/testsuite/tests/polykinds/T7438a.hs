{-# LANGUAGE PolyKinds, GADTs, KindSignatures, DataKinds, FlexibleInstances #-}

module T7438a where

data Thrist :: k -> k -> * where
  Nil :: Thrist a a


