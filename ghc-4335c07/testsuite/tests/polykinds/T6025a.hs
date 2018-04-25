{-# LANGUAGE DataKinds, GADTs #-}
module T6025a (Other (..)) where

data Other a where
  OTrue :: Other True
  OFalse :: Other False
