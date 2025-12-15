{-# LANGUAGE GADTs, DataKinds #-}
module T15116 where

import Data.Proxy

data A (a :: k) where
  MkA :: A MkA


