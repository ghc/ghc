{-# LANGUAGE GADTs, DataKinds, PolyKinds, EmptyDataDecls #-}

module T16263 where

import Data.Kind

data Q :: Eq a => Type where {}
