{-# LANGUAGE PolyKinds, TypeFamilies, DataKinds #-}

module T19611 where

import Data.Kind

type family Result k :: Type
type family Apply (f :: k) :: Result k

data TyCon = DataCon
type instance Result TyCon = Type

type instance Apply 'DataCon = Int
