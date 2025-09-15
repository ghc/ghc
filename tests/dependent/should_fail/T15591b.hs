{-# LANGUAGE PolyKinds, MultiParamTypeClasses, DataKinds, TypeFamilies #-}

module T15591b where

import Data.Proxy
import Data.Kind

class C2 (a :: Type) (b :: Proxy a) (c :: Proxy b) where
  type T4 a c
