{-# LANGUAGE PolyKinds, MultiParamTypeClasses, DataKinds, TypeFamilies #-}

module T15591c where

import Data.Proxy
import Data.Kind

class C3 (a :: Type) (b :: Proxy a) (c :: Proxy b) where
  type T5 c a
