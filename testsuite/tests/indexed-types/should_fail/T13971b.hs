{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module T13971b where

import Data.Kind

class C (a :: j) where
  type T (a :: j) (b :: k)
  type T (a :: k) (b :: k) = k
