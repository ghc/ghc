{-# LANGUAGE TypeFamilies, PolyKinds, DataKinds, GADTs, FlexibleInstances #-}

module T14045a where

import Data.Kind

class C (a :: k) where
  data S (a :: k)

instance C (z :: Bool) where
  data S :: Bool -> Type where
    SF :: S False
    ST :: S True
