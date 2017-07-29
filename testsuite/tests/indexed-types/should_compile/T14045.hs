{-# LANGUAGE TypeFamilies, PolyKinds, DataKinds, GADTs #-}

module T14045 where

import Data.Kind

data family Sing (a :: k)
data instance Sing :: Bool -> Type where
  SFalse :: Sing False
  STrue  :: Sing True
