{-# LANGUAGE PolyKinds, GADTs #-}

module T17541b where

import Data.Kind

data T k :: k -> Type where
    MkT1 :: T Type Int
    MkT2 :: T (Type -> Type) Maybe
