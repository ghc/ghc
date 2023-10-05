{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedNewtypes #-}
module T17021 where

import Data.Kind
import GHC.Exts

type family Id (x :: a) :: a where
  Id x = x

newtype T :: TYPE (Id LiftedRep) where
  MkT :: Int -> T

f :: T
f = MkT 42
