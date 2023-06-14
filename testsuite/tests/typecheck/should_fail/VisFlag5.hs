{-# LANGUAGE TypeFamilies #-}

module VisFlag5 where

import Data.Kind

data family   D a   :: (forall i -> i -> i) -> Type
data instance D Int :: (forall i.   i -> i) -> Type
