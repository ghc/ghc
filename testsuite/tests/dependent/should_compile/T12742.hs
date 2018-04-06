{-# LANGUAGE TypeInType, RankNTypes, TypeFamilies #-}

module T12742 where

import Data.Kind

type family F :: forall k2. (k1, k2)

data T :: (forall k2. (Bool, k2)) -> Type

type S = T F
