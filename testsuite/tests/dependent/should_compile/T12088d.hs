{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module T12088d where

import Data.Kind

data family T b

type family K a where K Type = Type

type family G
type instance G = F

data instance T b = T (b :: K G)

type F = Type

-- type family G