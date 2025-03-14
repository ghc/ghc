{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module T12088e where

import Data.Kind

type family K a where K Type = Type

type family G
type instance G = F

data T b = T (b :: K G)

type F = Type
