{-# Language DataKinds, TypeOperators, PolyKinds, GADTs, PatternSynonyms #-}

module T15685 where

import Data.Kind

data NS f as where
 Here :: f a -> NS f (a:as)

data NP :: (k -> Type) -> ([k] -> Type) where
 Nil :: NP f '[]

pattern HereNil = Here Nil
