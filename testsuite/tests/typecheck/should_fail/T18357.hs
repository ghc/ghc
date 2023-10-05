{-# LANGUAGE StandaloneKindSignatures, TypeFamilies, GADTs, DataKinds #-}

module T18357 where

import Data.Kind

type family Star where Star = Type

type W :: Star
type W = T

newtype T where
  MkT :: Int -> W
