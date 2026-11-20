{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, TypeFamilies #-}
module NativeTypeCaseHiddenErased where

type Hidden a = 'Just ('Just 'True)
type F d = case d of
  Hidden a -> Int
  Just _ -> Int
  Nothing -> Char
