{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, TypeFamilies #-}
module NativeTypeCaseLetMonomorphic where
import Data.Kind (Type)
type AtBool (x :: [Maybe Bool]) = Int
type Bad = let x = '[ 'Nothing] in case 'Just (x :: [Maybe Type]) of
  'Nothing -> AtBool x
  'Just y -> AtBool x
