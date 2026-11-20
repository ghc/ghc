{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, TypeFamilies #-}
module NativeTypeCaseNestedAliasHead where
type Identity x = x
type Bad d = case d of
  Just (Identity x) -> Bool
  Nothing -> Bool
