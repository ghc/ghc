{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeFamilies #-}
module NativeTypeCaseDominatedRhs where

type F d = case d of
  Just (Just _) -> Int
  Just (Just _) -> NotInScope
  Just Nothing -> Bool
  Nothing -> Char
