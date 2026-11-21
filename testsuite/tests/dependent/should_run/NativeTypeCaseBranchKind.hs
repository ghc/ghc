{-# LANGUAGE DataKinds, ExperimentalDependentTypes, RankNTypes #-}
module NativeTypeCaseBranchKind where

type F a = case a of
  True -> Int
  False -> 'False

type BadArrow a = () -> (case a of
  True -> Int
  False -> 'False)

type BadForall = forall (b :: Bool). () -> (case b of
  True -> Int
  False -> 'False)

type BadArrowDomain a = (case a of
  True -> Int
  False -> 'False) -> ()
