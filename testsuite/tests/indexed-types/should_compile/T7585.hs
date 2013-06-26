{-# LANGUAGE GADTs, RankNTypes, KindSignatures, PolyKinds, TypeOperators, DataKinds,
             TypeFamilies #-}

module Bug where

data SBool :: Bool -> * where
  SFalse :: SBool False
  STrue :: SBool True

data SList :: [Bool] -> * where
  SNil :: SList '[]
  SCons :: SBool h -> SList t -> SList (h ': t)

type family (a :: [k]) :==: (b :: [k]) :: Bool where
  '[] :==: '[] = True
  (h1 ': t1) :==: (h2 ': t2) = True
  a :==: b = False

(%==%) :: SList ls1 -> SList ls2 -> SBool (ls1 :==: ls2)
SNil %==% (SCons _ _) = SFalse