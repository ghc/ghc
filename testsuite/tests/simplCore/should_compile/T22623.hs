{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module T22623 where

import T22623a

type BindNonEmptyList :: NonEmpty -> NonEmpty -> [Q]
type family BindNonEmptyList (x :: NonEmpty) (y :: NonEmpty) :: [Q] where
  BindNonEmptyList ('(:|) a as) c = Tail c ++ Foldr2 a c as

sBindNonEmptyList ::
  forall (t :: NonEmpty)
         (c :: NonEmpty). SNonEmpty t -> SNonEmpty c -> SList (BindNonEmptyList t c :: [Q])
sBindNonEmptyList
  ((:%|) (sA :: SQ a) (sAs :: SList as)) (sC :: SNonEmpty c)
  = let
      sMyHead :: SNonEmpty c -> SQ (MyHead a c)
      sMyHead ((:%|) x _) = x

      sFoldr :: forall t. SList t -> SList (Foldr2 a c t)
      sFoldr SNil = SNil
      sFoldr (SCons _ sYs) = SCons (sMyHead sC) (sFoldr sYs)

      sF :: Id (SLambda (ConstSym1 c))
      sF = SLambda (const sC)

      sBs :: SList (Tail c)
      _ :%| sBs = applySing sF sA
    in
      sBs %++ sFoldr sAs
