{-# LANGUAGE ConstraintKinds, TypeFamilies, UndecidableInstances #-}

module AltPrelude where

import GHC.Prim (Constraint)

class MyFunctor f where
       type FunctorCtxt f a :: Constraint
       type FunctorCtxt f a = ()
       (<$>) :: (FunctorCtxt f a, FunctorCtxt f b) => (a -> b) -> f a -> f b

class MyFunctor ap => MyApplicative ap where
       type ApplicativeCtxt ap a :: Constraint
       type ApplicativeCtxt ap a = FunctorCtxt ap a
       (<***>) :: (ApplicativeCtxt ap a, ApplicativeCtxt ap b) => ap (a -> b) -> ap a -> ap b
