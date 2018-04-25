{-# language ConstraintKinds, FlexibleContexts, TypeFamilies,
    UndecidableInstances, DeriveFunctor #-}

module T13320 where

import GHC.Exts        (Constraint)

data QCGen

newtype Gen a = MkGen { unGen :: QCGen -> Int -> a }
  deriving Functor

sized :: (Int -> Gen a) -> Gen a
sized f = MkGen (\r n -> let MkGen m = f n in m r n)

class Arbitrary a where
  arbitrary :: Gen a

type family X_Var ξ

data TermX ξ = Var (X_Var ξ)

type ForallX (φ :: * -> Constraint) ξ = ( φ (X_Var ξ) )

-- This type signature used to be necessary to prevent the
-- type checker from looping.
-- genTerm :: ForallX Arbitrary ξ => Int -> Gen (TermX ξ)
genTerm 0 = Var <$> arbitrary
genTerm n = Var <$> genTerm (n - 1)

instance ForallX Arbitrary ξ => Arbitrary (TermX ξ) where
  arbitrary = sized genTerm
