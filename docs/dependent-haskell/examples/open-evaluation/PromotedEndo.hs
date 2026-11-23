{-# LANGUAGE DataKinds, GADTs, KindSignatures, Modifiers, PolyKinds,
             StandaloneKindSignatures, TypeAbstractions, TypeApplications,
             TypeFamilies #-}
module PromotedEndo (Endo(Endo), Eval, Exp, Pure1, UnEndo) where

import Data.Kind (Type)
import GHC.Matchability (M, Matchability)

-- | A first-class-families expression with result of kind a.
type Exp a = a %M -> Type
-- | Evaluate a first-class expression.
type family Eval (e :: Exp a) :: a

-- | Lift a matchable function into a first-class expression.
data Pure1 :: (a -> b) -> a -> Exp b
type instance Eval (Pure1 f x) = f x

-- | A callback whose two arrows can have different matchabilities.
type Endo :: forall (m :: Matchability) (n :: Matchability). Type -> Type
newtype Endo @m @n a = Endo (a %m -> a %n -> Type)

-- | Recover a matchable callback from its promoted constructor.
type family UnEndo (e :: Endo @M @M a) :: a -> Exp a where
  UnEndo ('Endo @M @M f) = f
