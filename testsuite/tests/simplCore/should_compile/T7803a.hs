{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module T7803a
    ( Evaluate (..)
    , HasIntegral (..)
    , Poly4 (..)
    , Domain
    , IntOfLogPoly4 (..)
    , Piecewise (..)
    ) where

import Data.Kind (Constraint, Type)

class Evaluate poly where
    evaluate :: (Domain' poly a) => poly a -> a -> a

class Evaluate (IntegralOf poly) => HasIntegral poly where
    type IntegralOf poly :: Type -> Type
    indefinite :: (Domain' poly a, Domain' (IntegralOf poly) a)
               => poly a -> IntegralOf poly a

type family Domain (f :: Type -> Type) a :: Constraint

class Domain f a => Domain' f a
instance Domain f a => Domain' f a

newtype Poly4 a = Poly4 { poly4_a :: a }

type instance Domain Poly4 a = Fractional a

data IntOfLogPoly4 a = IntOfLogPoly4 { ilp4_k :: !a, ilp4_u :: !a }

type instance Domain IntOfLogPoly4 a = Floating a

instance Evaluate IntOfLogPoly4 where
    evaluate (IntOfLogPoly4 k _) x = k + x * exp5Tail (- log x)
    {-# INLINE evaluate #-}

{-# RULES "exp5Tail/Double" exp5Tail = exp5TailDouble #-}
{-# NOINLINE exp5Tail #-}

exp5Tail :: (Floating a) => a -> a
exp5Tail x = x / 120

exp5TailDouble :: Double -> Double
exp5TailDouble x = x / 120

instance HasIntegral Poly4 where
    type IntegralOf Poly4 = IntOfLogPoly4
    indefinite = \ (Poly4 a) -> IntOfLogPoly4 0 a
    {-# INLINE indefinite #-}

newtype Piecewise poly a = Piecewise { unPiecewise :: [poly a] }

type instance Domain (Piecewise poly) a = (Domain poly a, Num a)

instance (Evaluate poly) => Evaluate (Piecewise poly) where
    evaluate = \ (Piecewise pp) x ->
        case pp of
          p : _ -> evaluate p x
          []    -> error "evaluate: empty Piecewise"
    {-# INLINE evaluate #-}

instance (HasIntegral poly) => HasIntegral (Piecewise poly) where
    type IntegralOf (Piecewise poly) = Piecewise (IntegralOf poly)
    indefinite (Piecewise m) = Piecewise $ map indefinite m
    {-# INLINE indefinite #-}
