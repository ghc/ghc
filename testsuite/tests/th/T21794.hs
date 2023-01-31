{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module T21794 where

import Data.Kind
$([d|
        data P = L | R
        data T (a :: P) where
                A :: T a
                B :: T R

        type TConstraint = forall a . T a -> Constraint

        type ForAllA1 :: TConstraint -> Constraint
        class (forall a . constr @a A) => ForAllA1 constr
        instance forall (constr :: TConstraint) . (forall a . constr @a A) => ForAllA1 constr

        type ForAllA2 :: TConstraint -> Constraint
        class (forall a . constr @a A) => ForAllA2 constr
        deriving anyclass instance forall (constr :: TConstraint) . (forall a . constr @a A) => ForAllA2 constr

   |])


$([d|
        type C :: forall {k} {l}. k -> l -> Constraint
        class C a b

        instance forall k (a :: k) (b :: Type). C a k
   |])