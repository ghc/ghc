{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}

{-# LANGUAGE FlexibleInstances, UndecidableInstances, FunctionalDependencies #-}

-- The code from the ticket lacked necessary extension FlexibleContexts
-- which crashed the compiler with "GHC internal error"
-- This test case reproduces that scenario
{- # LANGUAGE FlexibleContexts #-}

module T12055a where

import GHC.Base ( Constraint, Type )
import GHC.Exts ( type (~~) )

type Cat k = k -> k -> Type

class Category (p :: Cat k) where
    type Ob p :: k -> Constraint

class (Category (Dom f), Category (Cod f)) => Functor (f :: j -> k) where
    type Dom f :: Cat j
    type Cod f :: Cat k
    functor :: forall a b.
               Iso Constraint (:-) (:-)
               (Ob (Dom f) a)     (Ob (Dom f) b)
               (Ob (Cod f) (f a)) (Ob (Cod f) (f b))

class (Functor f , Dom f ~ p, Cod f ~ q) =>
    Fun (p :: Cat j) (q :: Cat k) (f :: j -> k) | f -> p q
instance (Functor f , Dom f ~ p, Cod f ~ q) =>
    Fun (p :: Cat j) (q :: Cat k) (f :: j -> k)

data Nat (p :: Cat j) (q :: Cat k) (f :: j -> k) (g :: j -> k)

type Iso k (c :: Cat k) (d :: Cat k) s t a b =
    forall p. (Cod p ~~ Nat d (->)) => p a b -> p s t

data (p :: Constraint) :- (q :: Constraint)
