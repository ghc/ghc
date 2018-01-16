{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}

-- The code from the ticket lacked these extensions,
-- but crashed the compiler with "GHC internal error"
-- It doesn't crash now; and in this test case I've added
-- the extensions, which makes it compile cleanly
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, FunctionalDependencies #-}


module T12055 where

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
