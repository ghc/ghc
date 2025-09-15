{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

-- The code from the ticket lacked these extensions,
-- but crashed the compiler with "GHC internal error"
-- It doesn't crash now; and in this test case I've added
-- the extensions, which makes it compile cleanly
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, FunctionalDependencies #-}


module T12055 where

import Data.Kind (Constraint, Type)
import Data.Type.Equality (type (~), type (~~))

type Cat k = k -> k -> Type

type Category :: forall k. Cat k -> Constraint
class Category @k p where
    type Ob p :: k -> Constraint

type Functor :: forall j k. (j -> k) -> Constraint
class (Category (Dom f), Category (Cod f)) => Functor @j @k f where
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
