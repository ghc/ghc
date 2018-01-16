{-# LANGUAGE GADTs #-}

module Tdpe where

data Type t where
    TBase :: Type Base
    TFun  :: Type a -> Type b -> Type (a -> b)

b :: Type Base
b = TBase

newtype Base = In { out :: Term Base }

data Term t where
    App :: Term (a->b) -> Term a -> Term b
    Fun :: (Term a -> Term b) -> Term (a->b)

reify :: Type t -> t -> Term t
reify (TBase) v    = out v
reify (TFun a b) v = Fun (\x -> reify b (v (reflect a x)))

reflect :: Type t -> Term t -> t
reflect (TBase) e    = In e
reflect (TFun a b) e = \x -> reflect b (App e (reify a x))
