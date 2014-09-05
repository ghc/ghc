{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-

Exercising avoidance of known landmines.

We need one each of

  PostTc id Kind
  PostTc id Type

  PostRn id Fixity
  PostRn id NameSet


-}
module MineKind where

data HList :: [*] -> * where
  HNil  :: HList '[]
  HCons :: a -> HList t -> HList (a ': t)

data Tuple :: (*,*) -> * where
  Tuple :: a -> b -> Tuple '(a,b)
