{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeAbstractions #-}

module T15352 where

import Data.Kind

class C (x :: Type) (y :: k) where
  type F y


type Hom k = k -> k -> Type

type family Ob (p :: Hom k) :: k -> Constraint

type Functor' :: forall i j.
  (i -> Constraint) -> Hom i -> Hom i ->
  (j -> Constraint) -> Hom j -> Hom j ->
  (i -> j) -> Constraint
class ( obP ~ Ob p
      , opP ~ Dom p
      , obQ ~ Ob q
      , opQ ~ Dom q
      , p ~ Dom f
      , q ~ Cod f
      ) => Functor' @i @j obP opP p obQ opQ q f where
  type Dom f :: Hom i
  type Cod f :: Hom j
