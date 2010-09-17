{-# LANGUAGE EmptyDataDecls, TypeFamilies, TypeOperators, GADTs, KindSignatures #-}

module T3330c where

data (f :+: g) x = Inl (f x) | Inr (g x)

data R :: (* -> *) -> * where
  RSum  :: R f -> R g -> R (f :+: g)

class Rep f where
  rep :: R f

instance (Rep f, Rep g) => Rep (f :+: g) where
  rep = RSum rep rep

type family Der (f :: * -> *) :: * -> *
type instance Der (f :+: g) = Der f :+: Der g

plug :: Rep f => Der f x -> x -> f x
plug = plug' rep where
  plug' :: R f -> Der f x -> x -> f x
  plug' (RSum rf rg) (Inl df) x = Inl (plug rf df x)
