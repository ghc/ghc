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

{-
rf :: R f1, rg :: R g1
Given by GADT match: f ~ f1 :+: g1

Second arg has type (Der f x)
    = (Der (f1:+:g1) x)
    = (:+:) (Der f1) (Der g1) x
Hence df :: Der f1 x

Inl {f3,g3,x} (plug {f2,x1} rf df x)  gives rise to
  result of Inl:   ((:+:) f3 g3 x ~ f x)
  first arg (rf):  (R f1 ~ Der f2 x1)
  second arg (df): (Der f1 x ~ x1)
  result of plug:  (f2 x1 ~ x -> f3 x)

  result of Inl: ((:+:) f3 g3 x ~ f x)
       by given  ((:+:) f3 g3 x ~ (:+:) f1 g1 x)
  hence need f3~f1, g3~g1

So we are left with
   first arg:      (R f1 ~ Der f2 x1)
   second arg:     (Der f1 x ~ x1)
   result:         (f2 x1  ~  (->) x (f3 x))

Decompose result:
          f2 ~ (->) x
          x1 ~ f1 x
Hence
        first:  R f1 ~ Der ((->) x) (f1 x)
  decompose  : R ~ Der ((->) x)
               f1 ~ f1 x


-}