{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module T15050 where
data P a = P
data T1 a where
  MkT1a :: forall a.              P a               -> T1 a
  MkT1b :: forall a.              P a               -> T1 a
  MkT1c :: forall a.              P a               -> T1 a
  MkT2  :: forall a.              P a               -> T1 (a,a)
  MkT3  :: forall a b. b ~ Int => P a -> P b        -> T1 a
  MkT4  :: forall a b.            P a -> P b        -> T1 a
  MkT5  :: forall a b c. b ~ c => P a -> P b -> P c -> T1 a

foo :: T1 (Int, Int) -> ()
foo (MkT1a (P::P (Int,Int)))    = ()
foo (MkT1b (P::P (Int,x)))      = (() :: x ~ Int => ())
foo (MkT1c (P::P x))            = (() :: x ~ (Int,Int) => ())
foo (MkT2  (P::P x))            = (() :: x ~ Int => ())
foo (MkT3  P (P::P Int))        = ()
foo (MkT4  P (P::P b))          = ()
foo (MkT5  P (P::P b) (P::P b)) = ()
