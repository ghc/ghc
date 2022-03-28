{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bug where

data Foo a b where
  Foo :: Bar a b c -> Foo as c -> Foo (a,as) b

data Bar a b c where

data HList xs where
  Nil :: HList ()
  Cons :: x -> HList xs -> HList (x,xs)

data EqualLists xs ys where
  Refl :: EqualLists () ()
  ConsRefl :: EqualLists xs ys -> EqualLists (x,xs) (x,ys)

foo :: EqualLists xs ys -> HList xs -> HList ys -> Foo xs b -> Foo ys b
foo (ConsRefl equal) (Cons _ (xs :: HList xs)) (Cons _ (ys :: HList ys)) f =
  let k :: forall d. Foo xs d -> Foo ys d
      k = foo equal xs ys
  in case f of
    Foo bar -> Foo bar $ k foo
