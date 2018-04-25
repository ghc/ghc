{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

-- Results in context reduction stack overflow

module Class1 where

class C a where
  foo :: a x -> a y

class C (T a) => D a where
  type T a :: * -> *

  bar :: a -> T a x -> T a y

instance C Maybe where
  foo Nothing = Nothing

instance D () where
  type T () = Maybe

  bar x t = foo t
