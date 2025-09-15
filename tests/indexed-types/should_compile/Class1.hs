{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

-- Results in context reduction stack overflow

module Class1 where

import Data.Kind (Type)

class C a where
  foo :: a x -> a y

class C (T a) => D a where
  type T a :: Type -> Type

  bar :: a -> T a x -> T a y

instance C Maybe where
  foo Nothing = Nothing

instance D () where
  type T () = Maybe

  bar x t = foo t
