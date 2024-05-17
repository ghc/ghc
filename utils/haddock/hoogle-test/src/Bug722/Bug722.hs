{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeOperators, TypeFamilies #-}
module Bug722 where

import Data.Kind (Type)

class Foo a where
  (!@#) :: a -> a -> a
infixl 4 !@#

type family (&*) :: Type -> Type -> Type
infixr 3 &*

data a :-& b = a :^& b
infixl 6 :-&, :^&

