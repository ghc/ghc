{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeOperators, TypeFamilies #-}
module Bug722 where

class Foo a where
  (!@#) :: a -> a -> a
infixl 4 !@#

type family (&*) :: * -> * -> *
infixr 3 &*

data a :-& b = a :^& b
infixl 6 :-&, :^&

