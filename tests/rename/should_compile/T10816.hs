{-# LANGUAGE TypeOperators, TypeFamilies #-}
module T10816 where

class C a where
  type a # b
  infix 4 #

  type a *** b
  type a +++ b
  infixr 5 ***, +++
  (***), (+++) :: a -> a -> a
