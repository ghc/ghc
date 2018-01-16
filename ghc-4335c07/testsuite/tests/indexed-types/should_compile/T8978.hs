{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module T8978 where

type Syn a = Associated a

class Eq (Associated a) => Foo a where
    type Associated a :: *
    foo :: a -> Syn a -> Bool

instance Foo () where
    type Associated () = Int
    foo _ x = x == x
