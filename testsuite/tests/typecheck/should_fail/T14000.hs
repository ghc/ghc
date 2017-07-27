{-# LANGUAGE TypeFamilies #-}
module T14000 where

class C a where
    type T a
    c :: a -> T a

foo = c noSuchThing   -- noSuchThing is not in scope
