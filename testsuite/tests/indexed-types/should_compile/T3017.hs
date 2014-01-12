{-# LANGUAGE TypeFamilies #-}

-- Trac #3017

module Foo where
 class Coll c where
   type Elem c
   empty :: c
   insert :: Elem c -> c -> c

 data ListColl a = L [a]
 instance Coll (ListColl a) where
   type Elem (ListColl a) = a
   empty = L []
   insert x (L xs) = L (x:xs)

 emptyL :: ListColl a
 emptyL = empty

 test2 c = insert (0, 0) c
