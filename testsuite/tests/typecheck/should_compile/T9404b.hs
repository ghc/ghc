{-# LANGUAGE RankNTypes, TypeFamilies #-}

module T9404b where

type family ListTF x where
  ListTF x = [x]

bar :: (forall x. ListTF x -> Int) -> ()
bar _ = ()

myconst :: ((forall r. ListTF r -> Int) -> ()) -> x -> (forall r. ListTF r -> Int) -> ()
myconst x _ = x

-- foo = (myconst bar ()) $ length
foo = (bar `myconst` ()) $ length
-- foo2 = (myconst bar ()) $ length
