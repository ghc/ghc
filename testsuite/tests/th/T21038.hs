{-# LANGUAGE TemplateHaskell #-}
module T21038 where

data Foo = MkFoo (forall a. a -> a)

worksOnAllGHCs1 :: Foo
worksOnAllGHCs1 = MkFoo (\x -> x)

worksOnAllGHCs2 :: Foo
worksOnAllGHCs2 = MkFoo $ \x -> x

worksOnAllGHCs3 :: Foo
worksOnAllGHCs3 = $([| MkFoo |]) (\x -> x)

doesn'tWorkOnGHC9'2'1 :: Foo
doesn'tWorkOnGHC9'2'1 = $([| MkFoo |]) $ \x -> x
