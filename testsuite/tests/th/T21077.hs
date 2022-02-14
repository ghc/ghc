{-# LANGUAGE TemplateHaskell #-}
module T21077 where

import Language.Haskell.TH.Syntax

data Foo = MkFoo () (forall a. a -> a)

worksOnAllGHCs1 :: Foo
worksOnAllGHCs1 = MkFoo () (\x -> x)

worksOnAllGHCs2 :: Foo
worksOnAllGHCs2 = MkFoo () $ \x -> x

worksOnAllGHCs3 :: Foo
worksOnAllGHCs3 = $([| MkFoo () |]) (\x -> x)

doesn'tWorkOnGHC9'2A :: Foo
doesn'tWorkOnGHC9'2A = $([| MkFoo () |]) $ \x -> x

doesn'tWorkOnGHC9'2B :: Foo
doesn'tWorkOnGHC9'2B = $([| $([| MkFoo () |]) |]) $ \x -> x

doesn'tWorkOnGHC9'2C :: Foo
doesn'tWorkOnGHC9'2C = $(do addModFinalizer (runIO (putStrLn "C"))
                            [| MkFoo () |]) $ \x -> x

doesn'tWorkOnGHC9'2D :: Foo
doesn'tWorkOnGHC9'2D = $(do addModFinalizer (runIO (putStrLn "D2"))
                            [| $(do addModFinalizer (runIO (putStrLn "D1"))
                                    [| MkFoo () |])
                             |]) $ \x -> x
