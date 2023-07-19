{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module T21077 where

import Language.Haskell.TH.Syntax
import System.IO
import T21077_Lib

worksOnAllGHCs1 :: Foo
worksOnAllGHCs1 = MkFoo () (\x -> x)

worksOnAllGHCs2 :: Foo
worksOnAllGHCs2 = MkFoo () $ \x -> x

-- TemplateHaskell

worksOnAllGHCs3 :: Foo
worksOnAllGHCs3 = $([| MkFoo () |]) (\x -> x)

doesn'tWorkOnGHC9'2A :: Foo
doesn'tWorkOnGHC9'2A = $([| MkFoo () |]) $ \x -> x

doesn'tWorkOnGHC9'2B :: Foo
doesn'tWorkOnGHC9'2B = $([| $([| MkFoo () |]) |]) $ \x -> x

doesn'tWorkOnGHC9'2C :: Foo
doesn'tWorkOnGHC9'2C = $(do addModFinalizer (runIO (hPutStrLn stderr "C"))
                            [| MkFoo () |]) $ \x -> x

doesn'tWorkOnGHC9'2D :: Foo
doesn'tWorkOnGHC9'2D = $(do addModFinalizer (runIO (hPutStrLn stderr "D2"))
                            [| $(do addModFinalizer (runIO (hPutStrLn stderr "D1"))
                                    [| MkFoo () |])
                             |]) $ \x -> x

-- QuasiQuotes

worksOnAllGHCs4 :: Foo
worksOnAllGHCs4 = [qq| doesn't matter |] (\x -> x)

doesn'tWorkOnGHC9'2E :: Foo
doesn'tWorkOnGHC9'2E = [qq| doesn't matter |] $ \x -> x

doesn'tWorkOnGHC9'2F :: Foo
doesn'tWorkOnGHC9'2F = [qq| doesn't matter |] $ \x -> x

doesn'tWorkOnGHC9'2G :: Foo
doesn'tWorkOnGHC9'2G = [qqMod1| doesn't matter |] $ \x -> x

doesn'tWorkOnGHC9'2H :: Foo
doesn'tWorkOnGHC9'2H = [qqMod2| doesn't matter |] $ \x -> x
