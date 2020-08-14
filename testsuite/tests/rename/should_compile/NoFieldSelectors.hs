{-# LANGUAGE NoFieldSelectors #-}

module NoFieldSelectors
where

import Prelude

data Foo = Foo { foo :: Int, bar :: String }
-- data Bar = Bar { foo :: Int, baz :: String }

foo = 3 -- should not conflict
fooX = foo + 1

foo1 = Foo 3 "bar"
foo2 = Foo { foo = 3, bar = "bar" } -- disambiguate foo
foo3 = foo1 { foo = 4 } -- update
foo4 = foo1 { bar = "baz" } -- bar is unambiguous