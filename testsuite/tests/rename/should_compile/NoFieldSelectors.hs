{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}


module NoFieldSelectors
where

import Prelude


data Foo = Foo { foo :: Int, bar :: String }
data Bar = Bar { foo :: Int, bar' :: String }

foo = 3 -- should not conflict

fooX = foo + 1

rwcPatFoo Foo{..} = show (foo, bar)
rwcConFoo = Foo{..} where
  foo = 42
  bar = "hello"

foo1 :: Foo
foo1 = Foo 3 "bar"

foo2 = Foo { foo = 3, bar = "bar" } -- disambiguate foo

foo4 = foo1 { bar = "baz" } -- bar is unambiguous

bar0 = Bar { foo = 0, bar' = "bar'" }
