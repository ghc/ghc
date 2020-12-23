{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}


module NoFieldSelectors
where

import Prelude


data Foo = Foo { foo :: Int, bar :: String }

{-# ANN foo () #-}
foo = 3 -- should not conflict

fooX = foo + 1

rwcPatFoo Foo{..} = show (foo, bar)
rwcConFoo = Foo{..} where
  foo = 42
  bar = "hello"

foo1 :: Foo
foo1 = Foo 3 "bar"

foo2 = Foo { foo = 3, bar = "bar" } -- disambiguate foo

-- foo3 = foo1 { foo = 4 } -- currently rejected, see #18999

foo4 = foo1 { bar = "baz" } -- bar is unambiguous
