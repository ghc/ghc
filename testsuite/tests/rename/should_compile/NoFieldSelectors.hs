{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}


module NoFieldSelectors
where

import Prelude

data Foo = Foo { foo :: Int, bar :: String }
data Bar = Bar { barX :: Int, barY :: String }

foo = 3 -- should not conflict
fooX = foo + 1

{-
NoFieldSelectors.hs:17:32: error:
    • Variable not in scope: bar
    • Perhaps you meant one of these:
        ‘bar’ (line 17), data constructor ‘Bar’ (line 11)

NoFieldSelectors.hs:18:17: error:
    • Variable not in scope: bar :: String
    • Perhaps you meant one of these:
        ‘bar’ (line 20), data constructor ‘Bar’ (line 11)

rwcPatFoo Foo{..} = show (foo, bar)
rwcConFoo = Foo{..} where
  foo = 42
  bar = "hello"
-}

foo1 = Foo 3 "bar"
foo2 = Foo { foo = 3, bar = "bar" } -- disambiguate foo
foo3 = foo1 { foo = 4 } -- update
foo4 = foo1 { bar = "baz" } -- bar is unambiguous

barY' :: Int
barY' = 0 -- barY
{-
NoFieldSelectors.hs:27:9: error:
    • Couldn't match expected type ‘Int’
                  with actual type ‘Bar -> String’
    • In the expression: barY
      In an equation for ‘barY'’: barY' = barY
-}