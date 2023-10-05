module Test where

class C s where
   foo :: (Int -> Int) -> s -> s

instance C Int where
   foo = undefined --prevent warning

bar _ = baz where
   baz :: C s => s -> s
   baz = foo baz
