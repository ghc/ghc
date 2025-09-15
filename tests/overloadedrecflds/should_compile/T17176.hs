{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DuplicateRecordFields #-}
module T17176 (Foo(Bar,bar,Baz)) where

data Foo =
    Bar    { bar :: Int }
  | BadBaz { baz :: Int }

pattern Baz :: Int -> Foo
pattern Baz{baz} = BadBaz baz

pattern Woz :: Int -> Foo
pattern Woz{baz} = Baz{baz=baz}

foo = Baz { baz = 42 }
woo (Woz{baz=z}) = z
