module T13290 where

data Foo = Bar Int Char | Baz Char

{-# RULES
"BarBaz" Bar 0 'a' = Baz 'b'
 #-}
