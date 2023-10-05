module A(foo, Bar(..)) where

foo :: Bar
foo = MkBar 42

data Bar = MkBar Int
