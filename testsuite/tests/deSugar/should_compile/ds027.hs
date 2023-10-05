-- !!! ds027 -- simple instances
--
module ShouldCompile where

data Foo = Bar | Baz

instance Eq Foo where
  Bar == Baz = True
  Bar /= Baz = False
