-- Trac #1806

module ShouldFail where

data Foo = (:::)

foo (x ::: y) = ()
