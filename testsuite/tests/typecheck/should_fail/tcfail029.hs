-- !!! tests for InstOpErr
module ShouldFail where

data Foo = Bar | Baz

f x = x > Bar
