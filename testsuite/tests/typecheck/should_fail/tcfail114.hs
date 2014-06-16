-- Killed GHC 5.04.2

module ShouldFail where

class Foo a where
  foo :: a -> ()

data Bar = Bar { bar :: () }

test :: Bar
test = undefined { foo = () }
	-- The point is that foo is a class method,
	-- but not a record selector
