module Foo (f) where
-- export food
f x = x

-- !!! weird patterns with no variables
1  = f 1
[] = f []
1  = f (f 1)
[] = f (f [])
