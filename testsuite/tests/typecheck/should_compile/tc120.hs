-- !!! Check that we can have a type for main that is more general than IO a

-- main :: forall a.a   certainly also has type IO a, so it should be fine.

module Main(main) where

main :: a
main = error "not much luck"
