-- !!! Catch an invalid Main.main type
module Main(main) where

main :: a
main = error "not much luck"
