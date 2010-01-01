-- For testing +RTS -xc
import Control.Exception
main = try (evaluate (f ()))

f x = g x

g x = error (show x)
