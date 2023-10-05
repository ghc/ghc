module Main(main) where
import Data82
import Inst82_1
import Inst82_2

data Baz = Baz deriving Read

main     = print ((read "FooData")::FooData)

