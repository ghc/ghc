module Main where
import T3983_Foo
import T3983_Bar

main = catchX (foo False True) >>= print