-- fails core-lint in 6.2
module ShouldCompile where

newtype Foo = Foo [Foo]
newtype Bar = Bar Foo

unBar :: Bar -> Foo
unBar (Bar x) = x
