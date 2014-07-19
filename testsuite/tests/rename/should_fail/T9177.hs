module T9177 where

-- the main use case
type Foo = (int)

-- other interesting cases
type Foo2 = (integerr)

foo3 = bar
foo4 = Fun

-- this warning is suboptimal (fun would be illegal here)
foo5 Fun = ()

-- No errors here:
data Bar = Bar
fun x = x
