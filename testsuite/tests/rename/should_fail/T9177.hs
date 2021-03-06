module T9177 where

-- the main use case
type Foo = (int)

-- other interesting cases
type Foo2 = (integerr)

-- These two out-of-scope errors aren't caught until
-- the type checker, so they aren't reported at all
-- because the renamer aborts compilation
foo3 = bar
foo4 = Fun

-- this warning was suboptimal (fun would be illegal here) as of #9177
-- fixed with #19843
foo5 Fun = ()

-- No errors here:
data Bar = Bar
fun x = x
