-- The type sig for 'test' is illegal in H98 because of the
-- partial application of the type sig.
-- But with the LiberalTypeSynonyms extension enabled it
-- should be OK because when you expand the type synonyms
-- it's just Int->Int
-- c.f should_compile/tc155.hs
module ShouldFail where

type Thing m = m ()

type Const a b = a

test :: Thing (Const Int) -> Thing (Const Int)
test = test
