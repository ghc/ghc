-- This one broke GHC 5.02, because of the unsaturated
-- uses of type synonyms, which are nevertheless kind-correct.

module ShouldCompile where

type A i = i
data T k = MkT (k Int)

f :: T A	-- BAD!
f = error "foo"
