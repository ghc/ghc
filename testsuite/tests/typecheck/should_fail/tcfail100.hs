-- This one broke GHC 5.02, because of the unsaturated
-- uses of type synonyms, which are nevertheless kind-correct.

module ShouldCompile where

type A i = i
type B = A
