-- | The function here exploit matches of arity 2 that split the uncovered set
-- in two. Too many for -fmax-pmcheck-models=0!
-- As a result, these functions elicit the symptoms describe in the warnings
-- message, e.g.
--   - False positives on exhaustivity
--   - Turns redundant into inaccessible clauses
--   - Fails to report redundant matches
module TooManyDeltas where

data T = A | B

-- | Reports that a clause for _ _ is missing.
f :: T -> T -> ()
f A A = ()

-- | Reports that the third clause is inaccessible, when really it is
-- redundant.
g :: T -> T -> ()
g _ A = ()
g A A = () -- inaccessible, correctly flagged
g A A = () -- redundant, not inaccessible!
g _ _ = () -- (this one is not about exhaustivity)

-- | Fails to report that the second clause is redundant.
h :: T -> T -> ()
h A A = () -- covered, emits no warning
h A A = () -- redundant, not covered!
h _ _ = () -- (this one is not about exhaustivity)
