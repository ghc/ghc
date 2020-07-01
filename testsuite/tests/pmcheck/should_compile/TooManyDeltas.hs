-- | The function here exploit matches of arity 2 that split the uncovered set
-- in two. Too many for -fmax-pmcheck-models=0!
-- As a result, these functions elicit the symptoms describe in the warnings
-- message, e.g.
--   - False positives on exhaustivity
--   - Fails to report redundant matches
--
-- We used to turn redundant into inaccessible clauses, but SG was unable to
-- produce a testcase. See the comment below.
module TooManyDeltas where

data T = A | B

-- | Reports that a clause for _ _ is missing.
f :: T -> T -> ()
f A A = ()

-- SG: As of July 2020, g doesn't reproduce anymore.
-- Because we treat constructor matches lazily and push data con match
-- strictness into a preceding bang guard, The single place that calls the
-- throttling function will not regress in laziness. Note that by throttling we
-- can only "forget" the x /~ K constraint from unc_this, not the preceding
-- x /~ ⊥ constraint.

-- | Reports that the third clause is inaccessible, when really it is
-- redundant.
g :: T -> T -> ()
g _ A = ()
g A A = () -- inaccessible, correctly flagged
g A A = () -- redundant, used to be inaccessible (see above).
g _ _ = () -- (this one is not about exhaustivity)

-- | Fails to report that the second clause is redundant.
h :: T -> T -> ()
h A A = () -- covered, emits no warning
h A A = () -- redundant, not covered!
h _ _ = () -- (this one is not about exhaustivity)
