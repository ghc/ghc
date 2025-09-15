-- | The following match demonstrates why we need to detect cyclic solutions
-- when extending 'GHC.HsToCore.PmCheck.Oracle.tm_pos'.
--
-- TLDR; solving @x :-> y@ where @x@ is the representative of @y@'s equivalence
-- class can easily lead to a cycle in the substitution.
module CyclicSubst where

-- | The match is translated to @b | a <- b@, the initial unification variable
-- is @a@ (for some reason). VarVar will assign @b :-> a@ in the match of @a@
-- against @b@ (vars occurring in a pattern are flexible). The @PmGrd a b@ is
-- desugared as a match of @$pm_x@ against @a@, where @$pm_x :-> b@, which is
-- stored as @$pm_x :-> a@ due to the previous solution. Now, VarVar will
-- assign @a :-> $pm_x@, causing a cycle.
foo :: Int -> Int
foo a@b = a + b
