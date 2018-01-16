-- See trac bug 179

-- Gives a bogus type error
--    No instance for (Show (t -> Bool))
--      arising from use of `show' at tc175.hs:11:8-11
--    In the definition of `foo': foo x = show (\ _ -> True)
-- because the instance decl has type variables with
-- kind *, whereas the constraint (Show (x -> Bool)) has x::??
-- Kind of stupid, really, but awkward to fix.

module ShouldCompile where

instance Show (a->b)

foo x = show (\ _ -> True)
