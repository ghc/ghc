-- See SourceForge bug 807249 

-- Dies because the instance decl has type variables with
-- kind *, whereas the constraint (Show (x -> Bool)) has x::??
-- Kind of stupid, really, but awkward to fix.

module ShouldCompile where

instance Show (a->b)

foo x = show (\ _ -> True)
