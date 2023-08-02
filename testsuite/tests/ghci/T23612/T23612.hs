module T23612 where

-- | This will be inlined into @f2@.
-- Then @a@, @x@, and @y@ will be floated out as constants using @3@ for @a@.
-- @x@ and @y@ get a breakpoint around the RHS, which is then inlined and
-- retains a reference to @a@.
--
-- Since the actual terms in @x@ and @y@ are now constants, the dependency
-- analysis for fingerprinting in Recomp doesn't register @a@ as a free variable
-- anymore.
-- But when the fingerprints are computed, the breakpoint triggers a lookup of
-- @a@ (called @f2_a@ then), which fails.
--
-- The fix was to include the FVs in the dependencies in @freeNamesIfExpr@.
-- This has the side effect that the floated out @a@ will still remain in the
-- program.
f1 :: Int -> (Int, Int)
f1 a =
    let x = a + 1
        y = a * 2
    in  (x, y)

f2 = f1 3
