--!!! Testing the garbage collector
module TestGC where

-- All these tests should be run in a freshly started system
-- and with the specified heap size/ heap configuration.
-- 
-- (Of course, they should run successfully in a non-fresh system,
-- with different heap sizes, etc. - but they've been known to fail
-- with the specified size.)


-- build Hugs with all gc tests turned on and run with a small heap.

-- 27/11/95: This test works fine - but fails when entered on the
--   command line.  The difference must be that the top level
--   thunk isn't being treated as a root by the GC system.
-- 3/6/96: Requires 210kbyte heap to run - which is double the size of
--         the string it generates.  This has to get stored since
--         test1 is a CAF and the 2-space GC doubles the requirement.
--         If evaluated on the command line, it runs in 16kbytes
--         which is about the smallest possible heap given the
--         setting of minRecovery (1000), the size of a cell (8 bytes)
--         and the GC's need for two equally size semispaces.
test1 = show [1..1500]

-- 27/11/95: This test produces different results on command line
--   and when executed as given.  Again, I think I'm failing to make
--   the top-level object a root.
-- 20/5/96: This test runs out of space - I think black holing would fix it.
-- 3/6/96:  Now works fine.  Nothing to do with blackholing!  All I had to do
--          was restore Mark's definitions of sum and product.  These used
--          foldl' which is a strict version of foldl.
test2 = show (sum [1..100000])

