module M where

-- There should be a cost center in core prep output but not in
-- -ddump-simpl output with -fprof-late
{-# INLINE doStuff #-}
doStuff x = do
    print x
    return x
