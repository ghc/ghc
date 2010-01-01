-- !!! Tests garbage collection in the branch of a case
-- !!!	alternative where the constructor is returned in the heap.

{- This is also a rather stressful test for another reason.
   The mutual recursion between munch and f causes lots of
   closures to be built, of the form (munch n s), for some n and s.
   Now, all of these closures are entered and each has as its value
   the result delivere by the next; so the result is that there is
   a massive chain of identical updates.

   As it turns out, they are mostly garbage, so the GC could eliminate
   them (though this isn't implemented at present), but that isn't
   necessarily the case.  

   The only correct solution is to spot that the updates are all
   updating with the same value (update frames stacked on top of each
   other), and update all but one with indirections to the last
   remaining one.  This could be done by GC, or at the moment the
   frame is pushed.

   Incidentally, hbc won't have this particular problem, because it
   updates immediately.

   NOTE: [March 97]  Now that stack squeezing happens when GC happens,
   the stack is squished at GC.  So this program uses a small stack
   in a small heap (eg 4m heap 2m stack), but in a big heap (no GC)
   it needs a much bigger stack (10m)!  It would be better to try GC/stack
   squeezing on stack oflo.
-}

module Main where

main = munch 100000 (inf 3)

data Stream a
  = MkStream a a a a a a a a a (Stream a)
  | Empty

inf :: Int -> Stream Int
inf n = MkStream n n n n n n n n n (inf n)

munch :: Int -> Stream a -> IO ()

munch n Empty = return () -- error "this never happens!\n"
    -- this first equation mks it non-strict in "n"
    -- (NB: call the "error" makes it strict)

munch 0 _ = putStr "I succeeded!\n"
munch n s = case (f n s) of
	      (True, rest) -> rest
	      (False, _)   -> error "this never happens either\n"

--f :: Int -> Stream a -> (Bool, [Request])

f n (MkStream _ _ _ _ _ _ _ _ _ rest)
  = -- garbage collection *HERE*, please!
    -- (forced by the closure for n-1)
    (True, munch (n - 1) rest)

-- munch and f are mutually recursive, just to be nasty
