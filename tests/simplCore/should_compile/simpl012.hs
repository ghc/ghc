-- This is a version of tc095.hs, which makes the compiler 
-- loop (with -O) because of the recursive data-type bug, 
-- unless you have the NOINLINE (which Happy does,
-- as it happens).

-- Keywords: diverge, contravariant, fixpoint

module ShouldSucceed where

happyParse = happyFail 7 7 (error "reading EOF!") (HappyState happyFail) [] [] []

data HappyState b c = HappyState
        (Int ->                         -- token number
         Int ->                         -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)

{-# NOINLINE happyFail #-}
happyFail _ i tk st@(HappyState action) sts stk = action (-1) (-1) tk st sts ( stk)


