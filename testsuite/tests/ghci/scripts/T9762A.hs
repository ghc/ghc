module T9762A (a) where
-- By marking a INLINE, we create a reference from B to A's tickboxes.
{-# INLINE a #-}
a :: Int
a = 3
