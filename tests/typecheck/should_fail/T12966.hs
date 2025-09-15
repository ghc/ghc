{-# LANGUAGE RankNTypes #-}
module T12966 where
-- This should fail with a proper error message, not a compiler panic.
type Maybeify c = forall d. (c d) => ((~) (Maybe d))
