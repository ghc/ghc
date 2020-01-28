-- To ensure we miss the mapM specialization we:
-- * Prevent a unfolding with NOINLINE
-- * Turn of dicts-strict, to keep a dictionary as argument.

{-# OPTIONS_GHC -fno-dicts-strict #-}

module T16282A where

{-# NOINLINE myMapM_ #-}
myMapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
myMapM_ f = Prelude.foldr c (return ())
  -- See Note [List fusion and continuations in 'c']
  where c x k = f x >> k
        {-# INLINE c #-}
