-- To ensure we miss the mapM specialization we:
-- * Prevent a unfolding with NOINLINE
-- * Turn of dicts-strict, to keep a dictionary as argument.

{-# OPTIONS_GHC -fno-dicts-strict #-}

module T16282A where

import Data.Map as M (Map, toList)

newtype MyMap v = MyMap (Map Int v)

instance (Show a) => Show (MyMap a) where
  showsPrec d (MyMap m)  = showParen (d > 10) $
    showString "fromList " . shows (M.toList m)

{-# NOINLINE myMapM_ #-}
myMapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
myMapM_ f = Prelude.foldr c (return ())
  -- See Note [List fusion and continuations in 'c']
  where c x k = f x >> k
        {-# INLINE c #-}
