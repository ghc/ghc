-- test1 and test2 should have the same Core.
-- Until GHC 8.2, test1 was faster than test2.
{-# LANGUAGE FlexibleContexts #-}

module T14062 (test1, test2) where

import Control.Monad.State.Strict
import Control.Monad.Identity

repeatM :: Monad m => m a -> Int -> m ()
repeatM f = go where
    go 0 = pure ()
    go i = f >> go (i - 1)
{-# INLINE repeatM #-}

incState :: MonadState Int m => m ()
incState = modify' (1+) ; {-# INLINE incState #-}

test1, test2 :: Int -> Int
test1 = \n -> (runIdentity . flip evalStateT 0 . (\a -> repeatM incState a >> get)) n ; {-# INLINE test1 #-}
test2 = \n -> runIdentity . flip evalStateT 0 $ repeatM incState n >> get ; {-# INLINE test2 #-}
