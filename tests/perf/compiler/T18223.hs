{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE Strict #-}

{- Notes on T18223
~~~~~~~~~~~~~~~~~~
If we inline
      modify' :: MonadState s m => (s -> s) -> m ()
early, before the specialiser, the casts all collapse immediately.
It turns out that fixing #21286 causes this to happen, because
we no longer w/w modify'.

If we don't inline it before the specialiser we generate
a specialised version of it.  Then it gets inlined and all
the casts collapse, but we end up keeping the code for the
specialised version right through the pipeline.
-}

import Control.Monad.State

tester :: MonadState a m => m ()
tester = modify' id

-- manyState :: StateT () (StateT () IO) () -> IO ()
-- manyState :: _ -> IO ()
manyState x =
  (flip evalStateT ()     -- 1
  . flip evalStateT ()    -- 2
  . flip evalStateT ()    -- 3
  . flip evalStateT ()    -- 4
  . flip evalStateT ()    -- 5
  . flip evalStateT ()    -- 6
  . flip evalStateT ()    -- 7
  . flip evalStateT ()    -- 8
  . flip evalStateT ()    -- 9
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()

  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()

  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()

  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  . flip evalStateT ()
  ) x :: IO ()

main :: IO ()
main = manyState tester >>= print
