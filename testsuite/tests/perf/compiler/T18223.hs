{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE Strict #-}

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
