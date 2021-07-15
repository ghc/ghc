-- | A vendored version of the 'Concurrently' monad from 'async' which
-- also has a Monad instance.
module GHC.Utils.Monad.Concurrently where

import GHC.Prelude
import Control.Monad.IO.Class
import qualified GHC.Exception as MC
import qualified Control.Monad.Catch as MC
import Control.Exception
import Data.IORef
import Control.Monad
import Control.Concurrent

newtype Concurrently a = Concurrently { runConcurrently :: IO a }

instance Functor Concurrently where
  fmap f (Concurrently a) = Concurrently $ f <$> a

instance Applicative Concurrently where
  pure = Concurrently . return
  Concurrently fs <*> Concurrently as =
    Concurrently $ (\(f, a) -> f a) <$> concurrently fs as
instance Monad Concurrently where
    return = pure
    x >>= f = Concurrently $ runConcurrently x >>= (runConcurrently . f)

instance MonadIO Concurrently where
  liftIO m = Concurrently m

-- concurrently :: IO a -> IO b -> IO (a,b)
concurrently :: IO a -> IO b -> IO (a, b)
concurrently left right = concurrently' left right (collect [])
  where
    collect [Left a, Right b] _ = return (a,b)
    collect [Right b, Left a] _ = return (a,b)
    collect xs m = do
        e <- m
        case e of
            Left ex -> throwIO ex
            Right r -> collect (r:xs) m

concurrently' :: IO a -> IO b
             -> (IO (Either MC.SomeException (Either a b)) -> IO r)
             -> IO r
concurrently' left right collect = do
    done <- newEmptyMVar
    MC.mask $ \restore -> do
        -- Note: uninterruptibleMask here is because we must not allow
        -- the putMVar in the exception handler to be interrupted,
        -- otherwise the parent thread will deadlock when it waits for
        -- the thread to terminate.
        lid <- forkIO $ MC.uninterruptibleMask_ $
          restore (left >>= putMVar done . Right . Left)
            `MC.catchAll` (putMVar done . Left)
        rid <- forkIO $ MC.uninterruptibleMask_ $
          restore (right >>= putMVar done . Right . Right)
            `MC.catchAll` (putMVar done . Left)

        count <- newIORef (2 :: Int)
        let takeDone = do
                r <- takeMVar done      -- interruptible
                -- Decrement the counter so we know how many takes are left.
                -- Since only the parent thread is calling this, we can
                -- use non-atomic modifications.
                -- NB. do this *after* takeMVar, because takeMVar might be
                -- interrupted.
                modifyIORef count (subtract 1)
                return r

        let tryAgain f = f `MC.catch` \BlockedIndefinitelyOnMVar -> f

            stop = do
                -- kill right before left, to match the semantics of
                -- the version using withAsync. (#27)
                MC.uninterruptibleMask_ $ do
                  count' <- readIORef count
                  -- we only need to use killThread if there are still
                  -- children alive.  Note: forkIO here is because the
                  -- child thread could be in an uninterruptible
                  -- putMVar.
                  when (count' > 0) $
                    void $ forkIO $ do
                      throwTo rid ThreadKilled
                      throwTo lid ThreadKilled
                  -- ensure the children are really dead
                  replicateM_ count' (tryAgain $ takeMVar done)

        r <- collect (tryAgain $ takeDone) `onException` stop
        stop
        return r

