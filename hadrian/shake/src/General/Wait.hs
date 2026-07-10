{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}

-- | A bit like 'Fence', but not thread safe and optimised for avoiding taking the fence
module General.Wait(
    Wait(Now,Later), runWait, quickly, fromLater,
    firstJustWaitUnordered, firstLeftWaitUnordered
    ) where

import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.IORef.Extra
import Data.List.Extra
import Data.Primitive.Array
import GHC.Exts(RealWorld)
import Control.Monad.Fail
import Prelude


runWait :: Monad m => Wait m a -> m (Wait m a)
runWait (Lift x) = runWait =<< x
runWait x = pure x

fromLater :: Monad m => Wait m a -> (a -> m ()) -> m ()
fromLater (Lift x) f = do x <- x; fromLater x f
fromLater (Now x) f = f x
fromLater (Later x) f = x f

quickly :: Functor m => m a -> Wait m a
quickly = Lift . fmap Now

data Wait m a = Now a
              | Lift (m (Wait m a))
              | Later ((a -> m ()) -> m ())
                deriving Functor

instance (Monad m, Applicative m) => Applicative (Wait m) where
    pure = Now
    Now x <*> y = x <$> y
    Lift x <*> y = Lift $ (<*> y) <$> x
    Later x <*> Now y = Later $ \c -> x $ \x -> c $ x y
    -- Note: We pull the Lift from the right BEFORE the Later, to enable parallelism
    Later x <*> Lift y = Lift $ do y <- y; pure $ Later x <*> y
    Later x <*> Later y = Later $ \c -> x $ \x -> y $ \y -> c $ x y

instance (Monad m, Applicative m) => Monad (Wait m) where
    return = pure
    (>>) = (*>)
    Now x >>= f = f x
    Lift x >>= f = Lift $ do x <- x; pure $ x >>= f
    Later x >>= f = Later $ \c -> x $ \x -> do
        x <- runWait $ f x
        case x of
            Now x -> c x
            _ -> fromLater x c

instance (MonadIO m,  Applicative m) => MonadIO (Wait m) where
    liftIO = Lift . liftIO . fmap Now

instance MonadFail m => MonadFail (Wait m) where
    fail = Lift . Control.Monad.Fail.fail

firstJustWaitUnordered :: MonadIO m => (a -> Wait m (Maybe b)) -> [a] -> Wait m (Maybe b)
firstJustWaitUnordered f = go 0 [] . map f
    where
        -- keep a list of those things we might visit later, and ask for each we see in turn
        go :: MonadIO m => Int -> [(Maybe a -> m ()) -> m ()] -> [Wait m (Maybe a)] -> Wait m (Maybe a)
        go !nlater later (x:xs) = case x of
            Now (Just a) -> Now $ Just a
            Now Nothing -> go nlater later xs
            Later l -> go (succ nlater) (l:later) xs
            Lift x -> Lift $ do
                x <- x
                pure $ go nlater later (x:xs)
        go _ [] [] = Now Nothing
        go _ [l] [] = Later l
        go nls ls [] = Later $ \callback -> do
            ref <- liftIO $ newIORef nls
            forM_ ls $ \l -> l $ \r -> do
                old <- liftIO $ readIORef ref
                when (old > 0) $ case r of
                    Just a -> do
                        liftIO $ writeIORef' ref 0
                        callback $ Just a
                    Nothing -> do
                        liftIO $ writeIORef' ref $ old-1
                        when (old == 1) $ callback Nothing


firstLeftWaitUnordered :: MonadIO m => (a -> Wait m (Either e b)) -> [a] -> Wait m (Either e [b])
firstLeftWaitUnordered f xs = do
        let n = length xs
        mut <- liftIO $ newArray n undefined
        res <- go mut [] $ zipFrom 0 $ map f xs
        case res of
            Just e -> pure $ Left e
            Nothing -> liftIO $ Right <$> mapM (readArray mut) [0..n-1]
    where
        -- keep a list of those things we might visit later, and ask for each we see in turn
        go :: MonadIO m => MutableArray RealWorld b -> [(Int, (Either e b -> m ()) -> m ())] -> [(Int, Wait m (Either e b))] -> Wait m (Maybe e)
        go mut later ((i,x):xs) = case x of
            Now (Left e) -> Now $ Just e
            Now (Right b) -> do
                liftIO $ writeArray mut i b
                go mut later xs
            Later l -> go mut ((i,l):later) xs
            Lift x -> Lift $ do
                x <- x
                pure $ go mut later ((i,x):xs)
        go _ [] [] = Now Nothing
        go mut ls [] = Later $ \callback -> do
            ref <- liftIO $ newIORef $ length ls
            forM_ ls $ \(i,l) -> l $ \r -> do
                old <- liftIO $ readIORef ref
                when (old > 0) $ case r of
                    Left a -> do
                        liftIO $ writeIORef' ref 0
                        callback $ Just a
                    Right v -> do
                        liftIO $ writeArray mut i v
                        liftIO $ writeIORef' ref $ old-1
                        when (old == 1) $ callback Nothing
