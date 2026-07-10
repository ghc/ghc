{-# LANGUAGE RecordWildCards, TupleSections #-}

module Development.Shake.Internal.Core.Pool(
    addPoolWait, actionFenceSteal, actionFenceRequeue,
    actionAlwaysRequeue, actionAlwaysRequeuePriority,
    addPoolWait_,
    actionFenceRequeueBy
    ) where

import Control.Exception
import General.Pool
import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Core.Monad
import System.Time.Extra
import Data.Either.Extra
import Control.Monad.IO.Class
import General.Fence


priority x = if isLeft x then PoolException else PoolResume


-- | Enqueue an Action into the pool and return a Fence to wait for it.
--   Returns the value along with how long it spent executing.
addPoolWait :: PoolPriority -> Action a -> Action (Fence IO (Either SomeException (Seconds, a)))
addPoolWait pri act = do
    ro@Global{..} <- Action getRO
    rw <- Action getRW
    liftIO $ do
        fence <- newFence
        let act2 = do offset <- liftIO offsetTime; res <- act; offset <- liftIO offset; pure (offset, res)
        addPool pri globalPool $ runAction ro rw act2 $ signalFence fence
        pure fence

-- | Like 'addPoolWait' but doesn't provide a fence to wait for it - a fire and forget version.
--   Warning: If Action throws an exception, it would be lost, so must be executed with try. Seconds are not tracked.
addPoolWait_ :: PoolPriority -> Action a -> Action ()
addPoolWait_ pri act = do
    ro@Global{..} <- Action getRO
    rw <- Action getRW
    liftIO $ addPool pri globalPool $ runAction ro rw act $ \_ -> pure ()


actionFenceSteal :: Fence IO (Either SomeException a) -> Action (Seconds, a)
actionFenceSteal fence = do
    res <- liftIO $ testFence fence
    case res of
        Just (Left e) -> Action $ throwRAW e
        Just (Right v) -> pure (0, v)
        Nothing -> Action $ captureRAW $ \continue -> do
            offset <- offsetTime
            waitFence fence $ \v -> do
                offset <- offset
                continue $ (offset,) <$> v


actionFenceRequeue :: Fence IO (Either SomeException b) -> Action (Seconds, b)
actionFenceRequeue = actionFenceRequeueBy id

actionFenceRequeueBy :: (a -> Either SomeException b) -> Fence IO a -> Action (Seconds, b)
actionFenceRequeueBy op fence = Action $ do
    res <- liftIO $ testFence fence
    case fmap op res of
        Just (Left e) -> throwRAW e
        Just (Right v) -> pure (0, v)
        Nothing -> do
            Global{..} <- getRO
            offset <- liftIO offsetTime
            captureRAW $ \continue -> waitFence fence $ \v -> do
                let v2 = op v
                addPool (priority v2) globalPool $ do
                    offset <- offset
                    continue $ (offset,) <$> v2


actionAlwaysRequeue :: Either SomeException a -> Action (Seconds, a)
actionAlwaysRequeue res = actionAlwaysRequeuePriority (priority res) res

actionAlwaysRequeuePriority :: PoolPriority -> Either SomeException a -> Action (Seconds, a)
actionAlwaysRequeuePriority pri res = Action $ do
    Global{..} <- getRO
    offset <- liftIO offsetTime
    captureRAW $ \continue ->
        addPool pri globalPool $ do
            offset <- offset
            continue $ (offset,) <$> res
