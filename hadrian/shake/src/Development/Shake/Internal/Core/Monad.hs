{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs, ScopedTypeVariables, TupleSections, GeneralizedNewtypeDeriving #-}

module Development.Shake.Internal.Core.Monad(
    RAW, Capture, runRAW,
    getRO, getRW, putRW, modifyRW,
    stepRAW,
    catchRAW, tryRAW, throwRAW, finallyRAW,
    captureRAW,
    ) where

import Control.Exception.Extra
import Development.Shake.Internal.Errors
import Control.Monad.IO.Class
import Data.IORef
import Control.Monad
import System.IO
import Data.Semigroup
import Control.Monad.Fail
import Prelude


data RAW k v ro rw a where
    Fmap :: (a -> b) -> RAW k v ro rw a -> RAW k v ro rw b
    Pure :: a -> RAW k v ro rw a
    Ap :: RAW k v ro rw (a -> b) -> RAW k v ro rw a -> RAW k v ro rw b
    Next :: RAW k v ro rw a -> RAW k v ro rw b -> RAW k v ro rw b
    Bind :: RAW k v ro rw a -> (a -> RAW k v ro rw b) -> RAW k v ro rw b
    LiftIO :: IO a -> RAW k v ro rw a
    GetRO :: RAW k v ro rw ro
    GetRW :: RAW k v ro rw rw
    PutRW :: !rw -> RAW k v ro rw ()
    ModifyRW :: (rw -> rw) -> RAW k v ro rw ()
    StepRAW :: k -> RAW k v ro rw v
    CaptureRAW :: Capture (Either SomeException a) -> RAW k v ro rw a
    CatchRAW :: RAW k v ro rw a -> (SomeException -> RAW k v ro rw a) -> RAW k v ro rw a

instance Functor (RAW k v ro rw) where
    fmap = Fmap

instance Applicative (RAW k v ro rw) where
    pure = Pure
    (*>) = Next
    (<*>) = Ap

instance Monad (RAW k v ro rw) where
    return = pure
    (>>) = (*>)
    (>>=) = Bind

instance MonadIO (RAW k v ro rw) where
    liftIO = LiftIO

instance MonadFail (RAW k v ro rw) where
    fail = liftIO . Control.Monad.Fail.fail

instance Semigroup a => Semigroup (RAW k v ro rw a) where
    (<>) a b = (<>) <$> a <*> b

instance (Semigroup a, Monoid a) => Monoid (RAW k v ro rw a) where
    mempty = pure mempty
    mappend = (<>)


type Capture a = (a -> IO ()) -> IO ()


-- Useful for checking that all continuations are run only once
-- Cannot be enabled for performance reasons and because some of
-- "monad test" deliberately breaks the invariant to check it doesn't go wrong
assertOnceCheck = False

assertOnce :: MonadIO m => String -> (a -> m b) -> IO (a -> m b)
assertOnce msg k
    | not assertOnceCheck = pure k
    | otherwise = do
        ref <- liftIO $ newIORef False
        pure $ \v -> do
            liftIO $ join $ atomicModifyIORef ref $ \old -> (True,) $ when old $ do
                hPutStrLn stderr "FATAL ERROR: assertOnce failed"
                Prelude.fail $ "assertOnce failed: " ++ msg
            k v

-- | Run and then call a continuation.
runRAW :: ([k] -> RAW k v ro rw [v]) -> ro -> rw -> RAW k v ro rw a -> Capture (Either SomeException a)
runRAW step ro rw m k = do
    k <- assertOnce "runRAW" k
    rw <- newIORef rw
    handler <- newIORef throwIO
    steps <- newSteps
    writeIORef handler $ \e -> do
        -- make sure we never call the error continuation twice
        writeIORef handler throwIO
        k $ Left e
    -- If the continuation itself throws an error we need to make sure we
    -- don't end up running it twice (once with its result, once with its own exception)
    goRAW step steps handler ro rw m (\v -> do writeIORef handler throwIO; k $ Right v)
        `catch_` \e -> ($ e) =<< readIORef handler


goRAW :: forall k v ro rw a . ([k] -> RAW k v ro rw [v]) -> Steps k v -> IORef (SomeException -> IO ()) -> ro -> IORef rw -> RAW k v ro rw a -> Capture a
goRAW step steps handler ro rw = \x k -> go x $ \v -> sio v k
    where
        sio :: SIO b -> Capture b
        sio (SIO v) k = flush $ do v <- v; k v

        flush :: IO () -> IO ()
        flush k = do
            v <- flushSteps steps
            case v of
                Nothing -> k
                Just f -> go (f step) $ const k

        unflush :: IO ()
        unflush = unflushSteps steps

        go :: RAW k v ro rw b -> Capture (SIO b)
        go x k = case x of
            Fmap f a -> go a $ \v -> k $ fmap f v
            Pure a -> k $ pure a
            Ap f x -> go f $ \f -> go x $ \v -> k $ f <*> v
            Next a b -> go a $ \a -> go b $ \b -> k $ a *> b
            StepRAW q -> do
                v <- addStep steps q
                k v

            Bind a b -> go a $ \a -> sio a $ \a -> go (b a) k
            LiftIO act -> flush $ do v <- act; k $ pure v

            GetRO -> k $ pure ro
            GetRW -> flush $ k . pure =<< readIORef rw
            PutRW x -> flush $ writeIORef rw x >> k (pure ())
            ModifyRW f -> flush $ modifyIORef' rw f >> k (pure ())

            CatchRAW m hdl -> flush $ do
                hdl <- assertOnce "CatchRAW" hdl
                old <- readIORef handler
                writeIORef handler $ \e -> do
                    writeIORef handler old
                    go (hdl e) k `catch_`
                        \e -> do unflush; ($ e) =<< readIORef handler
                go m $ \x -> writeIORef handler old >> k x

            CaptureRAW f -> flush $ do
                f <- assertOnce "CaptureRAW" f
                old <- readIORef handler
                writeIORef handler throwIO
                f $ \case
                    Left e -> old e
                    Right v -> do
                        writeIORef handler old
                        k (pure v) `catch_` \e -> do unflush; ($ e) =<< readIORef handler


newtype SIO a = SIO (IO a)
    deriving (Functor, Monad, Applicative)


newtype Steps k v = Steps (IORef [(k, IORef v)])

newSteps :: IO (Steps k v)
newSteps = Steps <$> newIORef []

addStep :: Steps k v -> k -> IO (SIO v)
addStep (Steps ref) k = do
    out <- newIORef $ throwImpure $ errorInternal "Monad, addStep not flushed"
    modifyIORef ref ((k,out):)
    pure $ SIO $ readIORef out

unflushSteps :: Steps k v -> IO ()
unflushSteps (Steps ref) = writeIORef ref []

flushSteps :: MonadIO m => Steps k v -> IO (Maybe (([k] -> m [v]) -> m ()))
flushSteps (Steps ref) = do
    v <- reverse <$> readIORef ref
    case v of
        [] -> pure Nothing
        xs -> do
            writeIORef ref []
            pure $ Just $ \step -> do
                vs <- step $ map fst xs
                liftIO $ zipWithM_ writeIORef (map snd xs) vs


---------------------------------------------------------------------
-- STANDARD

getRO :: RAW k v ro rw ro
getRO = GetRO

getRW :: RAW k v ro rw rw
getRW = GetRW

-- | Strict version
putRW :: rw -> RAW k v ro rw ()
putRW = PutRW

modifyRW :: (rw -> rw) -> RAW k v ro rw ()
modifyRW = ModifyRW


---------------------------------------------------------------------
-- EXCEPTIONS

catchRAW :: RAW k v ro rw a -> (SomeException -> RAW k v ro rw a) -> RAW k v ro rw a
catchRAW = CatchRAW

tryRAW :: RAW k v ro rw a -> RAW k v ro rw (Either SomeException a)
tryRAW m = catchRAW (fmap Right m) (pure . Left)

throwRAW :: Exception e => e -> RAW k v ro rw a
-- Note that while we could directly pass this to the handler
-- that would avoid triggering the catch, which would mean they built up on the stack
throwRAW = liftIO . throwIO

finallyRAW :: RAW k v ro rw a -> RAW k v ro rw b -> RAW k v ro rw a
finallyRAW a undo = do
    r <- catchRAW a (\e -> undo >> throwRAW e)
    undo
    pure r


---------------------------------------------------------------------
-- CONTINUATIONS

-- | Capture a continuation. The continuation should be called at most once.
--   Calling the same continuation, multiple times, in parallel, results in incorrect behaviour.
captureRAW :: Capture (Either SomeException a) -> RAW k v ro rw a
captureRAW = CaptureRAW


---------------------------------------------------------------------
-- STEPS

stepRAW :: k -> RAW k v ro rw v
stepRAW = StepRAW
