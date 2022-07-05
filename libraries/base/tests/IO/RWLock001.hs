{-# language LambdaCase, DerivingStrategies #-}

import qualified GHC.Event.RWLock as RWLock

import Control.Concurrent
import Control.Exception
import Data.Void
import Data.Monoid
import Control.Monad
import Control.Monad.Fix
import Data.IORef
import Data.Functor

data Problem = Problem String
  deriving stock (Eq, Show)

instance Exception Problem

check_more_negative :: Int -> Int -> IO ()
check_more_negative x y
  | x < y = pure ()
  | otherwise = throwIO $ Problem $ "check_more_negative " <> show (x,y)

main :: IO ()
main = do
  l <- RWLock.new
  ref <- newIORef (1, [1..1000000])
  writers <- forM [0.10] $ \_ -> do
    mv <- newEmptyMVar
    tid <- forkIO $ do
      r <- try $ let
        loop = do
          b <- RWLock.withWriteLock l $ do
            (z1, xs) <- readIORef ref
            check_more_negative 0 z1
            case xs of
              [] -> pure False
              x : xss -> do
                modifyIORef' ref (\(z,xs) -> (negate z - x, xss))
                threadDelay 1
                modifyIORef' ref (\(z,xs) -> (negate z, xs))
                readIORef ref >>= \(z,_) -> check_more_negative z1 z
                threadDelay 1
                pure True
          when b loop
        in loop
      putMVar mv r
    pure (mv, tid)

  readers <- forM [0..100] $ \_ -> do
    mv <- newEmptyMVar
    tid <- forkIO $ do
      r <- try $ let
        loop = do
          b <- RWLock.withReadLock l $ do
            mb_v <- atomicModifyIORef' ref $ \case
              x@(z, [])  -> (x, Nothing)
              (z, x : xss) -> ((z + x, xss), Just z)
            case mb_v of
              Nothing -> pure False
              Just z -> do
                check_more_negative 0 z
                pure True
          when b loop
        in loop
      putMVar mv r
    pure (mv, tid)

  r <- getAp $ flip foldMap (readers ++ writers) $
    \(mv, tid) -> Ap $ takeMVar mv <&> \case
      Left e | Just ThreadKilled <- fromException e  -> mempty
      Left e -> [e]
      Right {} -> mempty

  readIORef ref >>= print . (,r)
