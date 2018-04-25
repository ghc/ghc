{-# LANGUAGE DeriveDataTypeable #-}
module UnitTests.Distribution.Client.JobControl (tests) where

import Distribution.Client.JobControl

import Data.List
import Data.Maybe
import Data.IORef
import Control.Monad
import Control.Concurrent (threadDelay)
import Control.Exception  (Exception, try, throwIO)
import Data.Typeable      (Typeable)
import qualified Data.Set as Set

import Test.Tasty
import Test.Tasty.QuickCheck hiding (collect)


tests :: [TestTree]
tests =
  [ testGroup "serial"
      [ testProperty "submit batch"       prop_submit_serial
      , testProperty "submit batch"       prop_remaining_serial
      , testProperty "submit interleaved" prop_interleaved_serial
      , testProperty "concurrent jobs"    prop_concurrent_serial
      , testProperty "cancel"             prop_cancel_serial
      , testProperty "exceptions"         prop_exception_serial
      ]
  , testGroup "parallel"
      [ testProperty "submit batch"       prop_submit_parallel
      , testProperty "submit batch"       prop_remaining_parallel
      , testProperty "submit interleaved" prop_interleaved_parallel
      , testProperty "concurrent jobs"    prop_concurrent_parallel
      , testProperty "cancel"             prop_cancel_parallel
      , testProperty "exceptions"         prop_exception_parallel
      ]
  ]


prop_submit_serial :: [Int] -> Property
prop_submit_serial xs =
  ioProperty $ do
    jobCtl <- newSerialJobControl
    prop_submit jobCtl xs

prop_submit_parallel :: Positive (Small Int) -> [Int] -> Property
prop_submit_parallel (Positive (Small maxJobLimit)) xs =
  ioProperty $ do
    jobCtl <- newParallelJobControl maxJobLimit
    prop_submit jobCtl xs

prop_remaining_serial :: [Int] -> Property
prop_remaining_serial xs =
  ioProperty $ do
    jobCtl <- newSerialJobControl
    prop_remaining jobCtl xs

prop_remaining_parallel :: Positive (Small Int) -> [Int] -> Property
prop_remaining_parallel (Positive (Small maxJobLimit)) xs =
  ioProperty $ do
    jobCtl <- newParallelJobControl maxJobLimit
    prop_remaining jobCtl xs

prop_interleaved_serial :: [Int] -> Property
prop_interleaved_serial xs =
  ioProperty $ do
    jobCtl <- newSerialJobControl
    prop_submit_interleaved jobCtl xs

prop_interleaved_parallel :: Positive (Small Int) -> [Int] -> Property
prop_interleaved_parallel (Positive (Small maxJobLimit)) xs =
  ioProperty $ do
    jobCtl <- newParallelJobControl maxJobLimit
    prop_submit_interleaved jobCtl xs

prop_submit :: JobControl IO Int -> [Int] -> IO Bool
prop_submit jobCtl xs = do
    mapM_ (\x -> spawnJob jobCtl (return x)) xs
    xs' <- mapM (\_ -> collectJob jobCtl) xs
    return (sort xs == sort xs')

prop_remaining :: JobControl IO Int -> [Int] -> IO Bool
prop_remaining jobCtl xs = do
    mapM_ (\x -> spawnJob jobCtl (return x)) xs
    xs' <- collectRemainingJobs jobCtl
    return (sort xs == sort xs')

collectRemainingJobs :: Monad m => JobControl m a -> m [a]
collectRemainingJobs jobCtl = go []
  where
    go xs = do
      remaining <- remainingJobs jobCtl
      if remaining
        then do x <- collectJob jobCtl
                go (x:xs)
        else return xs

prop_submit_interleaved :: JobControl IO (Maybe Int) -> [Int] -> IO Bool
prop_submit_interleaved jobCtl xs = do
    xs' <- sequence
      [ spawn >> collect
      | let spawns   = map (\x -> spawnJob jobCtl (return (Just x))) xs
                    ++ repeat (return ())
            collects = replicate 5 (return Nothing)
                    ++ map (\_ -> collectJob jobCtl) xs
      , (spawn, collect) <- zip spawns collects
      ]
    return (sort xs == sort (catMaybes xs'))

prop_concurrent_serial :: NonNegative (Small Int) -> Property
prop_concurrent_serial (NonNegative (Small ntasks)) =
  ioProperty $ do
    jobCtl   <- newSerialJobControl
    countRef <- newIORef (0 :: Int)
    replicateM_ ntasks (spawnJob jobCtl (task countRef))
    counts   <- replicateM ntasks (collectJob jobCtl)
    return $ length counts == ntasks
          && all (\(n0, n1) -> n0 == 0 && n1 == 1) counts
  where
    task countRef = do
      n0 <- atomicModifyIORef countRef (\n -> (n+1, n))
      threadDelay 100
      n1 <- atomicModifyIORef countRef (\n -> (n-1, n))
      return (n0, n1)

prop_concurrent_parallel :: Positive (Small Int) -> NonNegative Int -> Property
prop_concurrent_parallel (Positive (Small maxJobLimit)) (NonNegative ntasks) =
  ioProperty $ do
    jobCtl   <- newParallelJobControl maxJobLimit
    countRef <- newIORef (0 :: Int)
    replicateM_ ntasks (spawnJob jobCtl (task countRef))
    counts   <- replicateM ntasks (collectJob jobCtl)
    return $ length counts == ntasks
          && all (\(n0, n1) -> n0 >= 0 && n0 <  maxJobLimit
                            && n1 >  0 && n1 <= maxJobLimit) counts
             -- we do hit the concurrency limit (in the right circumstances)
          && if ntasks >= maxJobLimit*2 -- give us enough of a margin
               then any (\(_,n1) -> n1 == maxJobLimit) counts
               else True
  where
    task countRef = do
      n0 <- atomicModifyIORef countRef (\n -> (n+1, n))
      threadDelay 100
      n1 <- atomicModifyIORef countRef (\n -> (n-1, n))
      return (n0, n1)

prop_cancel_serial :: [Int] -> [Int] -> Property
prop_cancel_serial xs ys =
  ioProperty $ do
    jobCtl <- newSerialJobControl
    mapM_ (\x -> spawnJob jobCtl (return x)) (xs++ys)
    xs' <- mapM (\_ -> collectJob jobCtl) xs
    cancelJobs jobCtl
    ys' <- collectRemainingJobs jobCtl
    return (sort xs == sort xs' && null ys')

prop_cancel_parallel :: Positive (Small Int) -> [Int] -> [Int] -> Property
prop_cancel_parallel (Positive (Small maxJobLimit)) xs ys = do
  ioProperty $ do
    jobCtl <- newParallelJobControl maxJobLimit
    mapM_ (\x -> spawnJob jobCtl (threadDelay 100  >> return x)) (xs++ys)
    xs' <- mapM (\_ -> collectJob jobCtl) xs
    cancelJobs jobCtl
    ys' <- collectRemainingJobs jobCtl
    return $ Set.fromList (xs'++ys') `Set.isSubsetOf` Set.fromList (xs++ys)

data TestException = TestException Int
  deriving (Typeable, Show)

instance Exception TestException

prop_exception_serial :: [Either Int Int] -> Property
prop_exception_serial xs =
  ioProperty $ do
    jobCtl <- newSerialJobControl
    prop_exception jobCtl xs

prop_exception_parallel :: Positive (Small Int) -> [Either Int Int] -> Property
prop_exception_parallel (Positive (Small maxJobLimit)) xs =
  ioProperty $ do
    jobCtl <- newParallelJobControl maxJobLimit
    prop_exception jobCtl xs

prop_exception :: JobControl IO Int -> [Either Int Int] -> IO Bool
prop_exception jobCtl xs = do
    mapM_ (\x -> spawnJob jobCtl (either (throwIO . TestException) return x)) xs
    xs' <- replicateM (length xs) $ do
             mx <- try (collectJob jobCtl)
             return $ case mx of
               Left (TestException n) -> Left  n
               Right               n  -> Right n
    return (sort xs == sort xs')

