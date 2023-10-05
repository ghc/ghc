{-# LANGUAGE RecursiveDo, LambdaCase, BangPatterns #-}

import Control.Monad.Fix
import Data.IORef
import System.Mem.Weak
import System.Mem

import Control.Monad
import Data.Foldable

data Pull = Pull
  { weakSelf  :: Weak Pull
  , compute :: Weak Pull -> IO Int
  , invalidators :: IORef [Weak Pull]
  , cached  :: IORef (Maybe Int)
  }


makePull :: (Weak Pull -> IO Int) -> IO Pull
makePull f = do
  rec
    -- This seems to be the culprit, changing the order makes the weakRef get gc'ed
    -- In this configuration it crashes

    !foo <- Pull weak f <$> newIORef [] <*> newIORef Nothing
    weak <- mkWeakPtr foo Nothing

  return foo


invalidate :: Pull -> IO ()
invalidate p = do
  writeIORef (cached p) Nothing
  invs <- readIORef (invalidators p)
  writeIORef (invalidators p) []
  traverse_ (deRefWeak >=> traverse_ invalidate) invs


pull :: Weak Pull -> Pull -> IO Int
pull weak p = do
  modifyIORef (invalidators p) (weak :)
  pull' p

pull' :: Pull -> IO Int
pull' p = do
  readIORef (cached p) >>= \case
    Nothing -> do
      r <- compute p (weakSelf p)
      writeIORef (cached p) (Just r)
      return r

    Just r -> return r

add :: Pull -> Int -> IO (Pull)
add p n = makePull (\w -> (+n) <$> pull w p)

main = do
  h <- newIORef 0

  source <- makePull (const $ readIORef h)
  p <- foldM add source (take 1000 (repeat 1))   -- 100 is not enough for crash

  forM_ [1..10] $ \i -> do

    writeIORef h i
    invalidate source -- Crashes here on second iteration

    --performGC
    -- This avoids the crash

    print =<< pull' p






