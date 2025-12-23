module Main (main) where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar)
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Exception (SomeException, evaluate, try)
import Data.Array (Array, listArray)
import Data.Array.IO (IOArray, newArray)
import Data.IORef (IORef, newIORef)
import GHC.DeepSeq (force, forceIO, forceST)
import GHC.Compact (compactWithSharing, getCompact)
import GHC.ST (runST)
import System.Mem.Weak (mkWeak)
import System.Timeout (timeout)

deepEvaluate :: a -> IO a
deepEvaluate a = do
  (_, a') <- forceIO a
  pure a'

deepEvaluateWithFlag :: a -> IO (Bool, a)
deepEvaluateWithFlag = forceIO

mkThunk :: Int -> Int
mkThunk x = x + 1
{-# NOINLINE mkThunk #-}

boomVal :: Int
boomVal = error "boom"
{-# NOINLINE boomVal #-}

funVal :: Int -> Int
funVal _ = boomVal
{-# NOINLINE funVal #-}

main :: IO ()
main = do
  r1 <- try (deepEvaluate (1 :: Int, error "boom") >> pure ()) :: IO (Either SomeException ())
  case r1 of
    Left _  -> putStrLn "thunk-forced"
    Right _ -> putStrLn "unexpected-no-exn"

  r2 <- try (deepEvaluate funVal) :: IO (Either SomeException (Int -> Int))
  case r2 of
    Left _ -> putStrLn "unexpected-exn"
    Right _ -> putStrLn "fun-ok"

  (forced2, ()) <- deepEvaluateWithFlag ()
  if not forced2
    then putStrLn "noforce-ok"
    else putStrLn "noforce-bad"

  x <- evaluate (42 :: Int)
  (forced2b, _) <- deepEvaluateWithFlag x
  if not forced2b
    then putStrLn "noforce-int-ok"
    else putStrLn "noforce-int-bad"

  let (forced2c, _) = force x
  if not forced2c
    then putStrLn "force-int-ok"
    else putStrLn "force-int-bad"

  let (forced2d, _) = runST (forceST x)
  if not forced2d
    then putStrLn "forcest-int-ok"
    else putStrLn "forcest-int-bad"

  let v = (1 :: Int, mkThunk 2)
  (forced3, v') <- deepEvaluateWithFlag v
  if forced3 && snd v' == 3
    then putStrLn "thunk-ok"
    else putStrLn "unexpected"

  let arr :: Array Int Int
      arr = listArray (0, 0) [boomVal]
  r3 <- try (deepEvaluate arr >> pure ()) :: IO (Either SomeException ())
  case r3 of
    Left _  -> putStrLn "array-thunk-forced"
    Right _ -> putStrLn "array-unforced"

  ioArr <- newArray (0, 0) boomVal :: IO (IOArray Int Int)
  r4 <- try (deepEvaluate ioArr >> pure ()) :: IO (Either SomeException ())
  case r4 of
    Left _  -> putStrLn "ioarray-thunk-forced"
    Right _ -> putStrLn "ioarray-unforced"

  ref <- newIORef boomVal :: IO (IORef Int)
  r5 <- try (deepEvaluate ref >> pure ()) :: IO (Either SomeException ())
  case r5 of
    Left _  -> putStrLn "ioref-thunk-forced"
    Right _ -> putStrLn "ioref-unforced"

  mvar <- newEmptyMVar :: IO (MVar Int)
  putMVar mvar boomVal
  r6 <- try (deepEvaluate mvar >> pure ()) :: IO (Either SomeException ())
  case r6 of
    Left _  -> putStrLn "mvar-thunk-forced"
    Right _ -> putStrLn "mvar-unforced"

  tvar <- newTVarIO boomVal :: IO (TVar Int)
  r6b <- try (deepEvaluate tvar >> pure ()) :: IO (Either SomeException ())
  case r6b of
    Left _  -> putStrLn "tvar-thunk-forced"
    Right _ -> putStrLn "tvar-unforced"

  keyRef <- newIORef ()
  weak <- mkWeak keyRef boomVal Nothing
  r7 <- try (deepEvaluate weak >> pure ()) :: IO (Either SomeException ())
  case r7 of
    Left _  -> putStrLn "weak-thunk-forced"
    Right _ -> putStrLn "weak-unforced"

  let cyclic :: [Int]
      cyclic = let xs = 1 : xs in xs
  compacted <- compactWithSharing cyclic
  let cyclic' = getCompact compacted
  _ <- evaluate cyclic'
  r8 <- timeout 2000000 (deepEvaluateWithFlag cyclic')
  case r8 of
    Nothing -> putStrLn "compact-loop-timeout"
    Just _ -> putStrLn "compact-loop-ok"
