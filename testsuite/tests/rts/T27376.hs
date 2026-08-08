-- Regression test for #27376: Nonmoving write barrier for array copies

{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns #-}

module Main where

import GHC.Exts
import GHC.IO
import Control.Monad
import Control.Concurrent
import System.Mem
import Data.IORef

data MA = MA (MutableArray# RealWorld Node)
data Node = Node !Int Node | Nil

mkNode :: Int -> Node
mkNode i = Node i (Node (i + 1000000) Nil)

checkNode :: Int -> Node -> Bool
checkNode i (Node a (Node b Nil)) = a == i && b == i + 1000000
checkNode _ _ = False

newA :: Int -> Node -> IO MA
newA (I# n) x = IO $ \s -> case newArray# n x s of (# s', a #) -> (# s', MA a #)

readA :: MA -> Int -> IO Node
readA (MA a) (I# i) = IO (readArray# a i)

writeA :: MA -> Int -> Node -> IO ()
writeA (MA a) (I# i) x = IO $ \s -> (# writeArray# a i x s, () #)

copyMA :: MA -> Int -> MA -> Int -> Int -> IO ()
copyMA (MA src) (I# so) (MA dst) (I# d) (I# n) =
  IO $ \s -> (# copyMutableArray# src so dst d n s, () #)

-- spin without allocating; distinct seeds keep the calls from being shared
spin :: Int -> Int -> Int
spin seed m = go m seed
  where go 0 !acc = acc
        go j !acc = go (j-1) (acc + j)
{-# NOINLINE spin #-}

opaque :: a -> IO a
opaque x = newIORef x >>= readIORef
{-# NOINLINE opaque #-}

main :: IO ()
main = do
  let n     = 100000  -- array size
      iters = 50      -- passes over the array
  k  <- opaque 1024   -- chunk size (dynamic copyMutableArray# length)
  sp <- opaque 300000 -- busy-wait per chunk
  arr  <- newA n Nil
  junk <- newA n Nil
  forM_ [0..n-1] $ \i -> writeA arr i (mkNode i)
  -- promote everything into the nonmoving heap
  performMajorGC
  performMajorGC
  -- background thread to keep GC cycles coming
  done <- newIORef False
  _ <- forkIO $ let go = do d <- readIORef done
                            unless d $ do
                              performMajorGC
                              threadDelay 3000
                              go
                in go
  errs <- newIORef (0::Int)
  forM_ [1..iters] $ \t -> do
    -- one pass over the array in chunks of k
    let go lo | lo + k > n = return ()
              | otherwise = do
            -- 1. load elements (refs only in this list + the array)
            saved <- mapM (readA arr) [lo .. lo+k-1]
            -- 2. delete them from the array with a dynamic-length copy
            copyMA junk 0 arr lo k
            -- 3. widen the window
            _ <- evaluate' (spin lo sp)
            -- 4. write them back
            zipWithM_ (\i x -> writeA arr i x) [lo .. lo+k-1] saved
            go (lo + k)
    go 0
    -- validate
    forM_ [0 .. n-1] $ \i -> do
      x <- readA arr i
      unless (checkNode i x) $ do
        modifyIORef' errs (+1)
        putStrLn ("CORRUPT at t=" ++ show t ++ " i=" ++ show i)
    e <- readIORef errs
    when (e > 0) $ do
      writeIORef done True
      putStrLn ("FAILED after iteration " ++ show t)
      error "corruption detected"
  writeIORef done True
  putStrLn "OK"
  where
    evaluate' x = IO $ \s -> seq# x s
{-# NOINLINE main #-}
