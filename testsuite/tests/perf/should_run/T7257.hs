{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.ByteString as S
import Data.IORef
import Control.Monad

makeBs :: Int -> S.ByteString
makeBs n = S.replicate n (fromIntegral n)

doStuff :: IORef [S.ByteString] -> Int -> IO ()
doStuff ref n = do
    let !bs = makeBs n
    modifyIORef ref (bs:)
{-# NOINLINE doStuff #-}

undo :: IORef [S.ByteString] -> IO ()
undo ref = do
    h <- atomicModifyIORef ref (\(x:xs) -> (xs,x))
    S.length h `seq` return ()

main = do
    ref <- newIORef [S.empty]
    let fn n = do
        doStuff ref n
        when (rem 5 n /= 0 ) $ undo ref
        
    mapM_ fn (take 5000000 $ cycle [1..100])
    var <- readIORef ref
    print $ length var
