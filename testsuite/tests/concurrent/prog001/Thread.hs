
module Thread (threadTesting1) where

import Control.Concurrent
import Control.Concurrent.MVar
import Stream
import Converter

threadTesting1 :: Gray -> Gray -> IO Int
threadTesting1 xs ys = do
		    m <- newEmptyMVar
		    c1 <- forkIO (t1 m xs ys)
		    c2 <- forkIO (t2 m xs ys)
		    c3 <- forkIO (t3 m xs ys)
		    c4 <- forkIO (t4 m xs ys)
		    c5 <- forkIO (t5 m xs ys)
		    c6 <- forkIO (t6 m xs ys)
		    c7 <- forkIO (t7 m xs ys)
		    c8 <- forkIO (t8 m xs ys)
		    c9 <- forkIO (t9 m xs ys)
		    c <- takeMVar m
		    killThread c1
		    killThread c2
		    killThread c3
		    killThread c4
		    killThread c5
		    killThread c6
		    killThread c7
		    killThread c8
		    killThread c9
		    return c








t1 :: MVar Int -> Stream -> Stream -> IO()
t1 m (0:0:x) (0:0:y) = putMVar m 101
t1 m (0:0:x) (0:1:y) = putMVar m 102
t1 m (0:0:x) (1:0:y) = putMVar m 103
t1 m (0:0:x) (1:1:y) = putMVar m 104

t1 m (0:1:x) (0:0:y) = putMVar m 201
t1 m (0:1:x) (0:1:y) = putMVar m 202
t1 m (0:1:x) (1:0:y) = putMVar m 203
t1 m (0:1:x) (1:1:y) = putMVar m 204

t1 m (1:0:x) (0:0:y) = putMVar m 103
t1 m (1:0:x) (0:1:y) = putMVar m 104
t1 m (1:0:x) (1:0:y) = putMVar m 101
t1 m (1:0:x) (1:1:y) = putMVar m 102


t1 m (1:1:x) (0:0:y) = putMVar m 203
t1 m (1:1:x) (0:1:y) = putMVar m 204
t1 m (1:1:x) (1:0:y) = putMVar m 201
t1 m (1:1:x) (1:1:y) = putMVar m 202

			
t2 :: MVar Int -> Stream -> Stream -> IO()
t2 m (0:0:x) (b:1:0:y) = putMVar m 30
t2 m (1:0:x) (b:1:0:y) = putMVar m 31						
t2 m (0:1:x) (b:1:0:y) = putMVar m 60
t2 m (1:1:x) (b:1:0:y) = putMVar m 61
t2 m x y = yield


t3 m (0:0:x) (0:b:1:y) = putMVar m 40
t3 m (1:0:x) (1:b:1:y) = putMVar m 40
t3 m (0:0:x) (1:b:1:y) = putMVar m 41
t3 m (1:0:x) (0:b:1:y) = putMVar m 41

t3 m (0:1:x) (0:b:1:y) = putMVar m 50
t3 m (1:1:x) (1:b:1:y) = putMVar m 50
t3 m (0:1:x) (1:b:1:y) = putMVar m 51
t3 m (1:1:x) (0:b:1:y) = putMVar m 51
t3 m x y = yield

t4 m (0:a:1:y) (0:0:x) = putMVar m 70
t4 m (1:a:1:y) (1:0:x) = putMVar m 70
t4 m (1:a:1:y) (0:0:x) = putMVar m 70
t4 m (0:a:1:y) (1:0:x) = putMVar m 70
t4 m (0:a:1:y) (0:1:x) = putMVar m 70
t4 m (1:a:1:y) (1:1:x) = putMVar m 70
t4 m (1:a:1:y) (0:1:x) = putMVar m 70
t4 m (0:a:1:y) (1:1:x) = putMVar m 70
t4 m x y = yield


t5 m (a:1:0:y) (0:0:x) = putMVar m 70
t5 m (a:1:0:y) (1:0:x) = putMVar m 70
t5 m (a:1:0:y) (0:1:x) = putMVar m 70
t5 m (a:1:0:y) (1:1:x) = putMVar m 70
t5 m x y = yield

t6 m (0:a:1:x) (0:b:1:y) = putMVar m 80
t6 m (1:a:1:x) (1:b:1:y) = putMVar m 80
t6 m (0:a:1:x) (1:b:1:y) = putMVar m 81
t6 m (1:a:1:x) (0:b:1:y) = putMVar m 81
t6 m x y = yield

t7 m (0:a:1:x) (b:1:0:y) = putMVar m 90
t7 m (1:a:1:x) (b:1:0:y) = putMVar m 91
t7 m x y = yield

t8 m (a:1:0:x) (b:1:0:y) = putMVar m 100
t8 m x y = yield

t9 m (a:1:0:x) (0:b:1:y) = putMVar m 70
t9 m (a:1:0:x) (1:b:1:y) = putMVar m 70
t9 m x y = yield
