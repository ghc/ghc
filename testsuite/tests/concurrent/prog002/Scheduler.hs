module Scheduler 
( runTIO
, module Event
, module Thread
, TTree
, TIO
) 
where

import Event
import Thread
import Control.Concurrent
import System.IO

--------------------------------
type TTree = ThreadTree SysReq SysRsp IO
type TIO   = ContM SysReq SysRsp IO


runTIO :: [TIO ()] -> IO ()
runTIO l = runThreads $ map buildThread l 

data World = World 
    { mReadyQ :: ! (Chan (TTree))  }

max_steps = 1
worker_pure world= 
  do 
      t <- readChan readyq
      case t of
        (Atom _) -> return ()
        _ -> return ()
      exec_thread max_steps t
      return ()
 where
 readyq = mReadyQ world

 exec_thread 0 t =
  do putStr "."; hFlush stdout
     writeChan readyq t
 exec_thread c (Atom mx) = 
     do 
        x <- mx
        exec_thread (c-1) x
 exec_thread c (Stop) = return ()
 
runThreads :: [TTree] -> IO ()
runThreads l =
  do 
     mready <- newChan
     writeList2Chan mready l
     let world = World mready 
     multiloop world

loop_p world = do worker_pure world; loop_p world

multiloop world = 
  do 
	-- a mixture of bound threads & lightweight threads 
	-- to make things interesting...
     forkOS (loop_p world)
     forkOS (loop_p world)
     forkOS (loop_p world)
     forkOS (loop_p world)
     forkOS (loop_p world)
     forkOS (loop_p world)
     forkIO (loop_p world)
     forkIO (loop_p world)
     forkIO (loop_p world)
     forkIO (loop_p world)
     forkIO (loop_p world)
     forkIO (loop_p world)
     forkIO (loop_p world)
     loop_p world
