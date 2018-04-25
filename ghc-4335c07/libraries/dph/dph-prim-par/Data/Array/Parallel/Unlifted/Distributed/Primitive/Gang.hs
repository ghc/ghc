{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}

-- If a work request is sent to the gang while another is already running
-- then just run it sequentially instead of dying.
#define SEQ_IF_GANG_BUSY 1

-- Trace all work requests sent to the gang.
#define TRACE_GANG 1

-- | Gang primitives.
module Data.Array.Parallel.Unlifted.Distributed.Primitive.Gang
        ( Gang
        , Workload      (..)
        , seqGang
        , forkGang
        , gangSize
        , gangIO, gangST)
where
import GHC.IO
import GHC.ST
import Control.Concurrent        (forkOn)
import Control.Concurrent.MVar
import Control.Exception         (assert)
import Control.Monad

#if TRACE_GANG
import Debug.Trace              (traceEventIO)
import System.Time ( ClockTime(..), getClockTime )
#endif 


-- Requests and operations on them --------------------------------------------
-- | The 'Req' type encapsulates work requests for individual members of a gang. 
data Req 
        -- | Instruct the worker to run the given action then signal it's done
        --   by writing to the MVar.
        = ReqDo        (Int -> IO ()) (MVar ())

        -- | Tell the worker that we're shutting the gang down.
        --   The worker should signal that it's received the equest down by
        --   writing to the MVar before returning to its caller (forkGang)      
        | ReqShutdown  (MVar ())


-- | Create a new request for the given action.
newReq :: (Int -> IO ()) -> IO Req
newReq p 
 = do   mv      <- newEmptyMVar
        return  $ ReqDo p mv


-- | Block until a thread request has been executed.
--   NOTE: only one thread can wait for the request.
waitReq :: Req -> IO ()
waitReq req
 = case req of
        ReqDo     _ varDone     -> takeMVar varDone
        ReqShutdown varDone     -> takeMVar varDone


-- Thread gangs and operations on them ----------------------------------------
-- | A 'Gang' is a group of threads which execute arbitrary work requests.
data Gang 
        = Gang !Int           -- Number of 'Gang' threads
               [MVar Req]     -- One 'MVar' per thread
               (MVar Bool)    -- Indicates whether the 'Gang' is busy


instance Show Gang where
  showsPrec p (Gang n _ _) 
        = showString "<<"
        . showsPrec p n
        . showString " threads>>"


-- | A sequential gang has no threads.
seqGang :: Gang -> Gang
seqGang (Gang n _ mv) = Gang n [] mv


-- | The worker thread of a 'Gang'.
--   The threads blocks on the MVar waiting for a work request.
gangWorker :: Int -> MVar Req -> IO ()
gangWorker threadId varReq
 = do   traceWorker threadId $ "ready."
        req     <- takeMVar varReq
        
        case req of
         ReqDo action varDone
          -> do traceWorker threadId $ " begin."
                start   <- getGangTime
                action threadId
                end     <- getGangTime
                traceWorker threadId $ " end (" ++ diffTime start end ++ ")."
                
                putMVar varDone ()
                gangWorker threadId varReq

         ReqShutdown varDone
          -> do traceWorker threadId $ " shutting down."
                putMVar varDone ()

traceWorker :: Int -> String -> IO ()
traceWorker threadId str
 = traceGang 
        $ "Worker " ++ show threadId 
        ++ " "
        ++ replicate (threadId * 10) ' '
        ++ str


-- | Finaliser for worker threads.
--   We want to shutdown the corresponding thread when it's MVar becomes
--   unreachable. Without this the program can compilain about 
--   "Blocked indefinitely on an MVar" because worker threads are still
--   blocked on the request MVars when the program ends. Whether this finalizer
--   is called or not is very racey. It can happen 1 in 10 times, or less often.
-- 
--   We're relying on the comment in System.Mem.Weak that says
--   "If there are no other threads to run, the runtime system will check for
--    runnable finalizers before declaring the system to be deadlocked."
-- 
--   If we were creating and destroying the gang cleanly we wouldn't need this,
--   but theGang is created with a top-level unsafePerformIO.
--   Hacks beget hacks beget hacks...
--
finaliseWorker :: MVar Req -> IO ()
finaliseWorker varReq
 = do   varDone <- newEmptyMVar
        putMVar varReq (ReqShutdown varDone) 
        takeMVar varDone
        return ()


-- | Fork a 'Gang' with the given number of threads (at least 1).
forkGang :: Int -> IO Gang
forkGang n
 = assert (n > 0) 
 $ do   
        -- Create the vars we'll use to issue work requests.
        mvs     <- sequence . replicate n $ newEmptyMVar
        
        -- Add finalisers so we can shut the workers down cleanly if they
        -- become unreachable.
        mapM_ (\var -> addMVarFinalizer var (finaliseWorker var)) mvs

        -- Create all the worker threads
        zipWithM_ forkOn [0..] 
                $ zipWith gangWorker [0 .. n-1] mvs

        -- The gang is currently idle.
        busy    <- newMVar False
        
        return $ Gang n mvs busy


-- | O(1). Yield the number of threads in the 'Gang'.
gangSize :: Gang -> Int
gangSize (Gang n _ _) = n


-------------------------------------------------------------------------------
data Workload
        -- | Unknown workload. Just run it in parallel.
        = WorkUnknown

        -- | Memory bound copy-like workload, 
        --   of the given number of bytes.
        | WorkCopy      Int
        deriving (Eq, Show)


-- | Decide whether a workload is too small to bother running in parallel.
--   TODO: We want to determine this based on similar workloads that 
--         we have run before. The gang should know what its minumum latency is.
workloadIsSmall :: Workload -> Bool
workloadIsSmall ww
 = case ww of
        WorkUnknown     -> False
        WorkCopy bytes  -> bytes < 1000



-- | Issue work requests for the 'Gang' and wait until they have been executed.
--   If the gang is already busy then just run the action in the requesting
--   thread. 
gangIO  :: Gang
        -> String 
        -> Workload
        -> (Int -> IO ())
        -> IO ()

-- Hrm. Gang hasn't been created yet. 
-- Just run the requests in the main thread.
gangIO (Gang n [] _) _what _workload p 
 = mapM_ p [0 .. n-1]

#if SEQ_IF_GANG_BUSY
gangIO (Gang n mvs busy) what workload p 
 = do   let !small      = workloadIsSmall workload
        if small 
         then do
                traceGang $ "Issuing  small " ++ what
                mapM_ p [0 .. n-1]
         else do
                isBusy          <- swapMVar busy True
                if isBusy 
                 then do 
                        traceGang $ "WARNING: Gang was already busy, running sequentially: " ++ what
                        mapM_ p [0 .. n-1]
                 else do
                        traceGangSplit $ "Issuing  par   " ++ what
                        parIO what n mvs p
                        _ <- swapMVar busy False
                        return ()
#else
gangIO (Gang n mvs busy) what _workload p 
        = parIO n mvs p
#endif


-- | Issue some requests to the worker threads and wait for them to complete.
parIO   :: String
        -> Int                  -- ^ Number of threads in the gang.
        -> [MVar Req]           -- ^ Request vars for worker threads.
        -> (Int -> IO ())       -- ^ Action to run in all the workers, it's
                                --   given the ix of the particular worker
                                ---  thread it's running on.
        -> IO ()

parIO what n mvs p 
 = do   start   <- getGangTime
        reqs    <- sequence . replicate n $ newReq p

        zipWithM_ putMVar mvs reqs

        traceGang $ "Running."
        mapM_ waitReq reqs
        end     <- getGangTime

        traceGangSplit $ "Complete par   " ++ what ++ " in " ++ diffTime start end ++ "us."


-- | Same as 'gangIO' but in the 'ST' monad.
gangST :: Gang -> String -> Workload -> (Int -> ST s ()) -> ST s ()
gangST gang what workload p 
        = unsafeIOToST 
        $ gangIO gang what workload
        $ unsafeSTToIO . p


-- Tracing -------------------------------------------------------------------
#if TRACE_GANG
getGangTime :: IO Integer
getGangTime
 = do   TOD sec pico    <- getClockTime
        let !micro      = pico `div` 1000000
        return (micro + sec * 1000000)

diffTime :: Integer -> Integer -> String
diffTime x y = show (y-x)

-- | Emit a GHC event for debugging, but don't mind if it gets truncated
traceGang :: String -> IO ()
traceGang s
 = do   traceEventIO $ "GANG " ++ s

-- | Emit a GHC event for debugging. Split across multiple events if necessary.
traceGangSplit :: String -> IO ()
traceGangSplit s
 = do   let xs = chunks 500 s
        let max= show $ length xs
        mapM_ (\(x,i) -> traceEventIO $ "GANG[" ++ show i ++ "/" ++ max ++ "] " ++ x) (xs `zip` [1..])
 where
        chunks len [] = []
        chunks len str
         = let (f,r) = splitAt len str
           in  f : chunks len r

#else
getGangTime :: IO ()
getGangTime = return ()

diffTime :: () -> () -> String
diffTime _ _ = ""

-- | Emit a GHC event for debugging.
traceGang :: String -> IO ()
traceGang _ = return ()

traceGangSplit :: String -> IO ()
traceGangSplit _ = return ()
#endif
