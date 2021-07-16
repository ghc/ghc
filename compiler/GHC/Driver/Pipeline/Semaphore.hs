{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingVia #-}
module GHC.Driver.Pipeline.Semaphore where

import GHC.Prelude
import qualified GHC.Data.DependentMap as M
import Control.Concurrent
import Control.Monad
import Data.IORef
import GHC.Types.Error
import GHC.Types.SrcLoc
import GHC.Utils.Logger
import qualified Control.Monad.Catch as MC
import GHC.Utils.Outputable
import GHC.Utils.Trace

data ActionResult a = ActionResult { actionResult :: MVar (Maybe a) -- Where the result will end up
                                   , killAction   :: IO () -- How to kill the running action
                                   , actionName   :: SDoc  -- For debugging
                                   }

waitResult :: ActionResult a -> IO (Maybe a)
waitResult ar = do
  rs <- readMVar (actionResult ar)
  return rs

mkAction :: SDoc -> IO (Maybe a) -> IO (ActionResult a)
mkAction name act = do
  res_var <- newEmptyMVar
  -- MP: There used to be a forkIOWithUnmask here, but there was not a corresponding
  -- mask so unmasking was a no-op.
  r <- forkIO $ do
        r <- act
        putMVar res_var r
  return $ ActionResult res_var (killThread r) name

type ActionMap f = M.DependentMap f ActionResult

emptyActionMap :: ActionMap f
emptyActionMap  = M.emptyDepMap

killAllActions :: ActionMap f -> IO ()
killAllActions =
  MC.uninterruptibleMask_ . sequence_ . M.elemsDepMap getKill
  where
    getKill :: ActionResult a -> IO ()
    getKill = killAction

queueAction :: (Outputable (f a), M.GOrd f) => MVar (ActionMap f)
                                            -> f a
                                            -> IO (Maybe a)
                                            -> IO (Maybe a)
queueAction act_var key raw_act = do
  join $ modifyMVar act_var (\m ->
    case M.lookupDepMap key m of
      Just a -> do
        return (m, waitResult a)
      Nothing -> do
        pprTraceM "create" (ppr key)
        wrapped_act <- mkAction (ppr key) raw_act
        return (M.insertDepMap key wrapped_act m, waitResult wrapped_act))

-- LogQueue Abstraction

-- | Each module is given a unique 'LogQueue' to redirect compilation messages
-- to. A 'Nothing' value contains the result of compilation, and denotes the
-- end of the message queue.
data LogQueue = LogQueue !(IORef [Maybe (MessageClass, SrcSpan, SDoc)])
                         !(MVar ())

newLogQueue :: IO LogQueue
newLogQueue = do
  mqueue <- newIORef []
  sem <- newMVar ()
  return (LogQueue mqueue sem)

finishLogQueue :: LogQueue -> IO ()
finishLogQueue lq =
  writeLogQueueInternal lq Nothing


writeLogQueue :: LogQueue -> (MessageClass,SrcSpan,SDoc) -> IO ()
writeLogQueue lq msg =
  writeLogQueueInternal lq (Just msg)

-- | Internal helper for writing log messages
writeLogQueueInternal :: LogQueue -> Maybe (MessageClass,SrcSpan,SDoc) -> IO ()
writeLogQueueInternal (LogQueue ref sem) msg = do
    atomicModifyIORef' ref $ \msgs -> (msg:msgs,())
    _ <- tryPutMVar sem ()
    return ()

-- The log_action callback that is used to synchronize messages from a
-- worker thread.
parLogAction :: LogQueue -> LogAction
parLogAction log_queue _dflags !msgClass !srcSpan !msg =
    writeLogQueue log_queue (msgClass,srcSpan,msg)

-- Print each message from the log_queue using the global logger
printLogs :: Logger -> LogQueue -> IO ()
printLogs !logger (LogQueue ref sem) = read_msgs
  where read_msgs = do
            takeMVar sem
            msgs <- atomicModifyIORef' ref $ \xs -> ([], reverse xs)
            print_loop msgs

        print_loop [] = read_msgs
        print_loop (x:xs) = case x of
            Just (msgClass,srcSpan,msg) -> do
                logMsg logger msgClass srcSpan msg
                print_loop xs
            -- Exit the loop once we encounter the end marker.
            Nothing -> return ()