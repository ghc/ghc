{-# LANGUAGE NoImplicitPrelude #-}

module GHC.IO.URing
    ( submit
    , submitAndBlock
    , supportsIOURing
    ) where

import GHC.Base
import GHC.Conc
import GHC.MVar
import GHC.Num ((-), (+))
import GHC.PerCapability
import GHC.Show
import System.Posix.Internals (puts)
import GHC.Real
import GHC.IO (catch)
import GHC.IO.Exception
import GHC.IO.Unsafe (unsafePerformIO)
import Data.Int

import qualified GHC.Event.IntTable as IT
import GHC.Event.Unique

import qualified System.Linux.IO.URing as URing
import System.Linux.IO.URing.Sqe (SqeBuilder, UserData, SqeIndex)
import qualified System.Linux.IO.URing.Sqe as URing.Sqe
import qualified System.Linux.IO.URing.Ring as URing.Ring
import System.Linux.IO.URing.Cqe (Cqe(..))

supportsIOURing :: Bool
supportsIOURing = unsafePerformIO checkIOURing
{-# NOINLINE supportsIOURing #-}

checkIOURing :: IO Bool
checkIOURing
  | c_rtsIsThreaded == 0 = return False
  | otherwise = do
    catch (URing.Ring.newURing 16 >> return True) uhOh
  where
    uhOh :: IOError -> IO Bool
    uhOh ioe
      | UnsupportedOperation <- ioe_type ioe = return False
      | otherwise = do
        puts $ "Unexpected error while checking io-uring support: " ++ show ioe
        return False

foreign import ccall unsafe "rts_isThreaded" c_rtsIsThreaded :: Int

type Completion = Int32 -> IO ()

data PendingReq = PendingReq !SqeIndex !Completion

data URingMgr = URingMgr { uring :: !URing.URing
                         , uniqueSource :: !UniqueSource
                         , requests :: !(IT.IntTable PendingReq)
                         , freeSqSlot :: !(MVar ())
                         }

-- | When a thread tries to submit a request but finds that there are no SQs/SQEs 
-- if blocks on this MVar, which gets filled when a pending request completes.
--
-- Note that this is technically a deadlock hazard. For instance, if a program
-- has two threads which respectively read and write to a pipe, one thread may
-- attempt to read, filling the SQ, while the other the other thread attempts
-- to write, which blocks.
type FreeSqEvent = MVar ()

mAX_REQS :: Int
mAX_REQS = 256

getSystemURingManager :: IO (Maybe URingMgr)
getSystemURingManager
  | not supportsIOURing = return Nothing
  | otherwise = do
      Just `fmap` getPerCapability uringMgrs

uringMgrs :: PerCapability URingMgr
uringMgrs = unsafePerformIO $ do
    let new_cap = newURingMgr
        free_cap mgr = return () -- TODO
    newPerCapability new_cap free_cap
    -- TODO: shared CAF
{-# NOINLINE uringMgrs #-}

newURingMgr :: Int -> IO URingMgr
newURingMgr cap = do
    ring <- URing.newURing mAX_REQS
    usrc <- newSource
    reqs <- IT.new mAX_REQS
    free_sq <- newEmptyMVar
    let mgr = URingMgr { uring = ring
                       , uniqueSource = usrc
                       , requests = reqs
                       , freeSqSlot = free_sq
                       }
    _ <- forkOn cap (startCompletionThread mgr)
    return mgr

submit' :: URingMgr
        -> (UserData -> SqeBuilder a)
            -- ^ User data to SqeBuilder
        -> Completion
            -- ^ Action to run on completion
        -> IO (Int, a)
submit' mgr mkSqe compl = do
    Unique u <- newUnique (uniqueSource mgr)
    go u
  where
    dup_uniq_err = error "repeated IO request unique"

    go u = do
        sqeIdx_mb <- URing.Ring.getSqe (uring mgr)
        case sqeIdx_mb of
          Just sqeIdx -> do
            _ <- IT.insertWith dup_uniq_err (fromIntegral u) (PendingReq sqeIdx compl) (requests mgr)
            let sqe = mkSqe (fromIntegral u)
            r <- URing.Sqe.pokeSqe sqe (URing.Ring.sqePtr (uring mgr) sqeIdx)
            pushRes <- URing.Ring.pushSqe (uring mgr) sqeIdx
            if pushRes
              then do
                  completed <- URing.submit (uring mgr) 1
                  return (completed, r)
              else URing.freeSqe (uring mgr) sqeIdx >> no_sqs u
          Nothing -> no_sqs u

    -- Retry when there are no SQ or SQE slots...
    no_sqs u = takeMVar (freeSqSlot mgr) >> go u

submit :: (UserData -> SqeBuilder a)
       -> Completion
       -> IO a
submit mkSqe compl = withURingMgr $ \mgr -> do
    withURingMgr $ \mgr -> do
        (completed, r) <- submit' mgr mkSqe compl
        -- TODO: data race here
        when (completed > 0) $
            handleCompletions mgr
        return r

submitAndBlock :: (UserData -> SqeBuilder a) -> IO Int32
submitAndBlock mkSqe = do
    mvar <- newEmptyMVar
    submit mkSqe (putMVar mvar)
    r <- takeMVar mvar
    return r

withURingMgr :: (URingMgr -> IO a) -> IO a
withURingMgr f = do
    Just mgr <- getSystemURingManager
    -- TODO: lock URingMgr
    f mgr

startCompletionThread
    :: URingMgr
    -> IO ()
startCompletionThread mgr = do
    tid <- myThreadId
    labelThread tid "uring completion thread"
    go
  where
    go = do
        handleCompletions mgr
        _ <- URing.submitAndWait (uring mgr) 0 1
        go

handleCompletions :: URingMgr -> IO ()
handleCompletions mgr = loop
  where
    loop = do
        mb_cqe <- URing.popCq (uring mgr)
        case mb_cqe of
          Nothing -> return ()
          Just cqe -> do
              let reqId :: Int
                  reqId = fromIntegral $ cqeUserData cqe
              mb_req <- IT.delete reqId (requests mgr)
              case mb_req of
                Nothing -> error "No request"
                Just (PendingReq sqe_idx compl) -> do
                    URing.freeSqe (uring mgr) sqe_idx
                    tryPutMVar (freeSqSlot mgr) () -- TODO: How inefficient is this?
                    compl (cqeRes cqe)
              loop

