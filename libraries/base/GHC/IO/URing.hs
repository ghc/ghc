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
import GHC.Real
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
supportsIOURing = False -- TODO: Feature test

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
    _ <- forkOn cap (startCompletionThread ring reqs free_sq)
    return URingMgr { uring = ring
                    , uniqueSource = usrc
                    , requests = reqs
                    , freeSqSlot = free_sq
                    }

submit' :: URingMgr
        -> (UserData -> SqeBuilder a)
            -- ^ User data to SqeBuilder
        -> Completion
            -- ^ Action to run on completion
        -> IO a
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
                  _ <- URing.submit (uring mgr) 1
                  return r
              else URing.freeSqe (uring mgr) sqeIdx >> no_sqs u
          Nothing -> no_sqs u

    -- Retry when there are no SQ or SQE slots...
    no_sqs u = takeMVar (freeSqSlot mgr) >> go u

submit :: (UserData -> SqeBuilder a)
       -> Completion
       -> IO a
submit mkSqe compl = withURingMgr $ \mgr -> do
    submit' mgr mkSqe compl

submitAndBlock :: (UserData -> SqeBuilder a) -> IO Int32
submitAndBlock mkSqe = do
    mvar <- newEmptyMVar
    submit mkSqe (putMVar mvar)
    takeMVar mvar

withURingMgr :: (URingMgr -> IO a) -> IO a
withURingMgr f = do
    Just mgr <- getSystemURingManager
    -- TODO: lock URingMgr
    f mgr

startCompletionThread
    :: URing.URing
    -> IT.IntTable PendingReq
    -> FreeSqEvent
    -> IO ()
startCompletionThread ring requests free_sq = do
    tid <- myThreadId
    labelThread tid "uring completion thread"
    go
  where
    go = once >> go

    once :: IO ()
    once = do
        mb_cqe <- URing.popCq ring
        case mb_cqe of
          Nothing -> do
              _ <- URing.submitAndWait ring 0 1
              return ()
          Just cqe -> do
              let reqId :: Int
                  reqId = fromIntegral $ cqeUserData cqe
              mb_req <- IT.delete reqId requests
              case mb_req of
                Nothing -> error "No request"
                Just (PendingReq sqe_idx compl) -> do
                    URing.freeSqe ring sqe_idx
                    tryPutMVar free_sq () -- TODO: How inefficient is this?
                    compl (cqeRes cqe)

