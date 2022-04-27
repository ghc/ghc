{-# LANGUAGE NoImplicitPrelude #-}

module GHC.IO.URing
    ( submit
    , submitAndBlock
    ) where

import GHC.Base
import GHC.Conc
import GHC.MVar
import GHC.Real
import GHC.IO.Unsafe (unsafePerformIO)
import Data.Int

import qualified GHC.Event.IntTable as IT
import GHC.Event.Unique

import qualified System.Linux.IO.URing as URing
import System.Linux.IO.URing.Sqe
import System.Linux.IO.URing.Cqe (Cqe(..))

type Completion = Int32 -> IO ()

data URingMgr = URingMgr { uring :: !URing.URing
                         , uniqueSource :: !UniqueSource
                         , requests :: !(IT.IntTable Completion)
                         }

mAX_REQS :: Int
mAX_REQS = 256

newURingMgr :: IO URingMgr
newURingMgr = do
    _uring <- URing.newURing mAX_REQS
    usrc <- newSource
    reqs <- IT.new mAX_REQS
    _ <- forkIO (startCompletionThread _uring reqs)
    return URingMgr { uring = _uring
                    , uniqueSource = usrc
                    , requests = reqs
                    }

submit' :: URingMgr
        -> (UserData -> SqeBuilder a)
            -- ^ User data to SqeBuilder
        -> Completion
            -- ^ Action to run on completion
        -> IO a
submit' mgr mkSqe compl = do
    Unique u <- newUnique (uniqueSource mgr)
    let err = error "repeated IO request unique"
    _ <- IT.insertWith err (fromIntegral u) compl (requests mgr)
    mb_sqe <- URing.postSqe (uring mgr) (mkSqe $ fromIntegral u)
    (sqeIdx, r) <- case mb_sqe of
      Nothing -> error "failed to post" -- TODO
      Just r -> pure r
    _ <- URing.submit (uring mgr) 1
    return r

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
withURingMgr = withMVar globalURingMgr

globalURingMgr :: MVar URingMgr
globalURingMgr = unsafePerformIO $ do
    mgr <- newURingMgr
    newMVar mgr

startCompletionThread :: URing.URing
                      -> IT.IntTable Completion
                      -> IO ()
startCompletionThread uring requests = go
  where
    go = once >> go

    once :: IO ()
    once = do
        mb_cqe <- URing.popCq uring
        case mb_cqe of
          Nothing -> do
              _ <- URing.submitAndWait uring 0 1
              return ()
          Just cqe -> do
              let reqId :: Int
                  reqId = fromIntegral $ cqeUserData cqe
              mb_req <- IT.delete reqId requests
              case mb_req of
                Nothing -> error "No request"
                Just compl -> compl (cqeRes cqe)

