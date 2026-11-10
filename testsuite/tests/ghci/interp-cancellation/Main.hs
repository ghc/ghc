{-# LANGUAGE GADTs, LambdaCase, ScopedTypeVariables #-}

-- | Tests that after canceling a request we process the reply and free any
-- allocated resources.
--
-- Uses the servWithCustom hook to make sure requests get interrupted before
-- receiving the reply, triggering the cancel request and disposal of resources
-- path.
module Main (main) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (newMVar, readMVar)
import Control.Exception (SomeException, bracket, handle, uninterruptibleMask)
import Control.Monad (replicateM, unless, void)
import Data.Binary (Binary)
import qualified Data.ByteString.Char8 as BC
import Data.List (nub)
import Data.Word (Word64)
import GHC.Runtime.Interpreter (withIServ)
import GHC.Runtime.Interpreter.Process (sendMessage)
import GHC.Runtime.Interpreter.Types
  ( ExtInterpInstance(..)
  , ExtInterpState(..)
  , IServ
  , IServConfig(..)
  , InterpProcess(..)
  , InterpStatus(..)
  )
import GHCi.FFI (FFIType(..))
import GHCi.Message (RemoteResource(..),Message(..), Msg(..), Pipe, closePipe, mkPipeFromHandles)
import GHCi.RemoteTypes (HValueRef, RemotePtr(..), RemoteRef(..))
import GHCi.Server (MessageHook, serv)
import GHCi.Utils (readGhcHandle)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (die, exitFailure)
import System.IO
  ( BufferMode(..)
  , Handle
  , IOMode(..)
  , hFlush
  , hGetChar
  , hPutChar
  , hPutStrLn
  , hSetBinaryMode
  , hSetBuffering
  , stderr
  )
import System.Timeout (timeout)
import System.Posix.IO
  ( FdOption(CloseOnExec)
  , createPipe
  , fdToHandle
  , setFdOption
  )
import System.Process
  ( ProcessHandle
  , StdStream(Inherit)
  , createProcess
  , proc
  , std_err
  , std_in
  , std_out
  , terminateProcess
  , waitForProcess
  )
import System.Process.CommunicationHandle

--------------------------------------------------------------------------------
-- The messages the server holds back

-- | Messages whose reply is the only record of something the server allocated.
-- The server hook blocks on one of these until the client releases the gate,
-- which is what makes "the reply is in flight when the request is cancelled"
-- exact rather than a race.
gated :: Message a -> Bool
gated = \case
  MallocData{}    -> True
  MallocStrings{} -> True
  PrepFFI{}       -> True
  NewBreakArray{} -> True
  FreeRemoteResources{} -> True
  _               -> False

-- | How many allocate-cancel-release rounds to use when checking that the
-- release really happened. See 'reclaimed'.
rounds :: Int
rounds = 32

--------------------------------------------------------------------------------
-- Mode selection

main :: IO ()
main = getArgs >>= \case
  []              -> runClient
  ("server":rest) -> runServer rest
  args            -> die $ "unknown arguments: " ++ unwords args

--------------------------------------------------------------------------------
-- Server

-- | An iserv server whose hook gates the allocating messages. Written out
-- rather than reusing 'GHCi.Server.defaultServer' because that one installs
-- the empty hook.
runServer :: [String] -> IO ()
runServer [wfd, rfd, notifyFd, goFd] = do
  outh    <- readGhcHandle WriteMode wfd
  inh     <- readGhcHandle ReadMode  rfd
  notifyH <- readGhcHandle WriteMode notifyFd
  goH     <- readGhcHandle ReadMode  goFd
  prepareGateHandles [notifyH, goH]
  pipe <- mkPipeFromHandles inh outh
  -- As in 'defaultServer': we cannot take an async exception while
  -- communicating, or we lose sync in the protocol.
  uninterruptibleMask $ \restore -> serv False (gateHook notifyH goH) pipe restore
runServer args =
  die $ "server: expected <write-fd> <read-fd> <notify-fd> <go-fd>, got "
          ++ unwords args

gateHook :: Handle -> Handle -> MessageHook
gateHook notifyH goH m@(Msg msg)
  | gated msg = do
      -- Tell the client the request has arrived, and hold the reply until it
      -- has cancelled it.
      hPutChar notifyH '!'
      hFlush notifyH
      void (hGetChar goH)
      pure m
  | otherwise = pure m

--------------------------------------------------------------------------------
-- The client's end of the gate

data Gate = Gate
  { gateWait    :: IO () -- ^ Block until the server is holding a reply back.
  , gateRelease :: IO () -- ^ Let it send that reply.
  }

prepareGateHandles :: [Handle] -> IO ()
prepareGateHandles hs = do
  mapM_ (`hSetBuffering` NoBuffering) hs
  mapM_ (`hSetBinaryMode` True) hs

--------------------------------------------------------------------------------
-- An interpreter instance around the gated server

-- | Everything 'sendMessage' needs. The rest of what GHC hangs off an 'Interp'
-- (the loader, the symbol caches) plays no part in disposing of a reply.
mkInstance :: Pipe -> ProcessHandle -> IO (ExtInterpInstance ())
mkInstance pipe ph = do
  frees    <- newMVar []
  pure ExtInterpInstance
    { instProcess         = InterpProcess { interpPipe = pipe, interpHandle = ph }
    , instPendingFrees    = frees
    , instExtra           = ()
    }

mkIServ :: ExtInterpInstance () -> IO IServ
mkIServ inst = ExtInterpState cfg <$> newMVar (InterpRunning inst)
  where
    cfg = IServConfig
      { iservConfProgram  = "<never spawned>"
      , iservConfOpts     = []
      , iservConfProfiled = False
      , iservConfDynamic  = False
      , iservConfHook     = Nothing
      , iservConfTrace    = pure ()
      }

-- | Waits for a FreeRemoteResources emptying instPendingFrees to be sent and lets it through.
drain :: Gate -> IServ -> IO ()
drain gate iserv = withIServ iserv (\_ -> gateWait gate >> gateRelease gate)

pending :: ExtInterpInstance () -> IO [RemoteResource]
pending inst = readMVar (instPendingFrees inst)

--------------------------------------------------------------------------------
-- Client

runClient :: IO ()
runClient = withServer $ \pipe ph gate -> do
  inst  <- mkInstance pipe ph
  iserv <- mkIServ inst

  let -- Cancel a request the server is holding, and wait for the orphaned
      -- reply to be filed. Takes how many entries each queue should gain.
      cancel :: Binary a => Message a -> Int -> IO [RemoteResource]
      cancel = cancelInFlight inst gate

      -- Send what the queues hold, and insist they are left empty.
      drained what = do
        drain gate iserv
        frees <- pending inst
        must (what ++ ": the queues are empty once drained")
             (null frees)

  let payload = BC.pack (replicate 64 'a')
  mdFrees <- cancel (MallocData payload) 1
  check "cancelled MallocData is queued as RemotePtr"
        (map msgConName mdFrees == ["RemoteRemotePtr"])
  drained "MallocData"

  -- Same for a reply carrying several pointers at once.
  let strs = map BC.pack ["one", "two", "three"]
  msFrees <- cancel (MallocStrings strs) 3
  check ("cancelled MallocStrings is queued as one FreeData for every pointer")
        (nub (map msgConName msFrees) == ["RemoteRemotePtr"]
           && length msFrees == length strs)
  drained "MallocStrings"

  ffiFrees <- cancel (PrepFFI [FFISInt32, FFIPointer] FFISInt32) 1
  check "cancelled PrepFFI is queued as RemoteFFI"
        (map msgConName ffiFrees == ["RemoteFFI"])
  drained "PrepFFI"

  baFrees <- cancel (NewBreakArray 8) 1
  check "cancelled NewBreakArray is queued as RemoteRef"
        (length baFrees == 1 && map msgConName baFrees == ["RemoteHValueRef"])
  drained "NewBreakArray"

  -- The cancellations left the protocol in sync and the server alive, and a
  -- reply that was waited for belongs to its caller, not to the queues.
  ptrs <- passThrough inst gate (MallocStrings strs)
  check "server still answers after a cancellation" (length ptrs == length strs)
  liveFrees  <- pending inst
  check "a reply that was waited for is not queued"
        (null liveFrees)

  -- Now that the releases are being sent, check they reach the allocator.
  mallocAddrs <- replicateM rounds $ do
    cleanups <- cancel (MallocData payload) 1
    addr <- one "the MallocData cleanup names one pointer"
                (concatMap cleanupAddrs cleanups)
    drained "MallocData"
    pure addr
  check "cancelled MallocData allocations are reclaimed" (reclaimed mallocAddrs)

  refAddrs <- replicateM rounds $ do
    frees <- cancel (NewBreakArray 8) 1
    addr <- one "the NewBreakArray queue holds one ref" (concatMap cleanupAddrs frees)
    drained "NewBreakArray"
    pure addr
  check ("cancelled NewBreakArray refs are reclaimed") (reclaimed refAddrs)

  sendMessage inst Shutdown

-- | Send a message the server will hold back and kill the thread waiting for
-- the reply, then wait for 'disposeMessageResults' to file it.
--
-- The gate fixes the order: the server only replies once the request has
-- already been cancelled, so the reply is received by the dispose-after-canceling thread on the host.
cancelInFlight :: Binary a
               => ExtInterpInstance () -> Gate -> Message a -> Int
               -> IO [RemoteResource]
cancelInFlight inst gate msg want = do
  tid <- forkIO $ handle (\(_ :: SomeException) -> pure ()) $ void $
    sendMessage inst msg
  gateWait gate    -- the server has the request and is holding the reply
  killThread tid   -- sendMessage is not waiting for that reply any more
  gateRelease gate -- now let it arrive
  queues <- timeout disposeTimeout (awaitPending inst want)
  case queues of
    Nothing -> failWith (conName msg ++ ": the cancelled reply was never disposed of")
    Just frees -> do
      -- awaitPending only waits for the queues to be long enough, so a reply
      -- disposed of twice shows up here rather than hanging.
      must (conName msg ++ ": disposed of exactly once")
           (length frees == want)
      pure frees

awaitPending :: ExtInterpInstance () -> Int -> IO [RemoteResource]
awaitPending inst wantFrees = loop
  where
    loop = do
      frees <- pending inst
      if length frees >= wantFrees
        then pure frees
        else threadDelay 1000 >> loop

-- | Time to wait for awaitPending to return. We fail the test when it expires.
disposeTimeout :: Int
disposeTimeout = 30 * 1000000

-- | Send a "gated" message and unlock the handler on the fly.
passThrough :: Binary a => ExtInterpInstance () -> Gate -> Message a -> IO a
passThrough inst gate msg = do
  _ <- forkIO (gateWait gate >> gateRelease gate)
  sendMessage inst msg

--------------------------------------------------------------------------------
-- Assertions

-- | We loop (allocate >> free) enough times that we expect a address to be
-- reused. This function checks for a duplicate in the list of addresses.
reclaimed :: [Word64] -> Bool
reclaimed addrs = length (nub addrs) < length addrs

-- | The addresses a queued cleanup message is about to release.
cleanupAddrs :: RemoteResource -> [Word64]
cleanupAddrs = \case
  RemoteRemotePtr p -> [ addrOf p]
  RemoteFFI p   -> [addrOf p]
  RemoteHValueRef p -> [refAddrOf p]

addrOf :: RemotePtr a -> Word64
addrOf (RemotePtr w) = w

refAddrOf :: RemoteRef a -> Word64
refAddrOf (RemoteRef p) = addrOf p

-- | The constructor of a message, which is all of it that is deterministic:
-- the payload is a pointer the server chose.
conName :: Message a -> String
conName = takeWhile (/= ' ') . show

msgConName :: RemoteResource -> String
msgConName = takeWhile (/= ' ') . show

one :: String -> [a] -> IO a
one _ [x] = pure x
one what _ = failWith what

failWith :: String -> IO a
failWith what = do
  hPutStrLn stderr ("FAILED: " ++ what)
  hFlush stderr
  exitFailure

must :: String -> Bool -> IO ()
must what ok = unless ok (void (failWith what))

check :: String -> Bool -> IO ()
check what ok = must what ok >> putStrLn ("ok: " ++ what)

--------------------------------------------------------------------------------
-- Spawning the server

-- | Four pipes: the two the 'Pipe' is made of, plus the two halves of the gate.
withServer :: (Pipe -> ProcessHandle -> Gate -> IO a) -> IO a
withServer action = do
  serverExe <- getExecutablePath
  (ghcRead, serverWrite)    <- createWeReadTheyWritePipe
  (serverRead, ghcWrite)    <- createTheyReadWeWritePipe
  (notifyRead, notifyWrite) <- createWeReadTheyWritePipe -- server -> client
  (goRead, goWrite)         <- createTheyReadWeWritePipe -- client -> server
  let inherited = [serverWrite, serverRead, notifyWrite, goRead]
  ph <- spawnServer serverExe (map show inherited)
  mapM_ closeCommunicationHandle inherited
  prepareGateHandles [notifyRead, goWrite]
  let gate = Gate { gateWait    = void (hGetChar notifyRead)
                  , gateRelease = hPutChar goWrite 'g' >> hFlush goWrite
                  }
  bracket (mkHandles ghcRead ghcWrite >>= uncurry mkPipeFromHandles)
          (\pipe -> do
             closePipe pipe
             terminateProcess ph
             void (waitForProcess ph))
          (\pipe -> action pipe ph gate)
  where
    spawnServer :: FilePath -> [String] -> IO ProcessHandle
    spawnServer exe fds = do
      (_, _, _, ph) <- createProcess (proc exe ("server" : fds))
        { std_in  = Inherit
        , std_out = Inherit
        , std_err = Inherit
        }
      pure ph

    mkHandles hR hW = do
      mapM_ (`hSetBuffering` NoBuffering) [hR, hW]
      mapM_ (`hSetBinaryMode` True) [hR, hW]
      pure (hR, hW)
