{-# LANGUAGE CPP, GADTs, OverloadedStrings, LambdaCase #-}

{-
This is the proxy portion of iserv.

It acts as local bridge for GHC to call
a remote slave. This all might sound
confusing, so let's try to get some
naming down.

GHC is the actual Haskell compiler, that
acts as frontend to the code to be compiled.

iserv is the slave, that GHC delegates compilation
of TH to. As such it needs to be compiled for
and run on the Target. In the special case
where the Host and the Target are the same,
no proxy is needed. GHC and iserv communicate
via pipes.

iserv-proxy is the proxy instance to iserv.
The following illustration should make this
somewhat clear:

 .----- Host -----.     .- Target -.
 | GHC <--> proxy<+-----+>  iserv  |
 '----------------'  ^  '----------'
        ^            |
        |            '-- communication via sockets
        '--- communication via pipes

For now, we won't support multiple concurrent
invocations of the proxy instance, and that
behavior will be undefined, as this largely
depends on the capability of the iserv on the
target to spawn multiple process.  Spawning
multiple threads won't be sufficient, as the
GHC runtime has global state.

Also the GHC runtime needs to be able to
use the linker on the Target to link archives
and object files.

-}

module Main (main) where

import System.IO
import GHCi.Message
import GHCi.Utils
import GHCi.Signals

import Remote.Message

import Network.Socket
import Data.IORef
import Control.Monad
import System.Environment
import System.Exit
import Text.Printf
import GHC.Fingerprint (getFileHash)
import System.Directory
import System.FilePath (isAbsolute)

import Data.Binary
import qualified Data.ByteString as BS

import Control.Concurrent (threadDelay)
import qualified Control.Exception as E

trace :: String -> IO ()
trace s = getProgName >>= \name -> printf "[%20s] %s\n" name s

dieWithUsage :: IO a
dieWithUsage = do
    prog <- getProgName
    die $ prog ++ ": " ++ msg
  where
#if defined(WINDOWS)
    msg = "usage: iserv <write-handle> <read-handle> <slave ip> <slave port> [-v]"
#else
    msg = "usage: iserv <write-fd> <read-fd> <slave ip> <slave port> [-v]"
#endif

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering

  args <- getArgs
  (wfd1, rfd2, host_ip, port, rest) <-
      case args of
        arg0:arg1:arg2:arg3:rest -> do
            let wfd1 = read arg0
                rfd2 = read arg1
                ip   = arg2
                port = read arg3
            return (wfd1, rfd2, ip, port, rest)
        _ -> dieWithUsage

  verbose <- case rest of
    ["-v"] -> return True
    []     -> return False
    _      -> dieWithUsage

  when verbose $
    printf "GHC iserv starting (in: %d; out: %d)\n"
      (fromIntegral rfd2 :: Int) (fromIntegral wfd1 :: Int)
  inh  <- getGhcHandle rfd2
  outh <- getGhcHandle wfd1
  installSignalHandlers
  lo_ref <- newIORef Nothing
  let in_pipe = Pipe{pipeRead = inh, pipeWrite = outh, pipeLeftovers = lo_ref}

  when verbose $
    trace ("Trying to connect to " ++ host_ip ++ ":" ++ (show port))

  out_pipe <- do
    let go n = E.try (connectTo verbose host_ip port >>= socketToPipe) >>= \case
          Left e | n == 0 -> E.throw (e :: E.SomeException)
                 | n >  0 -> threadDelay 500000 >> go (n - 1)
          Right a -> return a
      in go 120 -- wait for up to 60seconds (polling every 0.5s).

  when verbose $
    trace "Starting proxy"
  proxy verbose in_pipe out_pipe

-- | A hook, to transform outgoing (proxy -> slave)
-- messages prior to sending them to the slave.
hook :: Msg -> IO Msg
hook = return

-- | Forward a single @THMessage@ from the slave
-- to ghc, and read back the result from GHC.
--
--  @Message@s go from ghc to the slave.
--    ghc --- proxy --> slave               (@Message@)
--  @THMessage@s go from the slave to ghc
--    ghc <-- proxy --- slave               (@THMessage@)
--
fwdTHMsg :: (Binary a) => Pipe -> THMessage a -> IO a
fwdTHMsg local msg = do
  writePipe local (putTHMessage msg)
  readPipe local get

-- | Fowarard a @Message@ call and handle @THMessages@.
fwdTHCall :: (Binary a) => Bool -> Pipe -> Pipe -> Message a -> IO a
fwdTHCall verbose local remote msg = do
  when verbose $ trace ("fwdTHCall: " ++ show msg)
  writePipe remote (putMessage msg)
  -- wait for control instructions
  when verbose $ trace "waiting for control instructions..."
  loopTH
  when verbose $ trace "reading remote pipe result"
  readPipe remote get
    where
      loopTH :: IO ()
      loopTH = do
        when verbose $
          trace "fwdTHCall/loopTH: reading remote pipe..."
        THMsg msg' <- readPipe remote getTHMessage
        when verbose $
          trace ("| TH Msg: ghc <- proxy -- slave: " ++ show msg')
        res <- fwdTHMsg local msg'
        when verbose $
          trace ("| Resp.:  ghc -- proxy -> slave: " ++ show res)
        writePipe remote (put res)
        case msg' of
          RunTHDone -> return ()
          _         -> loopTH

-- | Forwards a @Message@ call, and handle @SlaveMessage@.
-- Similar to @THMessages@, but @SlaveMessage@ are between
-- the slave and the proxy, and are not forwarded to ghc.
-- These message allow the Slave to query the proxy for
-- files.
--
--  ghc --- proxy --> slave  (@Message@)
--
--          proxy <-- slave  (@SlaveMessage@)
--
fwdLoadCall :: (Binary a, Show a) => Bool -> Pipe -> Pipe -> Message a -> IO a
fwdLoadCall verbose _ remote msg = do
  when verbose $ trace "fwdLoadCall: writing remote pipe"
  writePipe remote (putMessage msg)
  loopLoad
  when verbose $ trace "fwdLoadCall: reading local pipe"
  readPipe remote get
  where
    truncateMsg :: Int -> String -> String
    truncateMsg n s | length s > n = take n s ++ "..."
                    | otherwise    = s
    reply :: (Binary a, Show a) => a -> IO ()
    reply m = do
      when verbose $
        trace ("| Resp.:         proxy -> slave: "
                  ++ truncateMsg 80 (show m))
      writePipe remote (put m)
    loopLoad :: IO ()
    loopLoad = do
      when verbose $ trace "fwdLoadCall: reading remote pipe"
      SlaveMsg msg' <- readPipe remote getSlaveMessage
      when verbose $
        trace ("| Sl Msg:        proxy <- slave: " ++ show msg')
      case msg' of
        Done -> return ()
        Missing path -> do
          when verbose $
            trace $ "fwdLoadCall: missing path: " ++ path
          reply =<< BS.readFile path
          loopLoad
        Have path remoteHash -> do
          localHash <- getFileHash path
          reply =<< if localHash == remoteHash
                    then return Nothing
                    else Just <$> BS.readFile path
          loopLoad

-- | The actual proxy. Conntect local and remote pipe,
-- and does some message handling.
proxy :: Bool -> Pipe -> Pipe -> IO ()
proxy verbose local remote = loop
  where
    fwdCall :: (Binary a, Show a) => Message a -> IO a
    fwdCall msg = do
      when verbose $ trace "proxy/fwdCall: writing remote pipe"
      writePipe remote (putMessage msg)
      when verbose $ trace "proxy/fwdCall: reading remote pipe"
      readPipe remote get

    -- reply to ghc.
    reply :: (Show a, Binary a) => a -> IO ()
    reply msg = do
      when verbose $
        trace ("Resp.:    ghc <- proxy -- slave: " ++ show msg)
      writePipe local (put msg)

    loop = do
      (Msg msg) <- readPipe local getMessage
      when verbose $
        trace ("Msg:      ghc -- proxy -> slave: " ++ show msg)
      (Msg msg') <- hook (Msg msg)
      -- Note [proxy-communication]
      --
      -- The fwdTHCall/fwdLoadCall/fwdCall's have to match up
      -- with their endpoints in libiserv:Remote.Slave otherwise
      -- you will end up with hung connections.
      --
      -- We are intercepting some calls between ghc and iserv
      -- and augment the protocol here.  Thus these two sides
      -- need to line up and know what request/reply to expect.
      --
      case msg' of
        -- TH might send some message back to ghc.
        RunTH{} -> do
          resp <- fwdTHCall verbose local remote msg'
          reply resp
          loop
        RunModFinalizers{} -> do
          resp <- fwdTHCall verbose local remote msg'
          reply resp
          loop
        -- Load messages might send some messages back to the proxy, to
        -- requrest files that are not present on the device.
        LoadArchive{} -> do
          resp <- fwdLoadCall verbose local remote msg'
          reply resp
          loop
        LoadObj{} -> do
          resp <- fwdLoadCall verbose local remote msg'
          reply resp
          loop
        -- On windows we assume that we don't want to copy libraries
        -- that are referenced in C:\ these are usually system libraries.
        LoadDLL path@('C':':':_) -> do
          fwdCall msg' >>= reply >> loop
        LoadDLL path | isAbsolute path -> do
          resp <- fwdLoadCall verbose local remote msg'
          reply resp
          loop
        Shutdown{}    -> fwdCall msg' >> return ()
        _other        -> fwdCall msg' >>= reply >> loop


connectTo :: Bool -> String -> PortNumber -> IO Socket
connectTo verbose host port = do
  addr <- resolve host (show port)
  open addr
  where
    resolve host port = do
        let hints = defaultHints { addrSocketType = Stream }
        addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
        return addr
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        when verbose $
          trace $ "Created socket for " ++ host ++ ":" ++ show port
        connect sock $ addrAddress addr
        when verbose $
          trace "connected"
        return sock

-- | Turn a socket into an unbuffered pipe.
socketToPipe :: Socket -> IO Pipe
socketToPipe sock = do
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering

  lo_ref <- newIORef Nothing
  pure Pipe{ pipeRead = hdl, pipeWrite = hdl, pipeLeftovers = lo_ref }
