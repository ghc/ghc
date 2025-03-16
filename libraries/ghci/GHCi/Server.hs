{-# LANGUAGE CPP, RankNTypes, RecordWildCards, GADTs, ScopedTypeVariables #-}
module GHCi.Server
  ( serv
  , defaultServer
  )
where

import Prelude
import GHCi.Run
import GHCi.TH
import GHCi.Message
#if defined(wasm32_HOST_ARCH)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as LB
import Foreign
import Foreign.ForeignPtr.Unsafe
import GHC.Wasm.Prim
#else
import GHCi.Signals
import GHCi.Utils
#endif

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Concurrent (threadDelay)
import Data.Binary

import Text.Printf
import System.Environment (getProgName, getArgs)
import System.Exit

type MessageHook = Msg -> IO Msg

trace :: String -> IO ()
trace s = getProgName >>= \name -> printf "[%20s] %s\n" name s

serv :: Bool -> MessageHook -> Pipe -> (forall a .IO a -> IO a) -> IO ()
serv verbose hook pipe restore = loop
 where
  loop = do
    when verbose $ trace "reading pipe..."
    Msg msg <- readPipe pipe getMessage >>= hook

    discardCtrlC

    when verbose $ trace ("msg: " ++ (show msg))
    case msg of
      Shutdown -> return ()
      RunTH st q ty loc -> wrapRunTH $ runTH pipe st q ty loc
      RunModFinalizers st qrefs -> wrapRunTH $ runModFinalizerRefs pipe st qrefs
      _other -> run msg >>= reply

  reply :: forall a. (Binary a, Show a) => a -> IO ()
  reply r = do
    when verbose $ trace ("writing pipe: " ++ show r)
    writePipe pipe (put r)
    loop

  -- Run some TH code, which may interact with GHC by sending
  -- THMessage requests, and then finally send RunTHDone followed by a
  -- QResult.  For an overview of how TH works with Remote GHCi, see
  -- Note [Remote Template Haskell] in libraries/ghci/GHCi/TH.hs.
  wrapRunTH :: forall a. (Binary a, Show a) => IO a -> IO ()
  wrapRunTH io = do
    when verbose $ trace "wrapRunTH..."
    r <- try io
    when verbose $ trace "wrapRunTH done."
    when verbose $ trace "writing RunTHDone."
    writePipe pipe (putTHMessage RunTHDone)
    case r of
      Left e
        | Just (GHCiQException _ err) <- fromException e  -> do
           when verbose $ trace ("QFail " ++ show err)
           reply (QFail err :: QResult a)
        | otherwise -> do
           str <- showException e
           when verbose $ trace ("QException " ++ str)
           reply (QException str :: QResult a)
      Right a -> do
        when verbose $ trace "QDone"
        reply (QDone a)

  -- carefully when showing an exception, there might be other exceptions
  -- lurking inside it.  If so, we return the inner exception instead.
  showException :: SomeException -> IO String
  showException e0 = do
     when verbose $ trace "showException"
     r <- try $ evaluate (force (show (e0::SomeException)))
     case r of
       Left e -> showException e
       Right str -> return str

  -- throw away any pending ^C exceptions while we're not running
  -- interpreted code.  GHC will also get the ^C, and either ignore it
  -- (if this is GHCi), or tell us to quit with a Shutdown message.
  discardCtrlC = do
    when verbose $ trace "discardCtrlC"
    r <- try $ restore $ return ()
    case r of
      Left UserInterrupt -> return () >> discardCtrlC
      Left e -> throwIO e
      _ -> return ()

-- | Default server
#if defined(wasm32_HOST_ARCH)
defaultServer :: Callback (IO JSUint8Array) -> Callback (JSUint8Array -> IO ()) -> IO ()
defaultServer cb_recv cb_send = do
  args <- getArgs
  let rest = args
#else
defaultServer :: IO ()
defaultServer = do
  args <- getArgs
  (outh, inh, rest) <-
      case args of
        arg0:arg1:rest -> do
            inh  <- readGhcHandle arg1
            outh <- readGhcHandle arg0
            return (outh, inh, rest)
        _ -> dieWithUsage
#endif

  (verbose, rest') <- case rest of
    "-v":rest' -> return (True, rest')
    _ -> return (False, rest)

  (wait, rest'') <- case rest' of
    "-wait":rest'' -> return (True, rest'')
    _ -> return (False, rest')

  unless (null rest'') $
    dieWithUsage

#if defined(wasm32_HOST_ARCH)
  pipe <- mkPipeFromContinuations (recv_buf cb_recv) (send_buf cb_send)
#else
  when verbose $
    printf "GHC iserv starting (in: %s; out: %s)\n" (show inh) (show outh)
  installSignalHandlers
  pipe <- mkPipeFromHandles inh outh
#endif

  when wait $ do
    when verbose $
      putStrLn "Waiting 3s"
    threadDelay 3000000

  uninterruptibleMask $ serv verbose hook pipe

  where hook = return -- empty hook
    -- we cannot allow any async exceptions while communicating, because
    -- we will lose sync in the protocol, hence uninterruptibleMask.

dieWithUsage :: IO a
dieWithUsage = do
    prog <- getProgName
    die $ prog ++ ": " ++ msg
  where
#if defined(WINDOWS)
    msg = "usage: iserv <write-handle> <read-handle> [-v]"
#else
    msg = "usage: iserv <write-fd> <read-fd> [-v]"
#endif

#if defined(wasm32_HOST_ARCH)

newtype Callback a = Callback JSVal

newtype JSUint8Array = JSUint8Array { unJSUint8Array :: JSVal }

recv_buf :: Callback (IO JSUint8Array) -> IO ByteString
recv_buf cb = do
  buf <- js_recv_buf cb
  len <- js_buf_len buf
  fp <- mallocForeignPtrBytes len
  js_download_buf buf $ unsafeForeignPtrToPtr fp
  freeJSVal $ unJSUint8Array buf
  evaluate $ B.fromForeignPtr0 fp len

send_buf :: Callback (JSUint8Array -> IO ()) -> B.Builder -> IO ()
send_buf cb b = do
  buf <- evaluate $ LB.toStrict $ B.toLazyByteString b
  B.unsafeUseAsCStringLen buf $ \(ptr, len) -> js_send_buf cb ptr len

foreign import javascript "dynamic"
  js_recv_buf :: Callback (IO JSUint8Array) -> IO JSUint8Array

foreign import javascript unsafe "$1.byteLength"
  js_buf_len :: JSUint8Array -> IO Int

foreign import javascript unsafe "(new Uint8Array(__exports.memory.buffer, $2, $1.byteLength)).set($1)"
  js_download_buf :: JSUint8Array -> Ptr a -> IO ()

foreign import javascript unsafe "$1(new Uint8Array(__exports.memory.buffer, $2, $3))"
  js_send_buf :: Callback (JSUint8Array -> IO ()) -> Ptr a -> Int -> IO ()

foreign export javascript "defaultServer"
  defaultServer :: Callback (IO JSUint8Array) -> Callback (JSUint8Array -> IO ()) -> IO ()

#endif
