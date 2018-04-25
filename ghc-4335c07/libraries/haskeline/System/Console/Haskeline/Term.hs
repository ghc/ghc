module System.Console.Haskeline.Term where

import System.Console.Haskeline.Monads
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Key
import System.Console.Haskeline.Prefs(Prefs)
import System.Console.Haskeline.Completion(Completion)

import Control.Concurrent
import Control.Concurrent.STM
import Data.Word
import Control.Exception (fromException, AsyncException(..),bracket_)
import Data.Typeable
import System.IO
import Control.Monad(liftM,when,guard)
import System.IO.Error (isEOFError)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC

class (MonadReader Layout m, MonadException m) => Term m where
    reposition :: Layout -> LineChars -> m ()
    moveToNextLine :: LineChars -> m ()
    printLines :: [String] -> m ()
    drawLineDiff :: LineChars -> LineChars -> m ()
    clearLayout :: m ()
    ringBell :: Bool -> m ()

drawLine, clearLine :: Term m => LineChars -> m ()
drawLine = drawLineDiff ([],[])

clearLine = flip drawLineDiff ([],[])

data RunTerm = RunTerm {
            -- | Write unicode characters to stdout.
            putStrOut :: String -> IO (),
            termOps :: Either TermOps FileOps,
            wrapInterrupt :: forall a . IO a -> IO a,
            closeTerm :: IO ()
    }

-- | Operations needed for terminal-style interaction.
data TermOps = TermOps
    { getLayout :: IO Layout
    , withGetEvent :: forall m a . CommandMonad m => (m Event -> m a) -> m a
    , evalTerm :: forall m . CommandMonad m => EvalTerm m
    , saveUnusedKeys :: [Key] -> IO ()
    , externalPrint :: String -> IO ()
    }

-- This hack is needed to grab latest writes from some other thread.
-- Without it, if you are using another thread to process the logging
-- and write on screen via exposed externalPrint, latest writes from
-- this thread are not able to cross the thread boundary in time.
flushEventQueue :: (String -> IO ()) -> TChan Event -> IO ()
flushEventQueue print' eventChan = yield >> loopUntilFlushed
    where loopUntilFlushed = do
              flushed <- atomically $ isEmptyTChan eventChan
              if flushed then return () else do
                  event <- atomically $ readTChan eventChan
                  case event of
                      ExternalPrint str -> do
                          print' (str ++ "\n") >> loopUntilFlushed
                      -- We don't want to raise exceptions when doing cleanup.
                      _ -> loopUntilFlushed

-- | Operations needed for file-style interaction.
--
-- Backends can assume that getLocaleLine, getLocaleChar and maybeReadNewline
-- are "wrapped" by wrapFileInput.
data FileOps = FileOps {
            withoutInputEcho :: forall m a . MonadException m => m a -> m a,
            -- ^ Perform an action without echoing input.
            wrapFileInput :: forall a . IO a -> IO a,
            getLocaleLine :: MaybeT IO String,
            getLocaleChar :: MaybeT IO Char,
            maybeReadNewline :: IO ()
        }

-- | Are we using terminal-style interaction?
isTerminalStyle :: RunTerm -> Bool
isTerminalStyle r = case termOps r of
                    Left TermOps{} -> True
                    _ -> False

-- Specific, hidden terminal action type
-- Generic terminal actions which are independent of the Term being used.
data EvalTerm m
    = forall n . (Term n, CommandMonad n)
            => EvalTerm (forall a . n a -> m a) (forall a . m a -> n a)

mapEvalTerm :: (forall a . n a -> m a) -> (forall a . m a -> n a)
        -> EvalTerm n -> EvalTerm m
mapEvalTerm eval liftE (EvalTerm eval' liftE')
    = EvalTerm (eval . eval') (liftE' . liftE)

data Interrupt = Interrupt
                deriving (Show,Typeable,Eq)

instance Exception Interrupt where



class (MonadReader Prefs m , MonadReader Layout m, MonadException m)
        => CommandMonad m where
    runCompletion :: (String,String) -> m (String,[Completion])

instance (MonadTrans t, CommandMonad m, MonadReader Prefs (t m),
        MonadException (t m),
        MonadReader Layout (t m))
            => CommandMonad (t m) where
    runCompletion = lift . runCompletion

-- Utility function for drawLineDiff instances.
matchInit :: Eq a => [a] -> [a] -> ([a],[a])
matchInit (x:xs) (y:ys)  | x == y = matchInit xs ys
matchInit xs ys = (xs,ys)

data Event
  = WindowResize
  | KeyInput [Key]
  | ErrorEvent SomeException
  | ExternalPrint String
  deriving Show

keyEventLoop :: IO [Event] -> TChan Event -> IO Event
keyEventLoop readEvents eventChan = do
    -- first, see if any events are already queued up (from a key/ctrl-c
    -- event or from a previous call to getEvent where we read in multiple
    -- keys)
    isEmpty <- atomically $ isEmptyTChan eventChan
    if not isEmpty
        then atomically $ readTChan eventChan
        else do
            tid <- forkIO $ handleErrorEvent readerLoop
            atomically (readTChan eventChan) `finally` killThread tid
  where
    readerLoop = do
        es <- readEvents
        if null es
            then readerLoop
            else atomically $ mapM_ (writeTChan eventChan) es
    handleErrorEvent = handle $ \e -> case fromException e of
                                Just ThreadKilled -> return ()
                                _ -> atomically $ writeTChan eventChan (ErrorEvent e)

saveKeys :: TChan Event -> [Key] -> IO ()
saveKeys ch = atomically . writeTChan ch . KeyInput

data Layout = Layout {width, height :: Int}
                    deriving (Show,Eq)

-----------------------------------
-- Utility functions for the various backends.

-- | Utility function since we're not using the new IO library yet.
hWithBinaryMode :: MonadException m => Handle -> m a -> m a
hWithBinaryMode h = bracket (liftIO $ hGetEncoding h)
                        (maybe (return ()) (liftIO . hSetEncoding h))
                        . const . (liftIO (hSetBinaryMode h True) >>)

-- | Utility function for changing a property of a terminal for the duration of
-- a computation.
bracketSet :: (Eq a, MonadException m) => IO a -> (a -> IO ()) -> a -> m b -> m b
bracketSet getState set newState f = bracket (liftIO getState)
                            (liftIO . set)
                            (\_ -> liftIO (set newState) >> f)

-- | Returns one 8-bit word.  Needs to be wrapped by hWithBinaryMode.
hGetByte :: Handle -> MaybeT IO Word8
hGetByte = guardedEOF $ liftM (toEnum . fromEnum) . hGetChar

guardedEOF :: (Handle -> IO a) -> Handle -> MaybeT IO a
guardedEOF f h = do
    eof <- lift $ hIsEOF h
    guard (not eof)
    lift $ f h

-- If another character is immediately available, and it is a newline, consume it.
--
-- Two portability fixes:
--
-- 1) By itself, this (by using hReady) might crash on invalid characters.
-- The handle should be set to binary mode or a TextEncoder that
-- transliterates or ignores invalid input.
--
-- 1) Note that in ghc-6.8.3 and earlier, hReady returns False at an EOF,
-- whereas in ghc-6.10.1 and later it throws an exception.  (GHC trac #1063).
-- This code handles both of those cases.
hMaybeReadNewline :: Handle -> IO ()
hMaybeReadNewline h = returnOnEOF () $ do
    ready <- hReady h
    when ready $ do
        c <- hLookAhead h
        when (c == '\n') $ getChar >> return ()

returnOnEOF :: MonadException m => a -> m a -> m a
returnOnEOF x = handle $ \e -> if isEOFError e
                                then return x
                                else throwIO e

-- | Utility function to correctly get a line of input as an undecoded ByteString.
hGetLocaleLine :: Handle -> MaybeT IO ByteString
hGetLocaleLine = guardedEOF $ \h -> do
    -- It's more efficient to use B.getLine, but that function throws an
    -- error if the Handle (e.g., stdin) is set to NoBuffering.
    buff <- liftIO $ hGetBuffering h
    liftIO $ if buff == NoBuffering
        then fmap BC.pack $ System.IO.hGetLine h
        else BC.hGetLine h
