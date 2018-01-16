module System.Console.Haskeline.Backend.Posix (
                        withPosixGetEvent,
                        posixLayouts,
                        tryGetLayouts,
                        PosixT,
                        Handles(),
                        ehIn,
                        ehOut,
                        mapLines,
                        stdinTTYHandles,
                        ttyHandles,
                        posixRunTerm,
                        fileRunTerm
                 ) where

import Foreign
import Foreign.C.Types
import qualified Data.Map as Map
import System.Posix.Terminal hiding (Interrupt)
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent hiding (throwTo)
import Data.Maybe (catMaybes)
import System.Posix.Signals.Exts
import System.Posix.Types(Fd(..))
import Data.List
import System.IO
import System.Environment

import System.Console.Haskeline.Monads hiding (Handler)
import System.Console.Haskeline.Key
import System.Console.Haskeline.Term as Term
import System.Console.Haskeline.Prefs

import System.Console.Haskeline.Backend.Posix.Encoder

import GHC.IO.FD (fdFD)
import Data.Typeable (cast)
import System.IO.Error
import GHC.IO.Exception
import GHC.IO.Handle.Types hiding (getState)
import GHC.IO.Handle.Internals
import System.Posix.Internals (FD)

#if defined(USE_TERMIOS_H) || defined(__ANDROID__)
#include <termios.h>
#endif
#include <sys/ioctl.h>

-----------------------------------------------
-- Input/output handles
data Handles = Handles {hIn, hOut :: ExternalHandle
                        , closeHandles :: IO ()}

ehIn, ehOut :: Handles -> Handle
ehIn = eH . hIn
ehOut = eH . hOut

-------------------
-- Window size

foreign import ccall ioctl :: FD -> CULong -> Ptr a -> IO CInt

posixLayouts :: Handles -> [IO (Maybe Layout)]
posixLayouts h = [ioctlLayout $ ehOut h, envLayout]

ioctlLayout :: Handle -> IO (Maybe Layout)
ioctlLayout h = allocaBytes (#size struct winsize) $ \ws -> do
                fd <- unsafeHandleToFD h
                ret <- ioctl fd (#const TIOCGWINSZ) ws
                rows :: CUShort <- (#peek struct winsize,ws_row) ws
                cols :: CUShort <- (#peek struct winsize,ws_col) ws
                if ret >= 0
                    then return $ Just Layout {height=fromEnum rows,width=fromEnum cols}
                    else return Nothing

unsafeHandleToFD :: Handle -> IO FD
unsafeHandleToFD h =
  withHandle_ "unsafeHandleToFd" h $ \Handle__{haDevice=dev} -> do
  case cast dev of
    Nothing -> ioError (ioeSetErrorString (mkIOError IllegalOperation
                                           "unsafeHandleToFd" (Just h) Nothing)
                        "handle is not a file descriptor")
    Just fd -> return (fdFD fd)

envLayout :: IO (Maybe Layout)
envLayout = handle (\(_::IOException) -> return Nothing) $ do
    -- note the handle catches both undefined envs and bad reads
    r <- getEnv "ROWS"
    c <- getEnv "COLUMNS"
    return $ Just $ Layout {height=read r,width=read c}

tryGetLayouts :: [IO (Maybe Layout)] -> IO Layout
tryGetLayouts [] = return Layout {height=24,width=80}
tryGetLayouts (f:fs) = do
    ml <- f
    case ml of
        Just l | height l > 2 && width l > 2 -> return l
        _ -> tryGetLayouts fs


--------------------
-- Key sequences

getKeySequences :: (MonadIO m, MonadReader Prefs m)
        => Handle -> [(String,Key)] -> m (TreeMap Char Key)
getKeySequences h tinfos = do
    sttys <- liftIO $ sttyKeys h
    customKeySeqs <- getCustomKeySeqs
    -- note ++ acts as a union; so the below favors sttys over tinfos
    return $ listToTree
        $ ansiKeys ++ tinfos ++ sttys ++ customKeySeqs
  where
    getCustomKeySeqs = do
        kseqs <- asks customKeySequences
        termName <- liftIO $ handle (\(_::IOException) -> return "") (getEnv "TERM")
        let isThisTerm = maybe True (==termName)
        return $ map (\(_,cs,k) ->(cs,k))
            $ filter (\(kseqs',_,_) -> isThisTerm kseqs')
            $ kseqs


ansiKeys :: [(String, Key)]
ansiKeys = [("\ESC[D",  simpleKey LeftKey)
            ,("\ESC[C",  simpleKey RightKey)
            ,("\ESC[A",  simpleKey UpKey)
            ,("\ESC[B",  simpleKey DownKey)
            ,("\b",      simpleKey Backspace)
            -- ctrl-left/right aren't a standard
            -- part of terminfo, but enough people have complained
            -- that I've decided to hard-code them in.
            -- (Note they will be overridden by terminfo or .haskeline.)
            -- These appear to be the most common bindings:
            -- xterm:
            ,("\ESC[1;5D", ctrlKey $ simpleKey LeftKey)
            ,("\ESC[1;5C", ctrlKey $ simpleKey RightKey)
            -- Terminal.app:
            ,("\ESC[5D", ctrlKey $ simpleKey LeftKey)
            ,("\ESC[5C", ctrlKey $ simpleKey RightKey)
            -- rxvt: (Note: these will be superceded by e.g. xterm-color,
            -- which uses them as regular arrow keys.)
            ,("\ESC[OD", ctrlKey $ simpleKey LeftKey)
            ,("\ESC[OC", ctrlKey $ simpleKey RightKey)
            ]


sttyKeys :: Handle -> IO [(String, Key)]
sttyKeys h = do
    fd <- unsafeHandleToFD h
    attrs <- getTerminalAttributes (Fd fd)
    let getStty (k,c) = do {str <- controlChar attrs k; return ([str],c)}
    return $ catMaybes $ map getStty [(Erase,simpleKey Backspace),(Kill,simpleKey KillLine)]

newtype TreeMap a b = TreeMap (Map.Map a (Maybe b, TreeMap a b))
                        deriving Show

emptyTreeMap :: TreeMap a b
emptyTreeMap = TreeMap Map.empty

insertIntoTree :: Ord a => ([a], b) -> TreeMap a b -> TreeMap a b
insertIntoTree ([],_) _ = error "Can't insert empty list into a treemap!"
insertIntoTree ((c:cs),k) (TreeMap m) = TreeMap (Map.alter f c m)
    where
        alterSubtree = insertIntoTree (cs,k)
        f Nothing = Just $ if null cs
                            then (Just k, emptyTreeMap)
                            else (Nothing, alterSubtree emptyTreeMap)
        f (Just (y,t)) = Just $ if null cs
                                    then (Just k, t)
                                    else (y, alterSubtree t)

listToTree :: Ord a => [([a],b)] -> TreeMap a b
listToTree = foldl' (flip insertIntoTree) emptyTreeMap

-- for debugging '
mapLines :: (Show a, Show b) => TreeMap a b -> [String]
mapLines (TreeMap m) = let
    m2 = Map.map (\(k,t) -> show k : mapLines t) m
    in concatMap (\(k,ls) -> show k : map (' ':) ls) $ Map.toList m2

lexKeys :: TreeMap Char Key -> [Char] -> [Key]
lexKeys _ [] = []
lexKeys baseMap cs
    | Just (k,ds) <- lookupChars baseMap cs
            = k : lexKeys baseMap ds
lexKeys baseMap ('\ESC':cs)
-- TODO: what's the right thing ' to do here?
    | k:ks <- lexKeys baseMap cs
            = metaKey k : ks
lexKeys baseMap (c:cs) = simpleChar c : lexKeys baseMap cs

lookupChars :: TreeMap Char Key -> [Char] -> Maybe (Key,[Char])
lookupChars _ [] = Nothing
lookupChars (TreeMap tm) (c:cs) = case Map.lookup c tm of
    Nothing -> Nothing
    Just (Nothing,t) -> lookupChars t cs
    Just (Just k, t@(TreeMap tm2))
                | not (null cs) && not (Map.null tm2) -- ?? lookup d tm2?
                    -> lookupChars t cs
                | otherwise -> Just (k, cs)

-----------------------------

withPosixGetEvent :: (MonadException m, MonadReader Prefs m)
        => TChan Event -> Handles -> [(String,Key)]
                -> (m Event -> m a) -> m a
withPosixGetEvent eventChan h termKeys f = wrapTerminalOps h $ do
    baseMap <- getKeySequences (ehIn h) termKeys
    withWindowHandler eventChan
        $ f $ liftIO $ getEvent (ehIn h) baseMap eventChan

withWindowHandler :: MonadException m => TChan Event -> m a -> m a
withWindowHandler eventChan = withHandler windowChange $
    Catch $ atomically $ writeTChan eventChan WindowResize

withSigIntHandler :: MonadException m => m a -> m a
withSigIntHandler f = do
    tid <- liftIO myThreadId
    withHandler keyboardSignal
            (Catch (throwTo tid Interrupt))
            f

withHandler :: MonadException m => Signal -> Handler -> m a -> m a
withHandler signal handler f = do
    old_handler <- liftIO $ installHandler signal handler Nothing
    f `finally` liftIO (installHandler signal old_handler Nothing)

getEvent :: Handle -> TreeMap Char Key -> TChan Event -> IO Event
getEvent h baseMap = keyEventLoop $ do
        cs <- getBlockOfChars h
        return [KeyInput $ lexKeys baseMap cs]

-- Read at least one character of input, and more if immediately
-- available.  In particular the characters making up a control sequence
-- will all be available at once, so they can be processed together
-- (with Posix.lexKeys).
getBlockOfChars :: Handle -> IO String
getBlockOfChars h = do
    c <- hGetChar h
    loop [c]
  where
    loop cs = do
        isReady <- hReady h
        if not isReady
            then return $ reverse cs
            else do
                    c <- hGetChar h
                    loop (c:cs)

stdinTTYHandles, ttyHandles :: MaybeT IO Handles
stdinTTYHandles = do
    isInTerm <- liftIO $ hIsTerminalDevice stdin
    guard isInTerm
    h <- openTerm WriteMode
    -- Don't close stdin, since a different part of the program may use it later.
    return Handles
            { hIn = externalHandle stdin
            , hOut = h
            , closeHandles = hClose $ eH h
            }

ttyHandles = do
    -- Open the input and output as two separate Handles, since they need
    -- different buffering.
    h_in <- openTerm ReadMode
    h_out <- openTerm WriteMode
    return Handles
            { hIn = h_in
            , hOut = h_out
            , closeHandles = hClose (eH h_in) >> hClose (eH h_out)
            }

openTerm :: IOMode -> MaybeT IO ExternalHandle
openTerm mode = handle (\(_::IOException) -> mzero)
            $ liftIO $ openInCodingMode "/dev/tty" mode


posixRunTerm ::
    Handles
    -> [IO (Maybe Layout)]
    -> [(String,Key)]
    -> (forall m b . MonadException m => m b -> m b)
    -> (forall m . (MonadException m, CommandMonad m) => EvalTerm (PosixT m))
    -> IO RunTerm
posixRunTerm hs layoutGetters keys wrapGetEvent evalBackend = do
    ch <- newTChanIO
    fileRT <- posixFileRunTerm hs
    return fileRT
                { termOps = Left TermOps
                            { getLayout = tryGetLayouts layoutGetters
                            , withGetEvent = wrapGetEvent
                                            . withPosixGetEvent ch hs
                                                keys
                            , saveUnusedKeys = saveKeys ch
                            , evalTerm = mapEvalTerm
                                            (runPosixT hs) lift evalBackend
                            , externalPrint = atomically . writeTChan ch . ExternalPrint
                            }
                , closeTerm = do
                    flushEventQueue (putStrOut fileRT) ch
                    closeTerm fileRT
                }

type PosixT m = ReaderT Handles m

runPosixT :: Monad m => Handles -> PosixT m a -> m a
runPosixT h = runReaderT' h

fileRunTerm :: Handle -> IO RunTerm
fileRunTerm h_in = posixFileRunTerm Handles
                        { hIn = externalHandle h_in
                        , hOut = externalHandle stdout
                        , closeHandles = return ()
                        }

posixFileRunTerm :: Handles -> IO RunTerm
posixFileRunTerm hs = do
    return RunTerm
                { putStrOut = \str -> withCodingMode (hOut hs) $ do
                                        hPutStr (ehOut hs) str
                                        hFlush (ehOut hs)
                , closeTerm = closeHandles hs
                , wrapInterrupt = withSigIntHandler
                , termOps = let h_in = ehIn hs
                            in Right FileOps
                          { withoutInputEcho = bracketSet (hGetEcho h_in)
                                                          (hSetEcho h_in)
                                                          False
                          , wrapFileInput = withCodingMode (hIn hs)
                          , getLocaleChar = guardedEOF hGetChar h_in
                          , maybeReadNewline = hMaybeReadNewline h_in
                          , getLocaleLine = guardedEOF hGetLine h_in
                          }
                }

-- NOTE: If we set stdout to NoBuffering, there can be a flicker effect when many
-- characters are printed at once.  We'll keep it buffered here, and let the Draw
-- monad manually flush outputs that don't print a newline.
wrapTerminalOps :: MonadException m => Handles -> m a -> m a
wrapTerminalOps hs =
    bracketSet (hGetBuffering h_in) (hSetBuffering h_in) NoBuffering
    -- TODO: block buffering?  Certain \r and \n's are causing flicker...
    -- - moving to the right
    -- - breaking line after offset widechar?
    . bracketSet (hGetBuffering h_out) (hSetBuffering h_out) LineBuffering
    . bracketSet (hGetEcho h_in) (hSetEcho h_in) False
    . liftIOOp_ (withCodingMode $ hIn hs)
    . liftIOOp_ (withCodingMode $ hOut hs)
  where
    h_in = ehIn hs
    h_out = ehOut hs
