{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Utils
-- Copyright   :  Isaac Jones, Simon Marlow 2003-2004
-- License     :  BSD3
--                portions Copyright (c) 2007, Galois Inc.
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- A large and somewhat miscellaneous collection of utility functions used
-- throughout the rest of the Cabal lib and in other tools that use the Cabal
-- lib like @cabal-install@. It has a very simple set of logging actions. It
-- has low level functions for running programs, a bunch of wrappers for
-- various directory and file functions that do extra logging.

module Distribution.Simple.Utils (
        cabalVersion,

        -- * logging and errors
        -- Old style
        die, dieWithLocation,
        -- New style
        dieNoVerbosity,
        die', dieWithLocation',
        dieNoWrap,
        topHandler, topHandlerWith,
        warn,
        notice, noticeNoWrap, noticeDoc,
        setupMessage,
        info, infoNoWrap,
        debug, debugNoWrap,
        chattyTry,
        annotateIO,
        printRawCommandAndArgs, printRawCommandAndArgsAndEnv,

        -- * exceptions
        handleDoesNotExist,

        -- * running programs
        rawSystemExit,
        rawSystemExitCode,
        rawSystemExitWithEnv,
        rawSystemStdout,
        rawSystemStdInOut,
        rawSystemIOWithEnv,
        createProcessWithEnv,
        maybeExit,
        xargs,
        findProgramLocation,
        findProgramVersion,

        -- ** 'IOData' re-export
        --
        -- These types are re-exported from
        -- "Distribution.Utils.IOData" for convience as they're
        -- exposed in the API of 'rawSystemStdInOut'
        IOData(..),
        IODataMode(..),

        -- * copying files
        smartCopySources,
        createDirectoryIfMissingVerbose,
        copyFileVerbose,
        copyDirectoryRecursiveVerbose,
        copyFiles,
        copyFileTo,

        -- * installing files
        installOrdinaryFile,
        installExecutableFile,
        installMaybeExecutableFile,
        installOrdinaryFiles,
        installExecutableFiles,
        installMaybeExecutableFiles,
        installDirectoryContents,
        copyDirectoryRecursive,

        -- * File permissions
        doesExecutableExist,
        setFileOrdinary,
        setFileExecutable,

        -- * file names
        currentDir,
        shortRelativePath,
        dropExeExtension,
        exeExtensions,

        -- * finding files
        findFile,
        findFirstFile,
        findFileWithExtension,
        findFileWithExtension',
        findAllFilesWithExtension,
        findModuleFile,
        findModuleFiles,
        getDirectoryContentsRecursive,

        -- * environment variables
        isInSearchPath,
        addLibraryPath,

        -- * simple file globbing
        matchFileGlob,
        matchDirFileGlob,
        parseFileGlob,
        FileGlob(..),

        -- * modification time
        moreRecentFile,
        existsAndIsMoreRecentThan,

        -- * temp files and dirs
        TempFileOptions(..), defaultTempFileOptions,
        withTempFile, withTempFileEx,
        withTempDirectory, withTempDirectoryEx,

        -- * .cabal and .buildinfo files
        defaultPackageDesc,
        findPackageDesc,
        tryFindPackageDesc,
        defaultHookedPackageDesc,
        findHookedPackageDesc,

        -- * reading and writing files safely
        withFileContents,
        writeFileAtomic,
        rewriteFile,

        -- * Unicode
        fromUTF8BS,
        fromUTF8LBS,
        toUTF8BS,
        toUTF8LBS,
        readUTF8File,
        withUTF8FileContents,
        writeUTF8File,
        normaliseLineEndings,

        -- * BOM
        ignoreBOM,

        -- * generic utils
        dropWhileEndLE,
        takeWhileEndLE,
        equating,
        comparing,
        isInfixOf,
        intercalate,
        lowercase,
        listUnion,
        listUnionRight,
        ordNub,
        ordNubBy,
        ordNubRight,
        safeTail,
        unintersperse,
        wrapText,
        wrapLine,

        -- * FilePath stuff
        isAbsoluteOnAnyPlatform,
        isRelativeOnAnyPlatform,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Text
import Distribution.Utils.Generic
import Distribution.Utils.IOData (IOData(..), IODataMode(..))
import qualified Distribution.Utils.IOData as IOData
import Distribution.ModuleName as ModuleName
import Distribution.System
import Distribution.Version
import Distribution.Compat.CopyFile
import Distribution.Compat.Internal.TempFile
import Distribution.Compat.Exception
import Distribution.Compat.Stack
import Distribution.Verbosity
import Distribution.Types.PackageId

#if __GLASGOW_HASKELL__ < 711
#ifdef VERSION_base
#define BOOTSTRAPPED_CABAL 1
#endif
#else
#ifdef CURRENT_PACKAGE_KEY
#define BOOTSTRAPPED_CABAL 1
#endif
#endif

#ifdef BOOTSTRAPPED_CABAL
import qualified Paths_Cabal (version)
#endif

import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Data.Typeable
    ( cast )
import qualified Data.ByteString.Lazy.Char8 as BS.Char8

import System.Directory
    ( Permissions(executable), getDirectoryContents, getPermissions
    , doesDirectoryExist, doesFileExist, removeFile, findExecutable
    , getModificationTime, createDirectory, removeDirectoryRecursive )
import System.Environment
    ( getProgName )
import System.Exit
    ( exitWith, ExitCode(..) )
import System.FilePath
    ( normalise, (</>), (<.>)
    , getSearchPath, joinPath, takeDirectory, splitFileName
    , splitExtension, splitExtensions, splitDirectories
    , searchPathSeparator )
import System.IO
    ( Handle, hSetBinaryMode, hGetContents, stderr, stdout, hPutStr, hFlush
    , hClose, hSetBuffering, BufferMode(..) )
import System.IO.Error
import System.IO.Unsafe
    ( unsafeInterleaveIO )
import qualified Control.Exception as Exception

import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import Control.Exception (IOException, evaluate, throwIO)
import Control.Concurrent (forkIO)
import Numeric (showFFloat)
import qualified System.Process as Process
         ( CreateProcess(..), StdStream(..), proc)
import System.Process
         ( ProcessHandle, createProcess, rawSystem, runInteractiveProcess
         , showCommandForUser, waitForProcess)

import qualified Text.PrettyPrint as Disp

-- We only get our own version number when we're building with ourselves
cabalVersion :: Version
#if defined(BOOTSTRAPPED_CABAL)
cabalVersion = mkVersion' Paths_Cabal.version
#elif defined(CABAL_VERSION)
cabalVersion = mkVersion [CABAL_VERSION]
#else
cabalVersion = mkVersion [1,9999]  --used when bootstrapping
#endif

-- ----------------------------------------------------------------------------
-- Exception and logging utils

-- Cabal's logging infrastructure has a few constraints:
--
--  * We must make all logging formatting and emissions decisions based
--    on the 'Verbosity' parameter, which is the only parameter that is
--    plumbed to enough call-sites to actually be used for this matter.
--    (One of Cabal's "big mistakes" is to have never have defined a
--    monad of its own.)
--
--  * When we 'die', we must raise an IOError.  This a backwards
--    compatibility consideration, because that's what we've raised
--    previously, and if we change to any other exception type,
--    exception handlers which match on IOError will no longer work.
--    One case where it is known we rely on IOError being catchable
--    is 'readPkgConfigDb' in cabal-install; there may be other
--    user code that also assumes this.
--
--  * The 'topHandler' does not know what 'Verbosity' is, because
--    it gets called before we've done command line parsing (where
--    the 'Verbosity' parameter would come from).
--
-- This leads to two big architectural choices:
--
--  * Although naively we might imagine 'Verbosity' to be a simple
--    enumeration type, actually it is a full-on abstract data type
--    that may contain arbitrarily complex information.  At the
--    moment, it is fully representable as a string, but we might
--    eventually also use verbosity to let users register their
--    own logging handler.
--
--  * When we call 'die', we perform all the formatting and addition
--    of extra information we need, and then ship this in the IOError
--    to the top-level handler.  Here are alternate designs that
--    don't work:
--
--      a) Ship the unformatted info to the handler.  This doesn't
--      work because at the point the handler gets the message,
--      we've lost call stacks, and even if we did, we don't have access
--      to 'Verbosity' to decide whether or not to render it.
--
--      b) Print the information at the 'die' site, then raise an
--      error.  This means that if the exception is subsequently
--      caught by a handler, we will still have emitted the output,
--      which is not the correct behavior.
--
--    For the top-level handler to "know" that an error message
--    contains one of these fully formatted packets, we set a sentinel
--    in one of IOError's extra fields.  This is handled by
--    'ioeSetVerbatim' and 'ioeGetVerbatim'.
--

{-# DEPRECATED dieWithLocation "Messages thrown with dieWithLocation can't be controlled with Verbosity; use dieWithLocation' instead" #-}
dieWithLocation :: FilePath -> Maybe Int -> String -> IO a
dieWithLocation filename lineno msg =
  ioError . setLocation lineno
          . flip ioeSetFileName (normalise filename)
          $ userError msg
  where
    setLocation Nothing  err = err
    setLocation (Just n) err = ioeSetLocation err (show n)
    _ = callStack -- TODO: Attach CallStack to exception

{-# DEPRECATED die "Messages thrown with die can't be controlled with Verbosity; use die' instead, or dieNoVerbosity if Verbosity truly is not available" #-}
die :: String -> IO a
die = dieNoVerbosity

dieNoVerbosity :: String -> IO a
dieNoVerbosity msg
    = ioError (userError msg)
  where
    _ = callStack -- TODO: Attach CallStack to exception

-- | Tag an 'IOError' whose error string should be output to the screen
-- verbatim.
ioeSetVerbatim :: IOError -> IOError
ioeSetVerbatim e = ioeSetLocation e "dieVerbatim"

-- | Check if an 'IOError' should be output verbatim to screen.
ioeGetVerbatim :: IOError -> Bool
ioeGetVerbatim e = ioeGetLocation e == "dieVerbatim"

-- | Create a 'userError' whose error text will be output verbatim
verbatimUserError :: String -> IOError
verbatimUserError = ioeSetVerbatim . userError

dieWithLocation' :: Verbosity -> FilePath -> Maybe Int -> String -> IO a
dieWithLocation' verbosity filename mb_lineno msg = withFrozenCallStack $ do
    ts <- getPOSIXTime
    pname <- getProgName
    ioError . verbatimUserError
            . withMetadata ts AlwaysMark VerboseTrace verbosity
            . wrapTextVerbosity verbosity
            $ pname ++ ": " ++
              filename ++ (case mb_lineno of
                            Just lineno -> ":" ++ show lineno
                            Nothing -> "") ++
              ": " ++ msg

die' :: Verbosity -> String -> IO a
die' verbosity msg = withFrozenCallStack $ do
    ts <- getPOSIXTime
    pname <- getProgName
    ioError . verbatimUserError
            . withMetadata ts AlwaysMark VerboseTrace verbosity
            . wrapTextVerbosity verbosity
            $ pname ++ ": " ++ msg

dieNoWrap :: Verbosity -> String -> IO a
dieNoWrap verbosity msg = withFrozenCallStack $ do
    -- TODO: should this have program name or not?
    ts <- getPOSIXTime
    ioError . verbatimUserError
            . withMetadata ts AlwaysMark VerboseTrace verbosity
            $ msg

-- | Given a block of IO code that may raise an exception, annotate
-- it with the metadata from the current scope.  Use this as close
-- to external code that raises IO exceptions as possible, since
-- this function unconditionally wraps the error message with a trace
-- (so it is NOT idempotent.)
annotateIO :: Verbosity -> IO a -> IO a
annotateIO verbosity act = do
    ts <- getPOSIXTime
    modifyIOError (f ts) act
  where
    f ts ioe = ioeSetErrorString ioe
             . withMetadata ts NeverMark VerboseTrace verbosity
             $ ioeGetErrorString ioe


{-# NOINLINE topHandlerWith #-}
topHandlerWith :: forall a. (Exception.SomeException -> IO a) -> IO a -> IO a
topHandlerWith cont prog = do
    -- By default, stderr to a terminal device is NoBuffering. But this
    -- is *really slow*
    hSetBuffering stderr LineBuffering
    Exception.catches prog [
        Exception.Handler rethrowAsyncExceptions
      , Exception.Handler rethrowExitStatus
      , Exception.Handler handle
      ]
  where
    -- Let async exceptions rise to the top for the default top-handler
    rethrowAsyncExceptions :: Exception.AsyncException -> NoCallStackIO a
    rethrowAsyncExceptions a = throwIO a

    -- ExitCode gets thrown asynchronously too, and we don't want to print it
    rethrowExitStatus :: ExitCode -> NoCallStackIO a
    rethrowExitStatus = throwIO

    -- Print all other exceptions
    handle :: Exception.SomeException -> NoCallStackIO a
    handle se = do
      hFlush stdout
      pname <- getProgName
      hPutStr stderr (message pname se)
      cont se

    message :: String -> Exception.SomeException -> String
    message pname (Exception.SomeException se) =
      case cast se :: Maybe Exception.IOException of
        Just ioe
         | ioeGetVerbatim ioe ->
            -- Use the message verbatim
            ioeGetErrorString ioe ++ "\n"
         | isUserError ioe ->
          let file         = case ioeGetFileName ioe of
                               Nothing   -> ""
                               Just path -> path ++ location ++ ": "
              location     = case ioeGetLocation ioe of
                               l@(n:_) | isDigit n -> ':' : l
                               _                        -> ""
              detail       = ioeGetErrorString ioe
          in wrapText (pname ++ ": " ++ file ++ detail)
        _ ->
          displaySomeException se ++ "\n"

-- | BC wrapper around 'Exception.displayException'.
displaySomeException :: Exception.Exception e => e -> String
displaySomeException se =
#if __GLASGOW_HASKELL__ < 710
    show se
#else
    Exception.displayException se
#endif

topHandler :: IO a -> IO a
topHandler prog = topHandlerWith (const $ exitWith (ExitFailure 1)) prog

-- | Non fatal conditions that may be indicative of an error or problem.
--
-- We display these at the 'normal' verbosity level.
--
warn :: Verbosity -> String -> IO ()
warn verbosity msg = withFrozenCallStack $ do
  when (verbosity >= normal) $ do
    ts <- getPOSIXTime
    hFlush stdout
    hPutStr stderr . withMetadata ts NormalMark FlagTrace verbosity
                   . wrapTextVerbosity verbosity
                   $ "Warning: " ++ msg

-- | Useful status messages.
--
-- We display these at the 'normal' verbosity level.
--
-- This is for the ordinary helpful status messages that users see. Just
-- enough information to know that things are working but not floods of detail.
--
notice :: Verbosity -> String -> IO ()
notice verbosity msg = withFrozenCallStack $ do
  when (verbosity >= normal) $ do
    ts <- getPOSIXTime
    hPutStr stdout . withMetadata ts NormalMark FlagTrace verbosity
                   . wrapTextVerbosity verbosity
                   $ msg

-- | Display a message at 'normal' verbosity level, but without
-- wrapping.
--
noticeNoWrap :: Verbosity -> String -> IO ()
noticeNoWrap verbosity msg = withFrozenCallStack $ do
  when (verbosity >= normal) $ do
    ts <- getPOSIXTime
    hPutStr stdout . withMetadata ts NormalMark FlagTrace verbosity $ msg

-- | Pretty-print a 'Disp.Doc' status message at 'normal' verbosity
-- level.  Use this if you need fancy formatting.
--
noticeDoc :: Verbosity -> Disp.Doc -> IO ()
noticeDoc verbosity msg = withFrozenCallStack $ do
  when (verbosity >= normal) $ do
    ts <- getPOSIXTime
    hPutStr stdout . withMetadata ts NormalMark FlagTrace verbosity
                   . Disp.renderStyle defaultStyle $ msg

-- | Display a "setup status message".  Prefer using setupMessage'
-- if possible.
--
setupMessage :: Verbosity -> String -> PackageIdentifier -> IO ()
setupMessage verbosity msg pkgid = withFrozenCallStack $ do
    noticeNoWrap verbosity (msg ++ ' ': display pkgid ++ "...")

-- | More detail on the operation of some action.
--
-- We display these messages when the verbosity level is 'verbose'
--
info :: Verbosity -> String -> IO ()
info verbosity msg = withFrozenCallStack $
  when (verbosity >= verbose) $ do
    ts <- getPOSIXTime
    hPutStr stdout . withMetadata ts NeverMark FlagTrace verbosity
                   . wrapTextVerbosity verbosity
                   $ msg

infoNoWrap :: Verbosity -> String -> IO ()
infoNoWrap verbosity msg = withFrozenCallStack $
  when (verbosity >= verbose) $ do
    ts <- getPOSIXTime
    hPutStr stdout . withMetadata ts NeverMark FlagTrace verbosity
                   $ msg

-- | Detailed internal debugging information
--
-- We display these messages when the verbosity level is 'deafening'
--
debug :: Verbosity -> String -> IO ()
debug verbosity msg = withFrozenCallStack $
  when (verbosity >= deafening) $ do
    ts <- getPOSIXTime
    hPutStr stdout . withMetadata ts NeverMark FlagTrace verbosity
                   . wrapTextVerbosity verbosity
                   $ msg
    -- ensure that we don't lose output if we segfault/infinite loop
    hFlush stdout

-- | A variant of 'debug' that doesn't perform the automatic line
-- wrapping. Produces better output in some cases.
debugNoWrap :: Verbosity -> String -> IO ()
debugNoWrap verbosity msg = withFrozenCallStack $
  when (verbosity >= deafening) $ do
    ts <- getPOSIXTime
    hPutStr stdout . withMetadata ts NeverMark FlagTrace verbosity
                   $ msg
    -- ensure that we don't lose output if we segfault/infinite loop
    hFlush stdout

-- | Perform an IO action, catching any IO exceptions and printing an error
--   if one occurs.
chattyTry :: String  -- ^ a description of the action we were attempting
          -> IO ()   -- ^ the action itself
          -> IO ()
chattyTry desc action =
  catchIO action $ \exception ->
    putStrLn $ "Error while " ++ desc ++ ": " ++ show exception

-- | Run an IO computation, returning @e@ if it raises a "file
-- does not exist" error.
handleDoesNotExist :: a -> NoCallStackIO a -> NoCallStackIO a
handleDoesNotExist e =
    Exception.handleJust
      (\ioe -> if isDoesNotExistError ioe then Just ioe else Nothing)
      (\_ -> return e)

-- -----------------------------------------------------------------------------
-- Helper functions

-- | Wraps text unless the @+nowrap@ verbosity flag is active
wrapTextVerbosity :: Verbosity -> String -> String
wrapTextVerbosity verb
  | isVerboseNoWrap verb = withTrailingNewline
  | otherwise            = withTrailingNewline . wrapText


-- | Prepends a timestamp if @+timestamp@ verbosity flag is set
--
-- This is used by 'withMetadata'
--
withTimestamp :: Verbosity -> POSIXTime -> String -> String
withTimestamp v ts msg
  | isVerboseTimestamp v  = msg'
  | otherwise             = msg -- no-op
  where
    msg' = case lines msg of
      []      -> tsstr "\n"
      l1:rest -> unlines (tsstr (' ':l1) : map (contpfx++) rest)

    -- format timestamp to be prepended to first line with msec precision
    tsstr = showFFloat (Just 3) (realToFrac ts :: Double)

    -- continuation prefix for subsequent lines of msg
    contpfx = replicate (length (tsstr " ")) ' '

-- | Wrap output with a marker if @+markoutput@ verbosity flag is set.
--
-- NB: Why is markoutput done with start/end markers, and not prefixes?
-- Markers are more convenient to add (if we want to add prefixes,
-- we have to 'lines' and then 'map'; here's it's just some
-- concatenates).  Note that even in the prefix case, we can't
-- guarantee that the markers are unambiguous, because some of
-- Cabal's output comes straight from external programs, where
-- we don't have the ability to interpose on the output.
--
-- This is used by 'withMetadata'
--
withOutputMarker :: Verbosity -> String -> String
withOutputMarker v xs | not (isVerboseMarkOutput v) = xs
withOutputMarker _ "" = "" -- Minor optimization, don't mark uselessly
withOutputMarker _ xs =
    "-----BEGIN CABAL OUTPUT-----\n" ++
    withTrailingNewline xs ++
    "-----END CABAL OUTPUT-----\n"

-- | Append a trailing newline to a string if it does not
-- already have a trailing newline.
--
withTrailingNewline :: String -> String
withTrailingNewline "" = ""
withTrailingNewline (x:xs) = x : go x xs
  where
    go   _ (c:cs) = c : go c cs
    go '\n' "" = ""
    go   _  "" = "\n"

-- | Prepend a call-site and/or call-stack based on Verbosity
--
withCallStackPrefix :: WithCallStack (TraceWhen -> Verbosity -> String -> String)
withCallStackPrefix tracer verbosity s = withFrozenCallStack $
    (if isVerboseCallSite verbosity
        then parentSrcLocPrefix ++
             -- Hack: need a newline before starting output marker :(
             if isVerboseMarkOutput verbosity
                then "\n"
                else ""
        else "") ++
    (case traceWhen verbosity tracer of
        Just pre -> pre ++ prettyCallStack callStack ++ "\n"
        Nothing  -> "") ++
    s

-- | When should we emit the call stack?  We always emit
-- for internal errors, emit the trace for errors when we
-- are in verbose mode, and otherwise only emit it if
-- explicitly asked for using the @+callstack@ verbosity
-- flag.  (At the moment, 'AlwaysTrace' is not used.
--
data TraceWhen
    = AlwaysTrace
    | VerboseTrace
    | FlagTrace
    deriving (Eq)

-- | Determine if we should emit a call stack.
-- If we trace, it also emits any prefix we should append.
traceWhen :: Verbosity -> TraceWhen -> Maybe String
traceWhen _ AlwaysTrace = Just ""
traceWhen v VerboseTrace | v >= verbose         = Just ""
traceWhen v FlagTrace    | isVerboseCallStack v = Just "----\n"
traceWhen _ _ = Nothing

-- | When should we output the marker?  Things like 'die'
-- always get marked, but a 'NormalMark' will only be
-- output if we're not a quiet verbosity.
--
data MarkWhen = AlwaysMark | NormalMark | NeverMark

-- | Add all necessary metadata to a logging message
--
withMetadata :: WithCallStack (POSIXTime -> MarkWhen -> TraceWhen -> Verbosity -> String -> String)
withMetadata ts marker tracer verbosity x = withFrozenCallStack $
    -- NB: order matters.  Output marker first because we
    -- don't want to capture call stacks.
      withTrailingNewline
    . withCallStackPrefix tracer verbosity
    . (case marker of
        AlwaysMark -> withOutputMarker verbosity
        NormalMark | not (isVerboseQuiet verbosity)
                   -> withOutputMarker verbosity
                   | otherwise
                   -> id
        NeverMark  -> id)
    -- Clear out any existing markers
    . clearMarkers
    . withTimestamp verbosity ts
    $ x

clearMarkers :: String -> String
clearMarkers s = unlines . filter isMarker $ lines s
  where
    isMarker "-----BEGIN CABAL OUTPUT-----" = False
    isMarker "-----END CABAL OUTPUT-----"   = False
    isMarker _ = True

-- -----------------------------------------------------------------------------
-- rawSystem variants
maybeExit :: IO ExitCode -> IO ()
maybeExit cmd = do
  res <- cmd
  unless (res == ExitSuccess) $ exitWith res

printRawCommandAndArgs :: Verbosity -> FilePath -> [String] -> IO ()
printRawCommandAndArgs verbosity path args = withFrozenCallStack $
    printRawCommandAndArgsAndEnv verbosity path args Nothing Nothing

printRawCommandAndArgsAndEnv :: Verbosity
                             -> FilePath
                             -> [String]
                             -> Maybe FilePath
                             -> Maybe [(String, String)]
                             -> IO ()
printRawCommandAndArgsAndEnv verbosity path args mcwd menv = do
    case menv of
        Just env -> debugNoWrap verbosity ("Environment: " ++ show env)
        Nothing -> return ()
    case mcwd of
        Just cwd -> debugNoWrap verbosity ("Working directory: " ++ show cwd)
        Nothing -> return ()
    infoNoWrap verbosity (showCommandForUser path args)

-- Exit with the same exit code if the subcommand fails
rawSystemExit :: Verbosity -> FilePath -> [String] -> IO ()
rawSystemExit verbosity path args = withFrozenCallStack $ do
  printRawCommandAndArgs verbosity path args
  hFlush stdout
  exitcode <- rawSystem path args
  unless (exitcode == ExitSuccess) $ do
    debug verbosity $ path ++ " returned " ++ show exitcode
    exitWith exitcode

rawSystemExitCode :: Verbosity -> FilePath -> [String] -> IO ExitCode
rawSystemExitCode verbosity path args = withFrozenCallStack $ do
  printRawCommandAndArgs verbosity path args
  hFlush stdout
  exitcode <- rawSystem path args
  unless (exitcode == ExitSuccess) $ do
    debug verbosity $ path ++ " returned " ++ show exitcode
  return exitcode

rawSystemExitWithEnv :: Verbosity
                     -> FilePath
                     -> [String]
                     -> [(String, String)]
                     -> IO ()
rawSystemExitWithEnv verbosity path args env = withFrozenCallStack $ do
    printRawCommandAndArgsAndEnv verbosity path args Nothing (Just env)
    hFlush stdout
    (_,_,_,ph) <- createProcess $
                  (Process.proc path args) { Process.env = (Just env)
#ifdef MIN_VERSION_process
#if MIN_VERSION_process(1,2,0)
-- delegate_ctlc has been added in process 1.2, and we still want to be able to
-- bootstrap GHC on systems not having that version
                                           , Process.delegate_ctlc = True
#endif
#endif
                                           }
    exitcode <- waitForProcess ph
    unless (exitcode == ExitSuccess) $ do
        debug verbosity $ path ++ " returned " ++ show exitcode
        exitWith exitcode

-- Closes the passed in handles before returning.
rawSystemIOWithEnv :: Verbosity
                   -> FilePath
                   -> [String]
                   -> Maybe FilePath           -- ^ New working dir or inherit
                   -> Maybe [(String, String)] -- ^ New environment or inherit
                   -> Maybe Handle  -- ^ stdin
                   -> Maybe Handle  -- ^ stdout
                   -> Maybe Handle  -- ^ stderr
                   -> IO ExitCode
rawSystemIOWithEnv verbosity path args mcwd menv inp out err = withFrozenCallStack $ do
    (_,_,_,ph) <- createProcessWithEnv verbosity path args mcwd menv
                                       (mbToStd inp) (mbToStd out) (mbToStd err)
    exitcode <- waitForProcess ph
    unless (exitcode == ExitSuccess) $ do
      debug verbosity $ path ++ " returned " ++ show exitcode
    return exitcode
  where
    mbToStd :: Maybe Handle -> Process.StdStream
    mbToStd = maybe Process.Inherit Process.UseHandle

createProcessWithEnv ::
     Verbosity
  -> FilePath
  -> [String]
  -> Maybe FilePath           -- ^ New working dir or inherit
  -> Maybe [(String, String)] -- ^ New environment or inherit
  -> Process.StdStream  -- ^ stdin
  -> Process.StdStream  -- ^ stdout
  -> Process.StdStream  -- ^ stderr
  -> IO (Maybe Handle, Maybe Handle, Maybe Handle,ProcessHandle)
  -- ^ Any handles created for stdin, stdout, or stderr
  -- with 'CreateProcess', and a handle to the process.
createProcessWithEnv verbosity path args mcwd menv inp out err = withFrozenCallStack $ do
    printRawCommandAndArgsAndEnv verbosity path args mcwd menv
    hFlush stdout
    (inp', out', err', ph) <- createProcess $
                                (Process.proc path args) {
                                    Process.cwd           = mcwd
                                  , Process.env           = menv
                                  , Process.std_in        = inp
                                  , Process.std_out       = out
                                  , Process.std_err       = err
#ifdef MIN_VERSION_process
#if MIN_VERSION_process(1,2,0)
-- delegate_ctlc has been added in process 1.2, and we still want to be able to
-- bootstrap GHC on systems not having that version
                                  , Process.delegate_ctlc = True
#endif
#endif
                                  }
    return (inp', out', err', ph)

-- | Run a command and return its output.
--
-- The output is assumed to be text in the locale encoding.
--
rawSystemStdout :: Verbosity -> FilePath -> [String] -> IO String
rawSystemStdout verbosity path args = withFrozenCallStack $ do
  (IODataText output, errors, exitCode) <- rawSystemStdInOut verbosity path args
                                                  Nothing Nothing
                                                  Nothing IODataModeText
  when (exitCode /= ExitSuccess) $
    die errors
  return output

-- | Run a command and return its output, errors and exit status. Optionally
-- also supply some input. Also provides control over whether the binary/text
-- mode of the input and output.
--
rawSystemStdInOut :: Verbosity
                  -> FilePath                 -- ^ Program location
                  -> [String]                 -- ^ Arguments
                  -> Maybe FilePath           -- ^ New working dir or inherit
                  -> Maybe [(String, String)] -- ^ New environment or inherit
                  -> Maybe IOData             -- ^ input text and binary mode
                  -> IODataMode               -- ^ output in binary mode
                  -> IO (IOData, String, ExitCode) -- ^ output, errors, exit
rawSystemStdInOut verbosity path args mcwd menv input outputMode = withFrozenCallStack $ do
  printRawCommandAndArgs verbosity path args

  Exception.bracket
     (runInteractiveProcess path args mcwd menv)
     (\(inh,outh,errh,_) -> hClose inh >> hClose outh >> hClose errh)
    $ \(inh,outh,errh,pid) -> do

      -- output mode depends on what the caller wants
      -- but the errors are always assumed to be text (in the current locale)
      hSetBinaryMode errh False

      -- fork off a couple threads to pull on the stderr and stdout
      -- so if the process writes to stderr we do not block.

      err <- hGetContents errh

      out <- IOData.hGetContents outh outputMode

      mv <- newEmptyMVar
      let force str = do
            mberr <- Exception.try (evaluate (rnf str) >> return ())
            putMVar mv (mberr :: Either IOError ())
      _ <- forkIO $ force out
      _ <- forkIO $ force err

      -- push all the input, if any
      case input of
        Nothing -> return ()
        Just inputData -> do
          -- input mode depends on what the caller wants
          IOData.hPutContents inh inputData
          --TODO: this probably fails if the process refuses to consume
          -- or if it closes stdin (eg if it exits)

      -- wait for both to finish, in either order
      mberr1 <- takeMVar mv
      mberr2 <- takeMVar mv

      -- wait for the program to terminate
      exitcode <- waitForProcess pid
      unless (exitcode == ExitSuccess) $
        debug verbosity $ path ++ " returned " ++ show exitcode
                       ++ if null err then "" else
                          " with error message:\n" ++ err
                       ++ case input of
                            Nothing       -> ""
                            Just d | IOData.null d -> ""
                            Just (IODataText inp) -> "\nstdin input:\n" ++ inp
                            Just (IODataBinary inp) -> "\nstdin input (binary):\n" ++ show inp

      -- Check if we we hit an exception while consuming the output
      -- (e.g. a text decoding error)
      reportOutputIOError mberr1
      reportOutputIOError mberr2

      return (out, err, exitcode)
  where
    reportOutputIOError :: Either IOError () -> NoCallStackIO ()
    reportOutputIOError =
      either (\e -> throwIO (ioeSetFileName e ("output of " ++ path)))
             return


{-# DEPRECATED findProgramLocation
    "No longer used within Cabal, try findProgramOnSearchPath" #-}
-- | Look for a program on the path.
findProgramLocation :: Verbosity -> FilePath -> IO (Maybe FilePath)
findProgramLocation verbosity prog = withFrozenCallStack $ do
  debug verbosity $ "searching for " ++ prog ++ " in path."
  res <- findExecutable prog
  case res of
      Nothing   -> debug verbosity ("Cannot find " ++ prog ++ " on the path")
      Just path -> debug verbosity ("found " ++ prog ++ " at "++ path)
  return res


-- | Look for a program and try to find it's version number. It can accept
-- either an absolute path or the name of a program binary, in which case we
-- will look for the program on the path.
--
findProgramVersion :: String             -- ^ version args
                   -> (String -> String) -- ^ function to select version
                                         --   number from program output
                   -> Verbosity
                   -> FilePath           -- ^ location
                   -> IO (Maybe Version)
findProgramVersion versionArg selectVersion verbosity path = withFrozenCallStack $ do
  str <- rawSystemStdout verbosity path [versionArg]
         `catchIO`   (\_ -> return "")
         `catchExit` (\_ -> return "")
  let version :: Maybe Version
      version = simpleParse (selectVersion str)
  case version of
      Nothing -> warn verbosity $ "cannot determine version of " ++ path
                               ++ " :\n" ++ show str
      Just v  -> debug verbosity $ path ++ " is version " ++ display v
  return version


-- | Like the Unix xargs program. Useful for when we've got very long command
-- lines that might overflow an OS limit on command line length and so you
-- need to invoke a command multiple times to get all the args in.
--
-- Use it with either of the rawSystem variants above. For example:
--
-- > xargs (32*1024) (rawSystemExit verbosity) prog fixedArgs bigArgs
--
xargs :: Int -> ([String] -> IO ())
      -> [String] -> [String] -> IO ()
xargs maxSize rawSystemFun fixedArgs bigArgs =
  let fixedArgSize = sum (map length fixedArgs) + length fixedArgs
      chunkSize = maxSize - fixedArgSize
   in traverse_ (rawSystemFun . (fixedArgs ++)) (chunks chunkSize bigArgs)

  where chunks len = unfoldr $ \s ->
          if null s then Nothing
                    else Just (chunk [] len s)

        chunk acc _   []     = (reverse acc,[])
        chunk acc len (s:ss)
          | len' < len = chunk (s:acc) (len-len'-1) ss
          | otherwise  = (reverse acc, s:ss)
          where len' = length s

-- ------------------------------------------------------------
-- * File Utilities
-- ------------------------------------------------------------

----------------
-- Finding files

-- | Find a file by looking in a search path. The file path must match exactly.
--
findFile :: [FilePath]    -- ^search locations
         -> FilePath      -- ^File Name
         -> IO FilePath
findFile searchPath fileName =
  findFirstFile id
    [ path </> fileName
    | path <- nub searchPath]
  >>= maybe (die $ fileName ++ " doesn't exist") return

-- | Find a file by looking in a search path with one of a list of possible
-- file extensions. The file base name should be given and it will be tried
-- with each of the extensions in each element of the search path.
--
findFileWithExtension :: [String]
                      -> [FilePath]
                      -> FilePath
                      -> NoCallStackIO (Maybe FilePath)
findFileWithExtension extensions searchPath baseName =
  findFirstFile id
    [ path </> baseName <.> ext
    | path <- nub searchPath
    , ext <- nub extensions ]

findAllFilesWithExtension :: [String]
                          -> [FilePath]
                          -> FilePath
                          -> NoCallStackIO [FilePath]
findAllFilesWithExtension extensions searchPath basename =
  findAllFiles id
    [ path </> basename <.> ext
    | path <- nub searchPath
    , ext <- nub extensions ]

-- | Like 'findFileWithExtension' but returns which element of the search path
-- the file was found in, and the file path relative to that base directory.
--
findFileWithExtension' :: [String]
                       -> [FilePath]
                       -> FilePath
                       -> NoCallStackIO (Maybe (FilePath, FilePath))
findFileWithExtension' extensions searchPath baseName =
  findFirstFile (uncurry (</>))
    [ (path, baseName <.> ext)
    | path <- nub searchPath
    , ext <- nub extensions ]

findFirstFile :: (a -> FilePath) -> [a] -> NoCallStackIO (Maybe a)
findFirstFile file = findFirst
  where findFirst []     = return Nothing
        findFirst (x:xs) = do exists <- doesFileExist (file x)
                              if exists
                                then return (Just x)
                                else findFirst xs

findAllFiles :: (a -> FilePath) -> [a] -> NoCallStackIO [a]
findAllFiles file = filterM (doesFileExist . file)

-- | Finds the files corresponding to a list of Haskell module names.
--
-- As 'findModuleFile' but for a list of module names.
--
findModuleFiles :: [FilePath]   -- ^ build prefix (location of objects)
                -> [String]     -- ^ search suffixes
                -> [ModuleName] -- ^ modules
                -> IO [(FilePath, FilePath)]
findModuleFiles searchPath extensions moduleNames =
  traverse (findModuleFile searchPath extensions) moduleNames

-- | Find the file corresponding to a Haskell module name.
--
-- This is similar to 'findFileWithExtension'' but specialised to a module
-- name. The function fails if the file corresponding to the module is missing.
--
findModuleFile :: [FilePath]  -- ^ build prefix (location of objects)
               -> [String]    -- ^ search suffixes
               -> ModuleName  -- ^ module
               -> IO (FilePath, FilePath)
findModuleFile searchPath extensions mod_name =
      maybe notFound return
  =<< findFileWithExtension' extensions searchPath
                             (ModuleName.toFilePath mod_name)
  where
    notFound = die $ "Error: Could not find module: " ++ display mod_name
                  ++ " with any suffix: " ++ show extensions
                  ++ " in the search path: " ++ show searchPath

-- | List all the files in a directory and all subdirectories.
--
-- The order places files in sub-directories after all the files in their
-- parent directories. The list is generated lazily so is not well defined if
-- the source directory structure changes before the list is used.
--
getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive topdir = recurseDirectories [""]
  where
    recurseDirectories :: [FilePath] -> IO [FilePath]
    recurseDirectories []         = return []
    recurseDirectories (dir:dirs) = unsafeInterleaveIO $ do
      (files, dirs') <- collect [] [] =<< getDirectoryContents (topdir </> dir)
      files' <- recurseDirectories (dirs' ++ dirs)
      return (files ++ files')

      where
        collect files dirs' []              = return (reverse files
                                                     ,reverse dirs')
        collect files dirs' (entry:entries) | ignore entry
                                            = collect files dirs' entries
        collect files dirs' (entry:entries) = do
          let dirEntry = dir </> entry
          isDirectory <- doesDirectoryExist (topdir </> dirEntry)
          if isDirectory
            then collect files (dirEntry:dirs') entries
            else collect (dirEntry:files) dirs' entries

        ignore ['.']      = True
        ignore ['.', '.'] = True
        ignore _          = False

------------------------
-- Environment variables

-- | Is this directory in the system search path?
isInSearchPath :: FilePath -> NoCallStackIO Bool
isInSearchPath path = fmap (elem path) getSearchPath

addLibraryPath :: OS
               -> [FilePath]
               -> [(String,String)]
               -> [(String,String)]
addLibraryPath os paths = addEnv
  where
    pathsString = intercalate [searchPathSeparator] paths
    ldPath = case os of
               OSX -> "DYLD_LIBRARY_PATH"
               _   -> "LD_LIBRARY_PATH"

    addEnv [] = [(ldPath,pathsString)]
    addEnv ((key,value):xs)
      | key == ldPath =
          if null value
             then (key,pathsString):xs
             else (key,value ++ (searchPathSeparator:pathsString)):xs
      | otherwise     = (key,value):addEnv xs

----------------
-- File globbing

data FileGlob
   -- | No glob at all, just an ordinary file
   = NoGlob FilePath

   -- | dir prefix and extension, like @\"foo\/bar\/\*.baz\"@ corresponds to
   --    @FileGlob \"foo\/bar\" \".baz\"@
   | FileGlob FilePath String

parseFileGlob :: FilePath -> Maybe FileGlob
parseFileGlob filepath = case splitExtensions filepath of
  (filepath', ext) -> case splitFileName filepath' of
    (dir, "*") | '*' `elem` dir
              || '*' `elem` ext
              || null ext            -> Nothing
               | null dir            -> Just (FileGlob "." ext)
               | otherwise           -> Just (FileGlob dir ext)
    _          | '*' `elem` filepath -> Nothing
               | otherwise           -> Just (NoGlob filepath)

matchFileGlob :: FilePath -> IO [FilePath]
matchFileGlob = matchDirFileGlob "."

matchDirFileGlob :: FilePath -> FilePath -> IO [FilePath]
matchDirFileGlob dir filepath = case parseFileGlob filepath of
  Nothing -> die $ "invalid file glob '" ++ filepath
                ++ "'. Wildcards '*' are only allowed in place of the file"
                ++ " name, not in the directory name or file extension."
                ++ " If a wildcard is used it must be with an file extension."
  Just (NoGlob filepath') -> return [filepath']
  Just (FileGlob dir' ext) -> do
    files <- getDirectoryContents (dir </> dir')
    case   [ dir' </> file
           | file <- files
           , let (name, ext') = splitExtensions file
           , not (null name) && ext' == ext ] of
      []      -> die $ "filepath wildcard '" ++ filepath
                    ++ "' does not match any files."
      matches -> return matches

--------------------
-- Modification time

-- | Compare the modification times of two files to see if the first is newer
-- than the second. The first file must exist but the second need not.
-- The expected use case is when the second file is generated using the first.
-- In this use case, if the result is True then the second file is out of date.
--
moreRecentFile :: FilePath -> FilePath -> NoCallStackIO Bool
moreRecentFile a b = do
  exists <- doesFileExist b
  if not exists
    then return True
    else do tb <- getModificationTime b
            ta <- getModificationTime a
            return (ta > tb)

-- | Like 'moreRecentFile', but also checks that the first file exists.
existsAndIsMoreRecentThan :: FilePath -> FilePath -> NoCallStackIO Bool
existsAndIsMoreRecentThan a b = do
  exists <- doesFileExist a
  if not exists
    then return False
    else a `moreRecentFile` b

----------------------------------------
-- Copying and installing files and dirs

-- | Same as 'createDirectoryIfMissing' but logs at higher verbosity levels.
--
createDirectoryIfMissingVerbose :: Verbosity
                                -> Bool     -- ^ Create its parents too?
                                -> FilePath
                                -> IO ()
createDirectoryIfMissingVerbose verbosity create_parents path0
  | create_parents = withFrozenCallStack $ createDirs (parents path0)
  | otherwise      = withFrozenCallStack $ createDirs (take 1 (parents path0))
  where
    parents = reverse . scanl1 (</>) . splitDirectories . normalise

    createDirs []         = return ()
    createDirs (dir:[])   = createDir dir throwIO
    createDirs (dir:dirs) =
      createDir dir $ \_ -> do
        createDirs dirs
        createDir dir throwIO

    createDir :: FilePath -> (IOException -> IO ()) -> IO ()
    createDir dir notExistHandler = do
      r <- tryIO $ createDirectoryVerbose verbosity dir
      case (r :: Either IOException ()) of
        Right ()                   -> return ()
        Left  e
          | isDoesNotExistError  e -> notExistHandler e
          -- createDirectory (and indeed POSIX mkdir) does not distinguish
          -- between a dir already existing and a file already existing. So we
          -- check for it here. Unfortunately there is a slight race condition
          -- here, but we think it is benign. It could report an exception in
          -- the case that the dir did exist but another process deletes the
          -- directory and creates a file in its place before we can check
          -- that the directory did indeed exist.
          | isAlreadyExistsError e -> (do
              isDir <- doesDirectoryExist dir
              unless isDir $ throwIO e
              ) `catchIO` ((\_ -> return ()) :: IOException -> IO ())
          | otherwise              -> throwIO e

createDirectoryVerbose :: Verbosity -> FilePath -> IO ()
createDirectoryVerbose verbosity dir = withFrozenCallStack $ do
  info verbosity $ "creating " ++ dir
  createDirectory dir
  setDirOrdinary dir

-- | Copies a file without copying file permissions. The target file is created
-- with default permissions. Any existing target file is replaced.
--
-- At higher verbosity levels it logs an info message.
--
copyFileVerbose :: Verbosity -> FilePath -> FilePath -> IO ()
copyFileVerbose verbosity src dest = withFrozenCallStack $ do
  info verbosity ("copy " ++ src ++ " to " ++ dest)
  copyFile src dest

-- | Install an ordinary file. This is like a file copy but the permissions
-- are set appropriately for an installed file. On Unix it is \"-rw-r--r--\"
-- while on Windows it uses the default permissions for the target directory.
--
installOrdinaryFile :: Verbosity -> FilePath -> FilePath -> IO ()
installOrdinaryFile verbosity src dest = withFrozenCallStack $ do
  info verbosity ("Installing " ++ src ++ " to " ++ dest)
  copyOrdinaryFile src dest

-- | Install an executable file. This is like a file copy but the permissions
-- are set appropriately for an installed file. On Unix it is \"-rwxr-xr-x\"
-- while on Windows it uses the default permissions for the target directory.
--
installExecutableFile :: Verbosity -> FilePath -> FilePath -> IO ()
installExecutableFile verbosity src dest = withFrozenCallStack $ do
  info verbosity ("Installing executable " ++ src ++ " to " ++ dest)
  copyExecutableFile src dest

-- | Install a file that may or not be executable, preserving permissions.
installMaybeExecutableFile :: Verbosity -> FilePath -> FilePath -> IO ()
installMaybeExecutableFile verbosity src dest = withFrozenCallStack $ do
  perms <- getPermissions src
  if (executable perms) --only checks user x bit
    then installExecutableFile verbosity src dest
    else installOrdinaryFile   verbosity src dest

-- | Given a relative path to a file, copy it to the given directory, preserving
-- the relative path and creating the parent directories if needed.
copyFileTo :: Verbosity -> FilePath -> FilePath -> IO ()
copyFileTo verbosity dir file = withFrozenCallStack $ do
  let targetFile = dir </> file
  createDirectoryIfMissingVerbose verbosity True (takeDirectory targetFile)
  installOrdinaryFile verbosity file targetFile

-- | Common implementation of 'copyFiles', 'installOrdinaryFiles',
-- 'installExecutableFiles' and 'installMaybeExecutableFiles'.
copyFilesWith :: (Verbosity -> FilePath -> FilePath -> IO ())
              -> Verbosity -> FilePath -> [(FilePath, FilePath)] -> IO ()
copyFilesWith doCopy verbosity targetDir srcFiles = withFrozenCallStack $ do

  -- Create parent directories for everything
  let dirs = map (targetDir </>) . nub . map (takeDirectory . snd) $ srcFiles
  traverse_ (createDirectoryIfMissingVerbose verbosity True) dirs

  -- Copy all the files
  sequence_ [ let src  = srcBase   </> srcFile
                  dest = targetDir </> srcFile
               in doCopy verbosity src dest
            | (srcBase, srcFile) <- srcFiles ]

-- | Copies a bunch of files to a target directory, preserving the directory
-- structure in the target location. The target directories are created if they
-- do not exist.
--
-- The files are identified by a pair of base directory and a path relative to
-- that base. It is only the relative part that is preserved in the
-- destination.
--
-- For example:
--
-- > copyFiles normal "dist/src"
-- >    [("", "src/Foo.hs"), ("dist/build/", "src/Bar.hs")]
--
-- This would copy \"src\/Foo.hs\" to \"dist\/src\/src\/Foo.hs\" and
-- copy \"dist\/build\/src\/Bar.hs\" to \"dist\/src\/src\/Bar.hs\".
--
-- This operation is not atomic. Any IO failure during the copy (including any
-- missing source files) leaves the target in an unknown state so it is best to
-- use it with a freshly created directory so that it can be simply deleted if
-- anything goes wrong.
--
copyFiles :: Verbosity -> FilePath -> [(FilePath, FilePath)] -> IO ()
copyFiles v fp fs = withFrozenCallStack (copyFilesWith copyFileVerbose v fp fs)

-- | This is like 'copyFiles' but uses 'installOrdinaryFile'.
--
installOrdinaryFiles :: Verbosity -> FilePath -> [(FilePath, FilePath)] -> IO ()
installOrdinaryFiles v fp fs = withFrozenCallStack (copyFilesWith installOrdinaryFile v fp fs)

-- | This is like 'copyFiles' but uses 'installExecutableFile'.
--
installExecutableFiles :: Verbosity -> FilePath -> [(FilePath, FilePath)]
                          -> IO ()
installExecutableFiles v fp fs = withFrozenCallStack (copyFilesWith installExecutableFile v fp fs)

-- | This is like 'copyFiles' but uses 'installMaybeExecutableFile'.
--
installMaybeExecutableFiles :: Verbosity -> FilePath -> [(FilePath, FilePath)]
                               -> IO ()
installMaybeExecutableFiles v fp fs = withFrozenCallStack (copyFilesWith installMaybeExecutableFile v fp fs)

-- | This installs all the files in a directory to a target location,
-- preserving the directory layout. All the files are assumed to be ordinary
-- rather than executable files.
--
installDirectoryContents :: Verbosity -> FilePath -> FilePath -> IO ()
installDirectoryContents verbosity srcDir destDir = withFrozenCallStack $ do
  info verbosity ("copy directory '" ++ srcDir ++ "' to '" ++ destDir ++ "'.")
  srcFiles <- getDirectoryContentsRecursive srcDir
  installOrdinaryFiles verbosity destDir [ (srcDir, f) | f <- srcFiles ]

-- | Recursively copy the contents of one directory to another path.
copyDirectoryRecursive :: Verbosity -> FilePath -> FilePath -> IO ()
copyDirectoryRecursive verbosity srcDir destDir = withFrozenCallStack $ do
  info verbosity ("copy directory '" ++ srcDir ++ "' to '" ++ destDir ++ "'.")
  srcFiles <- getDirectoryContentsRecursive srcDir
  copyFilesWith (const copyFile) verbosity destDir [ (srcDir, f)
                                                   | f <- srcFiles ]

-------------------
-- File permissions

-- | Like 'doesFileExist', but also checks that the file is executable.
doesExecutableExist :: FilePath -> NoCallStackIO Bool
doesExecutableExist f = do
  exists <- doesFileExist f
  if exists
    then do perms <- getPermissions f
            return (executable perms)
    else return False

---------------------------------
-- Deprecated file copy functions

{-# DEPRECATED smartCopySources
      "Use findModuleFiles and copyFiles or installOrdinaryFiles" #-}
smartCopySources :: Verbosity -> [FilePath] -> FilePath
                 -> [ModuleName] -> [String] -> IO ()
smartCopySources verbosity searchPath targetDir moduleNames extensions = withFrozenCallStack $
      findModuleFiles searchPath extensions moduleNames
  >>= copyFiles verbosity targetDir

{-# DEPRECATED copyDirectoryRecursiveVerbose
      "You probably want installDirectoryContents instead" #-}
copyDirectoryRecursiveVerbose :: Verbosity -> FilePath -> FilePath -> IO ()
copyDirectoryRecursiveVerbose verbosity srcDir destDir = withFrozenCallStack $ do
  info verbosity ("copy directory '" ++ srcDir ++ "' to '" ++ destDir ++ "'.")
  srcFiles <- getDirectoryContentsRecursive srcDir
  copyFiles verbosity destDir [ (srcDir, f) | f <- srcFiles ]

---------------------------
-- Temporary files and dirs

-- | Advanced options for 'withTempFile' and 'withTempDirectory'.
data TempFileOptions = TempFileOptions {
  optKeepTempFiles :: Bool  -- ^ Keep temporary files?
  }

defaultTempFileOptions :: TempFileOptions
defaultTempFileOptions = TempFileOptions { optKeepTempFiles = False }

-- | Use a temporary filename that doesn't already exist.
--
withTempFile :: FilePath    -- ^ Temp dir to create the file in
                -> String   -- ^ File name template. See 'openTempFile'.
                -> (FilePath -> Handle -> IO a) -> IO a
withTempFile tmpDir template action =
  withTempFileEx defaultTempFileOptions tmpDir template action

-- | A version of 'withTempFile' that additionally takes a 'TempFileOptions'
-- argument.
withTempFileEx :: TempFileOptions
                 -> FilePath -- ^ Temp dir to create the file in
                 -> String   -- ^ File name template. See 'openTempFile'.
                 -> (FilePath -> Handle -> IO a) -> IO a
withTempFileEx opts tmpDir template action =
  Exception.bracket
    (openTempFile tmpDir template)
    (\(name, handle) -> do hClose handle
                           unless (optKeepTempFiles opts) $
                             handleDoesNotExist () . removeFile $ name)
    (withLexicalCallStack (uncurry action))

-- | Create and use a temporary directory.
--
-- Creates a new temporary directory inside the given directory, making use
-- of the template. The temp directory is deleted after use. For example:
--
-- > withTempDirectory verbosity "src" "sdist." $ \tmpDir -> do ...
--
-- The @tmpDir@ will be a new subdirectory of the given directory, e.g.
-- @src/sdist.342@.
--
withTempDirectory :: Verbosity -> FilePath -> String -> (FilePath -> IO a) -> IO a
withTempDirectory verbosity targetDir template f = withFrozenCallStack $
  withTempDirectoryEx verbosity defaultTempFileOptions targetDir template
    (withLexicalCallStack f)

-- | A version of 'withTempDirectory' that additionally takes a
-- 'TempFileOptions' argument.
withTempDirectoryEx :: Verbosity -> TempFileOptions
                       -> FilePath -> String -> (FilePath -> IO a) -> IO a
withTempDirectoryEx _verbosity opts targetDir template f = withFrozenCallStack $
  Exception.bracket
    (createTempDirectory targetDir template)
    (unless (optKeepTempFiles opts)
     . handleDoesNotExist () . removeDirectoryRecursive)
    (withLexicalCallStack f)

-----------------------------------
-- Safely reading and writing files

-- | Write a file but only if it would have new content. If we would be writing
-- the same as the existing content then leave the file as is so that we do not
-- update the file's modification time.
--
-- NB: the file is assumed to be ASCII-encoded.
rewriteFile :: Verbosity -> FilePath -> String -> IO ()
rewriteFile verbosity path newContent =
  flip catchIO mightNotExist $ do
    existingContent <- annotateIO verbosity $ readFile path
    _ <- evaluate (length existingContent)
    unless (existingContent == newContent) $
      annotateIO verbosity $
        writeFileAtomic path (BS.Char8.pack newContent)
  where
    mightNotExist e | isDoesNotExistError e
                    = annotateIO verbosity $ writeFileAtomic path
                        (BS.Char8.pack newContent)
                    | otherwise
                    = ioError e

-- | The path name that represents the current directory.
-- In Unix, it's @\".\"@, but this is system-specific.
-- (E.g. AmigaOS uses the empty string @\"\"@ for the current directory.)
currentDir :: FilePath
currentDir = "."

shortRelativePath :: FilePath -> FilePath -> FilePath
shortRelativePath from to =
    case dropCommonPrefix (splitDirectories from) (splitDirectories to) of
        (stuff, path) -> joinPath (map (const "..") stuff ++ path)
  where
    dropCommonPrefix :: Eq a => [a] -> [a] -> ([a],[a])
    dropCommonPrefix (x:xs) (y:ys)
        | x == y    = dropCommonPrefix xs ys
    dropCommonPrefix xs ys = (xs,ys)

-- | Drop the extension if it's one of 'exeExtensions', or return the path
-- unchanged.
dropExeExtension :: FilePath -> FilePath
dropExeExtension filepath =
  case splitExtension filepath of
    (filepath', extension) | extension `elem` exeExtensions -> filepath'
                           | otherwise                      -> filepath

-- | List of possible executable file extensions on the current platform.
exeExtensions :: [String]
exeExtensions = case buildOS of
  -- Possible improvement: on Windows, read the list of extensions from the
  -- PATHEXT environment variable. By default PATHEXT is ".com; .exe; .bat;
  -- .cmd".
  Windows -> ["", "exe"]
  Ghcjs   -> ["", "exe"]
  _       -> [""]

-- ------------------------------------------------------------
-- * Finding the description file
-- ------------------------------------------------------------

-- |Package description file (/pkgname/@.cabal@)
defaultPackageDesc :: Verbosity -> IO FilePath
defaultPackageDesc _verbosity = tryFindPackageDesc currentDir

-- |Find a package description file in the given directory.  Looks for
-- @.cabal@ files.
findPackageDesc :: FilePath                    -- ^Where to look
                -> NoCallStackIO (Either String FilePath) -- ^<pkgname>.cabal
findPackageDesc dir
 = do files <- getDirectoryContents dir
      -- to make sure we do not mistake a ~/.cabal/ dir for a <pkgname>.cabal
      -- file we filter to exclude dirs and null base file names:
      cabalFiles <- filterM doesFileExist
                       [ dir </> file
                       | file <- files
                       , let (name, ext) = splitExtension file
                       , not (null name) && ext == ".cabal" ]
      case cabalFiles of
        []          -> return (Left  noDesc)
        [cabalFile] -> return (Right cabalFile)
        multiple    -> return (Left  $ multiDesc multiple)

  where
    noDesc :: String
    noDesc = "No cabal file found.\n"
             ++ "Please create a package description file <pkgname>.cabal"

    multiDesc :: [String] -> String
    multiDesc l = "Multiple cabal files found.\n"
                  ++ "Please use only one of: "
                  ++ intercalate ", " l

-- |Like 'findPackageDesc', but calls 'die' in case of error.
tryFindPackageDesc :: FilePath -> IO FilePath
tryFindPackageDesc dir = either die return =<< findPackageDesc dir

-- |Optional auxiliary package information file (/pkgname/@.buildinfo@)
defaultHookedPackageDesc :: IO (Maybe FilePath)
defaultHookedPackageDesc = findHookedPackageDesc currentDir

-- |Find auxiliary package information in the given directory.
-- Looks for @.buildinfo@ files.
findHookedPackageDesc
    :: FilePath                 -- ^Directory to search
    -> IO (Maybe FilePath)      -- ^/dir/@\/@/pkgname/@.buildinfo@, if present
findHookedPackageDesc dir = do
    files <- getDirectoryContents dir
    buildInfoFiles <- filterM doesFileExist
                        [ dir </> file
                        | file <- files
                        , let (name, ext) = splitExtension file
                        , not (null name) && ext == buildInfoExt ]
    case buildInfoFiles of
        [] -> return Nothing
        [f] -> return (Just f)
        _ -> die ("Multiple files with extension " ++ buildInfoExt)

buildInfoExt  :: String
buildInfoExt = ".buildinfo"
