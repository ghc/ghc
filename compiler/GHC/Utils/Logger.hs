{-# LANGUAGE RankNTypes #-}

-- | Logger
--
-- The Logger is an configurable entity that is used by the compiler to output
-- messages on the console (stdout, stderr) and in dump files.
--
-- The behaviour of default Logger returned by `initLogger` can be modified with
-- hooks. The compiler itself uses hooks in multithreaded code (--make) and it
-- is also probably used by ghc-api users (IDEs, etc.).
--
-- In addition to hooks, the Logger suppors LogFlags: basically a subset of the
-- command-line flags that control the logger behaviour at a higher level than
-- hooks.
--
--  1. Hooks are used to define how to generate a info/warning/error/dump messages
--  2. LogFlags are used to decide when and how to generate messages
--
module GHC.Utils.Logger
    ( Logger
    , HasLogger (..)
    , ContainsLogger (..)

    -- * Logger setup
    , initLogger
    , LogAction
    , DumpAction
    , TraceAction
    , DumpFormat (..)

    -- ** Hooks
    , popLogHook
    , pushLogHook
    , popDumpHook
    , pushDumpHook
    , popTraceHook
    , pushTraceHook
    , makeThreadSafe

    -- ** Flags
    , LogFlags (..)
    , defaultLogFlags
    , log_dopt
    , log_set_dopt
    , setLogFlags
    , updateLogFlags
    , logFlags
    , logHasDumpFlag
    , logVerbAtLeast

    -- * Logging
    , jsonLogAction
    , putLogMsg
    , defaultLogAction
    , defaultLogActionHPrintDoc
    , defaultLogActionHPutStrDoc
    , logMsg
    , logDumpMsg

    -- * Dumping
    , defaultDumpAction
    , putDumpFile
    , putDumpFileMaybe
    , putDumpFileMaybe'
    , withDumpFileHandle
    , touchDumpFile
    , logDumpFile

    -- * Tracing
    , defaultTraceAction
    , putTraceMsg
    , loggerTraceFlushUpdate
    , loggerTraceFlush
    , logTraceMsg
    )
where

import GHC.Prelude
import GHC.Driver.Flags
import GHC.Types.Error
import GHC.Types.SrcLoc

import qualified GHC.Utils.Ppr as Pretty
import GHC.Utils.Outputable
import GHC.Utils.Json
import GHC.Utils.Panic

import GHC.Data.EnumSet (EnumSet)
import qualified GHC.Data.EnumSet as EnumSet

import Data.IORef
import System.Directory
import System.FilePath  ( takeDirectory, (</>) )
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (intercalate, stripPrefix)
import qualified Data.List.NonEmpty as NE
import Data.Time
import System.IO
import Control.Monad
import Control.Concurrent.MVar
import System.IO.Unsafe
import Debug.Trace (trace)

---------------------------------------------------------------
-- Log flags
---------------------------------------------------------------

-- | Logger flags
data LogFlags = LogFlags
  { log_default_user_context :: SDocContext
  , log_default_dump_context :: SDocContext
  , log_dump_flags           :: !(EnumSet DumpFlag) -- ^ Dump flags
  , log_show_caret           :: !Bool               -- ^ Show caret in diagnostics
  , log_show_warn_groups     :: !Bool               -- ^ Show warning flag groups
  , log_enable_timestamps    :: !Bool               -- ^ Enable timestamps
  , log_dump_to_file         :: !Bool               -- ^ Enable dump to file
  , log_dump_dir             :: !(Maybe FilePath)   -- ^ Dump directory
  , log_dump_prefix          :: !FilePath           -- ^ Normal dump path ("basename.")
  , log_dump_prefix_override :: !(Maybe FilePath)   -- ^ Overriden dump path
  , log_enable_debug         :: !Bool               -- ^ Enable debug output
  , log_verbosity            :: !Int                -- ^ Verbosity level
  }

-- | Default LogFlags
defaultLogFlags :: LogFlags
defaultLogFlags = LogFlags
  { log_default_user_context = defaultSDocContext
  , log_default_dump_context = defaultSDocContext
  , log_dump_flags           = EnumSet.empty
  , log_show_caret           = True
  , log_show_warn_groups     = True
  , log_enable_timestamps    = True
  , log_dump_to_file         = False
  , log_dump_dir             = Nothing
  , log_dump_prefix          = ""
  , log_dump_prefix_override = Nothing
  , log_enable_debug         = False
  , log_verbosity            = 0
  }

-- | Test if a DumpFlag is enabled
log_dopt :: DumpFlag -> LogFlags -> Bool
log_dopt f logflags = f `EnumSet.member` log_dump_flags logflags

-- | Enable a DumpFlag
log_set_dopt :: DumpFlag -> LogFlags -> LogFlags
log_set_dopt f logflags = logflags { log_dump_flags = EnumSet.insert f (log_dump_flags logflags) }

-- | Test if a DumpFlag is set
logHasDumpFlag :: Logger -> DumpFlag -> Bool
logHasDumpFlag logger f = log_dopt f (logFlags logger)

-- | Test if verbosity is >= to the given value
logVerbAtLeast :: Logger -> Int -> Bool
logVerbAtLeast logger v = log_verbosity (logFlags logger) >= v

-- | Update LogFlags
updateLogFlags :: Logger -> (LogFlags -> LogFlags) -> Logger
updateLogFlags logger f = setLogFlags logger (f (logFlags logger))

-- | Set LogFlags
setLogFlags :: Logger -> LogFlags -> Logger
setLogFlags logger flags = logger { logFlags = flags }


---------------------------------------------------------------
-- Logger
---------------------------------------------------------------

type LogAction = LogFlags
              -> MessageClass
              -> SrcSpan
              -> SDoc
              -> IO ()

type DumpAction = LogFlags
               -> PprStyle
               -> DumpFlag
               -> String
               -> DumpFormat
               -> SDoc
               -> IO ()

type TraceAction a = LogFlags -> String -> SDoc -> a -> a

-- | Format of a dump
--
-- Dump formats are loosely defined: dumps may contain various additional
-- headers and annotations and they may be partial. 'DumpFormat' is mainly a hint
-- (e.g. for syntax highlighters).
data DumpFormat
   = FormatHaskell   -- ^ Haskell
   | FormatCore      -- ^ Core
   | FormatSTG       -- ^ STG
   | FormatByteCode  -- ^ ByteCode
   | FormatCMM       -- ^ Cmm
   | FormatASM       -- ^ Assembly code
   | FormatC         -- ^ C code/header
   | FormatLLVM      -- ^ LLVM bytecode
   | FormatText      -- ^ Unstructured dump
   deriving (Show,Eq)

type DumpCache = IORef (Set FilePath)

data Logger = Logger
    { log_hook   :: [LogAction -> LogAction]
        -- ^ Log hooks stack

    , dump_hook  :: [DumpAction -> DumpAction]
        -- ^ Dump hooks stack

    , trace_hook :: forall a. [TraceAction a -> TraceAction a]
        -- ^ Trace hooks stack

    , generated_dumps :: DumpCache
        -- ^ Already dumped files (to append instead of overwriting them)

    , trace_flush :: IO ()
        -- ^ Flush the trace buffer

    , logFlags :: !LogFlags
        -- ^ Logger flags
    }

-- | Set the trace flushing function
--
-- The currently set trace flushing function is passed to the updating function
loggerTraceFlushUpdate :: Logger -> (IO () -> IO ()) -> Logger
loggerTraceFlushUpdate logger upd = logger { trace_flush = upd (trace_flush logger) }

-- | Calls the trace flushing function
loggerTraceFlush :: Logger -> IO ()
loggerTraceFlush logger = trace_flush logger

-- | Default trace flushing function (flush stderr)
defaultTraceFlush :: IO ()
defaultTraceFlush = hFlush stderr

initLogger :: IO Logger
initLogger = do
    dumps <- newIORef Set.empty
    return $ Logger
        { log_hook        = []
        , dump_hook       = []
        , trace_hook      = []
        , generated_dumps = dumps
        , trace_flush     = defaultTraceFlush
        , logFlags        = defaultLogFlags
        }

-- | Log something
putLogMsg :: Logger -> LogAction
putLogMsg logger = foldr ($) defaultLogAction (log_hook logger)

-- | Dump something
putDumpFile :: Logger -> DumpAction
putDumpFile logger =
    let
        fallback = putLogMsg logger
        dumps    = generated_dumps logger
        deflt    = defaultDumpAction dumps fallback
    in foldr ($) deflt (dump_hook logger)

-- | Trace something
putTraceMsg :: Logger -> TraceAction a
putTraceMsg logger = foldr ($) defaultTraceAction (trace_hook logger)


-- | Push a log hook
pushLogHook :: (LogAction -> LogAction) -> Logger -> Logger
pushLogHook h logger = logger { log_hook = h:log_hook logger }

-- | Pop a log hook
popLogHook :: Logger -> Logger
popLogHook logger = case log_hook logger of
    []   -> panic "popLogHook: empty hook stack"
    _:hs -> logger { log_hook = hs }

-- | Push a dump hook
pushDumpHook :: (DumpAction -> DumpAction) -> Logger -> Logger
pushDumpHook h logger = logger { dump_hook = h:dump_hook logger }

-- | Pop a dump hook
popDumpHook :: Logger -> Logger
popDumpHook logger = case dump_hook logger of
    []   -> panic "popDumpHook: empty hook stack"
    _:hs -> logger { dump_hook = hs }

-- | Push a trace hook
pushTraceHook :: (forall a. TraceAction a -> TraceAction a) -> Logger -> Logger
pushTraceHook h logger = logger { trace_hook = h:trace_hook logger }

-- | Pop a trace hook
popTraceHook :: Logger -> Logger
popTraceHook logger = case trace_hook logger of
    [] -> panic "popTraceHook: empty hook stack"
    _  -> logger { trace_hook = tail (trace_hook logger) }

-- | Make the logger thread-safe
makeThreadSafe :: Logger -> IO Logger
makeThreadSafe logger = do
    lock <- newMVar ()
    let
        with_lock :: forall a. IO a -> IO a
        with_lock act = withMVar lock (const act)

        log action logflags msg_class loc doc =
            with_lock (action logflags msg_class loc doc)

        dmp action logflags sty opts str fmt doc =
            with_lock (action logflags sty opts str fmt doc)

        trc :: forall a. TraceAction a -> TraceAction a
        trc action logflags str doc v =
            unsafePerformIO (with_lock (return $! action logflags str doc v))

    return $ pushLogHook log
           $ pushDumpHook dmp
           $ pushTraceHook trc
           $ logger

-- See Note [JSON Error Messages]
--
jsonLogAction :: LogAction
jsonLogAction _ (MCDiagnostic SevIgnore _) _ _ = return () -- suppress the message
jsonLogAction logflags msg_class srcSpan msg
  =
    defaultLogActionHPutStrDoc logflags True stdout
      (withPprStyle (PprCode CStyle) (doc $$ text ""))
    where
      str = renderWithContext (log_default_user_context logflags) msg
      doc = renderJSON $
              JSObject [ ( "span", json srcSpan )
                       , ( "doc" , JSString str )
                       , ( "messageClass", json msg_class )
                       ]

defaultLogAction :: LogAction
defaultLogAction logflags msg_class srcSpan msg
  | log_dopt Opt_D_dump_json logflags = jsonLogAction logflags msg_class srcSpan msg
  | otherwise = case msg_class of
      MCOutput                 -> printOut msg
      MCDump                   -> printOut (msg $$ blankLine)
      MCInteractive            -> putStrSDoc msg
      MCInfo                   -> printErrs msg
      MCFatal                  -> printErrs msg
      MCDiagnostic SevIgnore _ -> pure () -- suppress the message
      MCDiagnostic sev rea     -> printDiagnostics sev rea
    where
      printOut   = defaultLogActionHPrintDoc  logflags False stdout
      printErrs  = defaultLogActionHPrintDoc  logflags False stderr
      putStrSDoc = defaultLogActionHPutStrDoc logflags False stdout
      -- Pretty print the warning flag, if any (#10752)
      message sev rea = mkLocMessageAnn (flagMsg sev rea) msg_class srcSpan msg

      printDiagnostics severity reason = do
        hPutChar stderr '\n'
        caretDiagnostic <-
            if log_show_caret logflags
            then getCaretDiagnostic msg_class srcSpan
            else pure empty
        printErrs $ getPprStyle $ \style ->
          withPprStyle (setStyleColoured True style)
            (message severity reason $+$ caretDiagnostic)
        -- careful (#2302): printErrs prints in UTF-8,
        -- whereas converting to string first and using
        -- hPutStr would just emit the low 8 bits of
        -- each unicode char.

      flagMsg :: Severity -> DiagnosticReason -> Maybe String
      flagMsg SevIgnore _                 =  panic "Called flagMsg with SevIgnore"
      flagMsg SevError WarningWithoutFlag =  Just "-Werror"
      flagMsg SevError (WarningWithFlag wflag) = do
        let name = NE.head (warnFlagNames wflag)
        return $
          "-W" ++ name ++ warnFlagGrp wflag ++
          ", -Werror=" ++ name
      flagMsg SevError ErrorWithoutFlag = Nothing
      flagMsg SevWarning WarningWithoutFlag = Nothing
      flagMsg SevWarning (WarningWithFlag wflag) = do
        let name = NE.head (warnFlagNames wflag)
        return ("-W" ++ name ++ warnFlagGrp wflag)
      flagMsg SevWarning ErrorWithoutFlag =
        panic "SevWarning with ErrorWithoutFlag"

      warnFlagGrp flag
          | log_show_warn_groups logflags =
                case smallestWarningGroups flag of
                    [] -> ""
                    groups -> " (in " ++ intercalate ", " (map ("-W"++) groups) ++ ")"
          | otherwise = ""

-- | Like 'defaultLogActionHPutStrDoc' but appends an extra newline.
defaultLogActionHPrintDoc :: LogFlags -> Bool -> Handle -> SDoc -> IO ()
defaultLogActionHPrintDoc logflags asciiSpace h d
 = defaultLogActionHPutStrDoc logflags asciiSpace h (d $$ text "")

-- | The boolean arguments let's the pretty printer know if it can optimize indent
-- by writing ascii ' ' characters without going through decoding.
defaultLogActionHPutStrDoc :: LogFlags -> Bool -> Handle -> SDoc -> IO ()
defaultLogActionHPutStrDoc logflags asciiSpace h d
  -- Don't add a newline at the end, so that successive
  -- calls to this log-action can output all on the same line
  = printSDoc (log_default_user_context logflags) (Pretty.PageMode asciiSpace) h d

--
-- Note [JSON Error Messages]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- When the user requests the compiler output to be dumped as json
-- we used to collect them all in an IORef and then print them at the end.
-- This doesn't work very well with GHCi. (See #14078) So instead we now
-- use the simpler method of just outputting a JSON document inplace to
-- stdout.
--
-- Before the compiler calls log_action, it has already turned the `ErrMsg`
-- into a formatted message. This means that we lose some possible
-- information to provide to the user but refactoring log_action is quite
-- invasive as it is called in many places. So, for now I left it alone
-- and we can refine its behaviour as users request different output.

-- | Default action for 'dumpAction' hook
defaultDumpAction :: DumpCache -> LogAction -> DumpAction
defaultDumpAction dumps log_action logflags sty flag title _fmt doc =
  dumpSDocWithStyle dumps log_action sty logflags flag title doc

-- | Write out a dump.
--
-- If --dump-to-file is set then this goes to a file.
-- otherwise emit to stdout (via the the LogAction parameter).
--
-- When @hdr@ is empty, we print in a more compact format (no separators and
-- blank lines)
dumpSDocWithStyle :: DumpCache -> LogAction -> PprStyle -> LogFlags -> DumpFlag -> String -> SDoc -> IO ()
dumpSDocWithStyle dumps log_action sty logflags flag hdr doc =
    withDumpFileHandle dumps logflags flag writeDump
  where
    -- write dump to file
    writeDump (Just handle) = do
        doc' <- if null hdr
                then return doc
                else do timeStamp <- if log_enable_timestamps logflags
                          then (text . show) <$> getCurrentTime
                          else pure empty
                        let d = timeStamp
                                $$ blankLine
                                $$ doc
                        return $ mkDumpDoc hdr d
        -- When we dump to files we use UTF8. Which allows ascii spaces.
        defaultLogActionHPrintDoc logflags True handle (withPprStyle sty doc')

    -- write the dump to stdout
    writeDump Nothing = do
        let (doc', msg_class)
              | null hdr  = (doc, MCOutput)
              | otherwise = (mkDumpDoc hdr doc, MCDump)
        log_action logflags msg_class noSrcSpan (withPprStyle sty doc')


-- | Run an action with the handle of a 'DumpFlag' if we are outputting to a
-- file, otherwise 'Nothing'.
withDumpFileHandle :: DumpCache -> LogFlags -> DumpFlag -> (Maybe Handle -> IO ()) -> IO ()
withDumpFileHandle dumps logflags flag action = do
    let mFile = chooseDumpFile logflags flag
    case mFile of
      Just fileName -> do
        gd <- readIORef dumps
        let append = Set.member fileName gd
            mode = if append then AppendMode else WriteMode
        unless append $
            writeIORef dumps (Set.insert fileName gd)
        createDirectoryIfMissing True (takeDirectory fileName)
        withFile fileName mode $ \handle -> do
            -- We do not want the dump file to be affected by
            -- environment variables, but instead to always use
            -- UTF8. See:
            -- https://gitlab.haskell.org/ghc/ghc/issues/10762
            hSetEncoding handle utf8

            action (Just handle)
      Nothing -> action Nothing

-- | Choose where to put a dump file based on LogFlags and DumpFlag
chooseDumpFile :: LogFlags -> DumpFlag -> Maybe FilePath
chooseDumpFile logflags flag
    | log_dump_to_file logflags || forced_to_file
    = Just $ setDir (getPrefix ++ dump_suffix)

    | otherwise
    = Nothing
  where
    (forced_to_file, dump_suffix) = case flag of
        -- -dth-dec-file dumps expansions of TH
        -- splices into MODULE.th.hs even when
        -- -ddump-to-file isn't set
        Opt_D_th_dec_file -> (True, "th.hs")
        _                 -> (False, default_suffix)

    -- build a suffix from the flag name
    -- e.g. -ddump-asm => ".dump-asm"
    default_suffix = map (\c -> if c == '_' then '-' else c) $
      let str = show flag
      in case stripPrefix "Opt_D_" str of
        Just x  -> x
        Nothing -> panic ("chooseDumpFile: bad flag name: " ++ str)

    getPrefix
         -- dump file location is being forced
         --      by the --ddump-file-prefix flag.
       | Just prefix <- log_dump_prefix_override logflags
          = prefix
         -- dump file locations, module specified to [modulename] set by
         -- GHC.Driver.Pipeline.runPipeline; non-module specific, e.g. Chasing dependencies,
         -- to 'non-module' by default.
       | otherwise
          = log_dump_prefix logflags
    setDir f = case log_dump_dir logflags of
                 Just d  -> d </> f
                 Nothing ->       f



-- | Default action for 'traceAction' hook
defaultTraceAction :: TraceAction a
defaultTraceAction logflags title doc x =
  if not (log_enable_debug logflags)
    then x
    else trace (renderWithContext (log_default_dump_context logflags)
                             (sep [text title, nest 2 doc])) x


-- | Log something
logMsg :: Logger -> MessageClass -> SrcSpan -> SDoc -> IO ()
logMsg logger mc loc msg = putLogMsg logger (logFlags logger) mc loc msg

-- | Dump something
logDumpFile :: Logger -> PprStyle -> DumpFlag -> String -> DumpFormat -> SDoc -> IO ()
logDumpFile logger = putDumpFile logger (logFlags logger)

-- | Log a trace message
logTraceMsg :: Logger -> String -> SDoc -> a -> a
logTraceMsg logger hdr doc a = putTraceMsg logger (logFlags logger) hdr doc a

-- | Log a dump message (not a dump file)
logDumpMsg :: Logger -> String -> SDoc -> IO ()
logDumpMsg logger hdr doc = logMsg logger MCDump noSrcSpan
  (withPprStyle defaultDumpStyle
  (mkDumpDoc hdr doc))

mkDumpDoc :: String -> SDoc -> SDoc
mkDumpDoc hdr doc
   = vcat [blankLine,
           line <+> text hdr <+> line,
           doc,
           blankLine]
     where
        line = text "===================="


-- | Dump if the given DumpFlag is set
putDumpFileMaybe :: Logger -> DumpFlag -> String -> DumpFormat -> SDoc -> IO ()
putDumpFileMaybe logger = putDumpFileMaybe' logger alwaysQualify
{-# INLINE putDumpFileMaybe #-}  -- see Note [INLINE conditional tracing utilities]

-- | Dump if the given DumpFlag is set
--
-- Unlike 'putDumpFileMaybe', has a PrintUnqualified argument
putDumpFileMaybe'
    :: Logger
    -> PrintUnqualified
    -> DumpFlag
    -> String
    -> DumpFormat
    -> SDoc
    -> IO ()
putDumpFileMaybe' logger printer flag hdr fmt doc
  = when (logHasDumpFlag logger flag) $
    logDumpFile' logger printer flag hdr fmt doc
{-# INLINE putDumpFileMaybe' #-}  -- see Note [INLINE conditional tracing utilities]


logDumpFile' :: Logger -> PrintUnqualified -> DumpFlag
             -> String -> DumpFormat -> SDoc -> IO ()
{-# NOINLINE logDumpFile' #-}
-- NOINLINE: Now we are past the conditional, into the "cold" path,
--           don't inline, to reduce code size at the call site
-- See Note [INLINE conditional tracing utilities]
logDumpFile' logger printer flag hdr fmt doc
  = logDumpFile logger (mkDumpStyle printer) flag hdr fmt doc

-- | Ensure that a dump file is created even if it stays empty
touchDumpFile :: Logger -> DumpFlag -> IO ()
touchDumpFile logger flag =
    withDumpFileHandle (generated_dumps logger) (logFlags logger) flag (const (return ()))

class HasLogger m where
    getLogger :: m Logger

class ContainsLogger t where
    extractLogger :: t -> Logger

