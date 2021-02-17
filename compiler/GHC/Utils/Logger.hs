{-# LANGUAGE RankNTypes #-}

-- | Logger
module GHC.Utils.Logger
    ( Logger
    , initLogger
    , HasLogger (..)
    , ContainsLogger (..)
    , LogAction
    , DumpAction
    , TraceAction
    , DumpFormat (..)
    , putLogMsg
    , putDumpMsg
    , putTraceMsg

    -- * Hooks
    , popLogHook
    , pushLogHook
    , popDumpHook
    , pushDumpHook
    , popTraceHook
    , pushTraceHook
    , makeThreadSafe

    -- * Logging
    , jsonLogAction
    , defaultLogAction
    , defaultLogActionHPrintDoc
    , defaultLogActionHPutStrDoc

    -- * Dumping
    , defaultDumpAction
    , withDumpFileHandle
    , touchDumpFile
    , dumpIfSet
    , dumpIfSet_dyn
    , dumpIfSet_dyn_printer

    -- * Tracing
    , defaultTraceAction
    )
where

import GHC.Prelude
import GHC.Driver.Session
import GHC.Driver.Ppr
import GHC.Types.Error
import GHC.Types.SrcLoc

import qualified GHC.Utils.Ppr as Pretty
import GHC.Utils.Outputable
import GHC.Utils.Json
import GHC.Utils.Panic

import Data.IORef
import System.Directory
import System.FilePath  ( takeDirectory, (</>) )
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (intercalate, stripPrefix)
import Data.Time
import System.IO
import Control.Monad
import Control.Concurrent.MVar
import System.IO.Unsafe

type LogAction = DynFlags
              -> WarnReason
              -> Severity
              -> SrcSpan
              -> SDoc
              -> IO ()

type DumpAction = DynFlags
               -> PprStyle
               -> DumpFlag
               -> String
               -> DumpFormat
               -> SDoc
               -> IO ()

type TraceAction a = DynFlags -> String -> SDoc -> a -> a

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
    }

initLogger :: IO Logger
initLogger = do
    dumps <- newIORef Set.empty
    return $ Logger
        { log_hook        = []
        , dump_hook       = []
        , trace_hook      = []
        , generated_dumps = dumps
        }

-- | Log something
putLogMsg :: Logger -> LogAction
putLogMsg logger = foldr ($) defaultLogAction (log_hook logger)

-- | Dump something
putDumpMsg :: Logger -> DumpAction
putDumpMsg logger =
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

        log action dflags reason sev loc doc =
            with_lock (action dflags reason sev loc doc)

        dmp action dflags sty opts str fmt doc =
            with_lock (action dflags sty opts str fmt doc)

        trc :: forall a. TraceAction a -> TraceAction a
        trc action dflags str doc v =
            unsafePerformIO (with_lock (return $! action dflags str doc v))

    return $ pushLogHook log
           $ pushDumpHook dmp
           $ pushTraceHook trc
           $ logger

-- See Note [JSON Error Messages]
--
jsonLogAction :: LogAction
jsonLogAction dflags reason severity srcSpan msg
  =
    defaultLogActionHPutStrDoc dflags True stdout
      (withPprStyle (PprCode CStyle) (doc $$ text ""))
    where
      str = renderWithContext (initSDocContext dflags defaultUserStyle) msg
      doc = renderJSON $
              JSObject [ ( "span", json srcSpan )
                       , ( "doc" , JSString str )
                       , ( "severity", json severity )
                       , ( "reason" ,   json reason )
                       ]


defaultLogAction :: LogAction
defaultLogAction dflags reason severity srcSpan msg
  | dopt Opt_D_dump_json dflags = jsonLogAction dflags reason severity srcSpan msg
  | otherwise = case severity of
      SevOutput      -> printOut msg
      SevDump        -> printOut (msg $$ blankLine)
      SevInteractive -> putStrSDoc msg
      SevInfo        -> printErrs msg
      SevFatal       -> printErrs msg
      SevWarning     -> printWarns
      SevError       -> printWarns
    where
      printOut   = defaultLogActionHPrintDoc  dflags False stdout
      printErrs  = defaultLogActionHPrintDoc  dflags False stderr
      putStrSDoc = defaultLogActionHPutStrDoc dflags False stdout
      -- Pretty print the warning flag, if any (#10752)
      message = mkLocMessageAnn flagMsg severity srcSpan msg

      printWarns = do
        hPutChar stderr '\n'
        caretDiagnostic <-
            if gopt Opt_DiagnosticsShowCaret dflags
            then getCaretDiagnostic severity srcSpan
            else pure empty
        printErrs $ getPprStyle $ \style ->
          withPprStyle (setStyleColoured True style)
            (message $+$ caretDiagnostic)
        -- careful (#2302): printErrs prints in UTF-8,
        -- whereas converting to string first and using
        -- hPutStr would just emit the low 8 bits of
        -- each unicode char.

      flagMsg =
        case reason of
          NoReason -> Nothing
          Reason wflag -> do
            spec <- flagSpecOf wflag
            return ("-W" ++ flagSpecName spec ++ warnFlagGrp wflag)
          ErrReason Nothing ->
            return "-Werror"
          ErrReason (Just wflag) -> do
            spec <- flagSpecOf wflag
            return $
              "-W" ++ flagSpecName spec ++ warnFlagGrp wflag ++
              ", -Werror=" ++ flagSpecName spec

      warnFlagGrp flag
          | gopt Opt_ShowWarnGroups dflags =
                case smallestGroups flag of
                    [] -> ""
                    groups -> " (in " ++ intercalate ", " (map ("-W"++) groups) ++ ")"
          | otherwise = ""

-- | Like 'defaultLogActionHPutStrDoc' but appends an extra newline.
defaultLogActionHPrintDoc :: DynFlags -> Bool -> Handle -> SDoc -> IO ()
defaultLogActionHPrintDoc dflags asciiSpace h d
 = defaultLogActionHPutStrDoc dflags asciiSpace h (d $$ text "")

-- | The boolean arguments let's the pretty printer know if it can optimize indent
-- by writing ascii ' ' characters without going through decoding.
defaultLogActionHPutStrDoc :: DynFlags -> Bool -> Handle -> SDoc -> IO ()
defaultLogActionHPutStrDoc dflags asciiSpace h d
  -- Don't add a newline at the end, so that successive
  -- calls to this log-action can output all on the same line
  = printSDoc ctx (Pretty.PageMode asciiSpace) h d
    where
      ctx = initSDocContext dflags defaultUserStyle

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
defaultDumpAction dumps log_action dflags sty flag title _fmt doc =
  dumpSDocWithStyle dumps log_action sty dflags flag title doc

-- | Write out a dump.
--
-- If --dump-to-file is set then this goes to a file.
-- otherwise emit to stdout (via the the LogAction parameter).
--
-- When @hdr@ is empty, we print in a more compact format (no separators and
-- blank lines)
dumpSDocWithStyle :: DumpCache -> LogAction -> PprStyle -> DynFlags -> DumpFlag -> String -> SDoc -> IO ()
dumpSDocWithStyle dumps log_action sty dflags flag hdr doc =
    withDumpFileHandle dumps dflags flag writeDump
  where
    -- write dump to file
    writeDump (Just handle) = do
        doc' <- if null hdr
                then return doc
                else do t <- getCurrentTime
                        let timeStamp = if (gopt Opt_SuppressTimestamps dflags)
                                          then empty
                                          else text (show t)
                        let d = timeStamp
                                $$ blankLine
                                $$ doc
                        return $ mkDumpDoc hdr d
        -- When we dump to files we use UTF8. Which allows ascii spaces.
        defaultLogActionHPrintDoc dflags True handle (withPprStyle sty doc')

    -- write the dump to stdout
    writeDump Nothing = do
        let (doc', severity)
              | null hdr  = (doc, SevOutput)
              | otherwise = (mkDumpDoc hdr doc, SevDump)
        log_action dflags NoReason severity noSrcSpan (withPprStyle sty doc')


-- | Run an action with the handle of a 'DumpFlag' if we are outputting to a
-- file, otherwise 'Nothing'.
withDumpFileHandle :: DumpCache -> DynFlags -> DumpFlag -> (Maybe Handle -> IO ()) -> IO ()
withDumpFileHandle dumps dflags flag action = do
    let mFile = chooseDumpFile dflags flag
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

-- | Choose where to put a dump file based on DynFlags and DumpFlag
chooseDumpFile :: DynFlags -> DumpFlag -> Maybe FilePath
chooseDumpFile dflags flag
    | gopt Opt_DumpToFile dflags || forced_to_file
    , Just prefix <- getPrefix
    = Just $ setDir (prefix ++ dump_suffix)

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
       | Just prefix <- dumpPrefixForce dflags
          = Just prefix
         -- dump file location chosen by GHC.Driver.Pipeline.runPipeline
       | Just prefix <- dumpPrefix dflags
          = Just prefix
         -- we haven't got a place to put a dump file.
       | otherwise
          = Nothing
    setDir f = case dumpDir dflags of
                 Just d  -> d </> f
                 Nothing ->       f

-- | This is a helper for 'dumpIfSet' to ensure that it's not duplicated
-- despite the fact that 'dumpIfSet' has an @INLINE@.
doDump :: Logger -> DynFlags -> String -> SDoc -> IO ()
doDump logger dflags hdr doc =
  putLogMsg logger dflags
            NoReason
            SevDump
            noSrcSpan
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


dumpIfSet :: Logger -> DynFlags -> Bool -> String -> SDoc -> IO ()
dumpIfSet logger dflags flag hdr doc
  | not flag   = return ()
  | otherwise  = doDump logger dflags hdr doc
{-# INLINE dumpIfSet #-}  -- see Note [INLINE conditional tracing utilities]

-- | A wrapper around 'dumpAction'.
-- First check whether the dump flag is set
-- Do nothing if it is unset
dumpIfSet_dyn :: Logger -> DynFlags -> DumpFlag -> String -> DumpFormat -> SDoc -> IO ()
dumpIfSet_dyn = dumpIfSet_dyn_printer alwaysQualify
{-# INLINE dumpIfSet_dyn #-}  -- see Note [INLINE conditional tracing utilities]

-- | A wrapper around 'putDumpMsg'.
-- First check whether the dump flag is set
-- Do nothing if it is unset
--
-- Unlike 'dumpIfSet_dyn', has a printer argument
dumpIfSet_dyn_printer
    :: PrintUnqualified
    -> Logger
    -> DynFlags
    -> DumpFlag
    -> String
    -> DumpFormat
    -> SDoc
    -> IO ()
dumpIfSet_dyn_printer printer logger dflags flag hdr fmt doc
  = when (dopt flag dflags) $ do
      let sty = mkDumpStyle printer
      putDumpMsg logger dflags sty flag hdr fmt doc
{-# INLINE dumpIfSet_dyn_printer #-}  -- see Note [INLINE conditional tracing utilities]

-- | Ensure that a dump file is created even if it stays empty
touchDumpFile :: Logger -> DynFlags -> DumpFlag -> IO ()
touchDumpFile logger dflags flag =
    withDumpFileHandle (generated_dumps logger) dflags flag (const (return ()))


-- | Default action for 'traceAction' hook
defaultTraceAction :: TraceAction a
defaultTraceAction dflags title doc = pprTraceWithFlags dflags title doc



class HasLogger m where
    getLogger :: m Logger

class ContainsLogger t where
    extractLogger :: t -> Logger

