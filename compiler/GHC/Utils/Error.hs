{-# LANGUAGE ViewPatterns    #-}

{-
(c) The AQUA Project, Glasgow University, 1994-1998

\section[ErrsUtils]{Utilities for error reporting}
-}

module GHC.Utils.Error (
        -- * Basic types
        Validity'(..), Validity, andValid, allValid, getInvalids,
        Severity(..),

        -- * Messages
        Diagnostic(..),
        MsgEnvelope(..),
        MessageClass(..),
        SDoc,
        DecoratedSDoc(unDecorated),
        Messages,
        mkMessages, unionMessages,
        errorsFound, isEmptyMessages,

        -- ** Formatting
        pprMessageBag, pprMsgEnvelopeBagWithLoc, pprMsgEnvelopeBagWithLocDefault,
        pprMessages,
        pprLocMsgEnvelope, pprLocMsgEnvelopeDefault,
        formatBulleted,

        -- ** Construction
        DiagOpts (..), emptyDiagOpts, diag_wopt, diag_fatal_wopt,
        emptyMessages, mkDecorated, mkLocMessage,
        mkMsgEnvelope, mkPlainMsgEnvelope, mkPlainErrorMsgEnvelope, mkPlainWarningMsgEnvelope,
        mkErrorMsgEnvelope,
        mkMCDiagnostic, errorDiagnostic, diagReasonSeverity,

        mkPlainError,
        mkPlainDiagnostic,
        mkDecoratedError,
        mkDecoratedDiagnostic,
        noHints,

        -- * Utilities
        getCaretDiagnostic,

        -- * Issuing messages during compilation
        putMsg, printInfoForUser, printOutputForUser,
        logInfo, logOutput,
        errorMsg,
        fatalErrorMsg,
        compilationProgressMsg,
        showPass,
        withTiming, withTimingSilent,
        debugTraceMsg,
        ghcExit,
        prettyPrintGhcErrors,
        traceCmd,
        traceSystoolCommand,

        sortMsgBag
    ) where

import GHC.Prelude

import GHC.Driver.Flags

import GHC.Data.Bag
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.EnumSet (EnumSet)

import GHC.Utils.Exception
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic
import GHC.Utils.Logger
import GHC.Types.Error
import GHC.Types.SrcLoc as SrcLoc
import GHC.Unit.Module.Warnings

import System.Exit      ( ExitCode(..), exitWith )
import Data.List        ( sortBy )
import Data.Function
import Debug.Trace
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch as MC (handle)
import GHC.Conc         ( getAllocationCounter )
import System.CPUTime
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

data DiagOpts = DiagOpts
  { diag_warning_flags       :: !(EnumSet WarningFlag) -- ^ Enabled warnings
  , diag_fatal_warning_flags :: !(EnumSet WarningFlag) -- ^ Fatal warnings
  , diag_custom_warning_categories       :: !WarningCategorySet -- ^ Enabled custom warning categories
  , diag_fatal_custom_warning_categories :: !WarningCategorySet -- ^ Fatal custom warning categories
  , diag_warn_is_error       :: !Bool                  -- ^ Treat warnings as errors
  , diag_reverse_errors      :: !Bool                  -- ^ Reverse error reporting order
  , diag_max_errors          :: !(Maybe Int)           -- ^ Max reported error count
  , diag_ppr_ctx             :: !SDocContext           -- ^ Error printing context
  }

emptyDiagOpts :: DiagOpts
emptyDiagOpts =
    DiagOpts
        { diag_warning_flags = EnumSet.empty
        , diag_fatal_warning_flags = EnumSet.empty
        , diag_custom_warning_categories = emptyWarningCategorySet
        , diag_fatal_custom_warning_categories = emptyWarningCategorySet
        , diag_warn_is_error = False
        , diag_reverse_errors = False
        , diag_max_errors = Nothing
        , diag_ppr_ctx = defaultSDocContext
        }

diag_wopt :: WarningFlag -> DiagOpts -> Bool
diag_wopt wflag opts = wflag `EnumSet.member` diag_warning_flags opts

diag_fatal_wopt :: WarningFlag -> DiagOpts -> Bool
diag_fatal_wopt wflag opts = wflag `EnumSet.member` diag_fatal_warning_flags opts

diag_wopt_custom :: WarningCategory -> DiagOpts -> Bool
diag_wopt_custom wflag opts = wflag `elemWarningCategorySet` diag_custom_warning_categories opts

diag_fatal_wopt_custom :: WarningCategory -> DiagOpts -> Bool
diag_fatal_wopt_custom wflag opts = wflag `elemWarningCategorySet` diag_fatal_custom_warning_categories opts

-- | Computes the /right/ 'Severity' for the input 'DiagnosticReason' out of
-- the 'DiagOpts. This function /has/ to be called when a diagnostic is constructed,
-- i.e. with a 'DiagOpts \"snapshot\" taken as close as possible to where a
-- particular diagnostic message is built, otherwise the computed 'Severity' might
-- not be correct, due to the mutable nature of the 'DynFlags' in GHC.
--
--
diagReasonSeverity :: DiagOpts -> DiagnosticReason -> Severity
diagReasonSeverity opts reason = fst (diag_reason_severity opts reason)

-- Like the diagReasonSeverity but the second half of the pair is a small
-- ReasolvedDiagnosticReason which would cause the diagnostic to be triggered with the
-- same severity.
--
-- See Note [Warnings controlled by multiple flags]
--
diag_reason_severity :: DiagOpts -> DiagnosticReason -> (Severity, ResolvedDiagnosticReason)
diag_reason_severity opts reason = fmap ResolvedDiagnosticReason $ case reason of
  WarningWithFlags wflags -> case wflags' of
    []     -> (SevIgnore, reason)
    w : ws -> case wflagsE of
      []     -> (SevWarning, WarningWithFlags (w :| ws))
      e : es -> (SevError, WarningWithFlags (e :| es))
    where
      wflags' = NE.filter (\wflag -> diag_wopt wflag opts) wflags
      wflagsE = filter (\wflag -> diag_fatal_wopt wflag opts) wflags'

  WarningWithCategory wcat
    | not (diag_wopt_custom wcat opts) -> (SevIgnore, reason)
    | diag_fatal_wopt_custom wcat opts -> (SevError, reason)
    | otherwise                        -> (SevWarning, reason)
  WarningWithoutFlag
    | diag_warn_is_error opts -> (SevError, reason)
    | otherwise             -> (SevWarning, reason)
  ErrorWithoutFlag
    -> (SevError, reason)

-- | Make a 'MessageClass' for a given 'DiagnosticReason', consulting the
-- 'DiagOpts'.
mkMCDiagnostic :: DiagOpts -> DiagnosticReason -> Maybe DiagnosticCode -> MessageClass
mkMCDiagnostic opts reason code = MCDiagnostic sev reason' code
  where
    (sev, reason') = diag_reason_severity opts reason

-- | Varation of 'mkMCDiagnostic' which can be used when we are /sure/ the
-- input 'DiagnosticReason' /is/ 'ErrorWithoutFlag' and there is no diagnostic code.
errorDiagnostic :: MessageClass
errorDiagnostic = MCDiagnostic SevError (ResolvedDiagnosticReason ErrorWithoutFlag) Nothing

--
-- Creating MsgEnvelope(s)
--

mk_msg_envelope
  :: Diagnostic e
  => Severity
  -> SrcSpan
  -> NamePprCtx
  -> ResolvedDiagnosticReason
  -> e
  -> MsgEnvelope e
mk_msg_envelope severity locn name_ppr_ctx reason err
 = MsgEnvelope { errMsgSpan = locn
               , errMsgContext = name_ppr_ctx
               , errMsgDiagnostic = err
               , errMsgSeverity = severity
               , errMsgReason = reason
               }

-- | Wrap a 'Diagnostic' in a 'MsgEnvelope', recording its location.
-- If you know your 'Diagnostic' is an error, consider using 'mkErrorMsgEnvelope',
-- which does not require looking at the 'DiagOpts'
mkMsgEnvelope
  :: Diagnostic e
  => DiagOpts
  -> SrcSpan
  -> NamePprCtx
  -> e
  -> MsgEnvelope e
mkMsgEnvelope opts locn name_ppr_ctx err
 = mk_msg_envelope sev locn name_ppr_ctx reason err
  where
    (sev, reason) = diag_reason_severity opts (diagnosticReason err)

-- | Wrap a 'Diagnostic' in a 'MsgEnvelope', recording its location.
-- Precondition: the diagnostic is, in fact, an error. That is,
-- @diagnosticReason msg == ErrorWithoutFlag@.
mkErrorMsgEnvelope :: Diagnostic e
                   => SrcSpan
                   -> NamePprCtx
                   -> e
                   -> MsgEnvelope e
mkErrorMsgEnvelope locn name_ppr_ctx msg =
 assert (diagnosticReason msg == ErrorWithoutFlag) $ mk_msg_envelope SevError locn name_ppr_ctx (ResolvedDiagnosticReason ErrorWithoutFlag) msg

-- | Variant that doesn't care about qualified/unqualified names.
mkPlainMsgEnvelope :: Diagnostic e
                   => DiagOpts
                   -> SrcSpan
                   -> e
                   -> MsgEnvelope e
mkPlainMsgEnvelope opts locn msg =
  mkMsgEnvelope opts locn alwaysQualify msg

-- | Variant of 'mkPlainMsgEnvelope' which can be used when we are /sure/ we
-- are constructing a diagnostic with a 'ErrorWithoutFlag' reason.
mkPlainErrorMsgEnvelope :: Diagnostic e
                        => SrcSpan
                        -> e
                        -> MsgEnvelope e
mkPlainErrorMsgEnvelope locn msg =
  mk_msg_envelope SevError locn alwaysQualify (ResolvedDiagnosticReason ErrorWithoutFlag) msg

-- | Variant of 'mkPlainMsgEnvelope' which can be used when we are /sure/ we
-- are constructing a diagnostic with a 'WarningWithoutFlag' reason.
mkPlainWarningMsgEnvelope :: Diagnostic e
                        => SrcSpan
                        -> e
                        -> MsgEnvelope e
mkPlainWarningMsgEnvelope locn msg =
  mk_msg_envelope SevWarning locn alwaysQualify (ResolvedDiagnosticReason WarningWithoutFlag) msg

-------------------------
data Validity' a
  = IsValid      -- ^ Everything is fine
  | NotValid a   -- ^ A problem, and some indication of why
  deriving Functor

-- | Monomorphic version of @Validity'@ specialised for 'SDoc's.
type Validity = Validity' SDoc

andValid :: Validity' a -> Validity' a -> Validity' a
andValid IsValid v = v
andValid v _       = v

-- | If they aren't all valid, return the first
allValid :: [Validity' a] -> Validity' a
allValid []       = IsValid
allValid (v : vs) = v `andValid` allValid vs

getInvalids :: [Validity' a] -> [a]
getInvalids vs = [d | NotValid d <- vs]

-- -----------------------------------------------------------------------------
-- Collecting up messages for later ordering and printing.

----------------
-- | Formats the input list of structured document, where each element of the list gets a bullet.
formatBulleted :: DecoratedSDoc -> SDoc
formatBulleted (unDecorated -> docs)
  = sdocWithContext $ \ctx -> case msgs ctx of
        []    -> Outputable.empty
        [msg] -> msg
        xs    -> vcat $ map starred xs
    where
    msgs ctx = filter (not . Outputable.isEmpty ctx) docs
    starred = (bullet<+>)

pprMessages :: Diagnostic e => DiagnosticOpts e -> Messages e -> SDoc
pprMessages e = vcat . pprMsgEnvelopeBagWithLoc e . getMessages

pprMsgEnvelopeBagWithLoc :: Diagnostic e => DiagnosticOpts e -> Bag (MsgEnvelope e) -> [SDoc]
pprMsgEnvelopeBagWithLoc e bag = [ pprLocMsgEnvelope e item | item <- sortMsgBag Nothing bag ]

-- | Print the messages with the suitable default configuration, usually not what you want but sometimes you don't really
-- care about what the configuration is (for example, if the message is in a panic).
pprMsgEnvelopeBagWithLocDefault :: forall e . Diagnostic e => Bag (MsgEnvelope e) -> [SDoc]
pprMsgEnvelopeBagWithLocDefault bag = [ pprLocMsgEnvelopeDefault item | item <- sortMsgBag Nothing bag ]

pprLocMsgEnvelopeDefault :: forall e . Diagnostic e => MsgEnvelope e -> SDoc
pprLocMsgEnvelopeDefault = pprLocMsgEnvelope (defaultDiagnosticOpts @e)

pprLocMsgEnvelope :: Diagnostic e => DiagnosticOpts e -> MsgEnvelope e -> SDoc
pprLocMsgEnvelope opts (MsgEnvelope { errMsgSpan      = s
                               , errMsgDiagnostic = e
                               , errMsgSeverity  = sev
                               , errMsgContext   = name_ppr_ctx
                               , errMsgReason    = reason })
  = withErrStyle name_ppr_ctx $
      mkLocMessage
        (MCDiagnostic sev reason (diagnosticCode e))
        s
        (formatBulleted $ diagnosticMessage opts e)

sortMsgBag :: Maybe DiagOpts -> Bag (MsgEnvelope e) -> [MsgEnvelope e]
sortMsgBag mopts = maybeLimit . sortBy (cmp `on` errMsgSpan) . bagToList
  where
    cmp
      | Just opts <- mopts
      , diag_reverse_errors opts
      = SrcLoc.rightmost_smallest
      | otherwise
      = SrcLoc.leftmost_smallest
    maybeLimit
      | Just opts <- mopts
      , Just err_limit <- diag_max_errors opts
      = take err_limit
      | otherwise
      = id

ghcExit :: Logger -> Int -> IO ()
ghcExit logger val
  | val == 0  = exitWith ExitSuccess
  | otherwise = do errorMsg logger (text "\nCompilation had errors\n\n")
                   exitWith (ExitFailure val)

-- -----------------------------------------------------------------------------
-- Outputting messages from the compiler

errorMsg :: Logger -> SDoc -> IO ()
errorMsg logger msg
   = logMsg logger errorDiagnostic noSrcSpan $
     withPprStyle defaultErrStyle msg

fatalErrorMsg :: Logger -> SDoc -> IO ()
fatalErrorMsg logger msg =
    logMsg logger MCFatal noSrcSpan $ withPprStyle defaultErrStyle msg

compilationProgressMsg :: Logger -> SDoc -> IO ()
compilationProgressMsg logger msg = do
  let logflags = logFlags logger
  let str = renderWithContext (log_default_user_context logflags) (text "GHC progress: " <> msg)
  traceEventIO str
  when (logVerbAtLeast logger 1) $
    logOutput logger $ withPprStyle defaultUserStyle msg

showPass :: Logger -> String -> IO ()
showPass logger what =
  when (logVerbAtLeast logger 2) $
    logInfo logger $ withPprStyle defaultUserStyle (text "***" <+> text what <> colon)

data PrintTimings = PrintTimings | DontPrintTimings
  deriving (Eq, Show)

-- | Time a compilation phase.
--
-- When timings are enabled (e.g. with the @-v2@ flag), the allocations
-- and CPU time used by the phase will be reported to stderr. Consider
-- a typical usage:
-- @withTiming getDynFlags (text "simplify") force PrintTimings pass@.
-- When timings are enabled the following costs are included in the
-- produced accounting,
--
--  - The cost of executing @pass@ to a result @r@ in WHNF
--  - The cost of evaluating @force r@ to WHNF (e.g. @()@)
--
-- The choice of the @force@ function depends upon the amount of forcing
-- desired; the goal here is to ensure that the cost of evaluating the result
-- is, to the greatest extent possible, included in the accounting provided by
-- 'withTiming'. Often the pass already sufficiently forces its result during
-- construction; in this case @const ()@ is a reasonable choice.
-- In other cases, it is necessary to evaluate the result to normal form, in
-- which case something like @Control.DeepSeq.rnf@ is appropriate.
--
-- To avoid adversely affecting compiler performance when timings are not
-- requested, the result is only forced when timings are enabled.
--
-- See Note [withTiming] for more.
withTiming :: MonadIO m
           => Logger
           -> SDoc         -- ^ The name of the phase
           -> (a -> ())    -- ^ A function to force the result
                           -- (often either @const ()@ or 'rnf')
           -> m a          -- ^ The body of the phase to be timed
           -> m a
withTiming logger what force action =
  withTiming' logger what force PrintTimings action

-- | Same as 'withTiming', but doesn't print timings in the
--   console (when given @-vN@, @N >= 2@ or @-ddump-timings@).
--
--   See Note [withTiming] for more.
withTimingSilent
  :: MonadIO m
  => Logger
  -> SDoc       -- ^ The name of the phase
  -> (a -> ())  -- ^ A function to force the result
                -- (often either @const ()@ or 'rnf')
  -> m a        -- ^ The body of the phase to be timed
  -> m a
withTimingSilent logger what force action =
  withTiming' logger what force DontPrintTimings action

-- | Worker for 'withTiming' and 'withTimingSilent'.
withTiming' :: MonadIO m
            => Logger
            -> SDoc         -- ^ The name of the phase
            -> (a -> ())    -- ^ A function to force the result
                            -- (often either @const ()@ or 'rnf')
            -> PrintTimings -- ^ Whether to print the timings
            -> m a          -- ^ The body of the phase to be timed
            -> m a
withTiming' logger what force_result prtimings action
  = if logVerbAtLeast logger 2 || logHasDumpFlag logger Opt_D_dump_timings
    then do when printTimingsNotDumpToFile $ liftIO $
              logInfo logger $ withPprStyle defaultUserStyle $
                text "***" <+> what <> colon
            let ctx = log_default_user_context (logFlags logger)
            alloc0 <- liftIO getAllocationCounter
            start <- liftIO getCPUTime
            eventBegins ctx what
            recordAllocs alloc0
            !r <- action
            () <- pure $ force_result r
            eventEnds ctx what
            end <- liftIO getCPUTime
            alloc1 <- liftIO getAllocationCounter
            recordAllocs alloc1
            -- recall that allocation counter counts down
            let alloc = alloc0 - alloc1
                time = realToFrac (end - start) * 1e-9

            when (logVerbAtLeast logger 2 && printTimingsNotDumpToFile)
                $ liftIO $ logInfo logger $ withPprStyle defaultUserStyle
                    (text "!!!" <+> what <> colon <+> text "finished in"
                     <+> doublePrec 2 time
                     <+> text "milliseconds"
                     <> comma
                     <+> text "allocated"
                     <+> doublePrec 3 (realToFrac alloc / 1024 / 1024)
                     <+> text "megabytes")

            whenPrintTimings $
                putDumpFileMaybe logger Opt_D_dump_timings "" FormatText
                    $ text $ showSDocOneLine ctx
                    $ hsep [ what <> colon
                           , text "alloc=" <> ppr alloc
                           , text "time=" <> doublePrec 3 time
                           ]
            pure r
     else action

    where whenPrintTimings =
            liftIO . when printTimings

          printTimings =
            prtimings == PrintTimings

          -- Avoid both printing to console and dumping to a file (#20316).
          printTimingsNotDumpToFile =
            printTimings
            && not (log_dump_to_file (logFlags logger))

          recordAllocs alloc =
            liftIO $ traceMarkerIO $ "GHC:allocs:" ++ show alloc

          eventBegins ctx w = do
            let doc = eventBeginsDoc ctx w
            whenPrintTimings $ traceMarkerIO doc
            liftIO $ traceEventIO doc

          eventEnds ctx w = do
            let doc = eventEndsDoc ctx w
            whenPrintTimings $ traceMarkerIO doc
            liftIO $ traceEventIO doc

          eventBeginsDoc ctx w = showSDocOneLine ctx $ text "GHC:started:" <+> w
          eventEndsDoc   ctx w = showSDocOneLine ctx $ text "GHC:finished:" <+> w

debugTraceMsg :: Logger -> Int -> SDoc -> IO ()
debugTraceMsg logger val msg =
   when (log_verbosity (logFlags logger) >= val) $
      logInfo logger (withPprStyle defaultDumpStyle msg)
{-# INLINE debugTraceMsg #-}  -- see Note [INLINE conditional tracing utilities]

putMsg :: Logger -> SDoc -> IO ()
putMsg logger msg = logInfo logger (withPprStyle defaultUserStyle msg)

printInfoForUser :: Logger -> NamePprCtx -> SDoc -> IO ()
printInfoForUser logger name_ppr_ctx msg
  = logInfo logger (withUserStyle name_ppr_ctx AllTheWay msg)

printOutputForUser :: Logger -> NamePprCtx -> SDoc -> IO ()
printOutputForUser logger name_ppr_ctx msg
  = logOutput logger (withUserStyle name_ppr_ctx AllTheWay msg)

logInfo :: Logger -> SDoc -> IO ()
logInfo logger msg = logMsg logger MCInfo noSrcSpan msg

-- | Like 'logInfo' but with 'SevOutput' rather then 'SevInfo'
logOutput :: Logger -> SDoc -> IO ()
logOutput logger msg = logMsg logger MCOutput noSrcSpan msg


prettyPrintGhcErrors :: ExceptionMonad m => Logger -> m a -> m a
prettyPrintGhcErrors logger = do
  let ctx = log_default_user_context (logFlags logger)
  MC.handle $ \e -> case e of
    PprPanic str doc ->
        pprDebugAndThen ctx panic (text str) doc
    PprSorry str doc ->
        pprDebugAndThen ctx sorry (text str) doc
    PprProgramError str doc ->
        pprDebugAndThen ctx pgmError (text str) doc
    _ -> liftIO $ throwIO e

-- | Trace a command (when verbosity level >= 3)
traceCmd :: Logger -> String -> String -> IO a -> IO a
traceCmd logger phase_name cmd_line action = do
  showPass logger phase_name
  let
    cmd_doc = text cmd_line
    handle_exn exn = do
      debugTraceMsg logger 2 (char '\n')
      debugTraceMsg logger 2 (text "Failed:" <+> cmd_doc <+> text (show exn))
      throwGhcExceptionIO (ProgramError (show exn))
  debugTraceMsg logger 3 cmd_doc
  loggerTraceFlush logger
   -- And run it!
  action `catchIO` handle_exn


-- * Tracing utility

-- | Record in the eventlog when the given tool command starts
--   and finishes, prepending the given 'String' with
--   \"systool:\", to easily be able to collect and process
--   all the systool events.
--
--   For those events to show up in the eventlog, you need
--   to run GHC with @-v2@ or @-ddump-timings@.
traceSystoolCommand :: Logger -> String -> IO a -> IO a
traceSystoolCommand logger tool = withTiming logger (text "systool:" <> text tool) (const ())


{- Note [withTiming]
~~~~~~~~~~~~~~~~~~~~

For reference:

  withTiming
    :: MonadIO
    => m DynFlags   -- how to get the DynFlags
    -> SDoc         -- label for the computation we're timing
    -> (a -> ())    -- how to evaluate the result
    -> PrintTimings -- whether to report the timings when passed
                    -- -v2 or -ddump-timings
    -> m a          -- computation we're timing
    -> m a

withTiming lets you run an action while:

(1) measuring the CPU time it took and reporting that on stderr
    (when PrintTimings is passed),
(2) emitting start/stop events to GHC's event log, with the label
    given as an argument.

Evaluation of the result
------------------------

'withTiming' takes as an argument a function of type 'a -> ()', whose purpose is
to evaluate the result "sufficiently". A given pass might return an 'm a' for
some monad 'm' and result type 'a', but where the 'a' is complex enough
that evaluating it to WHNF barely scratches its surface and leaves many
complex and time-consuming computations unevaluated. Those would only be
forced by the next pass, and the time needed to evaluate them would be
mis-attributed to that next pass. A more appropriate function would be
one that deeply evaluates the result, so as to assign the time spent doing it
to the pass we're timing.

Note: as hinted at above, the time spent evaluating the application of the
forcing function to the result is included in the timings reported by
'withTiming'.

How we use it
-------------

We measure the time and allocations of various passes in GHC's pipeline by just
wrapping the whole pass with 'withTiming'. This also materializes by having
a label for each pass in the eventlog, where each pass is executed in one go,
during a continuous time window.

However, from STG onwards, the pipeline uses streams to emit groups of
STG/Cmm/etc declarations one at a time, and process them until we get to
assembly code generation. This means that the execution of those last few passes
is interleaved and that we cannot measure how long they take by just wrapping
the whole thing with 'withTiming'. Instead we wrap the processing of each
individual stream element, all along the codegen pipeline, using the appropriate
label for the pass to which this processing belongs. That generates a lot more
data but allows us to get fine-grained timings about all the passes and we can
easily compute totals with tools like ghc-events-analyze (see below).


Producing an eventlog for GHC
-----------------------------

You can produce an eventlog when compiling, for instance, hello.hs by simply
running:

  If GHC was built by Hadrian:
  $ _build/stage1/bin/ghc -ddump-timings hello.hs -o hello +RTS -l

  If GHC was built with Make:
  $ inplace/bin/ghc-stage2 -ddump-timing hello.hs -o hello +RTS -l

You could alternatively use -v<N> (with N >= 2) instead of -ddump-timings,
to ask GHC to report timings (on stderr and the eventlog).

This will write the eventlog to ./ghc.eventlog in both cases. You can then
visualize it or look at the totals for each label by using ghc-events-analyze,
threadscope or any other eventlog consumer. Illustrating with
ghc-events-analyze:

  $ ghc-events-analyze --timed --timed-txt --totals \
                       --start "GHC:started:" --stop "GHC:finished:" \
                       ghc.eventlog

This produces ghc.timed.txt (all event timestamps), ghc.timed.svg (visualisation
of the execution through the various labels) and ghc.totals.txt (total time
spent in each label).

-}
