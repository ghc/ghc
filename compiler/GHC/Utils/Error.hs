{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE ViewPatterns    #-}

{-
(c) The AQUA Project, Glasgow University, 1994-1998

\section[ErrsUtils]{Utilities for error reporting}
-}

module GHC.Utils.Error (
        -- * Basic types
        Validity(..), andValid, allValid, isValid, getInvalids, orValid,
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
        pprMessageBag, pprMsgEnvelopeBagWithLoc,
        pprMessages,
        pprLocMsgEnvelope,
        formatBulleted,

        -- ** Construction
        emptyMessages, mkDecorated, mkLocMessage, mkLocMessageAnn,
        mkMsgEnvelope, mkPlainMsgEnvelope, mkPlainErrorMsgEnvelope,
        mkErrorMsgEnvelope,
        mkMCDiagnostic, errorDiagnostic, diagReasonSeverity,

        mkPlainError,
        mkPlainDiagnostic,
        mkDecoratedError,
        mkDecoratedDiagnostic,
        noHints,

        -- * Utilities
        doIfSet, doIfSet_dyn,
        getCaretDiagnostic,

        -- * Issuing messages during compilation
        putMsg, printInfoForUser, printOutputForUser,
        logInfo, logOutput,
        errorMsg, warningMsg,
        fatalErrorMsg, fatalErrorMsg'',
        compilationProgressMsg,
        showPass,
        withTiming, withTimingSilent,
        debugTraceMsg,
        ghcExit,
        prettyPrintGhcErrors,
        traceCmd,

        sortMsgBag
    ) where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Driver.Ppr

import GHC.Data.Bag
import GHC.Utils.Exception
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Utils.Logger
import GHC.Types.Error
import GHC.Types.SrcLoc as SrcLoc

import System.Exit      ( ExitCode(..), exitWith )
import Data.List        ( sortBy )
import Data.Maybe       ( fromMaybe )
import Data.Function
import Debug.Trace
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch as MC (handle)
import GHC.Conc         ( getAllocationCounter )
import System.CPUTime

-- | Computes the /right/ 'Severity' for the input 'DiagnosticReason' out of
-- the 'DynFlags'. This function /has/ to be called when a diagnostic is constructed,
-- i.e. with a 'DynFlags' \"snapshot\" taken as close as possible to where a
-- particular diagnostic message is built, otherwise the computed 'Severity' might
-- not be correct, due to the mutable nature of the 'DynFlags' in GHC.
diagReasonSeverity :: DynFlags -> DiagnosticReason -> Severity
diagReasonSeverity dflags (WarningWithFlag wflag) | not (wopt wflag dflags)     = SevIgnore
                                                  | wopt_fatal wflag dflags     = SevError
                                                  | otherwise                   = SevWarning
diagReasonSeverity dflags WarningWithoutFlag      | gopt Opt_WarnIsError dflags = SevError
                                                  | otherwise                   = SevWarning
diagReasonSeverity _      ErrorWithoutFlag                                      = SevError



-- | Make a 'MessageClass' for a given 'DiagnosticReason', consulting the 'DynFlags'.
mkMCDiagnostic :: DynFlags -> DiagnosticReason -> MessageClass
mkMCDiagnostic dflags reason = MCDiagnostic (diagReasonSeverity dflags reason) reason

-- | Varation of 'mkMCDiagnostic' which can be used when we are /sure/ the
-- input 'DiagnosticReason' /is/ 'ErrorWithoutFlag'.
errorDiagnostic :: MessageClass
errorDiagnostic = MCDiagnostic SevError ErrorWithoutFlag

--
-- Creating MsgEnvelope(s)
--

mk_msg_envelope
  :: Diagnostic e
  => Severity
  -> SrcSpan
  -> PrintUnqualified
  -> e
  -> MsgEnvelope e
mk_msg_envelope severity locn print_unqual err
 = MsgEnvelope { errMsgSpan = locn
               , errMsgContext = print_unqual
               , errMsgDiagnostic = err
               , errMsgSeverity = severity
               }

-- | Wrap a 'Diagnostic' in a 'MsgEnvelope', recording its location.
-- If you know your 'Diagnostic' is an error, consider using 'mkErrorMsgEnvelope',
-- which does not require looking at the 'DynFlags'
mkMsgEnvelope
  :: Diagnostic e
  => DynFlags
  -> SrcSpan
  -> PrintUnqualified
  -> e
  -> MsgEnvelope e
mkMsgEnvelope dflags locn print_unqual err
 = mk_msg_envelope (diagReasonSeverity dflags (diagnosticReason err)) locn print_unqual err

-- | Wrap a 'Diagnostic' in a 'MsgEnvelope', recording its location.
-- Precondition: the diagnostic is, in fact, an error. That is,
-- @diagnosticReason msg == ErrorWithoutFlag@.
mkErrorMsgEnvelope :: Diagnostic e
                   => SrcSpan
                   -> PrintUnqualified
                   -> e
                   -> MsgEnvelope e
mkErrorMsgEnvelope locn unqual msg =
 assert (diagnosticReason msg == ErrorWithoutFlag) $ mk_msg_envelope SevError locn unqual msg

-- | Variant that doesn't care about qualified/unqualified names.
mkPlainMsgEnvelope :: Diagnostic e
                   => DynFlags
                   -> SrcSpan
                   -> e
                   -> MsgEnvelope e
mkPlainMsgEnvelope dflags locn msg =
  mkMsgEnvelope dflags locn alwaysQualify msg

-- | Variant of 'mkPlainMsgEnvelope' which can be used when we are /sure/ we
-- are constructing a diagnostic with a 'ErrorWithoutFlag' reason.
mkPlainErrorMsgEnvelope :: Diagnostic e
                        => SrcSpan
                        -> e
                        -> MsgEnvelope e
mkPlainErrorMsgEnvelope locn msg =
  mk_msg_envelope SevError locn alwaysQualify msg

-------------------------
data Validity
  = IsValid            -- ^ Everything is fine
  | NotValid SDoc    -- ^ A problem, and some indication of why

isValid :: Validity -> Bool
isValid IsValid       = True
isValid (NotValid {}) = False

andValid :: Validity -> Validity -> Validity
andValid IsValid v = v
andValid v _       = v

-- | If they aren't all valid, return the first
allValid :: [Validity] -> Validity
allValid []       = IsValid
allValid (v : vs) = v `andValid` allValid vs

getInvalids :: [Validity] -> [SDoc]
getInvalids vs = [d | NotValid d <- vs]

orValid :: Validity -> Validity -> Validity
orValid IsValid _ = IsValid
orValid _       v = v

-- -----------------------------------------------------------------------------
-- Collecting up messages for later ordering and printing.

----------------
-- | Formats the input list of structured document, where each element of the list gets a bullet.
formatBulleted :: SDocContext -> DecoratedSDoc -> SDoc
formatBulleted ctx (unDecorated -> docs)
  = case msgs of
        []    -> Outputable.empty
        [msg] -> msg
        _     -> vcat $ map starred msgs
    where
    msgs    = filter (not . Outputable.isEmpty ctx) docs
    starred = (bullet<+>)

pprMessages :: Diagnostic e => Messages e -> SDoc
pprMessages = vcat . pprMsgEnvelopeBagWithLoc . getMessages

pprMsgEnvelopeBagWithLoc :: Diagnostic e => Bag (MsgEnvelope e) -> [SDoc]
pprMsgEnvelopeBagWithLoc bag = [ pprLocMsgEnvelope item | item <- sortMsgBag Nothing bag ]

pprLocMsgEnvelope :: Diagnostic e => MsgEnvelope e -> SDoc
pprLocMsgEnvelope (MsgEnvelope { errMsgSpan      = s
                               , errMsgDiagnostic = e
                               , errMsgSeverity  = sev
                               , errMsgContext   = unqual })
  = sdocWithContext $ \ctx ->
    withErrStyle unqual $
      mkLocMessage (MCDiagnostic sev (diagnosticReason e)) s (formatBulleted ctx $ diagnosticMessage e)

sortMsgBag :: Maybe DynFlags -> Bag (MsgEnvelope e) -> [MsgEnvelope e]
sortMsgBag dflags = maybeLimit . sortBy (cmp `on` errMsgSpan) . bagToList
  where cmp
          | fromMaybe False (fmap reverseErrors dflags) = SrcLoc.rightmost_smallest
          | otherwise                                   = SrcLoc.leftmost_smallest
        maybeLimit = case join (fmap maxErrors dflags) of
          Nothing        -> id
          Just err_limit -> take err_limit

ghcExit :: Logger -> DynFlags -> Int -> IO ()
ghcExit logger dflags val
  | val == 0  = exitWith ExitSuccess
  | otherwise = do errorMsg logger dflags (text "\nCompilation had errors\n\n")
                   exitWith (ExitFailure val)

doIfSet :: Bool -> IO () -> IO ()
doIfSet flag action | flag      = action
                    | otherwise = return ()

doIfSet_dyn :: DynFlags -> GeneralFlag -> IO () -> IO()
doIfSet_dyn dflags flag action | gopt flag dflags = action
                               | otherwise        = return ()

-- -----------------------------------------------------------------------------
-- Outputting messages from the compiler

-- We want all messages to go through one place, so that we can
-- redirect them if necessary.  For example, when GHC is used as a
-- library we might want to catch all messages that GHC tries to
-- output and do something else with them.

ifVerbose :: DynFlags -> Int -> IO () -> IO ()
ifVerbose dflags val act
  | verbosity dflags >= val = act
  | otherwise               = return ()
{-# INLINE ifVerbose #-}  -- see Note [INLINE conditional tracing utilities]

errorMsg :: Logger -> DynFlags -> SDoc -> IO ()
errorMsg logger dflags msg
   = putLogMsg logger dflags errorDiagnostic noSrcSpan $
     withPprStyle defaultErrStyle msg

warningMsg :: Logger -> DynFlags -> SDoc -> IO ()
warningMsg logger dflags msg
   = putLogMsg logger dflags (mkMCDiagnostic dflags WarningWithoutFlag) noSrcSpan $
     withPprStyle defaultErrStyle msg

fatalErrorMsg :: Logger -> DynFlags -> SDoc -> IO ()
fatalErrorMsg logger dflags msg =
    putLogMsg logger dflags MCFatal noSrcSpan $ withPprStyle defaultErrStyle msg

fatalErrorMsg'' :: FatalMessager -> String -> IO ()
fatalErrorMsg'' fm msg = fm msg

compilationProgressMsg :: Logger -> DynFlags -> SDoc -> IO ()
compilationProgressMsg logger dflags msg = do
    let str = showSDoc dflags msg
    traceEventIO $ "GHC progress: " ++ str
    ifVerbose dflags 1 $
        logOutput logger dflags $ withPprStyle defaultUserStyle msg

showPass :: Logger -> DynFlags -> String -> IO ()
showPass logger dflags what
  = ifVerbose dflags 2 $
    logInfo logger dflags $ withPprStyle defaultUserStyle (text "***" <+> text what <> colon)

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
           -> DynFlags     -- ^ DynFlags
           -> SDoc         -- ^ The name of the phase
           -> (a -> ())    -- ^ A function to force the result
                           -- (often either @const ()@ or 'rnf')
           -> m a          -- ^ The body of the phase to be timed
           -> m a
withTiming logger dflags what force action =
  withTiming' logger dflags what force PrintTimings action

-- | Same as 'withTiming', but doesn't print timings in the
--   console (when given @-vN@, @N >= 2@ or @-ddump-timings@).
--
--   See Note [withTiming] for more.
withTimingSilent
  :: MonadIO m
  => Logger
  -> DynFlags   -- ^ DynFlags
  -> SDoc       -- ^ The name of the phase
  -> (a -> ())  -- ^ A function to force the result
                -- (often either @const ()@ or 'rnf')
  -> m a        -- ^ The body of the phase to be timed
  -> m a
withTimingSilent logger dflags what force action =
  withTiming' logger dflags what force DontPrintTimings action

-- | Worker for 'withTiming' and 'withTimingSilent'.
withTiming' :: MonadIO m
            => Logger
            -> DynFlags   -- ^ 'DynFlags'
            -> SDoc         -- ^ The name of the phase
            -> (a -> ())    -- ^ A function to force the result
                            -- (often either @const ()@ or 'rnf')
            -> PrintTimings -- ^ Whether to print the timings
            -> m a          -- ^ The body of the phase to be timed
            -> m a
withTiming' logger dflags what force_result prtimings action
  = if verbosity dflags >= 2 || dopt Opt_D_dump_timings dflags
    then do whenPrintTimings $
              logInfo logger dflags $ withPprStyle defaultUserStyle $
                text "***" <+> what <> colon
            let ctx = initDefaultSDocContext dflags
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

            when (verbosity dflags >= 2 && prtimings == PrintTimings)
                $ liftIO $ logInfo logger dflags $ withPprStyle defaultUserStyle
                    (text "!!!" <+> what <> colon <+> text "finished in"
                     <+> doublePrec 2 time
                     <+> text "milliseconds"
                     <> comma
                     <+> text "allocated"
                     <+> doublePrec 3 (realToFrac alloc / 1024 / 1024)
                     <+> text "megabytes")

            whenPrintTimings $
                dumpIfSet_dyn logger dflags Opt_D_dump_timings "" FormatText
                    $ text $ showSDocOneLine ctx
                    $ hsep [ what <> colon
                           , text "alloc=" <> ppr alloc
                           , text "time=" <> doublePrec 3 time
                           ]
            pure r
     else action

    where whenPrintTimings = liftIO . when (prtimings == PrintTimings)

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

debugTraceMsg :: Logger -> DynFlags -> Int -> SDoc -> IO ()
debugTraceMsg logger dflags val msg =
   ifVerbose dflags val $
      logInfo logger dflags (withPprStyle defaultDumpStyle msg)
{-# INLINE debugTraceMsg #-}  -- see Note [INLINE conditional tracing utilities]

putMsg :: Logger -> DynFlags -> SDoc -> IO ()
putMsg logger dflags msg = logInfo logger dflags (withPprStyle defaultUserStyle msg)

printInfoForUser :: Logger -> DynFlags -> PrintUnqualified -> SDoc -> IO ()
printInfoForUser logger dflags print_unqual msg
  = logInfo logger dflags (withUserStyle print_unqual AllTheWay msg)

printOutputForUser :: Logger -> DynFlags -> PrintUnqualified -> SDoc -> IO ()
printOutputForUser logger dflags print_unqual msg
  = logOutput logger dflags (withUserStyle print_unqual AllTheWay msg)

logInfo :: Logger -> DynFlags -> SDoc -> IO ()
logInfo logger dflags msg
  = putLogMsg logger dflags MCInfo noSrcSpan msg

-- | Like 'logInfo' but with 'SevOutput' rather then 'SevInfo'
logOutput :: Logger -> DynFlags -> SDoc -> IO ()
logOutput logger dflags msg
  = putLogMsg logger dflags MCOutput noSrcSpan msg


prettyPrintGhcErrors :: ExceptionMonad m => DynFlags -> m a -> m a
prettyPrintGhcErrors dflags
    = MC.handle $ \e -> case e of
                      PprPanic str doc ->
                          pprDebugAndThen ctx panic (text str) doc
                      PprSorry str doc ->
                          pprDebugAndThen ctx sorry (text str) doc
                      PprProgramError str doc ->
                          pprDebugAndThen ctx pgmError (text str) doc
                      _ ->
                          liftIO $ throwIO e
      where
         ctx = initSDocContext dflags defaultUserStyle

traceCmd :: Logger -> DynFlags -> String -> String -> IO a -> IO a
-- trace the command (at two levels of verbosity)
traceCmd logger dflags phase_name cmd_line action
 = do   { let verb = verbosity dflags
        ; showPass logger dflags phase_name
        ; debugTraceMsg logger dflags 3 (text cmd_line)
        ; case flushErr dflags of
              FlushErr io -> io

           -- And run it!
        ; action `catchIO` handle_exn verb
        }
  where
    handle_exn _verb exn = do { debugTraceMsg logger dflags 2 (char '\n')
                              ; debugTraceMsg logger dflags 2
                                (text "Failed:"
                                 <+> text cmd_line
                                 <+> text (show exn))
                              ; throwGhcExceptionIO (ProgramError (show exn))}

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

To actually produce the eventlog, you need an eventlog-capable GHC build:

  With Hadrian:
  $ hadrian/build -j "stage1.ghc-bin.ghc.link.opts += -eventlog"

  With Make:
  $ make -j GhcStage2HcOpts+=-eventlog

You can then produce an eventlog when compiling say hello.hs by simply
doing:

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
