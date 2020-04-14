{-
(c) The AQUA Project, Glasgow University, 1994-1998

\section[ErrsUtils]{Utilities for error reporting}
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module GHC.Utils.Error (
        -- * Basic types
        Validity(..), andValid, allValid, isValid, getInvalids, orValid,
        Severity(..),

        -- * Messages
        ErrMsg, errMsgDoc, errMsgSeverity, errMsgReason,
        ErrDoc, errDoc, errDocImportant, errDocContext, errDocSupplementary,
        WarnMsg, MsgDoc,
        Messages, ErrorMessages, WarningMessages,
        unionMessages,
        errMsgSpan, errMsgContext,
        errorsFound, isEmptyMessages,
        isWarnMsgFatal,
        warningsToMessages,

        -- ** Formatting
        pprMessageBag, pprErrMsgBagWithLoc,
        pprLocErrMsg, printBagOfErrors,
        formatErrDoc,

        -- ** Construction
        emptyMessages, mkLocMessage, mkLocMessageAnn, makeIntoWarning,
        mkErrMsg, mkPlainErrMsg, mkErrDoc, mkLongErrMsg, mkWarnMsg,
        mkPlainWarnMsg,
        mkLongWarnMsg,

        -- * Utilities
        doIfSet, doIfSet_dyn,
        getCaretDiagnostic,

        -- * Dump files
        dumpIfSet, dumpIfSet_dyn, dumpIfSet_dyn_printer,
        dumpOptionsFromFlag, DumpOptions (..),
        DumpFormat (..), DumpAction, dumpAction, defaultDumpAction,
        TraceAction, traceAction, defaultTraceAction,
        touchDumpFile,

        -- * Issuing messages during compilation
        putMsg, printInfoForUser, printOutputForUser,
        logInfo, logOutput,
        errorMsg, warningMsg,
        fatalErrorMsg, fatalErrorMsg'',
        compilationProgressMsg,
        showPass,
        withTiming, withTimingSilent, withTimingD, withTimingSilentD,
        debugTraceMsg,
        ghcExit,
        prettyPrintGhcErrors,
        traceCmd
    ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Data.Bag
import GHC.Utils.Exception
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic
import qualified GHC.Utils.Ppr.Colour as Col
import GHC.Types.SrcLoc as SrcLoc
import GHC.Driver.Session
import GHC.Data.FastString (unpackFS)
import GHC.Data.StringBuffer (atLine, hGetStringBuffer, len, lexemeToString)
import GHC.Utils.Json

import System.Directory
import System.Exit      ( ExitCode(..), exitWith )
import System.FilePath  ( takeDirectory, (</>) )
import Data.List
import qualified Data.Set as Set
import Data.IORef
import Data.Maybe       ( fromMaybe )
import Data.Function
import Data.Time
import Debug.Trace
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch as MC (handle)
import System.IO
import System.IO.Error  ( catchIOError )
import GHC.Conc         ( getAllocationCounter )
import System.CPUTime

-------------------------
type MsgDoc  = SDoc

-------------------------
data Validity
  = IsValid            -- ^ Everything is fine
  | NotValid MsgDoc    -- ^ A problem, and some indication of why

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

getInvalids :: [Validity] -> [MsgDoc]
getInvalids vs = [d | NotValid d <- vs]

orValid :: Validity -> Validity -> Validity
orValid IsValid _ = IsValid
orValid _       v = v

-- -----------------------------------------------------------------------------
-- Basic error messages: just render a message with a source location.

type Messages        = (WarningMessages, ErrorMessages)
type WarningMessages = Bag WarnMsg
type ErrorMessages   = Bag ErrMsg

unionMessages :: Messages -> Messages -> Messages
unionMessages (warns1, errs1) (warns2, errs2) =
  (warns1 `unionBags` warns2, errs1 `unionBags` errs2)

data ErrMsg = ErrMsg {
        errMsgSpan        :: SrcSpan,
        errMsgContext     :: PrintUnqualified,
        errMsgDoc         :: ErrDoc,
        -- | This has the same text as errDocImportant . errMsgDoc.
        errMsgShortString :: String,
        errMsgSeverity    :: Severity,
        errMsgReason      :: WarnReason
        }
        -- The SrcSpan is used for sorting errors into line-number order


-- | Categorise error msgs by their importance.  This is so each section can
-- be rendered visually distinct.  See Note [Error report] for where these come
-- from.
data ErrDoc = ErrDoc {
        -- | Primary error msg.
        errDocImportant     :: [MsgDoc],
        -- | Context e.g. \"In the second argument of ...\".
        errDocContext       :: [MsgDoc],
        -- | Supplementary information, e.g. \"Relevant bindings include ...\".
        errDocSupplementary :: [MsgDoc]
        }

errDoc :: [MsgDoc] -> [MsgDoc] -> [MsgDoc] -> ErrDoc
errDoc = ErrDoc

type WarnMsg = ErrMsg

data Severity
  = SevOutput
  | SevFatal
  | SevInteractive

  | SevDump
    -- ^ Log message intended for compiler developers
    -- No file/line/column stuff

  | SevInfo
    -- ^ Log messages intended for end users.
    -- No file/line/column stuff.

  | SevWarning
  | SevError
    -- ^ SevWarning and SevError are used for warnings and errors
    --   o The message has a file/line/column heading,
    --     plus "warning:" or "error:",
    --     added by mkLocMessags
    --   o Output is intended for end users
  deriving Show


instance ToJson Severity where
  json s = JSString (show s)


instance Show ErrMsg where
    show em = errMsgShortString em

pprMessageBag :: Bag MsgDoc -> SDoc
pprMessageBag msgs = vcat (punctuate blankLine (bagToList msgs))

-- | Make an unannotated error message with location info.
mkLocMessage :: Severity -> SrcSpan -> MsgDoc -> MsgDoc
mkLocMessage = mkLocMessageAnn Nothing

-- | Make a possibly annotated error message with location info.
mkLocMessageAnn
  :: Maybe String                       -- ^ optional annotation
  -> Severity                           -- ^ severity
  -> SrcSpan                            -- ^ location
  -> MsgDoc                             -- ^ message
  -> MsgDoc
  -- Always print the location, even if it is unhelpful.  Error messages
  -- are supposed to be in a standard format, and one without a location
  -- would look strange.  Better to say explicitly "<no location info>".
mkLocMessageAnn ann severity locn msg
    = sdocOption sdocColScheme $ \col_scheme ->
      let locn' = sdocOption sdocErrorSpans $ \case
                     True  -> ppr locn
                     False -> ppr (srcSpanStart locn)

          sevColour = getSeverityColour severity col_scheme

          -- Add optional information
          optAnn = case ann of
            Nothing -> text ""
            Just i  -> text " [" <> coloured sevColour (text i) <> text "]"

          -- Add prefixes, like    Foo.hs:34: warning:
          --                           <the warning message>
          header = locn' <> colon <+>
                   coloured sevColour sevText <> optAnn

      in coloured (Col.sMessage col_scheme)
                  (hang (coloured (Col.sHeader col_scheme) header) 4
                        msg)

  where
    sevText =
      case severity of
        SevWarning -> text "warning:"
        SevError   -> text "error:"
        SevFatal   -> text "fatal:"
        _          -> empty

getSeverityColour :: Severity -> Col.Scheme -> Col.PprColour
getSeverityColour SevWarning = Col.sWarning
getSeverityColour SevError   = Col.sError
getSeverityColour SevFatal   = Col.sFatal
getSeverityColour _          = const mempty

getCaretDiagnostic :: Severity -> SrcSpan -> IO MsgDoc
getCaretDiagnostic _ (UnhelpfulSpan _) = pure empty
getCaretDiagnostic severity (RealSrcSpan span _) = do
  caretDiagnostic <$> getSrcLine (srcSpanFile span) row

  where
    getSrcLine fn i =
      getLine i (unpackFS fn)
        `catchIOError` \_ ->
          pure Nothing

    getLine i fn = do
      -- StringBuffer has advantages over readFile:
      -- (a) no lazy IO, otherwise IO exceptions may occur in pure code
      -- (b) always UTF-8, rather than some system-dependent encoding
      --     (Haskell source code must be UTF-8 anyway)
      content <- hGetStringBuffer fn
      case atLine i content of
        Just at_line -> pure $
          case lines (fix <$> lexemeToString at_line (len at_line)) of
            srcLine : _ -> Just srcLine
            _           -> Nothing
        _ -> pure Nothing

    -- allow user to visibly see that their code is incorrectly encoded
    -- (StringBuffer.nextChar uses \0 to represent undecodable characters)
    fix '\0' = '\xfffd'
    fix c    = c

    row = srcSpanStartLine span
    rowStr = show row
    multiline = row /= srcSpanEndLine span

    caretDiagnostic Nothing = empty
    caretDiagnostic (Just srcLineWithNewline) =
      sdocOption sdocColScheme$ \col_scheme ->
      let sevColour = getSeverityColour severity col_scheme
          marginColour = Col.sMargin col_scheme
      in
      coloured marginColour (text marginSpace) <>
      text ("\n") <>
      coloured marginColour (text marginRow) <>
      text (" " ++ srcLinePre) <>
      coloured sevColour (text srcLineSpan) <>
      text (srcLinePost ++ "\n") <>
      coloured marginColour (text marginSpace) <>
      coloured sevColour (text (" " ++ caretLine))

      where

        -- expand tabs in a device-independent manner #13664
        expandTabs tabWidth i s =
          case s of
            ""        -> ""
            '\t' : cs -> replicate effectiveWidth ' ' ++
                         expandTabs tabWidth (i + effectiveWidth) cs
            c    : cs -> c : expandTabs tabWidth (i + 1) cs
          where effectiveWidth = tabWidth - i `mod` tabWidth

        srcLine = filter (/= '\n') (expandTabs 8 0 srcLineWithNewline)

        start = srcSpanStartCol span - 1
        end | multiline = length srcLine
            | otherwise = srcSpanEndCol span - 1
        width = max 1 (end - start)

        marginWidth = length rowStr
        marginSpace = replicate marginWidth ' ' ++ " |"
        marginRow   = rowStr ++ " |"

        (srcLinePre,  srcLineRest) = splitAt start srcLine
        (srcLineSpan, srcLinePost) = splitAt width srcLineRest

        caretEllipsis | multiline = "..."
                      | otherwise = ""
        caretLine = replicate start ' ' ++ replicate width '^' ++ caretEllipsis

makeIntoWarning :: WarnReason -> ErrMsg -> ErrMsg
makeIntoWarning reason err = err
    { errMsgSeverity = SevWarning
    , errMsgReason = reason }

-- -----------------------------------------------------------------------------
-- Collecting up messages for later ordering and printing.

mk_err_msg :: DynFlags -> Severity -> SrcSpan -> PrintUnqualified -> ErrDoc -> ErrMsg
mk_err_msg dflags sev locn print_unqual doc
 = ErrMsg { errMsgSpan = locn
          , errMsgContext = print_unqual
          , errMsgDoc = doc
          , errMsgShortString = showSDoc dflags (vcat (errDocImportant doc))
          , errMsgSeverity = sev
          , errMsgReason = NoReason }

mkErrDoc :: DynFlags -> SrcSpan -> PrintUnqualified -> ErrDoc -> ErrMsg
mkErrDoc dflags = mk_err_msg dflags SevError

mkLongErrMsg, mkLongWarnMsg   :: DynFlags -> SrcSpan -> PrintUnqualified -> MsgDoc -> MsgDoc -> ErrMsg
-- ^ A long (multi-line) error message
mkErrMsg, mkWarnMsg           :: DynFlags -> SrcSpan -> PrintUnqualified -> MsgDoc            -> ErrMsg
-- ^ A short (one-line) error message
mkPlainErrMsg, mkPlainWarnMsg :: DynFlags -> SrcSpan ->                     MsgDoc            -> ErrMsg
-- ^ Variant that doesn't care about qualified/unqualified names

mkLongErrMsg   dflags locn unqual msg extra = mk_err_msg dflags SevError   locn unqual        (ErrDoc [msg] [] [extra])
mkErrMsg       dflags locn unqual msg       = mk_err_msg dflags SevError   locn unqual        (ErrDoc [msg] [] [])
mkPlainErrMsg  dflags locn        msg       = mk_err_msg dflags SevError   locn alwaysQualify (ErrDoc [msg] [] [])
mkLongWarnMsg  dflags locn unqual msg extra = mk_err_msg dflags SevWarning locn unqual        (ErrDoc [msg] [] [extra])
mkWarnMsg      dflags locn unqual msg       = mk_err_msg dflags SevWarning locn unqual        (ErrDoc [msg] [] [])
mkPlainWarnMsg dflags locn        msg       = mk_err_msg dflags SevWarning locn alwaysQualify (ErrDoc [msg] [] [])

----------------
emptyMessages :: Messages
emptyMessages = (emptyBag, emptyBag)

isEmptyMessages :: Messages -> Bool
isEmptyMessages (warns, errs) = isEmptyBag warns && isEmptyBag errs

errorsFound :: DynFlags -> Messages -> Bool
errorsFound _dflags (_warns, errs) = not (isEmptyBag errs)

warningsToMessages :: DynFlags -> WarningMessages -> Messages
warningsToMessages dflags =
  partitionBagWith $ \warn ->
    case isWarnMsgFatal dflags warn of
      Nothing -> Left warn
      Just err_reason ->
        Right warn{ errMsgSeverity = SevError
                  , errMsgReason = ErrReason err_reason }

printBagOfErrors :: DynFlags -> Bag ErrMsg -> IO ()
printBagOfErrors dflags bag_of_errors
  = sequence_ [ let style = mkErrStyle dflags unqual
                    ctx   = initSDocContext dflags style
                in putLogMsg dflags reason sev s $ withPprStyle style (formatErrDoc ctx doc)
              | ErrMsg { errMsgSpan      = s,
                         errMsgDoc       = doc,
                         errMsgSeverity  = sev,
                         errMsgReason    = reason,
                         errMsgContext   = unqual } <- sortMsgBag (Just dflags)
                                                                  bag_of_errors ]

formatErrDoc :: SDocContext -> ErrDoc -> SDoc
formatErrDoc ctx (ErrDoc important context supplementary)
  = case msgs of
        [msg] -> vcat msg
        _ -> vcat $ map starred msgs
    where
    msgs = filter (not . null) $ map (filter (not . Outputable.isEmpty ctx))
        [important, context, supplementary]
    starred = (bullet<+>) . vcat

pprErrMsgBagWithLoc :: Bag ErrMsg -> [SDoc]
pprErrMsgBagWithLoc bag = [ pprLocErrMsg item | item <- sortMsgBag Nothing bag ]

pprLocErrMsg :: ErrMsg -> SDoc
pprLocErrMsg (ErrMsg { errMsgSpan      = s
                     , errMsgDoc       = doc
                     , errMsgSeverity  = sev
                     , errMsgContext   = unqual })
  = sdocWithContext $ \ctx ->
    withErrStyle unqual $ mkLocMessage sev s (formatErrDoc ctx doc)

sortMsgBag :: Maybe DynFlags -> Bag ErrMsg -> [ErrMsg]
sortMsgBag dflags = maybeLimit . sortBy (cmp `on` errMsgSpan) . bagToList
  where cmp
          | fromMaybe False (fmap reverseErrors dflags) = SrcLoc.rightmost_smallest
          | otherwise                                   = SrcLoc.leftmost_smallest
        maybeLimit = case join (fmap maxErrors dflags) of
          Nothing        -> id
          Just err_limit -> take err_limit

ghcExit :: DynFlags -> Int -> IO ()
ghcExit dflags val
  | val == 0  = exitWith ExitSuccess
  | otherwise = do errorMsg dflags (text "\nCompilation had errors\n\n")
                   exitWith (ExitFailure val)

doIfSet :: Bool -> IO () -> IO ()
doIfSet flag action | flag      = action
                    | otherwise = return ()

doIfSet_dyn :: DynFlags -> GeneralFlag -> IO () -> IO()
doIfSet_dyn dflags flag action | gopt flag dflags = action
                               | otherwise        = return ()

-- -----------------------------------------------------------------------------
-- Dumping

dumpIfSet :: DynFlags -> Bool -> String -> SDoc -> IO ()
dumpIfSet dflags flag hdr doc
  | not flag   = return ()
  | otherwise  = doDump dflags hdr doc
{-# INLINE dumpIfSet #-}  -- see Note [INLINE conditional tracing utilities]

-- | This is a helper for 'dumpIfSet' to ensure that it's not duplicated
-- despite the fact that 'dumpIfSet' has an @INLINE@.
doDump :: DynFlags -> String -> SDoc -> IO ()
doDump dflags hdr doc =
  putLogMsg dflags
            NoReason
            SevDump
            noSrcSpan
            (withPprStyle defaultDumpStyle
              (mkDumpDoc hdr doc))

-- | A wrapper around 'dumpAction'.
-- First check whether the dump flag is set
-- Do nothing if it is unset
dumpIfSet_dyn :: DynFlags -> DumpFlag -> String -> DumpFormat -> SDoc -> IO ()
dumpIfSet_dyn = dumpIfSet_dyn_printer alwaysQualify
{-# INLINE dumpIfSet_dyn #-}  -- see Note [INLINE conditional tracing utilities]

-- | A wrapper around 'dumpAction'.
-- First check whether the dump flag is set
-- Do nothing if it is unset
--
-- Unlike 'dumpIfSet_dyn', has a printer argument
dumpIfSet_dyn_printer :: PrintUnqualified -> DynFlags -> DumpFlag -> String
                         -> DumpFormat -> SDoc -> IO ()
dumpIfSet_dyn_printer printer dflags flag hdr fmt doc
  = when (dopt flag dflags) $ do
      let sty = mkDumpStyle printer
      dumpAction dflags sty (dumpOptionsFromFlag flag) hdr fmt doc
{-# INLINE dumpIfSet_dyn_printer #-}  -- see Note [INLINE conditional tracing utilities]

mkDumpDoc :: String -> SDoc -> SDoc
mkDumpDoc hdr doc
   = vcat [blankLine,
           line <+> text hdr <+> line,
           doc,
           blankLine]
     where
        line = text (replicate 20 '=')


-- | Ensure that a dump file is created even if it stays empty
touchDumpFile :: DynFlags -> DumpOptions -> IO ()
touchDumpFile dflags dumpOpt = withDumpFileHandle dflags dumpOpt (const (return ()))

-- | Run an action with the handle of a 'DumpFlag' if we are outputting to a
-- file, otherwise 'Nothing'.
withDumpFileHandle :: DynFlags -> DumpOptions -> (Maybe Handle -> IO ()) -> IO ()
withDumpFileHandle dflags dumpOpt action = do
    let mFile = chooseDumpFile dflags dumpOpt
    case mFile of
      Just fileName -> do
        let gdref = generatedDumps dflags
        gd <- readIORef gdref
        let append = Set.member fileName gd
            mode = if append then AppendMode else WriteMode
        unless append $
            writeIORef gdref (Set.insert fileName gd)
        createDirectoryIfMissing True (takeDirectory fileName)
        withFile fileName mode $ \handle -> do
            -- We do not want the dump file to be affected by
            -- environment variables, but instead to always use
            -- UTF8. See:
            -- https://gitlab.haskell.org/ghc/ghc/issues/10762
            hSetEncoding handle utf8

            action (Just handle)
      Nothing -> action Nothing


-- | Write out a dump.
-- If --dump-to-file is set then this goes to a file.
-- otherwise emit to stdout.
--
-- When @hdr@ is empty, we print in a more compact format (no separators and
-- blank lines)
dumpSDocWithStyle :: PprStyle -> DynFlags -> DumpOptions -> String -> SDoc -> IO ()
dumpSDocWithStyle sty dflags dumpOpt hdr doc =
    withDumpFileHandle dflags dumpOpt writeDump
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
        defaultLogActionHPrintDoc dflags handle (withPprStyle sty doc')

    -- write the dump to stdout
    writeDump Nothing = do
        let (doc', severity)
              | null hdr  = (doc, SevOutput)
              | otherwise = (mkDumpDoc hdr doc, SevDump)
        putLogMsg dflags NoReason severity noSrcSpan (withPprStyle sty doc')


-- | Choose where to put a dump file based on DynFlags
--
chooseDumpFile :: DynFlags -> DumpOptions -> Maybe FilePath
chooseDumpFile dflags dumpOpt

        | gopt Opt_DumpToFile dflags || dumpForcedToFile dumpOpt
        , Just prefix <- getPrefix
        = Just $ setDir (prefix ++ dumpSuffix dumpOpt)

        | otherwise
        = Nothing

        where getPrefix
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

-- | Dump options
--
-- Dumps are printed on stdout by default except when the `dumpForcedToFile`
-- field is set to True.
--
-- When `dumpForcedToFile` is True or when `-ddump-to-file` is set, dumps are
-- written into a file whose suffix is given in the `dumpSuffix` field.
--
data DumpOptions = DumpOptions
   { dumpForcedToFile :: Bool   -- ^ Must be dumped into a file, even if
                                --   -ddump-to-file isn't set
   , dumpSuffix       :: String -- ^ Filename suffix used when dumped into
                                --   a file
   }

-- | Create dump options from a 'DumpFlag'
dumpOptionsFromFlag :: DumpFlag -> DumpOptions
dumpOptionsFromFlag Opt_D_th_dec_file =
   DumpOptions                        -- -dth-dec-file dumps expansions of TH
      { dumpForcedToFile = True       -- splices into MODULE.th.hs even when
      , dumpSuffix       = "th.hs"    -- -ddump-to-file isn't set
      }
dumpOptionsFromFlag flag =
   DumpOptions
      { dumpForcedToFile = False
      , dumpSuffix       = suffix -- build a suffix from the flag name
      }                           -- e.g. -ddump-asm => ".dump-asm"
   where
      str  = show flag
      suff = case stripPrefix "Opt_D_" str of
             Just x  -> x
             Nothing -> panic ("Bad flag name: " ++ str)
      suffix = map (\c -> if c == '_' then '-' else c) suff


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

errorMsg :: DynFlags -> MsgDoc -> IO ()
errorMsg dflags msg
   = putLogMsg dflags NoReason SevError noSrcSpan $ withPprStyle (defaultErrStyle dflags) msg

warningMsg :: DynFlags -> MsgDoc -> IO ()
warningMsg dflags msg
   = putLogMsg dflags NoReason SevWarning noSrcSpan $ withPprStyle (defaultErrStyle dflags) msg

fatalErrorMsg :: DynFlags -> MsgDoc -> IO ()
fatalErrorMsg dflags msg =
    putLogMsg dflags NoReason SevFatal noSrcSpan $ withPprStyle (defaultErrStyle dflags) msg

fatalErrorMsg'' :: FatalMessager -> String -> IO ()
fatalErrorMsg'' fm msg = fm msg

compilationProgressMsg :: DynFlags -> String -> IO ()
compilationProgressMsg dflags msg = do
    traceEventIO $ "GHC progress: " ++ msg
    ifVerbose dflags 1 $
        logOutput dflags $ withPprStyle defaultUserStyle (text msg)

showPass :: DynFlags -> String -> IO ()
showPass dflags what
  = ifVerbose dflags 2 $
    logInfo dflags $ withPprStyle defaultUserStyle (text "***" <+> text what <> colon)

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
           => DynFlags     -- ^ DynFlags
           -> SDoc         -- ^ The name of the phase
           -> (a -> ())    -- ^ A function to force the result
                           -- (often either @const ()@ or 'rnf')
           -> m a          -- ^ The body of the phase to be timed
           -> m a
withTiming dflags what force action =
  withTiming' dflags what force PrintTimings action

-- | Like withTiming but get DynFlags from the Monad.
withTimingD :: (MonadIO m, HasDynFlags m)
           => SDoc         -- ^ The name of the phase
           -> (a -> ())    -- ^ A function to force the result
                           -- (often either @const ()@ or 'rnf')
           -> m a          -- ^ The body of the phase to be timed
           -> m a
withTimingD what force action = do
  dflags <- getDynFlags
  withTiming' dflags what force PrintTimings action


-- | Same as 'withTiming', but doesn't print timings in the
--   console (when given @-vN@, @N >= 2@ or @-ddump-timings@).
--
--   See Note [withTiming] for more.
withTimingSilent
  :: MonadIO m
  => DynFlags   -- ^ DynFlags
  -> SDoc       -- ^ The name of the phase
  -> (a -> ())  -- ^ A function to force the result
                -- (often either @const ()@ or 'rnf')
  -> m a        -- ^ The body of the phase to be timed
  -> m a
withTimingSilent dflags what force action =
  withTiming' dflags what force DontPrintTimings action

-- | Same as 'withTiming', but doesn't print timings in the
--   console (when given @-vN@, @N >= 2@ or @-ddump-timings@)
--   and gets the DynFlags from the given Monad.
--
--   See Note [withTiming] for more.
withTimingSilentD
  :: (MonadIO m, HasDynFlags m)
  => SDoc       -- ^ The name of the phase
  -> (a -> ())  -- ^ A function to force the result
                -- (often either @const ()@ or 'rnf')
  -> m a        -- ^ The body of the phase to be timed
  -> m a
withTimingSilentD what force action = do
  dflags <- getDynFlags
  withTiming' dflags what force DontPrintTimings action

-- | Worker for 'withTiming' and 'withTimingSilent'.
withTiming' :: MonadIO m
            => DynFlags   -- ^ A means of getting a 'DynFlags' (often
                            -- 'getDynFlags' will work here)
            -> SDoc         -- ^ The name of the phase
            -> (a -> ())    -- ^ A function to force the result
                            -- (often either @const ()@ or 'rnf')
            -> PrintTimings -- ^ Whether to print the timings
            -> m a          -- ^ The body of the phase to be timed
            -> m a
withTiming' dflags what force_result prtimings action
  = do if verbosity dflags >= 2 || dopt Opt_D_dump_timings dflags
          then do whenPrintTimings $
                    logInfo dflags $ withPprStyle defaultUserStyle $
                      text "***" <+> what <> colon
                  let ctx = initDefaultSDocContext dflags
                  eventBegins ctx what
                  alloc0 <- liftIO getAllocationCounter
                  start <- liftIO getCPUTime
                  !r <- action
                  () <- pure $ force_result r
                  eventEnds ctx what
                  end <- liftIO getCPUTime
                  alloc1 <- liftIO getAllocationCounter
                  -- recall that allocation counter counts down
                  let alloc = alloc0 - alloc1
                      time = realToFrac (end - start) * 1e-9

                  when (verbosity dflags >= 2 && prtimings == PrintTimings)
                      $ liftIO $ logInfo dflags $ withPprStyle defaultUserStyle
                          (text "!!!" <+> what <> colon <+> text "finished in"
                           <+> doublePrec 2 time
                           <+> text "milliseconds"
                           <> comma
                           <+> text "allocated"
                           <+> doublePrec 3 (realToFrac alloc / 1024 / 1024)
                           <+> text "megabytes")

                  whenPrintTimings $
                      dumpIfSet_dyn dflags Opt_D_dump_timings "" FormatText
                          $ text $ showSDocOneLine ctx
                          $ hsep [ what <> colon
                                 , text "alloc=" <> ppr alloc
                                 , text "time=" <> doublePrec 3 time
                                 ]
                  pure r
           else action

    where whenPrintTimings = liftIO . when (prtimings == PrintTimings)
          eventBegins ctx w = do
            whenPrintTimings $ traceMarkerIO (eventBeginsDoc ctx w)
            liftIO $ traceEventIO (eventBeginsDoc ctx w)
          eventEnds ctx w = do
            whenPrintTimings $ traceMarkerIO (eventEndsDoc ctx w)
            liftIO $ traceEventIO (eventEndsDoc ctx w)

          eventBeginsDoc ctx w = showSDocOneLine ctx $ text "GHC:started:" <+> w
          eventEndsDoc   ctx w = showSDocOneLine ctx $ text "GHC:finished:" <+> w

debugTraceMsg :: DynFlags -> Int -> MsgDoc -> IO ()
debugTraceMsg dflags val msg =
   ifVerbose dflags val $
      logInfo dflags (withPprStyle defaultDumpStyle msg)
{-# INLINE debugTraceMsg #-}  -- see Note [INLINE conditional tracing utilities]

putMsg :: DynFlags -> MsgDoc -> IO ()
putMsg dflags msg = logInfo dflags (withPprStyle defaultUserStyle msg)

printInfoForUser :: DynFlags -> PrintUnqualified -> MsgDoc -> IO ()
printInfoForUser dflags print_unqual msg
  = logInfo dflags (withUserStyle print_unqual AllTheWay msg)

printOutputForUser :: DynFlags -> PrintUnqualified -> MsgDoc -> IO ()
printOutputForUser dflags print_unqual msg
  = logOutput dflags (withUserStyle print_unqual AllTheWay msg)

logInfo :: DynFlags -> MsgDoc -> IO ()
logInfo dflags msg
  = putLogMsg dflags NoReason SevInfo noSrcSpan msg

-- | Like 'logInfo' but with 'SevOutput' rather then 'SevInfo'
logOutput :: DynFlags -> MsgDoc -> IO ()
logOutput dflags msg
  = putLogMsg dflags NoReason SevOutput noSrcSpan msg

prettyPrintGhcErrors :: ExceptionMonad m => DynFlags -> m a -> m a
prettyPrintGhcErrors dflags
    = MC.handle $ \e -> case e of
                      PprPanic str doc ->
                          pprDebugAndThen dflags panic (text str) doc
                      PprSorry str doc ->
                          pprDebugAndThen dflags sorry (text str) doc
                      PprProgramError str doc ->
                          pprDebugAndThen dflags pgmError (text str) doc
                      _ ->
                          liftIO $ throwIO e

-- | Checks if given 'WarnMsg' is a fatal warning.
isWarnMsgFatal :: DynFlags -> WarnMsg -> Maybe (Maybe WarningFlag)
isWarnMsgFatal dflags ErrMsg{errMsgReason = Reason wflag}
  = if wopt_fatal wflag dflags
      then Just (Just wflag)
      else Nothing
isWarnMsgFatal dflags _
  = if gopt Opt_WarnIsError dflags
      then Just Nothing
      else Nothing

traceCmd :: DynFlags -> String -> String -> IO a -> IO a
-- trace the command (at two levels of verbosity)
traceCmd dflags phase_name cmd_line action
 = do   { let verb = verbosity dflags
        ; showPass dflags phase_name
        ; debugTraceMsg dflags 3 (text cmd_line)
        ; case flushErr dflags of
              FlushErr io -> io

           -- And run it!
        ; action `catchIO` handle_exn verb
        }
  where
    handle_exn _verb exn = do { debugTraceMsg dflags 2 (char '\n')
                              ; debugTraceMsg dflags 2
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

type DumpAction = DynFlags -> PprStyle -> DumpOptions -> String
                  -> DumpFormat -> SDoc -> IO ()

type TraceAction = forall a. DynFlags -> String -> SDoc -> a -> a

-- | Default action for 'dumpAction' hook
defaultDumpAction :: DumpAction
defaultDumpAction dflags sty dumpOpt title _fmt doc = do
   dumpSDocWithStyle sty dflags dumpOpt title doc

-- | Default action for 'traceAction' hook
defaultTraceAction :: TraceAction
defaultTraceAction dflags title doc = pprTraceWithFlags dflags title doc

-- | Helper for `dump_action`
dumpAction :: DumpAction
dumpAction dflags = dump_action dflags dflags

-- | Helper for `trace_action`
traceAction :: TraceAction
traceAction dflags = trace_action dflags dflags
