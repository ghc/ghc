%
% (c) The AQUA Project, Glasgow University, 1994-1998
%
\section[ErrsUtils]{Utilities for error reporting}

\begin{code}

module ErrUtils (
        ErrMsg, WarnMsg, Severity(..),
        Messages, ErrorMessages, WarningMessages,
        errMsgSpans, errMsgContext, errMsgShortDoc, errMsgExtraInfo,
        MsgDoc, mkLocMessage, pprMessageBag, pprErrMsgBag, pprErrMsgBagWithLoc,
        pprLocErrMsg, makeIntoWarning,
        
        errorsFound, emptyMessages, isEmptyMessages,
        mkErrMsg, mkPlainErrMsg, mkLongErrMsg, mkWarnMsg, mkPlainWarnMsg,
        printBagOfErrors, 
        warnIsErrorMsg, mkLongWarnMsg,

        ghcExit,
        doIfSet, doIfSet_dyn,
        dumpIfSet, dumpIfSet_dyn,
        mkDumpDoc, dumpSDoc,

        --  * Messages during compilation
        putMsg, putMsgWith,
        errorMsg,
        fatalErrorMsg, fatalErrorMsg', fatalErrorMsg'',
        compilationProgressMsg,
        showPass,
        debugTraceMsg,

        prettyPrintGhcErrors,
    ) where

#include "HsVersions.h"

import Bag              ( Bag, bagToList, isEmptyBag, emptyBag )
import Exception
import Outputable
import Panic
import FastString
import SrcLoc
import DynFlags

import System.Directory
import System.Exit      ( ExitCode(..), exitWith )
import System.FilePath
import Data.List
import qualified Data.Set as Set
import Data.IORef
import Data.Ord
import Data.Time
import Control.Monad
import Control.Monad.IO.Class
import System.IO

-- -----------------------------------------------------------------------------
-- Basic error messages: just render a message with a source location.

type Messages        = (WarningMessages, ErrorMessages)
type WarningMessages = Bag WarnMsg
type ErrorMessages   = Bag ErrMsg

data ErrMsg = ErrMsg {
        errMsgSpans     :: [SrcSpan],
        errMsgContext   :: PrintUnqualified,
        errMsgShortDoc  :: MsgDoc,   -- errMsgShort* should always
        errMsgShortString :: String, -- contain the same text
        errMsgExtraInfo :: MsgDoc,
        errMsgSeverity  :: Severity
        }
        -- The SrcSpan is used for sorting errors into line-number order

type WarnMsg = ErrMsg
type MsgDoc = SDoc

data Severity
  = SevOutput
  | SevDump
  | SevInteractive
  | SevInfo
  | SevWarning
  | SevError
  | SevFatal

instance Show ErrMsg where
    show em = errMsgShortString em

pprMessageBag :: Bag MsgDoc -> SDoc
pprMessageBag msgs = vcat (punctuate blankLine (bagToList msgs))

mkLocMessage :: Severity -> SrcSpan -> MsgDoc -> MsgDoc
  -- Always print the location, even if it is unhelpful.  Error messages
  -- are supposed to be in a standard format, and one without a location
  -- would look strange.  Better to say explicitly "<no location info>".
mkLocMessage severity locn msg
    = sdocWithDynFlags $ \dflags ->
      let locn' = if gopt Opt_ErrorSpans dflags
                  then ppr locn
                  else ppr (srcSpanStart locn)
      in hang (locn' <> colon <+> sev_info) 4 msg
  where
    sev_info = case severity of
                 SevWarning -> ptext (sLit "Warning:")
                 _other     -> empty                 
      -- For warnings, print    Foo.hs:34: Warning:
      --                           <the warning message>

makeIntoWarning :: ErrMsg -> ErrMsg
makeIntoWarning err = err { errMsgSeverity = SevWarning }

-- -----------------------------------------------------------------------------
-- Collecting up messages for later ordering and printing.

mk_err_msg :: DynFlags -> Severity -> SrcSpan -> PrintUnqualified -> MsgDoc -> SDoc -> ErrMsg
mk_err_msg  dflags sev locn print_unqual msg extra
 = ErrMsg { errMsgSpans = [locn], errMsgContext = print_unqual
          , errMsgShortDoc = msg , errMsgShortString = showSDoc dflags msg
          , errMsgExtraInfo = extra
          , errMsgSeverity = sev }

mkLongErrMsg, mkLongWarnMsg   :: DynFlags -> SrcSpan -> PrintUnqualified -> MsgDoc -> MsgDoc -> ErrMsg
-- A long (multi-line) error message
mkErrMsg, mkWarnMsg           :: DynFlags -> SrcSpan -> PrintUnqualified -> MsgDoc            -> ErrMsg
-- A short (one-line) error message
mkPlainErrMsg, mkPlainWarnMsg :: DynFlags -> SrcSpan ->                     MsgDoc            -> ErrMsg
-- Variant that doesn't care about qualified/unqualified names

mkLongErrMsg   dflags locn unqual msg extra = mk_err_msg dflags SevError   locn unqual        msg extra
mkErrMsg       dflags locn unqual msg       = mk_err_msg dflags SevError   locn unqual        msg empty
mkPlainErrMsg  dflags locn        msg       = mk_err_msg dflags SevError   locn alwaysQualify msg empty
mkLongWarnMsg  dflags locn unqual msg extra = mk_err_msg dflags SevWarning locn unqual        msg extra
mkWarnMsg      dflags locn unqual msg       = mk_err_msg dflags SevWarning locn unqual        msg empty
mkPlainWarnMsg dflags locn        msg       = mk_err_msg dflags SevWarning locn alwaysQualify msg empty

----------------
emptyMessages :: Messages
emptyMessages = (emptyBag, emptyBag)

isEmptyMessages :: Messages -> Bool
isEmptyMessages (warns, errs) = isEmptyBag warns && isEmptyBag errs

warnIsErrorMsg :: DynFlags -> ErrMsg
warnIsErrorMsg dflags
    = mkPlainErrMsg dflags noSrcSpan (text "\nFailing due to -Werror.")

errorsFound :: DynFlags -> Messages -> Bool
errorsFound _dflags (_warns, errs) = not (isEmptyBag errs)

printBagOfErrors :: DynFlags -> Bag ErrMsg -> IO ()
printBagOfErrors dflags bag_of_errors
  = printMsgBag dflags bag_of_errors

pprErrMsgBag :: Bag ErrMsg -> [SDoc]
pprErrMsgBag bag
  = [ sdocWithDynFlags $ \dflags ->
      let style = mkErrStyle dflags unqual
      in withPprStyle style (d $$ e)
    | ErrMsg { errMsgShortDoc  = d,
               errMsgExtraInfo = e,
               errMsgContext   = unqual } <- sortMsgBag bag ]

pprErrMsgBagWithLoc :: Bag ErrMsg -> [SDoc]
pprErrMsgBagWithLoc bag = [ pprLocErrMsg item | item <- sortMsgBag bag ]

pprLocErrMsg :: ErrMsg -> SDoc
pprLocErrMsg (ErrMsg { errMsgSpans     = spans
                     , errMsgShortDoc  = d
                     , errMsgExtraInfo = e
                     , errMsgSeverity  = sev
                     , errMsgContext   = unqual })
  = sdocWithDynFlags $ \dflags ->
    withPprStyle (mkErrStyle dflags unqual) (mkLocMessage sev s (d $$ e))
  where
    (s : _) = spans   -- Should be non-empty

printMsgBag :: DynFlags -> Bag ErrMsg -> IO ()
printMsgBag dflags bag
  = sequence_ [ let style = mkErrStyle dflags unqual
                in log_action dflags dflags sev s style (d $$ e)
              | ErrMsg { errMsgSpans     = s:_,
                         errMsgShortDoc  = d,
                         errMsgSeverity  = sev,
                         errMsgExtraInfo = e,
                         errMsgContext   = unqual } <- sortMsgBag bag ]

sortMsgBag :: Bag ErrMsg -> [ErrMsg]
sortMsgBag bag = sortBy (comparing (head . errMsgSpans)) $ bagToList bag
                 -- TODO: Why "head ."? Why not compare the whole list?

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
  | otherwise  = log_action dflags dflags SevDump noSrcSpan defaultDumpStyle (mkDumpDoc hdr doc)

dumpIfSet_dyn :: DynFlags -> DumpFlag -> String -> SDoc -> IO ()
dumpIfSet_dyn dflags flag hdr doc
  | dopt flag dflags
  = dumpSDoc dflags flag hdr doc
  | otherwise
  = return ()

mkDumpDoc :: String -> SDoc -> SDoc
mkDumpDoc hdr doc
   = vcat [blankLine,
           line <+> text hdr <+> line,
           doc,
           blankLine]
     where
        line = text (replicate 20 '=')


-- | Write out a dump.
--      If --dump-to-file is set then this goes to a file.
--      otherwise emit to stdout.
-- 
-- When hdr is empty, we print in a more compact format (no separators and
-- blank lines)
dumpSDoc :: DynFlags -> DumpFlag -> String -> SDoc -> IO ()
dumpSDoc dflags flag hdr doc
 = do let mFile = chooseDumpFile dflags flag
      case mFile of
            Just fileName
                 -> do
                        let gdref = generatedDumps dflags
                        gd <- readIORef gdref
                        let append = Set.member fileName gd
                            mode = if append then AppendMode else WriteMode
                        when (not append) $
                            writeIORef gdref (Set.insert fileName gd)
                        createDirectoryIfMissing True (takeDirectory fileName)
                        handle <- openFile fileName mode
                        doc' <- if null hdr
                                then return doc
                                else do t <- getCurrentTime
                                        let d = text (show t)
                                             $$ blankLine
                                             $$ doc
                                        return $ mkDumpDoc hdr d
                        defaultLogActionHPrintDoc dflags handle doc' defaultDumpStyle
                        hClose handle

            -- write the dump to stdout
            Nothing -> do
              let (doc', severity)
                    | null hdr  = (doc, SevOutput)
                    | otherwise = (mkDumpDoc hdr doc, SevDump)
              log_action dflags dflags severity noSrcSpan defaultDumpStyle doc'


-- | Choose where to put a dump file based on DynFlags
--
chooseDumpFile :: DynFlags -> DumpFlag -> Maybe String
chooseDumpFile dflags flag

        | gopt Opt_DumpToFile dflags
        , Just prefix <- getPrefix
        = Just $ setDir (prefix ++ (beautifyDumpName flag))

        | otherwise
        = Nothing

        where getPrefix
                 -- dump file location is being forced
                 --      by the --ddump-file-prefix flag.
               | Just prefix <- dumpPrefixForce dflags
                  = Just prefix
                 -- dump file location chosen by DriverPipeline.runPipeline
               | Just prefix <- dumpPrefix dflags
                  = Just prefix
                 -- we haven't got a place to put a dump file.
               | otherwise
                  = Nothing
              setDir f = case dumpDir dflags of
                         Just d  -> d </> f
                         Nothing ->       f

-- | Build a nice file name from name of a GeneralFlag constructor
beautifyDumpName :: DumpFlag -> String
beautifyDumpName flag
 = let str = show flag
       suff = case stripPrefix "Opt_D_" str of
              Just x -> x
              Nothing -> panic ("Bad flag name: " ++ str)
       dash = map (\c -> if c == '_' then '-' else c) suff
   in dash


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

putMsg :: DynFlags -> MsgDoc -> IO ()
putMsg dflags msg = log_action dflags dflags SevInfo noSrcSpan defaultUserStyle msg

putMsgWith :: DynFlags -> PrintUnqualified -> MsgDoc -> IO ()
putMsgWith dflags print_unqual msg
  = log_action dflags dflags SevInfo noSrcSpan sty msg
  where
    sty = mkUserStyle print_unqual AllTheWay

errorMsg :: DynFlags -> MsgDoc -> IO ()
errorMsg dflags msg =
    log_action dflags dflags SevError noSrcSpan (defaultErrStyle dflags) msg

fatalErrorMsg :: DynFlags -> MsgDoc -> IO ()
fatalErrorMsg dflags msg = fatalErrorMsg' (log_action dflags) dflags msg

fatalErrorMsg' :: LogAction -> DynFlags -> MsgDoc -> IO ()
fatalErrorMsg' la dflags msg =
    la dflags SevFatal noSrcSpan (defaultErrStyle dflags) msg

fatalErrorMsg'' :: FatalMessager -> String -> IO ()
fatalErrorMsg'' fm msg = fm msg

compilationProgressMsg :: DynFlags -> String -> IO ()
compilationProgressMsg dflags msg
  = ifVerbose dflags 1 (log_action dflags dflags SevOutput noSrcSpan defaultUserStyle (text msg))

showPass :: DynFlags -> String -> IO ()
showPass dflags what
  = ifVerbose dflags 2 (log_action dflags dflags SevInfo noSrcSpan defaultUserStyle (text "***" <+> text what <> colon))

debugTraceMsg :: DynFlags -> Int -> MsgDoc -> IO ()
debugTraceMsg dflags val msg
  = ifVerbose dflags val (log_action dflags dflags SevInfo noSrcSpan defaultDumpStyle msg)

prettyPrintGhcErrors :: ExceptionMonad m => DynFlags -> m a -> m a
prettyPrintGhcErrors dflags
    = ghandle $ \e -> case e of
                      PprPanic str doc ->
                          pprDebugAndThen dflags panic str doc
                      PprSorry str doc ->
                          pprDebugAndThen dflags sorry str doc
                      PprProgramError str doc ->
                          pprDebugAndThen dflags pgmError str doc
                      _ ->
                          liftIO $ throwIO e
\end{code}

