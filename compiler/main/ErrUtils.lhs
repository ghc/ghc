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
        
        errorsFound, emptyMessages,
        mkErrMsg, mkPlainErrMsg, mkLongErrMsg, mkWarnMsg, mkPlainWarnMsg,
        printBagOfErrors, 
        warnIsErrorMsg, mkLongWarnMsg,

        ghcExit,
        doIfSet, doIfSet_dyn,
        dumpIfSet, dumpIfSet_dyn, dumpIfSet_dyn_or,
        mkDumpDoc, dumpSDoc,

        --  * Messages during compilation
        putMsg, putMsgWith,
        errorMsg,
        fatalErrorMsg, fatalErrorMsg',
        compilationProgressMsg,
        showPass,
        debugTraceMsg,
    ) where

#include "HsVersions.h"

import Bag              ( Bag, bagToList, isEmptyBag, emptyBag )
import Util
import Outputable
import FastString
import SrcLoc
import DynFlags
import StaticFlags      ( opt_ErrorSpans )

import System.Directory
import System.Exit      ( ExitCode(..), exitWith )
import System.FilePath
import Data.List
import qualified Data.Set as Set
import Data.IORef
import Control.Monad
import System.IO

-- -----------------------------------------------------------------------------
-- Basic error messages: just render a message with a source location.

type Messages        = (WarningMessages, ErrorMessages)
type WarningMessages = Bag WarnMsg
type ErrorMessages   = Bag ErrMsg

data ErrMsg = ErrMsg {
        errMsgSpans     :: [SrcSpan],
        errMsgContext   :: PrintUnqualified,
        errMsgShortDoc  :: MsgDoc,
        errMsgExtraInfo :: MsgDoc,
        errMsgSeverity  :: Severity
        }
        -- The SrcSpan is used for sorting errors into line-number order

type WarnMsg = ErrMsg
type MsgDoc = SDoc

data Severity
  = SevOutput
  | SevDump
  | SevInfo
  | SevWarning
  | SevError
  | SevFatal

instance Show ErrMsg where
    show em = showSDoc (errMsgShortDoc em)

pprMessageBag :: Bag MsgDoc -> SDoc
pprMessageBag msgs = vcat (punctuate blankLine (bagToList msgs))

mkLocMessage :: Severity -> SrcSpan -> MsgDoc -> MsgDoc
  -- Always print the location, even if it is unhelpful.  Error messages
  -- are supposed to be in a standard format, and one without a location
  -- would look strange.  Better to say explicitly "<no location info>".
mkLocMessage severity locn msg
  | opt_ErrorSpans = hang (ppr locn <> colon <+> sev_info) 4 msg
  | otherwise      = hang (ppr (srcSpanStart locn) <> colon <+> sev_info) 4 msg
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

mk_err_msg :: Severity -> SrcSpan -> PrintUnqualified -> MsgDoc -> SDoc -> ErrMsg
mk_err_msg sev locn print_unqual msg extra 
 = ErrMsg { errMsgSpans = [locn], errMsgContext = print_unqual
          , errMsgShortDoc = msg, errMsgExtraInfo = extra
          , errMsgSeverity = sev }

mkLongErrMsg, mkLongWarnMsg   :: SrcSpan -> PrintUnqualified -> MsgDoc -> MsgDoc -> ErrMsg
-- A long (multi-line) error message
mkErrMsg, mkWarnMsg           :: SrcSpan -> PrintUnqualified -> MsgDoc            -> ErrMsg
-- A short (one-line) error message
mkPlainErrMsg, mkPlainWarnMsg :: SrcSpan ->                     MsgDoc            -> ErrMsg
-- Variant that doesn't care about qualified/unqualified names

mkLongErrMsg   locn unqual msg extra = mk_err_msg SevError   locn unqual        msg extra
mkErrMsg       locn unqual msg       = mk_err_msg SevError   locn unqual        msg empty
mkPlainErrMsg  locn        msg       = mk_err_msg SevError   locn alwaysQualify msg empty
mkLongWarnMsg  locn unqual msg extra = mk_err_msg SevWarning locn unqual        msg extra
mkWarnMsg      locn unqual msg       = mk_err_msg SevWarning locn unqual        msg empty
mkPlainWarnMsg locn        msg       = mk_err_msg SevWarning locn alwaysQualify msg empty

----------------
emptyMessages :: Messages
emptyMessages = (emptyBag, emptyBag)

warnIsErrorMsg :: ErrMsg
warnIsErrorMsg = mkPlainErrMsg noSrcSpan (text "\nFailing due to -Werror.")

errorsFound :: DynFlags -> Messages -> Bool
errorsFound _dflags (_warns, errs) = not (isEmptyBag errs)

printBagOfErrors :: DynFlags -> Bag ErrMsg -> IO ()
printBagOfErrors dflags bag_of_errors
  = printMsgBag dflags bag_of_errors

pprErrMsgBag :: Bag ErrMsg -> [SDoc]
pprErrMsgBag bag
  = [ let style = mkErrStyle unqual
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
  = withPprStyle (mkErrStyle unqual) (mkLocMessage sev s (d $$ e))
  where
    (s : _) = spans   -- Should be non-empty

printMsgBag :: DynFlags -> Bag ErrMsg -> IO ()
printMsgBag dflags bag
  = sequence_ [ let style = mkErrStyle unqual
                in log_action dflags sev s style (d $$ e)
              | ErrMsg { errMsgSpans     = s:_,
                         errMsgShortDoc  = d,
                         errMsgSeverity  = sev,
                         errMsgExtraInfo = e,
                         errMsgContext   = unqual } <- sortMsgBag bag ]

sortMsgBag :: Bag ErrMsg -> [ErrMsg]
sortMsgBag bag = sortLe srcOrder $ bagToList bag
  where
    srcOrder err1 err2 =
        case compare (head (errMsgSpans err1)) (head (errMsgSpans err2)) of
            LT -> True
            EQ -> True
            GT -> False

ghcExit :: DynFlags -> Int -> IO ()
ghcExit dflags val
  | val == 0  = exitWith ExitSuccess
  | otherwise = do errorMsg dflags (text "\nCompilation had errors\n\n")
                   exitWith (ExitFailure val)

doIfSet :: Bool -> IO () -> IO ()
doIfSet flag action | flag      = action
                    | otherwise = return ()

doIfSet_dyn :: DynFlags -> DynFlag -> IO () -> IO()
doIfSet_dyn dflags flag action | dopt flag dflags = action
                               | otherwise        = return ()

-- -----------------------------------------------------------------------------
-- Dumping

dumpIfSet :: DynFlags -> Bool -> String -> SDoc -> IO ()
dumpIfSet dflags flag hdr doc
  | not flag   = return ()
  | otherwise  = log_action dflags SevDump noSrcSpan defaultDumpStyle (mkDumpDoc hdr doc)

dumpIfSet_dyn :: DynFlags -> DynFlag -> String -> SDoc -> IO ()
dumpIfSet_dyn dflags flag hdr doc
  | dopt flag dflags || verbosity dflags >= 4
  = dumpSDoc dflags flag hdr doc
  | otherwise
  = return ()

dumpIfSet_dyn_or :: DynFlags -> [DynFlag] -> String -> SDoc -> IO ()
dumpIfSet_dyn_or _ [] _ _ = return ()
dumpIfSet_dyn_or dflags (flag : flags) hdr doc
    = if dopt flag dflags || verbosity dflags >= 4
      then dumpSDoc dflags flag hdr doc
      else dumpIfSet_dyn_or dflags flags hdr doc

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
dumpSDoc :: DynFlags -> DynFlag -> String -> SDoc -> IO ()
dumpSDoc dflags dflag hdr doc
 = do let mFile = chooseDumpFile dflags dflag
      case mFile of
            -- write the dump to a file
            -- don't add the header in this case, we can see what kind
            -- of dump it is from the filename.
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
                        hPrintDump handle doc
                        hClose handle

            -- write the dump to stdout
            Nothing
                 -> log_action dflags SevDump noSrcSpan defaultDumpStyle (mkDumpDoc hdr doc)


-- | Choose where to put a dump file based on DynFlags
--
chooseDumpFile :: DynFlags -> DynFlag -> Maybe String
chooseDumpFile dflags dflag

        | dopt Opt_DumpToFile dflags
        , Just prefix <- getPrefix
        = Just $ setDir (prefix ++ (beautifyDumpName dflag))

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

-- | Build a nice file name from name of a DynFlag constructor
beautifyDumpName :: DynFlag -> String
beautifyDumpName dflag
 = let str  = show dflag
       cut  = if isPrefixOf "Opt_D_" str then drop 6 str else str
       dash = map (\c -> if c == '_' then '-' else c) cut
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
putMsg dflags msg = log_action dflags SevInfo noSrcSpan defaultUserStyle msg

putMsgWith :: DynFlags -> PrintUnqualified -> MsgDoc -> IO ()
putMsgWith dflags print_unqual msg
  = log_action dflags SevInfo noSrcSpan sty msg
  where
    sty = mkUserStyle print_unqual AllTheWay

errorMsg :: DynFlags -> MsgDoc -> IO ()
errorMsg dflags msg = log_action dflags SevError noSrcSpan defaultErrStyle msg

fatalErrorMsg :: DynFlags -> MsgDoc -> IO ()
fatalErrorMsg dflags msg = fatalErrorMsg' (log_action dflags) msg

fatalErrorMsg' :: LogAction -> MsgDoc -> IO ()
fatalErrorMsg' la msg = la SevFatal noSrcSpan defaultErrStyle msg

compilationProgressMsg :: DynFlags -> String -> IO ()
compilationProgressMsg dflags msg
  = ifVerbose dflags 1 (log_action dflags SevOutput noSrcSpan defaultUserStyle (text msg))

showPass :: DynFlags -> String -> IO ()
showPass dflags what
  = ifVerbose dflags 2 (log_action dflags SevInfo noSrcSpan defaultUserStyle (text "***" <+> text what <> colon))

debugTraceMsg :: DynFlags -> Int -> MsgDoc -> IO ()
debugTraceMsg dflags val msg
  = ifVerbose dflags val (log_action dflags SevInfo noSrcSpan defaultDumpStyle msg)
\end{code}

