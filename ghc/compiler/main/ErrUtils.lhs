%
% (c) The AQUA Project, Glasgow University, 1994-1998
%
\section[ErrsUtils]{Utilities for error reporting}

\begin{code}
module ErrUtils (
	Message, mkLocMessage, printError,

	ErrMsg, WarnMsg,
	errMsgSpans, errMsgContext, errMsgShortDoc, errMsgExtraInfo,
	Messages, errorsFound, emptyMessages,
	mkErrMsg, mkWarnMsg, mkPlainErrMsg, mkLongErrMsg,
	printErrorsAndWarnings, pprBagOfErrors, pprBagOfWarnings,

	ghcExit,
	doIfSet, doIfSet_dyn, 
	dumpIfSet, dumpIfSet_core, dumpIfSet_dyn, dumpIfSet_dyn_or, mkDumpDoc,
	showPass
    ) where

#include "HsVersions.h"

import Bag		( Bag, bagToList, isEmptyBag, emptyBag )
import SrcLoc		( SrcSpan )
import Util		( sortLt )
import Outputable
import qualified Pretty
import SrcLoc		( srcSpanStart )
import CmdLineOpts	( DynFlags(..), DynFlag(..), dopt,
			  opt_ErrorSpans )

import List             ( replicate, sortBy )
import System		( ExitCode(..), exitWith )
import IO		( hPutStr, stderr, stdout )


-- -----------------------------------------------------------------------------
-- Basic error messages: just render a message with a source location.

type Message = SDoc

mkLocMessage :: SrcSpan -> Message -> Message
mkLocMessage locn msg
  | opt_ErrorSpans = hang (ppr locn <> colon) 4 msg
  | otherwise      = hang (ppr (srcSpanStart locn) <> colon) 4 msg
  -- always print the location, even if it is unhelpful.  Error messages
  -- are supposed to be in a standard format, and one without a location
  -- would look strange.  Better to say explicitly "<no location info>".

printError :: SrcSpan -> Message -> IO ()
printError span msg = printErrs (mkLocMessage span msg $ defaultErrStyle)


-- -----------------------------------------------------------------------------
-- Collecting up messages for later ordering and printing.

data ErrMsg = ErrMsg { 
	errMsgSpans     :: [SrcSpan],
	errMsgContext   :: PrintUnqualified,
	errMsgShortDoc  :: Message,
	errMsgExtraInfo :: Message
	}
	-- The SrcSpan is used for sorting errors into line-number order
	-- NB  Pretty.Doc not SDoc: we deal with the printing style (in ptic 
	-- whether to qualify an External Name) at the error occurrence

type WarnMsg = ErrMsg

-- A short (one-line) error message, with context to tell us whether
-- to qualify names in the message or not.
mkErrMsg :: SrcSpan -> PrintUnqualified -> Message -> ErrMsg
mkErrMsg locn print_unqual msg
  = ErrMsg [locn] print_unqual msg empty

-- Variant that doesn't care about qualified/unqualified names
mkPlainErrMsg :: SrcSpan -> Message -> ErrMsg
mkPlainErrMsg locn msg
  = ErrMsg [locn] alwaysQualify msg empty

-- A long (multi-line) error message, with context to tell us whether
-- to qualify names in the message or not.
mkLongErrMsg :: SrcSpan -> PrintUnqualified -> Message -> Message -> ErrMsg
mkLongErrMsg locn print_unqual msg extra 
 = ErrMsg [locn] print_unqual msg extra

-- A long (multi-line) error message, with context to tell us whether
-- to qualify names in the message or not.
mkLongMultiLocErrMsg :: [SrcSpan] -> PrintUnqualified -> Message -> Message -> ErrMsg
mkLongMultiLocErrMsg locns print_unqual msg extra
  = ErrMsg locns print_unqual msg extra

mkWarnMsg :: SrcSpan -> PrintUnqualified -> Message -> WarnMsg
mkWarnMsg = mkErrMsg

mkLongWarnMsg :: SrcSpan -> PrintUnqualified -> Message -> Message -> WarnMsg
mkLongWarnMsg = mkLongErrMsg

type Messages = (Bag WarnMsg, Bag ErrMsg)

emptyMessages :: Messages
emptyMessages = (emptyBag, emptyBag)

errorsFound :: DynFlags -> Messages -> Bool
-- The dyn-flags are used to see if the user has specified
-- -Werorr, which says that warnings should be fatal
errorsFound dflags (warns, errs) 
  | dopt Opt_WarnIsError dflags = not (isEmptyBag errs) || not (isEmptyBag warns)
  | otherwise  		        = not (isEmptyBag errs)

printErrorsAndWarnings :: Messages -> IO ()
printErrorsAndWarnings (warns, errs)
  | no_errs && no_warns  = return ()
  | no_errs		 = printErrs (pprBagOfWarnings warns)
			    -- Don't print any warnings if there are errors
  | otherwise		 = printErrs (pprBagOfErrors   errs)
  where
    no_warns = isEmptyBag warns
    no_errs  = isEmptyBag errs

pprBagOfErrors :: Bag ErrMsg -> Pretty.Doc
pprBagOfErrors bag_of_errors
  = Pretty.vcat [ let style = mkErrStyle unqual
		      doc = mkLocMessage s (d $$ e)
		  in
		  Pretty.text "" Pretty.$$ doc style
		| ErrMsg { errMsgSpans = s:ss,
			   errMsgShortDoc = d,
			   errMsgExtraInfo = e,
			   errMsgContext = unqual } <- sorted_errs ]
    where
      bag_ls	  = bagToList bag_of_errors
      sorted_errs = sortLt occ'ed_before bag_ls

      occ'ed_before err1 err2 = 
         LT == compare (head (errMsgSpans err1)) (head (errMsgSpans err2))

pprBagOfWarnings :: Bag WarnMsg -> Pretty.Doc
pprBagOfWarnings bag_of_warns = pprBagOfErrors bag_of_warns
\end{code}

\begin{code}
ghcExit :: Int -> IO ()
ghcExit val
  | val == 0  = exitWith ExitSuccess
  | otherwise = do hPutStr stderr "\nCompilation had errors\n\n"
	           exitWith (ExitFailure val)
\end{code}

\begin{code}
doIfSet :: Bool -> IO () -> IO ()
doIfSet flag action | flag      = action
		    | otherwise = return ()

doIfSet_dyn :: DynFlags -> DynFlag -> IO () -> IO()
doIfSet_dyn dflags flag action | dopt flag dflags = action
		               | otherwise        = return ()
\end{code}

\begin{code}
showPass :: DynFlags -> String -> IO ()
showPass dflags what
  | verbosity dflags >= 2 = hPutStr stderr ("*** "++what++":\n")
  | otherwise		  = return ()

dumpIfSet :: Bool -> String -> SDoc -> IO ()
dumpIfSet flag hdr doc
  | not flag   = return ()
  | otherwise  = printDump (mkDumpDoc hdr doc)

dumpIfSet_core :: DynFlags -> DynFlag -> String -> SDoc -> IO ()
dumpIfSet_core dflags flag hdr doc
  | dopt flag dflags
	|| verbosity dflags >= 4
	|| dopt Opt_D_verbose_core2core dflags 	= printDump (mkDumpDoc hdr doc)
  | otherwise                                   = return ()

dumpIfSet_dyn :: DynFlags -> DynFlag -> String -> SDoc -> IO ()
dumpIfSet_dyn dflags flag hdr doc
  | dopt flag dflags || verbosity dflags >= 4 
  = printDump (mkDumpDoc hdr doc)
  | otherwise
  = return ()

dumpIfSet_dyn_or :: DynFlags -> [DynFlag] -> String -> SDoc -> IO ()
dumpIfSet_dyn_or dflags flags hdr doc
  | or [dopt flag dflags | flag <- flags]
  || verbosity dflags >= 4 
  = printDump (mkDumpDoc hdr doc)
  | otherwise = return ()

mkDumpDoc hdr doc 
   = vcat [text "", 
	   line <+> text hdr <+> line,
	   doc,
	   text ""]
     where 
        line = text (replicate 20 '=')
\end{code}
