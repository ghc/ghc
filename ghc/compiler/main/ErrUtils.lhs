%
% (c) The AQUA Project, Glasgow University, 1994-1998
%
\section[ErrsUtils]{Utilities for error reporting}

\begin{code}
module ErrUtils (
	ErrMsg, WarnMsg, Message, Messages, errorsFound, warningsFound,

	addShortErrLocLine, addShortWarnLocLine,
	addErrLocHdrLine, addWarnLocHdrLine, dontAddErrLoc,

	printErrorsAndWarnings, pprBagOfErrors, pprBagOfWarnings,

	ghcExit,
	doIfSet, doIfSet_dyn, 
	dumpIfSet, dumpIfSet_core, dumpIfSet_dyn, dumpIfSet_dyn_or, 
	showPass
    ) where

#include "HsVersions.h"

import Bag		( Bag, bagToList, isEmptyBag )
import SrcLoc		( SrcLoc, noSrcLoc, isGoodSrcLoc )
import Util		( sortLt )
import Outputable
import CmdLineOpts	( DynFlags(..), DynFlag(..), dopt )

import System		( ExitCode(..), exitWith )
import IO		( hPutStr, stderr )
\end{code}

\begin{code}
type MsgWithLoc = (SrcLoc, SDoc)

type ErrMsg  = MsgWithLoc
type WarnMsg = MsgWithLoc
type Message = SDoc

addShortErrLocLine  :: SrcLoc -> Message -> ErrMsg
addErrLocHdrLine    :: SrcLoc -> Message -> Message -> ErrMsg
addWarnLocHdrLine    :: SrcLoc -> Message -> Message -> ErrMsg
addShortWarnLocLine :: SrcLoc -> Message -> WarnMsg

addShortErrLocLine locn rest_of_err_msg
  | isGoodSrcLoc locn = (locn, hang (ppr locn <> colon) 4 
				    rest_of_err_msg)
  | otherwise	      = (locn, rest_of_err_msg)

addErrLocHdrLine locn hdr rest_of_err_msg
  = ( locn
    , hang (ppr locn <> colon<+> hdr) 
         4 rest_of_err_msg
    )

addWarnLocHdrLine locn hdr rest_of_err_msg
  = ( locn
    , hang (ppr locn <> colon <+> ptext SLIT("Warning:") <+> hdr) 
         4 (rest_of_err_msg)
    )

addShortWarnLocLine locn rest_of_err_msg
  | isGoodSrcLoc locn = (locn, hang (ppr locn <> colon) 4 
				    (ptext SLIT("Warning:") <+> rest_of_err_msg))
  | otherwise	      = (locn, rest_of_err_msg)

dontAddErrLoc :: Message -> ErrMsg
dontAddErrLoc msg = (noSrcLoc, msg)

\end{code}


\begin{code}
type Messages = (Bag WarnMsg, Bag ErrMsg)

errorsFound :: Messages -> Bool
errorsFound (warns, errs) = not (isEmptyBag errs)

warningsFound :: Messages -> Bool
warningsFound (warns, errs) = not (isEmptyBag warns)

printErrorsAndWarnings :: PrintUnqualified -> Messages -> IO ()
	-- Don't print any warnings if there are errors
printErrorsAndWarnings unqual (warns, errs)
  | no_errs && no_warns  = return ()
  | no_errs		 = printErrs unqual (pprBagOfWarnings warns)
  | otherwise		 = printErrs unqual (pprBagOfErrors   errs)
  where
    no_warns = isEmptyBag warns
    no_errs  = isEmptyBag errs

pprBagOfErrors :: Bag ErrMsg -> SDoc
pprBagOfErrors bag_of_errors
  = vcat [text "" $$ p | (_,p) <- sorted_errs ]
    where
      bag_ls	  = bagToList bag_of_errors
      sorted_errs = sortLt occ'ed_before bag_ls

      occ'ed_before (a,_) (b,_) = LT == compare a b

pprBagOfWarnings :: Bag WarnMsg -> SDoc
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
  | otherwise  = printDump (dump hdr doc)

dumpIfSet_core :: DynFlags -> DynFlag -> String -> SDoc -> IO ()
dumpIfSet_core dflags flag hdr doc
  | dopt flag dflags
	|| verbosity dflags >= 4
	|| dopt Opt_D_verbose_core2core dflags 	= printDump (dump hdr doc)
  | otherwise                                   = return ()

dumpIfSet_dyn :: DynFlags -> DynFlag -> String -> SDoc -> IO ()
dumpIfSet_dyn dflags flag hdr doc
  | dopt flag dflags || verbosity dflags >= 4 = printDump (dump hdr doc)
  | otherwise                                 = return ()

dumpIfSet_dyn_or :: DynFlags -> [DynFlag] -> String -> SDoc -> IO ()
dumpIfSet_dyn_or dflags flags hdr doc
  | or [dopt flag dflags | flag <- flags]
  || verbosity dflags >= 4 
  = printDump (dump hdr doc)
  | otherwise = return ()

dump hdr doc 
   = vcat [text "", 
	   line <+> text hdr <+> line,
	   doc,
	   text ""]
     where 
        line = text (take 20 (repeat '='))
\end{code}
