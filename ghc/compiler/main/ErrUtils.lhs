%
% (c) The AQUA Project, Glasgow University, 1994-1998
%
\section[ErrsUtils]{Utilities for error reporting}

\begin{code}
module ErrUtils (
	ErrMsg, WarnMsg, Message,
	addShortErrLocLine, addShortWarnLocLine,
	addErrLocHdrLine,
	dontAddErrLoc,
	printErrorsAndWarnings, pprBagOfErrors, pprBagOfWarnings,
	ghcExit,
	doIfSet, dumpIfSet
    ) where

#include "HsVersions.h"

import Bag		( Bag, bagToList, isEmptyBag )
import SrcLoc		( SrcLoc, noSrcLoc )
import Util		( sortLt )
import Outputable

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
addShortWarnLocLine :: SrcLoc -> Message -> WarnMsg

addShortErrLocLine locn rest_of_err_msg
  = ( locn
    , hang (ppr locn <> colon) 
         4 rest_of_err_msg
    )

addErrLocHdrLine locn hdr rest_of_err_msg
  = ( locn
    , hang (ppr locn <> colon<+> hdr) 
         4 rest_of_err_msg
    )

addShortWarnLocLine locn rest_of_err_msg
  = ( locn
    , hang (ppr locn <> colon)
	 4 (ptext SLIT("Warning:") <+> rest_of_err_msg)
    )

dontAddErrLoc :: String -> Message -> ErrMsg
dontAddErrLoc title rest_of_err_msg
 | null title = (noSrcLoc, rest_of_err_msg)
 | otherwise  =
    ( noSrcLoc, hang (text title <> colon) 4 rest_of_err_msg )

printErrorsAndWarnings :: Bag ErrMsg -> Bag WarnMsg -> IO ()
	-- Don't print any warnings if there are errors
printErrorsAndWarnings errs warns
  | no_errs && no_warns  = return ()
  | no_errs		 = printErrs (pprBagOfWarnings warns)
  | otherwise		 = printErrs (pprBagOfErrors   errs)
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
\end{code}

\begin{code}
dumpIfSet :: Bool -> String -> SDoc -> IO ()
dumpIfSet flag hdr doc
  | not flag  = return ()
  | otherwise = printDump dump
  where
    dump = vcat [text "", 
		 line <+> text hdr <+> line,
		 doc,
		 text ""]
    line = text (take 20 (repeat '='))
\end{code}
