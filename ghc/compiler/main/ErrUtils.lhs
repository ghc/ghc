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
	pprBagOfErrors, pprBagOfWarnings,
	ghcExit,
	doIfSet, dumpIfSet
    ) where

#include "HsVersions.h"

import Bag		( Bag, bagToList )
import SrcLoc		( SrcLoc, noSrcLoc )
import Util		( sortLt )
import Outputable
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
    , hang (ppr locn <> ptext SLIT(": Warning:")) 
        4 rest_of_err_msg
    )

dontAddErrLoc :: String -> Message -> ErrMsg
dontAddErrLoc title rest_of_err_msg
 | null title = (noSrcLoc, rest_of_err_msg)
 | otherwise  =
    ( noSrcLoc, hang (hcat [text title, char ':'])
		  4  rest_of_err_msg )

pprBagOfErrors :: Bag ErrMsg -> SDoc
pprBagOfErrors bag_of_errors
  = vcat [space $$ p | (_,p) <- sorted_errs ]
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
  = if val /= 0
    then error "Compilation had errors\n"
    else return ()
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
