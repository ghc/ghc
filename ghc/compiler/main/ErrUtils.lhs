%
% (c) The AQUA Project, Glasgow University, 1994-1995
%
\section[ErrsUtils]{Utilities for error reporting}

\begin{code}
module ErrUtils (
	ErrMsg, WarnMsg, Message,
	addShortErrLocLine, addShortWarnLocLine,
	dontAddErrLoc,
	pprBagOfErrors, pprBagOfWarnings,
	ghcExit,
	doIfSet, dumpIfSet
    ) where

#include "HsVersions.h"

import Bag		( Bag, bagToList )
import SrcLoc		( SrcLoc )
import Outputable
\end{code}

\begin{code}
type ErrMsg   = SDoc
type WarnMsg = SDoc
type Message = SDoc

addShortErrLocLine, addShortWarnLocLine :: SrcLoc -> ErrMsg -> ErrMsg

addShortErrLocLine locn rest_of_err_msg
  = hang (ppr locn <> colon)
	 4 rest_of_err_msg

addShortWarnLocLine locn rest_of_err_msg
  = hang (ppr locn <> ptext SLIT(": Warning:"))
	 4 rest_of_err_msg

dontAddErrLoc :: String -> ErrMsg -> ErrMsg
dontAddErrLoc title rest_of_err_msg
  = hang (hcat [text title, char ':'])
    	 4 rest_of_err_msg

pprBagOfErrors :: Bag ErrMsg -> SDoc
pprBagOfErrors bag_of_errors
  = vcat [space $$ p | p <- bagToList bag_of_errors]

pprBagOfWarnings :: Bag ErrMsg -> SDoc
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
