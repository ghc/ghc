%
% (c) The AQUA Project, Glasgow University, 1994-1995
%
\section[ErrsUtils]{Utilities for error reporting}

\begin{code}
#include "HsVersions.h"

module ErrUtils (
	SYN_IE(Error), SYN_IE(Warning), SYN_IE(Message),
	addErrLoc,
	addShortErrLocLine, addShortWarnLocLine,
	dontAddErrLoc,
	pprBagOfErrors,
	ghcExit
    ) where

IMP_Ubiq(){-uitous-}

import Bag		--( bagToList )
import PprStyle		( PprStyle(..) )
import Pretty
import SrcLoc		( noSrcLoc, SrcLoc{-instance-} )
#if __GLASGOW_HASKELL__ >= 202
import Outputable
#endif
\end{code}

\begin{code}
type Error   = PprStyle -> Doc
type Warning = PprStyle -> Doc
type Message = PprStyle -> Doc

addErrLoc :: SrcLoc -> String -> Error -> Error
addErrLoc locn title rest_of_err_msg sty
  = hang (hcat [ppr PprForUser locn,
		if null title then empty else text (": " ++ title),
		char ':'])
    	 4 (rest_of_err_msg sty)

addShortErrLocLine, addShortWarnLocLine :: SrcLoc -> Error -> Error

addShortErrLocLine locn rest_of_err_msg sty
  = hang ((<>) (ppr PprForUser locn) (char ':'))
	 4 (rest_of_err_msg sty)

addShortWarnLocLine locn rest_of_err_msg sty
  = hang ((<>) (ppr PprForUser locn) (ptext SLIT(":warning:")))
	 4 (rest_of_err_msg sty)

dontAddErrLoc :: String -> Error -> Error
dontAddErrLoc title rest_of_err_msg sty
  = hang (hcat [text title, char ':'])
    	 4 (rest_of_err_msg sty)

pprBagOfErrors :: PprStyle -> Bag Error -> Doc
pprBagOfErrors sty bag_of_errors
  = let  pretties = map ( \ e -> e sty ) (bagToList bag_of_errors)  in
    vcat (map (\ p -> ($$) space p) pretties)
\end{code}

\begin{code}
ghcExit :: Int -> IO ()

ghcExit val
  = if val /= 0
    then error "Compilation had errors\n"
    else return ()
\end{code}
