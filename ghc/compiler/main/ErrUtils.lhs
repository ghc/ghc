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

import Bag		( bagToList )
import PprStyle		( PprStyle(..) )
import Pretty
import SrcLoc		( noSrcLoc, SrcLoc{-instance-} )
\end{code}

\begin{code}
type Error   = PprStyle -> Pretty
type Warning = PprStyle -> Pretty
type Message = PprStyle -> Pretty

addErrLoc :: SrcLoc -> String -> Error -> Error
addErrLoc locn title rest_of_err_msg sty
  = ppHang (ppBesides [ppr PprForUser locn,
		       if null title then ppNil else ppStr (": " ++ title),
		       ppChar ':'])
    	 4 (rest_of_err_msg sty)

addShortErrLocLine, addShortWarnLocLine :: SrcLoc -> Error -> Error

addShortErrLocLine locn rest_of_err_msg sty
  = ppHang (ppBeside (ppr PprForUser locn) (ppChar ':'))
	 4 (rest_of_err_msg sty)

addShortWarnLocLine locn rest_of_err_msg sty
  = ppHang (ppBeside (ppr PprForUser locn) (ppPStr SLIT(":warning:")))
	 4 (rest_of_err_msg sty)

dontAddErrLoc :: String -> Error -> Error
dontAddErrLoc title rest_of_err_msg sty
  = ppHang (ppBesides [ppStr title, ppChar ':'])
    	 4 (rest_of_err_msg sty)

pprBagOfErrors :: PprStyle -> Bag Error -> Pretty
pprBagOfErrors sty bag_of_errors
  = let  pretties = map ( \ e -> e sty ) (bagToList bag_of_errors)  in
    ppAboves (map (\ p -> ppAbove ppSP p) pretties)
\end{code}

\begin{code}
ghcExit :: Int -> IO ()

ghcExit val
  = if val /= 0
    then error "Compilation had errors\n"
    else return ()
\end{code}
