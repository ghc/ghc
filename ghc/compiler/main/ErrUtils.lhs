%
% (c) The AQUA Project, Glasgow University, 1994-1995
%
\section[ErrsUtils]{Utilities for error reporting}

This is an internal module---access to these functions is through
@Errors@.

DPH errors are in here, too.

\begin{code}
#include "HsVersions.h"

module ErrUtils where

import Bag		( Bag, bagToList )
import Outputable
import Pretty		-- to pretty-print error messages
import SrcLoc		( mkUnknownSrcLoc, SrcLoc )
import Util
\end{code}

\begin{code}
type Error = PprStyle -> Pretty

addErrLoc :: SrcLoc -> String -> Error -> Error
addErrLoc locn title rest_of_err_msg sty
  = ppHang (ppBesides [ppr PprForUser locn,
		       if null title then ppNil else ppStr (": " ++ title),
		       ppChar ':'])
    	 4 (rest_of_err_msg sty)

addShortErrLocLine :: SrcLoc -> Error -> Error
addShortErrLocLine locn rest_of_err_msg sty
  = ppHang (ppBeside (ppr PprForUser locn) (ppChar ':'))
	 4 (rest_of_err_msg sty)

dontAddErrLoc :: String -> Error -> Error
dontAddErrLoc title rest_of_err_msg sty
  = ppHang (ppBesides [ppStr title, ppChar ':'])
    	 4 (rest_of_err_msg sty)

pprBagOfErrors :: PprStyle -> Bag Error -> Pretty
pprBagOfErrors sty bag_of_errors
  = let  pretties = map ( \ e -> e sty ) (bagToList bag_of_errors)  in
    ppAboves (map (\ p -> ppAbove ppSP p) pretties)

#ifdef DPH
addWarningLoc :: SrcLoc -> Error -> Error
addWarningLoc locn rest_of_err_msg sty
  = ppHang (ppBesides [ppStr "*** Warning *** ",
		       ppr PprForUser locn,ppStr ": "])
    	 4 (ppAbove (rest_of_err_msg sty)
		    (ppSP))

addWarning :: Error -> Error
addWarning rest_of_err_msg sty
  = ppBeside (ppStr "*** Warning *** : ")
	     (rest_of_err_msg sty)
#endif {- Data Parallel Haskell -}
\end{code}
