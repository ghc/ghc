%
% (c) The AQUA Project, Glasgow University, 1994-1995
%
\section[ErrsUtils]{Utilities for error reporting}

\begin{code}
#include "HsVersions.h"

module ErrUtils (

	Error(..),
	addErrLoc, addShortErrLocLine,
	dontAddErrLoc, pprBagOfErrors,

	TcError(..), TcWarning(..), Message(..),
	mkTcErr, arityErr

    ) where

import Ubiq{-uitous-}

import Bag		( bagToList )
import PprStyle		( PprStyle(..) )
import Pretty
import SrcLoc		( mkUnknownSrcLoc, SrcLoc{-instance-} )
\end{code}

\begin{code}
type Error   = PprStyle -> Pretty

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
\end{code}

TypeChecking Errors
~~~~~~~~~~~~~~~~~~~

\begin{code}
type Message   = PprStyle -> Pretty
type TcError   = Message
type TcWarning = Message


mkTcErr :: SrcLoc 		-- Where
	-> [Message] 		-- Context
	-> Message 		-- What went wrong
	-> TcError		-- The complete error report

mkTcErr locn ctxt msg sty
  = ppHang (ppBesides [ppr PprForUser locn, ppStr ": ", msg sty])
    	 4 (ppAboves [msg sty | msg <- ctxt])


arityErr kind name n m sty =
    ppBesides [ ppStr "`", ppr sty name, ppStr "' should have ",
		n_arguments, ppStr ", but has been given ", ppInt m, ppChar '.']
    where
	errmsg = kind ++ " has too " ++ quantity ++ " arguments"
	quantity | m < n     = "few"
		 | otherwise = "many"
	n_arguments | n == 0 = ppStr "no arguments"
		    | n == 1 = ppStr "1 argument"
		    | True   = ppCat [ppInt n, ppStr "arguments"]
\end{code}
