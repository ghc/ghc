%
% (c) The AQUA Project, Glasgow University, 1993-1995
%
\section[TcDefaults]{Typechecking \tr{default} declarations}

\begin{code}
#include "HsVersions.h"

module TcDefaults ( tcDefaults ) where

import Ubiq

import HsSyn		( DefaultDecl(..), MonoType,
			  HsExpr, HsLit, ArithSeqInfo, Fake, InPat)
import RnHsSyn		( RenamedDefaultDecl(..) )
import TcHsSyn		( TcIdOcc )

import TcMonad		hiding ( rnMtoTcM )
import Inst		( InstOrigin(..) )
import TcEnv		( tcLookupClassByKey )
import TcMonoType	( tcMonoType )
import TcSimplify	( tcSimplifyCheckThetas )

import PrelInfo		( intTy, doubleTy, unitTy )
import Unique		( numClassKey )
import Util
\end{code}

\begin{code}
tcDefaults :: [RenamedDefaultDecl]
	   -> TcM s [Type] 	    -- defaulting types to heave
				    -- into Tc monad for later use
				    -- in Disambig.

tcDefaults []
  = returnTc [intTy, doubleTy] 	    -- language-specified default `default'

tcDefaults [DefaultDecl mono_tys locn]
  = tcAddSrcLoc locn $
    mapTc tcMonoType mono_tys	`thenTc` \ tau_tys ->

    case tau_tys of
      [] -> returnTc []		-- no defaults

      _  ->
	    -- Check that all the types are instances of Num
	    -- We only care about whether it worked or not

	tcLookupClassByKey numClassKey			`thenNF_Tc` \ num ->
	tcSimplifyCheckThetas DefaultDeclOrigin
		[ (num, ty) | ty <- tau_tys ]		`thenTc` \ _ ->

	returnTc tau_tys

\end{code}
