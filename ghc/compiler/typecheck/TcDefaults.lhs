%
% (c) The AQUA Project, Glasgow University, 1993-1995
%
\section[TcDefaults]{Typechecking \tr{default} declarations}

\begin{code}
#include "HsVersions.h"

module TcDefaults ( tcDefaults ) where

import TcMonad
import AbsSyn

import AbsPrel		( intTy, doubleTy, unitTy )
import AbsUniType	( UniType
			  IF_ATTACK_PRAGMAS(COMMA cmpUniType)
			)
import CE		( lookupCE, CE(..) )
import E
import Inst
import Name
import TcMonoType	( tcMonoType )
import TcSimplify	( tcSimplifyCheckThetas )
import TVE
import Unique		( numClassKey, Unique )
import Util
\end{code}

\begin{code}
tcDefaults :: E
	   -> [RenamedDefaultDecl]
	   -> TcM [UniType] 	    -- defaulting types to heave
				    -- into Tc monad for later use
				    -- in Disambig.

tcDefaults _ []
  = returnTc [intTy, doubleTy] -- language-specified default `default'

tcDefaults e [DefaultDecl mono_tys locn]
  = let
	ce  = getE_CE  e
	tce = getE_TCE e
	tve = nullTVE

	num_clas = lookupCE ce (PreludeClass numClassKey (panic "tcDefaults"))
    in
    babyTcMtoTcM (mapB_Tc (tcMonoType ce tce tve) mono_tys) `thenTc` \ tau_tys ->

	-- compensate for extreme parser hack: `default ()' actually
	-- sends the *type* () through to here.  Squash it.
    case tau_tys of
      [ty] | ty == unitTy -> returnTc []

      _  -> -- (Back to your regularly scheduled programming...)

	    -- Check that all the types are instances of Num

	tcSimplifyCheckThetas (DefaultDeclOrigin locn)
			 [ (num_clas, ty) | ty <- tau_tys ] `thenTc` \ _ ->
	    -- We only care about whether it worked or not

	returnTc tau_tys -- caller will bung them into Tc monad

tcDefaults _ (_:_)
  = error "ERROR: You can only have one `default' declaration per module."
    -- ToDo: proper error msg.
\end{code}
