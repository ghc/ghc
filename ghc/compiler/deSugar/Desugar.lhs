%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Desugar]{@deSugar@: the main function}

\begin{code}
module Desugar ( deSugar ) where

#include "HsVersions.h"

import CmdLineOpts	( opt_D_dump_ds )
import HsSyn		( MonoBinds )
import TcHsSyn		( TypecheckedMonoBinds, TypecheckedForeignDecl )
import CoreSyn
import DsMonad
import DsBinds		( dsMonoBinds, AutoScc(..) )
import DsForeign	( dsForeigns )
import DsUtils
import DsExpr		()	-- Forces DsExpr to be compiled; DsBinds only
				-- depends on DsExpr.hi-boot.
import Module		( Module, moduleString )
import Bag		( isEmptyBag, unionBags )
import CmdLineOpts	( opt_SccGroup, opt_SccProfilingOn )
import CoreLint		( beginPass, endPass )
import ErrUtils		( doIfSet, pprBagOfWarnings )
import Outputable
import UniqSupply	( splitUniqSupply, UniqSupply )
\end{code}

The only trick here is to get the @DsMonad@ stuff off to a good
start.

\begin{code}
deSugar :: UniqSupply		-- name supply
        -> ValueEnv		-- value env
	-> Module		-- module name
	-> TypecheckedMonoBinds
	-> [TypecheckedForeignDecl]
	-> IO ([CoreBind], SDoc, SDoc) -- output

deSugar us global_val_env mod_name all_binds fo_decls = do
	beginPass "Desugar"
	-- Do desugaring
	let (core_prs, ds_warns1) = initDs us1 global_val_env module_and_group 
				            (dsMonoBinds auto_scc all_binds [])
	    auto_scc | opt_SccProfilingOn = TopLevel
		     | otherwise          = NoSccs
            ds_binds' = [Rec core_prs]

    	    ((fi_binds, fe_binds, h_code, c_code), ds_warns2) = 
	            initDs us3 global_val_env module_and_group (dsForeigns mod_name fo_decls)

  	    ds_binds  = fi_binds ++ ds_binds' ++ fe_binds

	    ds_warns = ds_warns1 `unionBags` ds_warns2

	 -- Display any warnings
        doIfSet (not (isEmptyBag ds_warns))
		(printErrs (pprBagOfWarnings ds_warns))

	 -- Lint result if necessary
        endPass "Desugar" opt_D_dump_ds ds_binds
        return (ds_binds, h_code, c_code)
  where
    (us1, us2) = splitUniqSupply us
    (us3, us4) = splitUniqSupply us2

    module_and_group = (mod_name, grp_name)
    grp_name  = case opt_SccGroup of
	          Just xx -> _PK_ xx
	    	  Nothing -> _PK_ (moduleString mod_name)	-- default: module name

\end{code}
