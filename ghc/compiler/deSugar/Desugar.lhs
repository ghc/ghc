%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Desugar]{@deSugar@: the main function}

\begin{code}
module Desugar ( deSugar, pprDsWarnings ) where

#include "HsVersions.h"

import CmdLineOpts	( opt_D_dump_ds )
import HsSyn		( MonoBinds )
import TcHsSyn		( TypecheckedMonoBinds, TypecheckedForeignDecl )

import CoreSyn
import PprCore		( pprCoreBindings )
import DsMonad
import DsBinds		( dsMonoBinds )
import DsForeign	( dsForeigns )
import DsUtils

import Bag		( isEmptyBag )
import BasicTypes       ( Module )
import CmdLineOpts	( opt_SccGroup, opt_SccProfilingOn )
import CoreLift		( liftCoreBindings )
import CoreLint		( lintCoreBindings )
import Id		( nullIdEnv, GenId, Id )
import ErrUtils		( dumpIfSet, doIfSet )
import Outputable
import UniqSupply	( splitUniqSupply, UniqSupply )
\end{code}

The only trick here is to get the @DsMonad@ stuff off to a good
start.

\begin{code}
deSugar :: UniqSupply		-- name supply
	-> Module		-- module name
	-> TypecheckedMonoBinds
	-> [TypecheckedForeignDecl]
	-> IO ([CoreBinding], SDoc, SDoc, SDoc) -- output

deSugar us mod_name all_binds fo_decls
  = let
	(us1, us2) = splitUniqSupply us
	(us3, us4) = splitUniqSupply us2

        module_and_group = (mod_name, grp_name)
	grp_name  = case opt_SccGroup of
		    	Just xx -> _PK_ xx
		    	Nothing -> mod_name	-- default: module name

	(core_prs, ds_warns) = initDs us1 nullIdEnv module_and_group 
			       (dsMonoBinds opt_SccProfilingOn all_binds [])

	((fi_binds, fe_binds, hc_code, h_code, c_code), ds_warns2) = 
	           initDs us3 nullIdEnv module_and_group 
			 (dsForeigns fo_decls)

	ds_binds' = liftCoreBindings us4 [Rec (core_prs)]
	ds_binds  = fi_binds ++ ds_binds' ++ fe_binds
    in

	-- Display any warnings
    doIfSet (not (isEmptyBag ds_warns))
	(printErrs (pprDsWarnings ds_warns)) >>

	-- Lint result if necessary
    lintCoreBindings "Desugarer" False ds_binds >>

	-- Dump output
    dumpIfSet opt_D_dump_ds "Desugared:"
	(pprCoreBindings ds_binds)	>>

    return (ds_binds, hc_code, h_code, c_code)
\end{code}
