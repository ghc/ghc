%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Desugar]{@deSugar@: the main function}

\begin{code}
#include "HsVersions.h"

module Desugar ( deSugar, pprDsWarnings
#if __GLASGOW_HASKELL__ < 200
		, DsMatchContext
#endif
	       ) where

IMP_Ubiq(){-uitous-}

import CmdLineOpts	( opt_D_dump_ds )
import HsSyn		( HsBinds, HsExpr, MonoBinds,
			  SYN_IE(RecFlag), nonRecursive, recursive
			)
import TcHsSyn		( SYN_IE(TypecheckedMonoBinds), SYN_IE(TypecheckedHsExpr)
			)
import CoreSyn
import PprCore		( pprCoreBindings )
import Name             ( isExported )
import DsMonad
import DsBinds		( dsMonoBinds )
import DsUtils

import Bag		( unionBags, isEmptyBag )
import BasicTypes       ( SYN_IE(Module) )
import CmdLineOpts	( opt_DoCoreLinting, opt_SccGroup, opt_SccProfilingOn )
import CostCentre       ( IsCafCC(..), mkAutoCC )
import CoreLift		( liftCoreBindings )
import CoreLint		( lintCoreBindings )
import Id		( nullIdEnv, mkIdEnv, idType, 
			  SYN_IE(DictVar), GenId, SYN_IE(Id) )
import ErrUtils		( dumpIfSet, doIfSet )
import Outputable	( PprStyle(..), pprDumpStyle, pprErrorsStyle, printErrs )
import Pretty		( Doc )
import UniqSupply	( splitUniqSupply, UniqSupply )
\end{code}

The only trick here is to get the @DsMonad@ stuff off to a good
start.

\begin{code}
deSugar :: UniqSupply		-- name supply
	-> Module		-- module name
	-> TypecheckedMonoBinds
	-> IO [CoreBinding]	-- output

deSugar us mod_name all_binds
  = let
	(us1, us2) = splitUniqSupply us

        module_and_group = (mod_name, grp_name)
	grp_name  = case opt_SccGroup of
		    	Just xx -> _PK_ xx
		    	Nothing -> mod_name	-- default: module name

	(core_prs, ds_warns) = initDs us1 nullIdEnv module_and_group 
			       (dsMonoBinds opt_SccProfilingOn recursive all_binds [])

	ds_binds = liftCoreBindings us2 [Rec core_prs]
    in

	-- Display any warnings
    doIfSet (not (isEmptyBag ds_warns))
	(printErrs (pprDsWarnings pprErrorsStyle ds_warns)) >>

	-- Lint result if necessary
    lintCoreBindings "Desugarer" False ds_binds >>

	-- Dump output
    dumpIfSet opt_D_dump_ds "Desugared:"
	(pprCoreBindings pprDumpStyle ds_binds)	>>

    return ds_binds    
\end{code}
