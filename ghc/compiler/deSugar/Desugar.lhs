%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Desugar]{@deSugar@: the main function}

\begin{code}
#include "HsVersions.h"

module Desugar ( deSugar, DsMatchContext, pprDsWarnings, 
                 DsWarnFlavour -- removed when compiling with 1.4
	       ) where

IMP_Ubiq(){-uitous-}

import HsSyn		( HsBinds, HsExpr )
import TcHsSyn		( SYN_IE(TypecheckedHsBinds), SYN_IE(TypecheckedHsExpr) )
import CoreSyn
import Name             ( isExported )
import DsMonad
import DsBinds		( dsBinds, dsInstBinds )
import DsUtils

import Bag		( unionBags )
import CmdLineOpts	( opt_DoCoreLinting, opt_AutoSccsOnAllToplevs, 
		          opt_AutoSccsOnExportedToplevs, opt_SccGroup
			   )
import CostCentre       ( IsCafCC(..), mkAutoCC )
import CoreLift		( liftCoreBindings )
import CoreLint		( lintCoreBindings )
import Id		( nullIdEnv, mkIdEnv, idType, SYN_IE(DictVar), GenId )
import PprStyle		( PprStyle(..) )
import UniqSupply	( splitUniqSupply )
\end{code}

The only trick here is to get the @DsMonad@ stuff off to a good
start.

\begin{code}
deSugar :: UniqSupply		-- name supply
	-> FAST_STRING			-- module name

	-> (TypecheckedHsBinds, -- input: recsel, class, instance, and value
	    TypecheckedHsBinds, --   bindings; see "tcModule" (which produces
	    TypecheckedHsBinds,	--   them)
	    TypecheckedHsBinds,
	    [(Id, TypecheckedHsExpr)])
-- ToDo: handling of const_inst thingies is certainly WRONG ***************************

	-> ([CoreBinding],	-- output
	    DsWarnings)	    -- Shadowing complaints

deSugar us mod_name (recsel_binds, clas_binds, inst_binds, val_binds, const_inst_pairs)
  = let
	(us0, us0a) = splitUniqSupply us
	(us1, us1a) = splitUniqSupply us0a
	(us2, us2a) = splitUniqSupply us1a
	(us3, us3a) = splitUniqSupply us2a
	(us4, us5)  = splitUniqSupply us3a


	module_and_group = (mod_name, grp_name)
	grp_name  = case opt_SccGroup of
		    	Just xx -> _PK_ xx
		    	Nothing -> mod_name	-- default: module name

	((core_const_prs, consts_pairs), shadows1)
	    = initDs us0 nullIdEnv mod_name (dsInstBinds [] const_inst_pairs)

	consts_env = mkIdEnv consts_pairs

	(core_clas_binds, shadows2)
			= initDs us1 consts_env mod_name (dsBinds clas_binds)
	core_clas_prs	= pairsFromCoreBinds core_clas_binds

	(core_inst_binds, shadows3)
			= initDs us2 consts_env mod_name (dsBinds inst_binds)
	core_inst_prs	= pairsFromCoreBinds core_inst_binds

	(core_val_binds, shadows4)
			= initDs us3 consts_env mod_name (dsBinds val_binds)
	core_val_pairs	= map (addAutoScc module_and_group) (pairsFromCoreBinds core_val_binds)

	(core_recsel_binds, shadows5)
			= initDs us4 consts_env mod_name (dsBinds recsel_binds)
	core_recsel_prs	= pairsFromCoreBinds core_recsel_binds

    	final_binds
	  = if (null core_clas_prs && null core_inst_prs
	     && null core_recsel_prs {-???dont know???-} && null core_const_prs) then
		-- we don't have to make the whole thing recursive
		core_clas_binds ++ core_val_binds

	    else -- gotta make it recursive (sigh)
	       [Rec (core_clas_prs ++ core_inst_prs
		  ++ core_const_prs ++ core_val_pairs ++ core_recsel_prs)]

	lift_final_binds = liftCoreBindings us5 final_binds

	really_final_binds = if opt_DoCoreLinting
			     then lintCoreBindings PprDebug "Desugarer" False lift_final_binds
			     else lift_final_binds

	shadows = shadows1 `unionBags` shadows2 `unionBags`
		  shadows3 `unionBags` shadows4 `unionBags` shadows5
    in
    (really_final_binds, shadows)
\end{code}


%************************************************************************
%*									*
\subsection[addAutoScc]{Adding automatic sccs}
%*									*
%************************************************************************

\begin{code}
addAutoScc :: (FAST_STRING, FAST_STRING)	-- Module and group
	   -> (Id, CoreExpr)
	   -> (Id,CoreExpr)

addAutoScc (mod, grp) pair@(bndr, core_expr)
  | worthSCC core_expr &&
    (opt_AutoSccsOnAllToplevs ||
     (isExported bndr && opt_AutoSccsOnExportedToplevs))
  = (bndr, SCC (mkAutoCC bndr mod grp IsNotCafCC) core_expr)

  | otherwise
  = pair

worthSCC (SCC _ _) = False
worthSCC (Con _ _) = False
worthSCC core_expr = True
\end{code}
