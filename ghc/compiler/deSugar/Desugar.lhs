%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Desugar]{@deSugar@: the main function}

\begin{code}
#include "HsVersions.h"

module Desugar ( deSugar, pprDsWarnings
#if __GLASGOW_HASKELL__ < 200
		, DsMatchContext
	        , DsWarnFlavour -- fluff needed for closure, 
				 -- removed when compiling with 1.4
#endif
	       ) where

IMP_Ubiq(){-uitous-}

import HsSyn		( HsBinds, HsExpr, MonoBinds,
			  SYN_IE(RecFlag), nonRecursive
			)
import TcHsSyn		( SYN_IE(TypecheckedHsBinds), SYN_IE(TypecheckedHsExpr)
			)
import CoreSyn
import Name             ( isExported )
import DsMonad
import DsBinds		( dsBinds )
import DsUtils

import Bag		( unionBags )
import CmdLineOpts	( opt_DoCoreLinting, opt_SccGroup )
import CostCentre       ( IsCafCC(..), mkAutoCC )
import CoreLift		( liftCoreBindings )
import CoreLint		( lintCoreBindings )
import Id		( nullIdEnv, mkIdEnv, idType, 
			  SYN_IE(DictVar), GenId, SYN_IE(Id) )
import Outputable	( PprStyle(..) )
import UniqSupply	( splitUniqSupply, UniqSupply )
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
	    TypecheckedHsBinds)
-- ToDo: handling of const_inst thingies is certainly WRONG ***************************

	-> ([CoreBinding],	-- output
	    DsWarnings)	    -- Shadowing complaints

deSugar us mod_name (recsel_binds, clas_binds, inst_binds, val_binds, const_inst_binds)
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

	(core_const_binds, shadows1)
	    = initDs us0 nullIdEnv mod_name (dsBinds Nothing const_inst_binds)
	core_const_prs = pairsFromCoreBinds core_const_binds

	(core_clas_binds, shadows2)
			= initDs us1 nullIdEnv mod_name (dsBinds Nothing clas_binds)
	core_clas_prs	= pairsFromCoreBinds core_clas_binds

	(core_inst_binds, shadows3)
			= initDs us2 nullIdEnv mod_name (dsBinds Nothing inst_binds)
	core_inst_prs	= pairsFromCoreBinds core_inst_binds

	(core_val_binds, shadows4)
			= initDs us3 nullIdEnv mod_name (dsBinds (Just module_and_group) val_binds)
	core_val_pairs	= pairsFromCoreBinds core_val_binds

	(core_recsel_binds, shadows5)
			= initDs us4 nullIdEnv mod_name (dsBinds Nothing recsel_binds)
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
