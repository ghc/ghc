%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[Desugar]{@deSugar@: the main function}

\begin{code}
#include "HsVersions.h"

module Desugar (
	deSugar,

	-- and to make the interface self-sufficient...
	SplitUniqSupply, Binds, Expr, Id, TypecheckedPat,
	CoreBinding, GlobalSwitch, SwitchResult,
	Bag, DsMatchContext, DsMatchKind
    ) where


import AbsSyn		-- the stuff being desugared
import PlainCore	-- the output of desugaring;
			-- importing this module also gets all the
			-- CoreSyn utility functions
import DsMonad		-- the monadery used in the desugarer

import Bag		( unionBags, Bag )
import CmdLineOpts	( switchIsOn, GlobalSwitch(..), SwitchResult )
import CoreLift		( liftCoreBindings )
import CoreLint		( lintCoreBindings )
import DsBinds		( dsBinds, dsInstBinds )
import IdEnv
import Pretty		( PprStyle(..) )
import SplitUniq
import Util
\end{code}

The only trick here is to get the @DesugarMonad@ stuff off to a good
start.

\begin{code}
deSugar :: SplitUniqSupply		-- name supply
	-> (GlobalSwitch->SwitchResult)	-- switch looker upper
	-> FAST_STRING			-- module name

	-> (TypecheckedBinds,   -- input: class, instance, and value
	    TypecheckedBinds,	--   bindings; see "tcModule" (which produces
	    TypecheckedBinds,	--   them)
	    [(Inst, TypecheckedExpr)])
-- ToDo: handling of const_inst thingies is certainly WRONG ***************************

	-> ([PlainCoreBinding],	-- output
	    Bag DsMatchContext)	-- Shadowing complaints

deSugar us sw_chkr mod_name (clas_binds, inst_binds, val_binds, const_inst_pairs)
  = let
	(us0, us0a) = splitUniqSupply us
	(us1, us1a) = splitUniqSupply us0a
	(us2, us2a) = splitUniqSupply us1a
	(us3, us4)  = splitUniqSupply us2a

	((core_const_prs, consts_pairs), shadows1)
	    = initDs us0 nullIdEnv sw_chkr mod_name (dsInstBinds [] const_inst_pairs)

	consts_env = mkIdEnv consts_pairs

	(core_clas_binds, shadows2)
			= initDs us1 consts_env sw_chkr mod_name (dsBinds clas_binds)
	core_clas_prs	= pairsFromCoreBinds core_clas_binds
			
	(core_inst_binds, shadows3)
			= initDs us2 consts_env sw_chkr mod_name (dsBinds inst_binds)
	core_inst_prs	= pairsFromCoreBinds core_inst_binds
			
	(core_val_binds, shadows4)
			= initDs us3 consts_env sw_chkr mod_name (dsBinds val_binds)
	core_val_pairs	= pairsFromCoreBinds core_val_binds

    	final_binds
	  = if (null core_clas_prs && null core_inst_prs && null core_const_prs) then
		-- we don't have to make the whole thing recursive
		core_clas_binds ++ core_val_binds

	    else -- gotta make it recursive (sigh)
	       [CoRec (core_clas_prs ++ core_inst_prs ++ core_const_prs ++ core_val_pairs)]

	lift_final_binds = {-if switchIsOn sw_chkr GlasgowExts
			   then-} liftCoreBindings us4 final_binds
			   -- else final_binds

	really_final_binds = if switchIsOn sw_chkr DoCoreLinting
			     then lintCoreBindings PprDebug "Desugarer" False lift_final_binds
			     else lift_final_binds

	shadows = shadows1 `unionBags` shadows2 `unionBags` shadows3 `unionBags` shadows4
    in
    (really_final_binds, shadows)
\end{code}
