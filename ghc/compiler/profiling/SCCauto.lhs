%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
%
\section[SCCauto]{Automated SCC annotations}

Automatic insertion of \tr{_scc_} annotations for top-level bindings.

Automatic insertion of \tr{_scc_} annotations on CAFs is better left
until STG land.  We do DICT annotations there, too, but maybe
that will turn out to be a bummer...  (WDP 94/06)

This is a Core-to-Core pass (usually run {\em last}).

\begin{code}
#include "HsVersions.h"

module SCCauto ( addAutoCostCentres ) where

import CmdLineOpts
import Id		( isTopLevId )
import PlainCore
import Outputable	( isExported )
import CostCentre	-- ( mkAutoCC )
import Util		-- for pragmas only
\end{code}

\begin{code}
addAutoCostCentres
	:: (GlobalSwitch -> SwitchResult)	-- cmd-line switches
	-> FAST_STRING				-- module name
	-> [PlainCoreBinding]			-- input
	-> [PlainCoreBinding]			-- output

addAutoCostCentres sw_chkr mod_name binds
  = if not doing_something then
	binds -- now *that* was quick...
    else
	map scc_top_bind binds
  where
    doing_something = auto_all_switch_on || auto_exported_switch_on

    auto_all_switch_on	    = switchIsOn sw_chkr AutoSccsOnAllToplevs -- only use!
    auto_exported_switch_on = switchIsOn sw_chkr AutoSccsOnExportedToplevs -- only use!

    grp_name  = case (stringSwitchSet sw_chkr SccGroup) of
		  Just xx -> _PK_ xx
		  Nothing -> mod_name	-- default: module name

    -----------------------------
    scc_top_bind (CoNonRec binder rhs)
      = CoNonRec binder (scc_auto binder rhs)

    scc_top_bind (CoRec pairs)
      = CoRec (map scc_pair pairs)
      where
	scc_pair (binder, rhs) = (binder, scc_auto binder rhs)

    -----------------------------
    -- Automatic scc annotation for user-defined top-level Ids

    scc_auto binder rhs
      = if isTopLevId binder
	&& (auto_all_switch_on || isExported binder)
        then scc_rhs rhs
	else rhs
      where
	-- park auto SCC inside lambdas; don't put one there
	-- if there already is one.

	scc_rhs rhs
	  = let
		(tyvars, vars, body) = digForLambdas rhs
	    in
	    case body of
	      CoSCC _ _ -> rhs -- leave it
	      CoCon _ _ _ --??? | null vars
		-> rhs
	      _ -> mkFunction tyvars vars
			(CoSCC (mkAutoCC binder mod_name grp_name IsNotCafCC) body)
\end{code}
