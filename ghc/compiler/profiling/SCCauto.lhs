%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[SCCauto]{Automated SCC annotations}

Automatic insertion of \tr{_scc_} annotations for top-level bindings.

Automatic insertion of \tr{_scc_} annotations on CAFs is better left
until STG land.  We do DICT annotations there, too, but maybe that
will turn out to be a bummer...  (WDP 94/06)

This is a Core-to-Core pass (usually run {\em last}).

\begin{code}
#include "HsVersions.h"

module SCCauto ( addAutoCostCentres ) where

import Ubiq{-uitous-}

import CmdLineOpts	( opt_AutoSccsOnAllToplevs,
			  opt_AutoSccsOnExportedToplevs,
			  opt_SccGroup
			)
import CoreSyn
import Id		( isTopLevId, GenId{-instances-} )
import Outputable	( isExported )
import CostCentre	( mkAutoCC, IsCafCC(..) )
\end{code}

\begin{code}
addAutoCostCentres
	:: FAST_STRING				-- module name
	-> [CoreBinding]			-- input
	-> [CoreBinding]			-- output

addAutoCostCentres mod_name binds
  = if not doing_something then
	binds -- now *that* was quick...
    else
	map scc_top_bind binds
  where
    doing_something = auto_all_switch_on || auto_exported_switch_on

    auto_all_switch_on	    = opt_AutoSccsOnAllToplevs -- only use!
    auto_exported_switch_on = opt_AutoSccsOnExportedToplevs -- only use!

    grp_name
      = case opt_SccGroup of
	  Just xx -> xx
	  Nothing -> mod_name	-- default: module name

    -----------------------------
    scc_top_bind (NonRec binder rhs)
      = NonRec binder (scc_auto binder rhs)

    scc_top_bind (Rec pairs)
      = Rec (map scc_pair pairs)
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
		(usevars, tyvars, vars, body) = digForLambdas rhs
	    in
	    case body of
	      SCC _ _ -> rhs -- leave it
	      Con _ _ -> rhs
	      _ -> mkUseLam usevars (mkLam tyvars vars
			(SCC (mkAutoCC binder mod_name grp_name IsNotCafCC) body))
\end{code}
