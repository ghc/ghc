%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
%
\section[DsGRHSs]{Matching guarded right-hand-sides (GRHSs)}

\begin{code}
#include "HsVersions.h"

module DsGRHSs ( dsGuarded, dsGRHSs ) where


import AbsSyn		-- the stuff being desugared
import PlainCore	-- the output of desugaring;
			-- importing this module also gets all the
			-- CoreSyn utility functions
import DsMonad		-- the monadery used in the desugarer

import AbsPrel		( stringTy
			  IF_ATTACK_PRAGMAS(COMMA mkListTy COMMA charTy)
			)
import DsBinds		( dsBinds )
import DsExpr		( dsExpr )
import DsUtils
import Pretty
import Util
\end{code}

@dsGuarded@ is used for both @case@ expressions and pattern bindings.
It desugars:
\begin{verbatim}
	| g1 -> e1
	...
	| gn -> en
	where binds
\end{verbatim}
producing an expression with a runtime error in the corner if
necessary.  The type argument gives the type of the ei.

\begin{code}
dsGuarded :: TypecheckedGRHSsAndBinds
	  -> SrcLoc
	  -> DsM PlainCoreExpr

dsGuarded (GRHSsAndBindsOut grhss binds err_ty) err_loc
  = dsBinds binds				`thenDs` \ core_binds ->
    dsGRHSs err_ty PatBindMatch [] grhss 	`thenDs` \ (MatchResult can_it_fail _ core_grhss_fn _) ->
    case can_it_fail of
	CantFail -> returnDs (mkCoLetsAny core_binds (core_grhss_fn (panic "It can't fail")))
	CanFail  -> newSysLocalDs stringTy	`thenDs` \ str_var -> -- to hold the String
		    returnDs (mkCoLetsAny core_binds (core_grhss_fn (error_expr str_var)))
  where
    unencoded_part_of_msg = escErrorMsg (ppShow 80 (ppr PprForUser err_loc))

    error_expr :: Id -> PlainCoreExpr
    error_expr str_var = mkErrorCoApp err_ty str_var
			  (unencoded_part_of_msg
			  ++ "%N") --> ": non-exhaustive guards"
\end{code}

Desugar a list of (grhs, expr) pairs [grhs = guarded
right-hand-side], as in:
\begin{verbatim}
p | g1 = e1
  | g2 = e2
  ...
  | gm = em
\end{verbatim}
We supply a @PlainCoreExpr@ for the case in which all of
the guards fail.

\begin{code}
dsGRHSs :: UniType				-- Type of RHSs
	-> DsMatchKind -> [TypecheckedPat]	-- These are to build a MatchContext from
	-> [TypecheckedGRHS]			-- Guarded RHSs
	-> DsM MatchResult

dsGRHSs ty kind pats [grhs] = dsGRHS ty kind pats grhs

dsGRHSs ty kind pats (grhs:grhss)
  = dsGRHS ty kind pats grhs	`thenDs` \ match_result1 ->
    dsGRHSs ty kind pats grhss	`thenDs` \ match_result2 ->
    combineGRHSMatchResults match_result1 match_result2

dsGRHS ty kind pats (OtherwiseGRHS expr locn)
  = putSrcLocDs locn		 (
    dsExpr expr 	`thenDs` \ core_expr ->
    let
	expr_fn = \ ignore -> core_expr
    in
    returnDs (MatchResult CantFail ty expr_fn (DsMatchContext kind pats locn))
    )

dsGRHS ty kind pats (GRHS guard expr locn)
  = putSrcLocDs locn		 (
    dsExpr guard 	`thenDs` \ core_guard ->
    dsExpr expr  	`thenDs` \ core_expr  ->
    let
	expr_fn = \ fail -> mkCoreIfThenElse core_guard core_expr fail
    in
    returnDs (MatchResult CanFail ty expr_fn (DsMatchContext kind pats locn))
    )
\end{code}


