%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
%
\section[DsGRHSs]{Matching guarded right-hand-sides (GRHSs)}

\begin{code}
#include "HsVersions.h"

module DsGRHSs ( dsGuarded, dsGRHSs ) where

IMP_Ubiq()
IMPORT_DELOOPER(DsLoop)		-- break dsExpr/dsBinds-ish loop

import HsSyn		( GRHSsAndBinds(..), GRHS(..),
			  HsExpr, HsBinds )
import TcHsSyn		( SYN_IE(TypecheckedGRHSsAndBinds), SYN_IE(TypecheckedGRHS),
			  SYN_IE(TypecheckedPat), SYN_IE(TypecheckedHsBinds),
			  SYN_IE(TypecheckedHsExpr)	)
import CoreSyn		( SYN_IE(CoreBinding), SYN_IE(CoreExpr), mkCoLetsAny )

import DsMonad
import DsUtils

import CoreUtils	( mkCoreIfThenElse )
import PrelVals		( nON_EXHAUSTIVE_GUARDS_ERROR_ID )
import PprStyle		( PprStyle(..) )
import Pretty		( ppShow )
import SrcLoc		( SrcLoc{-instance-} )
import Util		( panic )
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
	  -> DsM CoreExpr

dsGuarded (GRHSsAndBindsOut grhss binds err_ty)
  = dsBinds False binds				`thenDs` \ core_binds ->
    dsGRHSs err_ty PatBindMatch [] grhss 	`thenDs` \ (MatchResult can_it_fail _ core_grhss_fn _) ->
    case can_it_fail of
	CantFail -> returnDs (mkCoLetsAny core_binds (core_grhss_fn (panic "It can't fail")))
	CanFail  -> mkErrorAppDs nON_EXHAUSTIVE_GUARDS_ERROR_ID err_ty "" `thenDs` \ error_expr ->
		    returnDs (mkCoLetsAny core_binds (core_grhss_fn error_expr))
\end{code}

Desugar a list of (grhs, expr) pairs [grhs = guarded
right-hand-side], as in:
\begin{verbatim}
p | g1 = e1
  | g2 = e2
  ...
  | gm = em
\end{verbatim}
We supply a @CoreExpr@ for the case in which all of
the guards fail.

\begin{code}
dsGRHSs :: Type				-- Type of RHSs
	-> DsMatchKind -> [TypecheckedPat]	-- These are to build a MatchContext from
	-> [TypecheckedGRHS]			-- Guarded RHSs
	-> DsM MatchResult

dsGRHSs ty kind pats [grhs] = dsGRHS ty kind pats grhs

dsGRHSs ty kind pats (grhs:grhss)
  = dsGRHS ty kind pats grhs	`thenDs` \ match_result1 ->
    dsGRHSs ty kind pats grhss	`thenDs` \ match_result2 ->
    combineGRHSMatchResults match_result1 match_result2

dsGRHS ty kind pats (OtherwiseGRHS expr locn)
  = putSrcLocDs locn $
    dsExpr expr 	`thenDs` \ core_expr ->
    let
	expr_fn = \ ignore -> core_expr
    in
    returnDs (MatchResult CantFail ty expr_fn (DsMatchContext kind pats locn))

dsGRHS ty kind pats (GRHS guard expr locn)
  = putSrcLocDs locn $
    dsExpr guard 	`thenDs` \ core_guard ->
    dsExpr expr  	`thenDs` \ core_expr  ->
    let
	expr_fn = \ fail -> mkCoreIfThenElse core_guard core_expr fail
    in
    returnDs (MatchResult CanFail ty expr_fn (DsMatchContext kind pats locn))
\end{code}


