%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[MatchLit]{Pattern-matching literal and n+k patterns}

\begin{code}
#include "HsVersions.h"

module MatchLit (
	matchLiterals
    ) where

import AbsSyn		-- the stuff being desugared
import PlainCore	-- the output of desugaring;
			-- importing this module also gets all the
			-- CoreSyn utility functions
import DsMonad		-- the monadery used in the desugarer

import AbsUniType	( isPrimType, getUniDataTyCon, kindFromType )
import BasicLit		( mkMachInt, BasicLit(..), PrimKind )
import DsExpr		( dsExpr )
import DsUtils
import Maybes		( Maybe(..), catMaybes )
import Match		( match )
import Id		( getIdUniType, eqId )
import Util
\end{code}

\begin{code}
matchLiterals :: [Id]
	      -> [EquationInfo]
	      -> [EquationInfo]		-- Shadows
	      -> DsM MatchResult
\end{code}

This first one is a {\em special case} where the literal patterns are
unboxed numbers (NB: the fiddling introduced by @tidyEqnInfo@).  We
want to avoid using the ``equality'' stuff provided by the
typechecker, and do a real ``case'' instead.  In that sense, the code
is much like @matchConFamily@, which uses @match_cons_used@ to create
the alts---here we use @match_prims_used@.

\begin{code}
matchLiterals all_vars@(var:vars) eqns_info@(EqnInfo (LitPat literal lit_ty : ps1) _ : eqns) shadows
  = -- GENERATE THE ALTS
    match_prims_used vars eqns_info shadows `thenDs` \ prim_alts ->

    -- MAKE THE PRIMITIVE CASE
    mkCoPrimCaseMatchResult var prim_alts
  where
    match_prims_used _ [{-no more eqns-}] _ = returnDs []

    match_prims_used vars eqns_info@(EqnInfo ((LitPat literal _):ps1) _ : eqns) shadows
      = let
	    (shifted_eqns_for_this_lit, eqns_not_for_this_lit)
	      = partitionEqnsByLit Nothing literal eqns_info
	    (shifted_shadows_for_this_lit, shadows_not_for_this_lit)
	      = partitionEqnsByLit Nothing literal shadows
	in
	-- recursive call to make other alts...
	match_prims_used vars eqns_not_for_this_lit shadows_not_for_this_lit	`thenDs` \ rest_of_alts ->

	-- (prim pats have no args; no selectMatchVars as in match_cons_used)
	-- now do the business to make the alt for _this_ LitPat ...
	match vars shifted_eqns_for_this_lit shifted_shadows_for_this_lit	`thenDs` \ match_result ->
	returnDs (
	    (mk_core_lit literal, match_result)
	    : rest_of_alts
	)
      where
	mk_core_lit :: Literal -> BasicLit

	mk_core_lit (IntPrimLit     i) = mkMachInt  i
	mk_core_lit (CharPrimLit    c) = MachChar   c
	mk_core_lit (StringPrimLit  s) = MachStr    s
	mk_core_lit (FloatPrimLit   f) = MachFloat  f
	mk_core_lit (DoublePrimLit  d) = MachDouble d
	mk_core_lit (LitLitLit    s t) = ASSERT(isPrimType t)
					 MachLitLit s (kindFromType t)
    	mk_core_lit other	       = panic "matchLiterals:mk_core_lit:unhandled"
\end{code}

\begin{code}
matchLiterals all_vars@(var:vars) eqns_info@(EqnInfo ((NPat literal lit_ty eq_chk):ps1) _ : eqns) shadows
  = let
	(shifted_eqns_for_this_lit, eqns_not_for_this_lit)
	  = partitionEqnsByLit Nothing literal eqns_info
	(shifted_shadows_for_this_lit, shadows_not_for_this_lit)
	      = partitionEqnsByLit Nothing literal shadows
    in
    dsExpr (App eq_chk (Var var))					`thenDs` \ pred_expr ->
    match vars shifted_eqns_for_this_lit shifted_shadows_for_this_lit	`thenDs` \ inner_match_result ->
    mkGuardedMatchResult pred_expr inner_match_result			`thenDs` \ match_result1 ->

    if (null eqns_not_for_this_lit)
    then 
	returnDs match_result1
    else 
	matchLiterals all_vars eqns_not_for_this_lit shadows_not_for_this_lit 	`thenDs` \ match_result2 ->
	combineMatchResults match_result1 match_result2
\end{code}

For an n+k pattern, we use the various magic expressions we've been given.
We generate:
\begin{verbatim}
    if ge var lit then
	let n = sub var lit
	in  <expr-for-a-successful-match>
    else
	<try-next-pattern-or-whatever>
\end{verbatim}

\begin{code}
matchLiterals all_vars@(var:vars) eqns_info@(EqnInfo ((NPlusKPat master_n k ty from_lit ge sub):ps1) _ : eqns) shadows
  = let
	(shifted_eqns_for_this_lit, eqns_not_for_this_lit)
	  = partitionEqnsByLit (Just master_n) k eqns_info
	(shifted_shadows_for_this_lit, shadows_not_for_this_lit)
	  = partitionEqnsByLit (Just master_n) k shadows
    in
    match vars shifted_eqns_for_this_lit shifted_shadows_for_this_lit	`thenDs` \ inner_match_result ->

    dsExpr from_lit			`thenDs` \ core_lit ->
    dsExpr (App ge (Var var))		`thenDs` \ var_ge ->
    dsExpr (App sub (Var var))		`thenDs` \ var_sub ->
    mkCoAppDs var_ge  core_lit		`thenDs` \ var_ge_lit ->
    mkCoAppDs var_sub core_lit		`thenDs` \ var_sub_lit ->

    mkGuardedMatchResult
	var_ge_lit
	(mkCoLetsMatchResult [CoNonRec master_n var_sub_lit] inner_match_result)
					`thenDs` \ match_result1 ->

    if (null eqns_not_for_this_lit)
    then 
	returnDs match_result1
    else 
	matchLiterals all_vars eqns_not_for_this_lit shadows_not_for_this_lit 	`thenDs` \ match_result2 ->
	combineMatchResults match_result1 match_result2
\end{code}

Given a blob of LitPats/NPats/NPlusKPats, we want to split them into those
that are ``same''/different as one we are looking at.  We need to know
whether we're looking at a LitPat/NPat or NPlusKPat (initial Bool arg is
@True@ for the latter), and what literal we're after.

\begin{code}
partitionEqnsByLit :: Maybe Id	-- (Just v) for N-plus-K patterns, where v
				-- is the "master" variable;
				-- Nothing for NPats and LitPats
		   -> Literal
		   -> [EquationInfo]
		   -> ([EquationInfo], 	-- These ones are for this lit, AND
					-- they've been "shifted" by stripping
					-- off the first pattern
		       [EquationInfo]	-- These are not for this lit; they
					-- are exactly as fed in.
		      )

partitionEqnsByLit want_NPlusK lit eqns
  = ( \ (xs,ys) -> (catMaybes xs, catMaybes ys))
	(unzip (map (partition_eqn want_NPlusK lit) eqns))
  where
    partition_eqn :: Maybe Id -> Literal -> EquationInfo ->
		(Maybe EquationInfo, Maybe EquationInfo)

    partition_eqn Nothing lit (EqnInfo (LitPat k _ : remaining_pats) match_result)
      | lit `eq_lit` k  = (Just (EqnInfo remaining_pats match_result), Nothing)
			  -- NB the pattern is stripped off thhe EquationInfo

    partition_eqn Nothing lit (EqnInfo (NPat k _ _ : remaining_pats) match_result)
      | lit `eq_lit` k  = (Just (EqnInfo remaining_pats match_result), Nothing)
			  -- NB the pattern is stripped off thhe EquationInfo

    partition_eqn (Just master_n) lit  (EqnInfo (NPlusKPat n k _ _ _ _ : remaining_pats) match_result)
      | lit `eq_lit` k  = (Just (EqnInfo remaining_pats new_match_result), Nothing)
			  -- NB the pattern is stripped off thhe EquationInfo
      where
	new_match_result = if master_n `eqId` n then 
				match_result
			   else 
				mkCoLetsMatchResult [CoNonRec n (CoVar master_n)] match_result

	-- Wild-card patterns, which will only show up in the shadows, go into both groups
    partition_eqn wantNPlusK lit eqn@(EqnInfo (WildPat _ : remaining_pats) match_result) 
			= (Just (EqnInfo remaining_pats match_result), Just eqn)

	-- Default case; not for this pattern
    partition_eqn wantNPlusK lit eqn = (Nothing, Just eqn)

-- ToDo: meditate about this equality business...

eq_lit (IntLit  i1)   	  (IntLit  i2)       = i1 == i2
eq_lit (FracLit f1) 	  (FracLit f2)       = f1 == f2
			  		     
eq_lit (IntPrimLit i1)	  (IntPrimLit i2)    = i1 == i2
eq_lit (FloatPrimLit f1)  (FloatPrimLit f2)  = f1 == f2
eq_lit (DoublePrimLit d1) (DoublePrimLit d2) = d1 == d2
eq_lit (CharLit c1) 	  (CharLit c2)       = c1 == c2
eq_lit (CharPrimLit c1)	  (CharPrimLit c2)   = c1 == c2
eq_lit (StringLit s1)	  (StringLit s2)     = s1 == s2
eq_lit (StringPrimLit s1) (StringPrimLit s2) = s1 == s2
eq_lit (LitLitLit s1 _)	  (LitLitLit s2 _)   = s1 == s2 -- ToDo: ??? (dubious)
eq_lit other1	      	  other2	     = panic "matchLiterals:eq_lit"
\end{code}
