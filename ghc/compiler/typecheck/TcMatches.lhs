%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
%
\section[TcMatches]{Typecheck some @Matches@}

\begin{code}
#include "HsVersions.h"

module TcMatches ( tcMatchesFun, tcMatchesCase, tcMatch ) where

import TcMonad		-- typechecking monad machinery
import TcMonadFns	( mkIdsWithOpenTyVarTys )
import AbsSyn		-- the stuff being typechecked

import AbsPrel		( mkFunTy )
import AbsUniType	( isTyVarTy, maybeUnpackFunTy )
import E		( E, growE_LVE, LVE(..), GVE(..) )
#if USE_ATTACK_PRAGMAS
import CE
import TCE
#endif
import Errors		( varyingArgsErr, Error(..), UnifyErrContext(..) )
import LIE		( LIE, plusLIE )
import Maybes		( Maybe(..) )
import TcGRHSs		( tcGRHSsAndBinds )
import TcPat		( tcPat )
import Unify		( unifyTauTy, unifyTauTyList )
import Util
\end{code}

@tcMatchesFun@ typechecks a @[Match]@ list which occurs in a
@FunMonoBind@.  The second argument is the name of the function, which
is used in error messages.  It checks that all the equations have the
same number of arguments before using @tcMatches@ to do the work.

\begin{code}
tcMatchesFun :: E -> Name 
	     -> UniType 		-- Expected type
	     -> [RenamedMatch]
	     -> TcM ([TypecheckedMatch], LIE)

tcMatchesFun e fun_name expected_ty matches@(first_match:_)
  =	 -- Set the location to that of the first equation, so that
	 -- any inter-equation error messages get some vaguely
	 -- sensible location.	Note: we have to do this odd
	 -- ann-grabbing, because we don't always have annotations in
	 -- hand when we call tcMatchesFun...

    addSrcLocTc (get_Match_loc first_match)	 (

	 -- Check that they all have the same no of arguments
    checkTc (not (all_same (noOfArgs matches)))
	    (varyingArgsErr fun_name matches) `thenTc_`

	-- ToDo: Don't use "expected" stuff if there ain't a type signature
	-- because inconsistency between branches
	-- may show up as something wrong with the (non-existent) type signature

	-- We need to substitute so that we can see as much about the type as possible
    applyTcSubstToTy expected_ty	`thenNF_Tc` \ expected_ty' ->
    tcMatchesExpected e expected_ty' (\ m -> FunMonoBindsCtxt fun_name [m]) matches

    )
  where
    all_same :: [Int] -> Bool
    all_same []	    = True	-- Should never happen (ToDo: panic?)
    all_same [x]    = True
    all_same (x:xs) = all ((==) x) xs
\end{code}

@tcMatchesCase@ doesn't do the argument-count check because the
parser guarantees that each equation has exactly one argument.

\begin{code}
tcMatchesCase :: E -> [RenamedMatch]
	      -> TcM ([TypecheckedMatch], LIE, UniType)

tcMatchesCase e matches
  =

	 -- Typecheck them
    tcMatches e matches			`thenTc` \ (matches', lie, tys@(first_ty:_)) ->

	-- Set the location to that of the first equation, so that
	-- any inter-equation error messages get some vaguely sensible location
    addSrcLocTc (get_Match_loc (head matches)) (
	    unifyTauTyList tys (CaseBranchesCtxt matches)
    )					 `thenTc_`

    returnTc (matches', lie, first_ty)
\end{code}


\begin{code}
tcMatchesExpected :: E 
		  -> UniType 
		  -> (RenamedMatch -> UnifyErrContext)
		  -> [RenamedMatch] 
		  -> TcM ([TypecheckedMatch], LIE)

tcMatchesExpected e expected_ty err_ctxt_fn [match]
  = addSrcLocTc (get_Match_loc match) (
  	tcMatchExpected e expected_ty (err_ctxt_fn match) match
    )						`thenTc` \ (match',  lie) ->
    returnTc ([match'], lie)

tcMatchesExpected e expected_ty err_ctxt_fn ms@(match1 : matches)
  = addSrcLocTc (get_Match_loc match1) (
  	tcMatchExpected e expected_ty (err_ctxt_fn match1) match1
    )						    	`thenTc` \ (match1',  lie1) ->
    tcMatchesExpected e expected_ty err_ctxt_fn matches	`thenTc` \ (matches', lie2) ->
    returnTc (match1' : matches', plusLIE lie1 lie2)

tcMatches :: E -> [RenamedMatch] -> TcM ([TypecheckedMatch], LIE, [UniType])

tcMatches e [match]
  = tcMatch e match		`thenTc` \ (match', lie, ty) ->
    returnTc ([match'], lie, [ty])

tcMatches e ms@(match1 : matches)
  = addSrcLocTc (get_Match_loc match1) (
	tcMatch e match1
    )				`thenTc` \ (match1',  lie1, match1_ty) ->
    tcMatches e matches		`thenTc` \ (matches', lie2, matches_ty) ->
    returnTc (match1' : matches', plusLIE lie1 lie2, match1_ty : matches_ty)
\end{code}

\begin{code}
tcMatchExpected 
	:: E 
	-> UniType 		-- This gives the expected
				-- result-type of the Match.  Early unification
				-- with this guy gives better error messages
	-> UnifyErrContext 
	-> RenamedMatch 	
	-> TcM (TypecheckedMatch,LIE)
				-- NB No type returned, because it was passed
				-- in instead!

tcMatchExpected e expected_ty err_ctxt the_match@(PatMatch pat match)
  = case maybeUnpackFunTy expected_ty of

	Nothing ->			-- Not a function type (eg type variable)
					-- So use tcMatch instead
	    tcMatch e the_match				`thenTc`   \ (match', lie_match, match_ty) ->
	    unifyTauTy match_ty expected_ty err_ctxt	`thenTc_`
	    returnTc (match', lie_match)

	Just (arg_ty,rest_ty) ->	-- It's a function type!
	    let binders = collectPatBinders pat
	    in
	    mkIdsWithOpenTyVarTys binders    `thenNF_Tc` \ lve ->
	    let e' = growE_LVE e lve
	    in
	    tcPat e' pat		`thenTc`   \ (pat',   lie_pat,   pat_ty) ->

    	    unifyTauTy arg_ty pat_ty err_ctxt	      `thenTc_`
	    tcMatchExpected e' rest_ty err_ctxt match `thenTc` \ (match', lie_match) ->
	    returnTc (PatMatch pat' match',
			  plusLIE lie_pat lie_match)

tcMatchExpected e expected_ty err_ctxt (GRHSMatch grhss_and_binds)
  = tcGRHSsAndBinds e grhss_and_binds   	`thenTc` \ (grhss_and_binds', lie, grhss_ty) ->
    unifyTauTy grhss_ty expected_ty err_ctxt	`thenTc_`
    returnTc (GRHSMatch grhss_and_binds', lie)

tcMatch	:: E 
	-> RenamedMatch 	
	-> TcM (TypecheckedMatch,LIE,UniType)

tcMatch e (PatMatch pat match)
  = let binders = collectPatBinders pat
    in
    mkIdsWithOpenTyVarTys binders    `thenNF_Tc` \ lve ->
    let e' = growE_LVE e lve
    in
    tcPat e' pat		`thenTc`   \ (pat',   lie_pat,   pat_ty) ->
    tcMatch e' match		`thenTc`   \ (match', lie_match, match_ty) ->

--    We don't do this any more, do we?
--    applyTcSubstToTy pat_ty	`thenNF_Tc`\ pat_ty' ->

    returnTc (PatMatch pat' match',
	      plusLIE lie_pat lie_match,
	      mkFunTy pat_ty match_ty)

tcMatch e (GRHSMatch grhss_and_binds)
  = tcGRHSsAndBinds e grhss_and_binds   `thenTc` \ (grhss_and_binds', lie, grhss_ty) ->
    returnTc (GRHSMatch grhss_and_binds', lie, grhss_ty)
\end{code}


@noOfArgs@ takes a @[RenamedMatch]@ and returns a list telling how
many arguments were used in each of the equations.  This is used to
report a sensible error message when different equations have
different numbers of arguments.

\begin{code}
noOfArgs :: [RenamedMatch] -> [Int]

noOfArgs ms = map args_in_match ms
  where
    args_in_match :: RenamedMatch -> Int
    args_in_match (GRHSMatch _) = 0
    args_in_match (PatMatch _ match) = 1 + args_in_match match
\end{code}

@get_Match_loc@ takes a @RenamedMatch@ and returns the
source-location gotten from the GRHS inside.
THis is something of a nuisance, but no more.

\begin{code}
get_Match_loc     :: RenamedMatch   -> SrcLoc

get_Match_loc (PatMatch _ m)    = get_Match_loc m
get_Match_loc (GRHSMatch (GRHSsAndBindsIn (g:_) _))
      = get_GRHS_loc g
      where
	get_GRHS_loc (OtherwiseGRHS _ locn) = locn
	get_GRHS_loc (GRHS _ _ locn)	    = locn
\end{code}
