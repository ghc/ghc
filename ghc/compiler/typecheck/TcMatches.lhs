%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcMatches]{Typecheck some @Matches@}

\begin{code}
module TcMatches ( tcMatchesFun, tcMatchesCase, tcMatchExpected ) where

#include "HsVersions.h"

import {-# SOURCE #-} TcGRHSs ( tcGRHSsAndBinds )

import HsSyn		( HsBinds(..), Match(..), GRHSsAndBinds(..),
			  MonoBinds(..), StmtCtxt(..),
			  pprMatch, getMatchLoc
			)
import RnHsSyn		( RenamedMatch )
import TcHsSyn		( TcMatch )

import TcMonad
import TcMonoType	( checkSigTyVars, noSigs, existentialPatCtxt )
import Inst		( Inst, LIE, plusLIE, emptyLIE )
import TcEnv		( tcExtendEnvWithPat, tcExtendGlobalTyVars )
import TcPat		( tcPat )
import TcType		( TcType, newTyVarTy )
import TcSimplify	( tcSimplifyAndCheck, bindInstsOfLocalFuns )
import TcUnify		( unifyFunTy )
import Name		( Name )

import BasicTypes	( RecFlag(..) )
import Type		( Kind, tyVarsOfType, isTauTy, mkFunTy, openTypeKind )
import VarSet
import Util
import Bag
import Outputable
import SrcLoc           (SrcLoc)
\end{code}

@tcMatchesFun@ typechecks a @[Match]@ list which occurs in a
@FunMonoBind@.  The second argument is the name of the function, which
is used in error messages.  It checks that all the equations have the
same number of arguments before using @tcMatches@ to do the work.

\begin{code}
tcMatchesFun :: Name
	     -> TcType s 		-- Expected type
	     -> [RenamedMatch]
	     -> TcM s ([TcMatch s], LIE s)

tcMatchesFun fun_name expected_ty matches@(first_match:_)
  =	 -- Set the location to that of the first equation, so that
	 -- any inter-equation error messages get some vaguely
	 -- sensible location.	Note: we have to do this odd
	 -- ann-grabbing, because we don't always have annotations in
	 -- hand when we call tcMatchesFun...

    tcAddSrcLoc (getMatchLoc first_match)	 (

	 -- Check that they all have the same no of arguments
    checkTc (all_same (noOfArgs matches))
	    (varyingArgsErr fun_name matches) `thenTc_`

	-- ToDo: Don't use "expected" stuff if there ain't a type signature
	-- because inconsistency between branches
	-- may show up as something wrong with the (non-existent) type signature

	-- No need to zonk expected_ty, because unifyFunTy does that on the fly
    tcMatchesExpected matches expected_ty (FunRhs fun_name)

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
tcMatchesCase :: TcType s 		-- Type of whole case expressions
	      -> [RenamedMatch]		-- The case alternatives
	      -> TcM s (TcType s,	-- Inferred type of the scrutinee
			[TcMatch s], 	-- Translated alternatives
			LIE s)

tcMatchesCase expr_ty matches
  = newTyVarTy openTypeKind 					`thenNF_Tc` \ scrut_ty ->
    tcMatchesExpected matches (mkFunTy scrut_ty expr_ty) CaseAlt `thenTc` \ (matches', lie) ->
    returnTc (scrut_ty, matches', lie)
\end{code}


\begin{code}
tcMatchesExpected :: [RenamedMatch]
		  -> TcType s
		  -> StmtCtxt
		  -> TcM s ([TcMatch s], LIE s)

tcMatchesExpected [match] expected_ty fun_or_case
  = tcAddSrcLoc (getMatchLoc match)		$
    tcAddErrCtxt (matchCtxt fun_or_case match)	$
    tcMatchExpected match expected_ty fun_or_case	`thenTc` \ (match',  lie) ->
    returnTc ([match'], lie)

tcMatchesExpected (match1 : matches) expected_ty fun_or_case
  = tcAddSrcLoc (getMatchLoc match1)	(
	tcAddErrCtxt (matchCtxt fun_or_case match1)	$
  	tcMatchExpected match1 expected_ty fun_or_case
    )						    	`thenTc` \ (match1',  lie1) ->
    tcMatchesExpected matches expected_ty fun_or_case	`thenTc` \ (matches', lie2) ->
    returnTc (match1' : matches', plusLIE lie1 lie2)
\end{code}

\begin{code}
tcMatchExpected
	:: RenamedMatch
	-> TcType s 		-- Expected result-type of the Match.
				-- Early unification with this guy gives better error messages
	-> StmtCtxt
	-> TcM s (TcMatch s,LIE s)

tcMatchExpected match expected_ty ctxt
  = tcMatchExpected_help emptyBag emptyBag emptyLIE match expected_ty ctxt


tcMatchExpected_help bound_tvs bound_ids bound_lie 
		     the_match@(PatMatch pat match) expected_ty ctxt
  = unifyFunTy expected_ty	`thenTc` \ (arg_ty, rest_ty) ->

    tcPat noSigs pat arg_ty	`thenTc` \ (pat', pat_lie, pat_tvs, pat_ids, avail_lie) ->

    tcMatchExpected_help
	(bound_tvs `unionBags` pat_tvs)
	(bound_ids `unionBags` pat_ids)
	(bound_lie `plusLIE`   avail_lie)
	match rest_ty ctxt			`thenTc` \ (match', lie_match) ->

    returnTc (PatMatch pat' match', pat_lie `plusLIE` lie_match)


tcMatchExpected_help bound_tvs bound_ids bound_lie
		     (GRHSMatch grhss_and_binds) expected_ty ctxt
  =     -- Check that the remaining "expected type" is not a rank-2 type
	-- If it is it'll mess up the unifier when checking the RHS
    checkTc (isTauTy expected_ty)
	    lurkingRank2SigErr 		`thenTc_`

    tcExtendEnvWithPat bound_ids (
        tcGRHSsAndBinds grhss_and_binds expected_ty ctxt
    )							`thenTc` \ (GRHSsAndBindsOut grhss binds ty, lie) ->


	-- Check for existentially bound type variables
    tcExtendGlobalTyVars (tyVarsOfType expected_ty) (
      tcAddErrCtxtM (existentialPatCtxt bound_tvs bound_ids)	$
      checkSigTyVars (bagToList bound_tvs)			`thenTc` \ zonked_pat_tvs ->
      tcSimplifyAndCheck 
	(text ("the existential context of a data constructor"))
	(mkVarSet zonked_pat_tvs)
	bound_lie lie
    )							`thenTc` \ (ex_lie, ex_binds) ->

    	-- In case there are any polymorpic, overloaded binders in the pattern
	-- (which can happen in the case of rank-2 type signatures, or data constructors
	-- with polymorphic arguments), we must do a bindInstsOfLocalFns here
    bindInstsOfLocalFuns ex_lie bound_id_list	 	`thenTc` \ (inst_lie, inst_binds) ->

    let
        binds' = ex_binds `glue_on` (inst_binds `glue_on` binds)
    in
    returnTc (GRHSMatch (GRHSsAndBindsOut grhss binds' ty), inst_lie)
  where
    bound_id_list = map snd (bagToList bound_ids)

	-- glue_on just avoids stupid dross
    glue_on EmptyMonoBinds binds = binds	-- The common case
    glue_on mbinds	   binds = MonoBind mbinds [] Recursive `ThenBinds` binds
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

Errors and contexts
~~~~~~~~~~~~~~~~~~~
\begin{code}
matchCtxt CaseAlt match
  = hang (ptext SLIT("In a \"case\" branch:"))
	 4 (pprMatch True{-is_case-} match)

matchCtxt (FunRhs fun) match
  = hang (hcat [ptext SLIT("In an equation for function "), quotes (ppr fun), char ':'])
	 4 (hcat [ppr fun, space, pprMatch False{-not case-} match])
\end{code}


\begin{code}
varyingArgsErr name matches
  = sep [ptext SLIT("Varying number of arguments for function"), quotes (ppr name)]

lurkingRank2SigErr
  = ptext SLIT("Too few explicit arguments when defining a function with a rank-2 type")
\end{code}
