%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
%
\section[TcMatches]{Typecheck some @Matches@}

\begin{code}
module TcMatches ( tcMatchesFun, tcMatchesCase, tcMatchExpected ) where

#include "HsVersions.h"

import {-# SOURCE #-} TcGRHSs ( tcGRHSsAndBinds )

import HsSyn		( HsBinds(..), Match(..), GRHSsAndBinds(..), GRHS(..),
			  HsExpr, MonoBinds(..),
			  collectPatBinders, pprMatch, getMatchLoc
			)
import RnHsSyn		( RenamedMatch )
import TcHsSyn		( TcIdBndr, TcMatch )

import TcMonad
import Inst		( Inst, LIE, plusLIE )
import TcEnv		( TcIdOcc(..), newMonoIds )
import TcPat		( tcPat )
import TcType		( TcType, TcMaybe, zonkTcType, newTyVarTy )
import TcSimplify	( bindInstsOfLocalFuns )
import Unify		( unifyTauTy, unifyFunTy )
import Name		( Name {- instance Outputable -} )

import Kind		( Kind, mkTypeKind )
import BasicTypes	( RecFlag(..) )
import Type		( isTauTy, mkFunTy )
import Util
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

	-- We need to substitute so that we can see as much about the type as possible
    zonkTcType expected_ty		`thenNF_Tc` \ expected_ty' ->
    tcMatchesExpected expected_ty' (MFun fun_name) matches

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
  = newTyVarTy mkTypeKind 					`thenNF_Tc` \ scrut_ty ->
    tcMatchesExpected (mkFunTy scrut_ty expr_ty) MCase matches	`thenTc` \ (matches', lie) ->
    returnTc (scrut_ty, matches', lie)
\end{code}


\begin{code}
data FunOrCase = MCase | MFun Name	-- Records whether doing  fun or case rhss;
					-- used to produced better error messages

tcMatchesExpected :: TcType s
		  -> FunOrCase
		  -> [RenamedMatch]
		  -> TcM s ([TcMatch s], LIE s)

tcMatchesExpected expected_ty fun_or_case [match]
  = tcAddSrcLoc (getMatchLoc match)		$
    tcAddErrCtxt (matchCtxt fun_or_case match)	$
    tcMatchExpected [] expected_ty match	`thenTc` \ (match',  lie) ->
    returnTc ([match'], lie)

tcMatchesExpected expected_ty fun_or_case (match1 : matches)
  = tcAddSrcLoc (getMatchLoc match1)	(
	tcAddErrCtxt (matchCtxt fun_or_case match1)	$
  	tcMatchExpected [] expected_ty  match1
    )						    	`thenTc` \ (match1',  lie1) ->
    tcMatchesExpected expected_ty fun_or_case matches	`thenTc` \ (matches', lie2) ->
    returnTc (match1' : matches', plusLIE lie1 lie2)
\end{code}

\begin{code}
tcMatchExpected
	:: [TcIdBndr s]		-- Ids bound by enclosing matches
	-> TcType s 		-- This gives the expected
				-- result-type of the Match.  Early unification
				-- with this guy gives better error messages
	-> RenamedMatch
	-> TcM s (TcMatch s,LIE s)	-- NB No type returned, because it was passed
					-- in instead!

tcMatchExpected matched_ids expected_ty the_match@(PatMatch pat match)
  = unifyFunTy expected_ty		`thenTc` \ (arg_ty, rest_ty) ->

    let binders = collectPatBinders pat
    in
    newMonoIds binders mkTypeKind (\ mono_ids ->
	tcPat pat			`thenTc` \ (pat', lie_pat, pat_ty) ->
	unifyTauTy pat_ty arg_ty	`thenTc_`

	tcMatchExpected (mono_ids ++ matched_ids)
			rest_ty match	`thenTc` \ (match', lie_match) ->

	returnTc (PatMatch pat' match',
		  plusLIE lie_pat lie_match)
    )

tcMatchExpected matched_ids expected_ty (GRHSMatch grhss_and_binds)
  =     -- Check that the remaining "expected type" is not a rank-2 type
	-- If it is it'll mess up the unifier when checking the RHS
    checkTc (isTauTy expected_ty)
	    lurkingRank2SigErr 		`thenTc_`

    tcGRHSsAndBinds expected_ty grhss_and_binds   	`thenTc` \ (GRHSsAndBindsOut grhss binds ty, lie) ->

    	-- In case there are any polymorpic, overloaded binders in the pattern
	-- (which can happen in the case of rank-2 type signatures, or data constructors
	-- with polymorphic arguments), we must do a bindInstsOfLocalFns here
    bindInstsOfLocalFuns lie matched_ids 	`thenTc` \ (lie', inst_mbinds) ->
    let
        binds' = case inst_mbinds of
		   EmptyMonoBinds -> binds	-- The common case
		   other	  -> MonoBind inst_mbinds [] Recursive `ThenBinds` binds
    in
    returnTc (GRHSMatch (GRHSsAndBindsOut grhss binds' ty), lie')
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
matchCtxt MCase match
  = hang (ptext SLIT("In a \"case\" branch:"))
	 4 (pprMatch True{-is_case-} match)

matchCtxt (MFun fun) match
  = hang (hcat [ptext SLIT("In an equation for function "), quotes (ppr fun), char ':'])
	 4 (hcat [ppr fun, space, pprMatch False{-not case-} match])
\end{code}


\begin{code}
varyingArgsErr name matches
  = sep [ptext SLIT("Varying number of arguments for function"), quotes (ppr name)]

lurkingRank2SigErr
  = ptext SLIT("Too few explicit arguments when defining a function with a rank-2 type")
\end{code}
