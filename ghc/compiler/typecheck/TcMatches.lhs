%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
%
\section[TcMatches]{Typecheck some @Matches@}

\begin{code}
#include "HsVersions.h"

module TcMatches ( tcMatchesFun, tcMatchesCase, tcMatchExpected ) where

IMP_Ubiq()

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 201
IMPORT_DELOOPER(TcLoop)		( tcGRHSsAndBinds )
#else
import {-# SOURCE #-} TcGRHSs ( tcGRHSsAndBinds )
#endif

import HsSyn		( Match(..), GRHSsAndBinds(..), GRHS(..), InPat, 
			  HsExpr(..), HsBinds(..), MonoBinds(..), OutPat, Fake, Stmt,
			  Sig, HsLit, DoOrListComp, Fixity, HsType, ArithSeqInfo, 
			  collectPatBinders, pprMatch )
import RnHsSyn		( SYN_IE(RenamedMatch) )
import TcHsSyn		( SYN_IE(TcMatch) )

import TcMonad
import Inst		( Inst, SYN_IE(LIE), plusLIE )
import TcEnv		( newMonoIds )
import TcPat		( tcPat )
import TcType		( TcIdOcc(..), SYN_IE(TcType), TcMaybe, zonkTcType )
import TcSimplify	( bindInstsOfLocalFuns )
import Unify		( unifyTauTy, unifyTauTyList, unifyFunTy )
import Name		( Name {- instance Outputable -} )

import Kind		( Kind, mkTypeKind )
import Pretty
import Type		( isTyVarTy, isTauTy, mkFunTy, getFunTy_maybe )
import Util
import Outputable
#if __GLASGOW_HASKELL__ >= 202
import SrcLoc           (SrcLoc)
#endif

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

    tcAddSrcLoc (get_Match_loc first_match)	 (

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
tcMatchesCase :: TcType s -> [RenamedMatch] -> TcM s ([TcMatch s], LIE s)
tcMatchesCase expected_ty matches = tcMatchesExpected expected_ty MCase matches
\end{code}


\begin{code}
data FunOrCase = MCase | MFun Name	-- Records whether doing  fun or case rhss;
					-- used to produced better error messages

tcMatchesExpected :: TcType s
		  -> FunOrCase
		  -> [RenamedMatch]
		  -> TcM s ([TcMatch s], LIE s)

tcMatchesExpected expected_ty fun_or_case [match]
  = tcAddSrcLoc (get_Match_loc match)		$
    tcAddErrCtxt (matchCtxt fun_or_case match)	$
    tcMatchExpected expected_ty match	`thenTc` \ (match',  lie) ->
    returnTc ([match'], lie)

tcMatchesExpected expected_ty fun_or_case (match1 : matches)
  = tcAddSrcLoc (get_Match_loc match1)	(
	tcAddErrCtxt (matchCtxt fun_or_case match1)	$
  	tcMatchExpected expected_ty  match1
    )						    	`thenTc` \ (match1',  lie1) ->
    tcMatchesExpected expected_ty fun_or_case matches	`thenTc` \ (matches', lie2) ->
    returnTc (match1' : matches', plusLIE lie1 lie2)
\end{code}

\begin{code}
tcMatchExpected
	:: TcType s 		-- This gives the expected
				-- result-type of the Match.  Early unification
				-- with this guy gives better error messages
	-> RenamedMatch
	-> TcM s (TcMatch s,LIE s)	-- NB No type returned, because it was passed
					-- in instead!

tcMatchExpected expected_ty the_match@(PatMatch pat match)
  = unifyFunTy expected_ty		`thenTc` \ (arg_ty, rest_ty) ->

    let binders = collectPatBinders pat
    in
    newMonoIds binders mkTypeKind (\ mono_ids ->
	tcPat pat			`thenTc` \ (pat', lie_pat, pat_ty) ->
	unifyTauTy pat_ty arg_ty	`thenTc_`
	tcMatchExpected rest_ty  match	`thenTc` \ (match', lie_match) ->
		-- In case there are any polymorpic, overloaded binders in the pattern
		-- (which can happen in the case of rank-2 type signatures, or data constructors
		-- with polymorphic arguments), we must do a bindInstsOfLocalFns here
		--
		-- 99% of the time there are no bindings.  In the unusual case we
		-- march down the match to dump them in the right place (boring but easy).
        bindInstsOfLocalFuns lie_match mono_ids 	`thenTc` \ (lie_match', inst_mbinds) ->
	let
	   inst_binds = MonoBind inst_mbinds [] False
	   match'' = case inst_mbinds of
			EmptyMonoBinds -> match'
			other          -> glue_on match'
	   glue_on (PatMatch p m) = PatMatch p (glue_on m)
	   glue_on (GRHSMatch (GRHSsAndBindsOut grhss binds ty))
		= (GRHSMatch (GRHSsAndBindsOut grhss 
					       (inst_binds `ThenBinds` binds)
					       ty))
	   glue_on (SimpleMatch expr) = SimpleMatch (HsLet inst_binds expr)
	in		
	returnTc (PatMatch pat' match'',
		  plusLIE lie_pat lie_match')
    )

tcMatchExpected expected_ty (GRHSMatch grhss_and_binds)
  = tcGRHSsAndBinds expected_ty grhss_and_binds   	`thenTc` \ (grhss_and_binds', lie) ->
    checkTc (isTauTy expected_ty)
	    lurkingRank2SigErr 		`thenTc_`
    returnTc (GRHSMatch grhss_and_binds', lie)
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

Errors and contexts
~~~~~~~~~~~~~~~~~~~
\begin{code}
matchCtxt MCase match sty
  = hang (ptext SLIT("In a \"case\" branch:"))
	 4 (pprMatch sty True{-is_case-} match)

matchCtxt (MFun fun) match sty
  = hang (hcat [ptext SLIT("In an equation for function "), ppr sty fun, char ':'])
	 4 (pprQuote sty $ \sty -> hcat [ppr sty fun, space, pprMatch sty False{-not case-} match])
\end{code}


\begin{code}
varyingArgsErr name matches sty
  = sep [ptext SLIT("Varying number of arguments for function"), ppr sty name]

lurkingRank2SigErr sty
  = ptext SLIT("Too few explicit arguments when defining a function with a rank-2 type")
\end{code}
