%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[MatchCon]{Pattern-matching constructors}

\begin{code}
#include "HsVersions.h"

module MatchCon (
    matchConFamily
) where

import AbsSyn		-- the stuff being desugared
import PlainCore	-- the output of desugaring;
			-- importing this module also gets all the
			-- CoreSyn utility functions
import DsMonad		-- the monadery used in the desugarer

import AbsUniType	( mkTyVarTy, splitType, TyVar, TyVarTemplate,
			  getTyConDataCons,
			  instantiateTauTy, TyCon, Class, UniType,
			  TauType(..), InstTyEnv(..)
			  IF_ATTACK_PRAGMAS(COMMA instantiateTy)
			)
import DsUtils
import Id		( eqId, getInstantiatedDataConSig,
			  getIdUniType, isDataCon, DataCon(..)
			)
import Maybes		( Maybe(..) )
import Match		( match )
import Util
\end{code}

\subsection[matchConFamily]{Making alternatives for a constructor family}

We are confronted with the first column of patterns in a set of
equations, all beginning with constructors from one ``family'' (e.g.,
@[]@ and @:@ make up the @List@ ``family'').  We want to generate the
alternatives for a @CoCase@ expression.  There are several choices:
\begin{enumerate}
\item
Generate an alternative for every constructor in the family, whether
they are used in this set of equations or not; this is what the Wadler
chapter does.
\begin{description}
\item[Advantages:]
(a)~Simple.  (b)~It may also be that large sparsely-used constructor families are mainly
handled by the code for literals.
\item[Disadvantages:]
(a)~Not practical for large sparsely-used constructor families, e.g., the
ASCII character set.  (b)~Have to look up (in the TDE environment) a
list of what constructors make up the whole family.  So far, this is
the only part of desugaring that needs information from the environments.
\end{description}

\item
Generate an alternative for each constructor used, then add a default
alternative in case some constructors in the family weren't used.
\begin{description}
\item[Advantages:]
(a)~Alternatives aren't generated for unused constructors.  (b)~The
STG is quite happy with defaults.  (c)~No lookup in an environment needed.
\item[Disadvantages:]
(a)~A spurious default alternative may be generated.
\end{description}

\item
``Do it right:'' generate an alternative for each constructor used,
and add a default alternative if all constructors in the family
weren't used.
\begin{description}
\item[Advantages:]
(a)~You will get cases with only one alternative (and no default),
which should be amenable to optimisation.  Tuples are a common example.
\item[Disadvantages:]
(b)~Have to look up constructor families in TDE (as above).
\end{description}
\end{enumerate}

We are implementing the ``do-it-right'' option for now.
The arguments to @matchConFamily@ are the same as to @match@; the extra
@Int@ returned is the number of constructors in the family.

The function @matchConFamily@ is concerned with this
have-we-used-all-the-constructors question; the local function
@match_cons_used@ does all the real work.
\begin{code}
matchConFamily :: [Id]
	       -> [EquationInfo]
	       -> [EquationInfo]	-- Shadows
	       -> DsM MatchResult

matchConFamily (var:vars) eqns_info shadows
  = match_cons_used vars eqns_info shadows `thenDs` \ alts ->
    mkCoAlgCaseMatchResult var alts
\end{code}

And here is the local function that does all the work.  It is more-or-less the
@matchCon@/@matchClause@ functions on page~94 in Wadler's chapter in SLPJ.
\begin{code}
match_cons_used _ [{- no more eqns -}] _ = returnDs []

match_cons_used vars eqns_info@(EqnInfo (ConPat data_con _ arg_pats : ps1) _ : eqns) shadows
  = ASSERT(isDataCon data_con)
    let
	(eqns_for_this_con, eqns_not_for_this_con)       = splitByCon eqns_info
	(shadows_for_this_con, shadows_not_for_this_con) = splitByCon shadows
    in
    -- Go ahead and do the recursive call to make the alts
    -- for the other ConPats in this con family...
    match_cons_used vars eqns_not_for_this_con shadows_not_for_this_con	`thenDs` \ rest_of_alts ->

    -- Make new vars for the con arguments; avoid new locals where possible
    selectMatchVars arg_pats						`thenDs` \ new_vars ->

    -- Now do the business to make the alt for _this_ ConPat ...
    match (new_vars++vars) 
	  (map shift_con_pat eqns_for_this_con)	
	  (map shift_con_pat shadows_for_this_con)			`thenDs` \ match_result ->

    returnDs (
	(data_con, new_vars, match_result)
	: rest_of_alts
    )
  where
    splitByCon :: [EquationInfo] -> ([EquationInfo], [EquationInfo])
    splitByCon [] = ([],[])
    splitByCon (info@(EqnInfo (pat : _) _) : rest) 
	= case pat of
		ConPat n _ _ | n `eqId` data_con -> (info:rest_yes, rest_no)
		WildPat _			 -> (info:rest_yes, info:rest_no)
			-- WildPats will be in the shadows only, 
			-- and they go into both groups
		other_pat			 -> (rest_yes,      info:rest_no)
	where
	  (rest_yes, rest_no) = splitByCon rest

    shift_con_pat :: EquationInfo -> EquationInfo
    shift_con_pat (EqnInfo (ConPat _ _ pats': pats) match_result)
      = EqnInfo (pats' ++ pats) match_result
    shift_con_pat (EqnInfo (WildPat _: pats) match_result)	-- Will only happen in shadow
      = EqnInfo ([WildPat (typeOfPat arg_pat) | arg_pat <- arg_pats] ++ pats) match_result
    shift_con_pat other = panic "matchConFamily:match_cons_used:shift_con_pat"
\end{code}

Note on @shift_con_pats@ just above: does what the list comprehension in
@matchClause@ (SLPJ, p.~94) does, except things are trickier in real
life.  Works for @ConPats@, and we want it to fail catastrophically
for anything else (which a list comprehension wouldn't).
Cf.~@shift_lit_pats@ in @MatchLits@.
