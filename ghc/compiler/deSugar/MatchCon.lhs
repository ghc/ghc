%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[MatchCon]{Pattern-matching constructors}

\begin{code}
module MatchCon ( matchConFamily ) where

#include "HsVersions.h"

import {-# SOURCE #-} Match	( match )

import HsSyn		( OutPat(..) )

import DsMonad
import DsUtils

import Id		( Id )
import CoreSyn
import Type		( mkTyVarTys )
import Unique		( Uniquable(..), Unique )
import UniqFM		-- Until equivClassesUniq moves to Util
import Outputable
\end{code}

We are confronted with the first column of patterns in a set of
equations, all beginning with constructors from one ``family'' (e.g.,
@[]@ and @:@ make up the @List@ ``family'').  We want to generate the
alternatives for a @Case@ expression.  There are several choices:
\begin{enumerate}
\item
Generate an alternative for every constructor in the family, whether
they are used in this set of equations or not; this is what the Wadler
chapter does.
\begin{description}
\item[Advantages:]
(a)~Simple.  (b)~It may also be that large sparsely-used constructor
families are mainly handled by the code for literals.
\item[Disadvantages:]
(a)~Not practical for large sparsely-used constructor families, e.g.,
the ASCII character set.  (b)~Have to look up a list of what
constructors make up the whole family.
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

We are implementing the ``do-it-right'' option for now.  The arguments
to @matchConFamily@ are the same as to @match@; the extra @Int@
returned is the number of constructors in the family.

The function @matchConFamily@ is concerned with this
have-we-used-all-the-constructors? question; the local function
@match_cons_used@ does all the real work.
\begin{code}
matchConFamily :: [Id]
	       -> [EquationInfo]
	       -> DsM MatchResult

matchConFamily (var:vars) eqns_info
  = let
	-- Sort into equivalence classes by the unique on the constructor
	-- All the EqnInfos should start with a ConPat
	eqn_groups = equivClassesByUniq get_uniq eqns_info
	get_uniq (EqnInfo _ _ (ConPat data_con _ _ _ _ : _) _) = getUnique data_con
    in
	-- Now make a case alternative out of each group
    mapDs (match_con vars) eqn_groups	`thenDs` \ alts ->

    returnDs (mkCoAlgCaseMatchResult var alts)
\end{code}

And here is the local function that does all the work.  It is
more-or-less the @matchCon@/@matchClause@ functions on page~94 in
Wadler's chapter in SLPJ.

\begin{code}
match_con vars all_eqns@(EqnInfo n ctx (ConPat data_con _ ex_tvs ex_dicts arg_pats : pats1) match_result1 : other_eqns)
  = -- Make new vars for the con arguments; avoid new locals where possible
    mapDs selectMatchVar arg_pats			   `thenDs` \ arg_vars ->

    -- Now do the business to make the alt for _this_ ConPat ...
    match (ex_dicts ++ arg_vars ++ vars)
	  (map shift_con_pat all_eqns)	`thenDs` \ match_result ->

	-- Substitute over the result
    let
	match_result' | null ex_tvs = match_result
		      | otherwise   = adjustMatchResult subst_it match_result
    in	
    returnDs (data_con, ex_tvs ++ ex_dicts ++ arg_vars, match_result')
  where
    shift_con_pat :: EquationInfo -> EquationInfo
    shift_con_pat (EqnInfo n ctx (ConPat _ _ ex_tvs' ex_dicts' arg_pats: pats) match_result)
      = EqnInfo n ctx (new_pats  ++ pats) match_result
      where
	new_pats  = map VarPat ex_dicts' ++ arg_pats 

	-- We 'substitute' by going: (/\ tvs' -> e) tvs
    subst_it e = foldr subst_one e other_eqns
    subst_one (EqnInfo _ _ (ConPat _ _ ex_tvs' _ _ : _) _) e = mkTyApps (mkLams ex_tvs' e) ex_tys
    ex_tys = mkTyVarTys ex_tvs


-- Belongs in Util.lhs
equivClassesByUniq :: (a -> Unique) -> [a] -> [[a]]
	-- NB: it's *very* important that if we have the input list [a,b,c],
	-- where a,b,c all have the same unique, then we get back the list
	-- 	[a,b,c]
	-- not
	--	[c,b,a]
	-- Hence the use of foldr, plus the reversed-args tack_on below
equivClassesByUniq get_uniq xs
  = eltsUFM (foldr add emptyUFM xs)
  where
    add a ufm = addToUFM_C tack_on ufm (get_uniq a) [a]
    tack_on old new = new++old
\end{code}

Note on @shift_con_pats@ just above: does what the list comprehension in
@matchClause@ (SLPJ, p.~94) does, except things are trickier in real
life.  Works for @ConPats@, and we want it to fail catastrophically
for anything else (which a list comprehension wouldn't).
Cf.~@shift_lit_pats@ in @MatchLits@.
