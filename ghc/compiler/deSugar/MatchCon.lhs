%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[MatchCon]{Pattern-matching constructors}

\begin{code}
module MatchCon ( matchConFamily ) where

#include "HsVersions.h"

import {-# SOURCE #-} Match	( match )

import HsSyn		( OutPat(..), HsLit, HsExpr )
import DsHsSyn		( outPatType )

import DsMonad
import DsUtils

import Id		( GenId{-instances-}, Id )
import Util		( panic, assertPanic )
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
  = match_cons_used vars eqns_info `thenDs` \ alts ->
    mkCoAlgCaseMatchResult var alts
\end{code}

And here is the local function that does all the work.  It is
more-or-less the @matchCon@/@matchClause@ functions on page~94 in
Wadler's chapter in SLPJ.
\begin{code}
match_cons_used _ [{- no more eqns -}] = returnDs []

match_cons_used vars eqns_info@(EqnInfo n ctx (ConPat data_con _ arg_pats : ps1) _ : eqns)
  = let
	(eqns_for_this_con, eqns_not_for_this_con)       = splitByCon eqns_info
    in
    -- Go ahead and do the recursive call to make the alts
    -- for the other ConPats in this con family...
    match_cons_used vars eqns_not_for_this_con 	          `thenDs` \ rest_of_alts ->

    -- Make new vars for the con arguments; avoid new locals where possible
    selectMatchVars arg_pats				   `thenDs` \ new_vars ->

    -- Now do the business to make the alt for _this_ ConPat ...
    match (new_vars++vars)
	  (map shift_con_pat eqns_for_this_con)		   `thenDs` \ match_result ->

    returnDs (
	(data_con, new_vars, match_result)
	: rest_of_alts
    )
  where
    splitByCon :: [EquationInfo] -> ([EquationInfo], [EquationInfo])
    splitByCon [] = ([],[])
    splitByCon (info@(EqnInfo _ _ (pat : _) _) : rest)
	= case pat of
		ConPat n _ _ | n == data_con -> (info:rest_yes, rest_no)
		other_pat		     -> (rest_yes,      info:rest_no)
	where
	  (rest_yes, rest_no) = splitByCon rest

    shift_con_pat :: EquationInfo -> EquationInfo
    shift_con_pat (EqnInfo n ctx (ConPat _ _ pats': pats) match_result)
      = EqnInfo n ctx (pats' ++ pats) match_result
    shift_con_pat (EqnInfo n ctx (WildPat _: pats) match_result) -- Will only happen in shadow
      = EqnInfo n ctx ([WildPat (outPatType arg_pat) | arg_pat <- arg_pats] ++ pats) match_result
    shift_con_pat other = panic "matchConFamily:match_cons_used:shift_con_pat"
\end{code}

Note on @shift_con_pats@ just above: does what the list comprehension in
@matchClause@ (SLPJ, p.~94) does, except things are trickier in real
life.  Works for @ConPats@, and we want it to fail catastrophically
for anything else (which a list comprehension wouldn't).
Cf.~@shift_lit_pats@ in @MatchLits@.
