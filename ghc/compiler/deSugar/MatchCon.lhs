
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[MatchCon]{Pattern-matching constructors}

\begin{code}
module MatchCon ( matchConFamily ) where

#include "HsVersions.h"

import {-# SOURCE #-} Match	( match )

import HsSyn		( Pat(..), HsConDetails(..) )
import DsBinds		( dsLHsBinds )
import DataCon		( isVanillaDataCon, dataConInstOrigArgTys )
import TcType		( tcTyConAppArgs )
import Type		( mkTyVarTys )
import CoreSyn
import DsMonad
import DsUtils

import Id		( Id )
import Type             ( Type )
import ListSetOps	( equivClassesByUniq )
import SrcLoc		( unLoc, Located(..) )
import Unique		( Uniquable(..) )
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
               -> Type
	       -> [EquationInfo]
	       -> DsM MatchResult
matchConFamily (var:vars) ty eqns_info
  = let
	-- Sort into equivalence classes by the unique on the constructor
	-- All the EqnInfos should start with a ConPat
	groups = equivClassesByUniq get_uniq eqns_info
	get_uniq (EqnInfo { eqn_pats = ConPatOut (L _ data_con) _ _ _ _ _ : _}) = getUnique data_con

	-- Get the wrapper from the head of each group.  We're going to
	-- use it as the pattern in this case expression, so we need to 
	-- ensure that any type variables it mentions in the pattern are
	-- in scope.  So we put its wrappers outside the case, and
	-- zap the wrapper for it. 
	wraps :: [CoreExpr -> CoreExpr]
	wraps = map (eqn_wrap . head) groups

	groups' = [ eqn { eqn_wrap = idWrapper } : eqns | eqn:eqns <- groups ]
    in
	-- Now make a case alternative out of each group
    mappM (match_con vars ty) groups'	`thenDs` \ alts ->
    returnDs (adjustMatchResult (foldr (.) idWrapper wraps) $
	      mkCoAlgCaseMatchResult var ty alts)
\end{code}

And here is the local function that does all the work.  It is
more-or-less the @matchCon@/@matchClause@ functions on page~94 in
Wadler's chapter in SLPJ.  The function @shift_con_pats@ does what the
list comprehension in @matchClause@ (SLPJ, p.~94) does, except things
are trickier in real life.  Works for @ConPats@, and we want it to
fail catastrophically for anything else (which a list comprehension
wouldn't).  Cf.~@shift_lit_pats@ in @MatchLits@.

\begin{code}
match_con vars ty eqns
  = do	{ -- Make new vars for the con arguments; avoid new locals where possible
	  arg_vars     <- selectMatchVars (map unLoc arg_pats1) arg_tys
	; eqns'        <- mapM shift eqns 
	; match_result <- match (arg_vars ++ vars) ty eqns'
	; return (con, tvs1 ++ dicts1 ++ arg_vars, match_result) }
  where
    ConPatOut (L _ con) tvs1 dicts1 _ (PrefixCon arg_pats1) pat_ty = firstPat (head eqns)

    shift eqn@(EqnInfo { eqn_wrap = wrap, 
		         eqn_pats = ConPatOut _ tvs ds bind (PrefixCon arg_pats) _ : pats })
	= do { prs <- dsLHsBinds bind
	     ; return (eqn { eqn_wrap = wrap . wrapBinds (tvs `zip` tvs1) 
					     . wrapBinds (ds  `zip` dicts1)
					     . mkDsLet (Rec prs),
			     eqn_pats = map unLoc arg_pats ++ pats }) }

     	-- Get the arg types, which we use to type the new vars
	-- to match on, from the "outside"; the types of pats1 may 
	-- be more refined, and hence won't do
    arg_tys = dataConInstOrigArgTys con inst_tys
    inst_tys | isVanillaDataCon con = tcTyConAppArgs pat_ty	-- Newtypes opaque!
	     | otherwise	    = mkTyVarTys tvs1
\end{code}

Note [Existentials in shift_con_pat]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
	data T = forall a. Ord a => T a (a->Int)

	f (T x f) True  = ...expr1...
	f (T y g) False = ...expr2..

When we put in the tyvars etc we get

	f (T a (d::Ord a) (x::a) (f::a->Int)) True =  ...expr1...
	f (T b (e::Ord b) (y::a) (g::a->Int)) True =  ...expr2...

After desugaring etc we'll get a single case:

	f = \t::T b::Bool -> 
	    case t of
	       T a (d::Ord a) (x::a) (f::a->Int)) ->
	    case b of
		True  -> ...expr1...
		False -> ...expr2...

*** We have to substitute [a/b, d/e] in expr2! **
Hence
		False -> ....((/\b\(e:Ord b).expr2) a d)....

Originally I tried to use 
	(\b -> let e = d in expr2) a 
to do this substitution.  While this is "correct" in a way, it fails
Lint, because e::Ord b but d::Ord a.  

