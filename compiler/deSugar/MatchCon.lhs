
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[MatchCon]{Pattern-matching constructors}

\begin{code}
module MatchCon ( matchConFamily ) where

#include "HsVersions.h"

import {-# SOURCE #-} Match	( match )

import HsSyn		( Pat(..), LPat, HsConDetails(..) )
import DsBinds		( dsLHsBinds )
import DataCon		( DataCon, dataConInstOrigArgTys, dataConEqSpec,
			  dataConFieldLabels, dataConSourceArity )
import TcType		( tcTyConAppArgs )
import Type		( mkTyVarTys )
import CoreSyn
import DsMonad
import DsUtils

import Id		( Id, idName )
import Type             ( Type )
import SrcLoc		( unLoc, Located(..) )
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
	       -> [[EquationInfo]]
	       -> DsM MatchResult
-- Each group of eqns is for a single constructor
matchConFamily (var:vars) ty groups
  = do	{ alts <- mapM (matchOneCon vars ty) groups
	; return (mkCoAlgCaseMatchResult var ty alts) }

matchOneCon vars ty (eqn1 : eqns)	-- All eqns for a single constructor
  = do	{ (wraps, eqns') <- mapAndUnzipM shift (eqn1:eqns)
	; arg_vars <- selectMatchVars (take (dataConSourceArity con) 
					    (eqn_pats (head eqns')))
		-- Use the new arugment patterns as a source of 
		-- suggestions for the new variables
	; match_result <- match (arg_vars ++ vars) ty eqns'
      	; return (con, tvs1 ++ dicts1 ++ arg_vars, 
		  adjustMatchResult (foldr1 (.) wraps) match_result) }
  where
    ConPatOut { pat_con = L _ con, pat_ty = pat_ty1,
	        pat_tvs = tvs1, pat_dicts = dicts1 } = firstPat eqn1
	
    arg_tys  = dataConInstOrigArgTys con inst_tys
    n_co_args = length (dataConEqSpec con)
    inst_tys = tcTyConAppArgs pat_ty1 ++ (drop n_co_args $ mkTyVarTys tvs1)
	-- Newtypes opaque, hence tcTyConAppArgs

    shift eqn@(EqnInfo { eqn_pats = ConPatOut{ pat_tvs = tvs, pat_dicts = ds, 
					       pat_binds = bind, pat_args = args
					      } : pats })
	= do { prs <- dsLHsBinds bind
	     ; return (wrapBinds (tvs `zip` tvs1) 
		       . wrapBinds (ds  `zip` dicts1)
		       . mkDsLet (Rec prs),
		       eqn { eqn_pats = conArgPats con arg_tys args ++ pats }) }

conArgPats :: DataCon 
	   -> [Type]	-- Instantiated argument types 
	   -> HsConDetails Id (LPat Id)
	   -> [Pat Id]
conArgPats data_con arg_tys (PrefixCon ps)   = map unLoc ps
conArgPats data_con arg_tys (InfixCon p1 p2) = [unLoc p1, unLoc p2]
conArgPats data_con arg_tys (RecCon rpats)
  | null rpats
  =	-- Special case for C {}, which can be used for 
	-- a constructor that isn't declared to have
	-- fields at all
    map WildPat arg_tys

  | otherwise
  = zipWith mk_pat (dataConFieldLabels data_con) arg_tys
  where
	-- mk_pat picks a WildPat of the appropriate type for absent fields,
	-- and the specified pattern for present fields
    mk_pat lbl arg_ty
	= case [ pat | (sel_id,pat) <- rpats, idName (unLoc sel_id) == lbl] of
	    (pat:pats) -> ASSERT( null pats ) unLoc pat
	    []	       -> WildPat arg_ty
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

