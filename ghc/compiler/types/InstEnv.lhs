%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section{Class Instance environments}

\begin{code}
module InstEnv (
	InstEnv, emptyInstEnv,  addToInstEnv, lookupInstEnv
    ) where

#include "HsVersions.h"

import Var		( TyVar, Id )
import VarSet
import VarEnv		( TyVarSubstEnv )
import Type		( Type, tyVarsOfTypes )
import Unify		( unifyTyListsX, matchTys )
import Outputable
import Maybes
\end{code}


%************************************************************************
%*									*
\section{InstEnv}
%*									*
%************************************************************************

\begin{code}
type InstEnv = [(TyVarSet, [Type], Id)]
\end{code}

In some InstEnvs overlap is prohibited; that is, no pair of templates unify.

In others, overlap is permitted, but only in such a way that one can make
a unique choice when looking up.  That is, overlap is only permitted if
one template matches the other, or vice versa.  So this is ok:

  [a]  [Int]

but this is not

  (Int,a)  (b,Int)

If overlap is permitted, the list is kept most specific first, so that
the first lookup is the right choice.


For now we just use association lists.

\begin{code}
emptyInstEnv :: InstEnv
emptyInstEnv = []

isEmptyInstEnv env = null env
\end{code}

@lookupInstEnv@ looks up in a @InstEnv@, using a one-way match.  Since the env is kept
ordered, the first match must be the only one.
The thing we are looking up can have an
arbitrary "flexi" part.

\begin{code}
lookupInstEnv :: SDoc		-- For error report
	      -> InstEnv 	-- The envt
	      -> [Type]		-- Key
	      -> Maybe (TyVarSubstEnv, Id)
		     
lookupInstEnv doc env key
  = find env
  where
    find [] = Nothing
    find ((tpl_tyvars, tpl, val) : rest)
      = case matchTys tpl_tyvars tpl key of
	  Nothing                 -> find rest
	  Just (subst, leftovers) -> ASSERT( null leftovers )
				     Just (subst, val)
\end{code}

@addToInstEnv@ extends a @InstEnv@, checking for overlaps.

A boolean flag controls overlap reporting.

True => overlap is permitted, but only if one template matches the other;
        not if they unify but neither is 

\begin{code}
addToInstEnv :: Bool                            -- True <=> overlap permitted
             -> InstEnv				-- Envt
	     -> [TyVar] -> [Type] -> Id		-- New item
	     -> MaybeErr InstEnv 		-- Success...
		         ([Type], Id)		-- Failure: Offending overlap

addToInstEnv overlap_ok env ins_tvs ins_tys value
  = insert env
  where
    ins_tv_set = mkVarSet ins_tvs
    ins_item = (ins_tv_set, ins_tys, value)

    insert [] = returnMaB [ins_item]
    insert env@(cur_item@(tpl_tvs, tpl_tys, val) : rest)

	-- FAIL if:
	-- (a) they are the same, or
	-- (b) they unify, and any sort of overlap is prohibited,
	-- (c) they unify but neither is more specific than t'other
      |  identical 
      || (unifiable && not overlap_ok)
      || (unifiable && not (ins_item_more_specific || cur_item_more_specific))
      =  failMaB (tpl_tys, val)

	-- New item is an instance of current item, so drop it here
      | ins_item_more_specific	= returnMaB (ins_item : env)

	-- Otherwise carry on
      | otherwise  = insert rest     `thenMaB` \ rest' ->
                     returnMaB (cur_item : rest')
      where
        unifiable = maybeToBool (unifyTyListsX (ins_tv_set `unionVarSet` tpl_tvs) tpl_tys ins_tys)
        ins_item_more_specific = maybeToBool (matchTys tpl_tvs    tpl_tys ins_tys)
        cur_item_more_specific = maybeToBool (matchTys ins_tv_set ins_tys tpl_tys)
	identical = ins_item_more_specific && cur_item_more_specific
\end{code}

