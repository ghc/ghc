%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[SpecEnv]{Specialisation info about an @Id@}

\begin{code}
module SpecEnv (
	SpecEnv,
	emptySpecEnv, isEmptySpecEnv,
	specEnvValues, specEnvToList, specEnvFromList,
	addToSpecEnv, lookupSpecEnv, substSpecEnv
    ) where

#include "HsVersions.h"

import Var		( TyVar )
import VarEnv
import VarSet
import Type		( Type, fullSubstTy, substTyVar )
import Unify		( unifyTyListsX, matchTys )
import Outputable
import Maybes
\end{code}



%************************************************************************
%*									*
\section{SpecEnv}
%*									*
%************************************************************************

\begin{code}
data SpecEnv value 
  = EmptySE 
  | SpecEnv [([TyVar], 	-- Really a set, but invariably small,
			-- so kept as a list
	      [Type], 
	      value)]

specEnvValues :: SpecEnv value -> [value]
specEnvValues EmptySE         = []
specEnvValues (SpecEnv alist) = [val | (_,_,val) <- alist]

specEnvToList :: SpecEnv value -> [([TyVar], [Type], value)]
specEnvToList EmptySE         = []
specEnvToList (SpecEnv alist) = alist

specEnvFromList :: [([TyVar], [Type], value)] -> SpecEnv value
	-- Assumes the list is in appropriate order
specEnvFromList []    = EmptySE
specEnvFromList alist = SpecEnv alist
\end{code}

In some SpecEnvs overlap is prohibited; that is, no pair of templates unify.

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
emptySpecEnv :: SpecEnv a
emptySpecEnv = EmptySE

isEmptySpecEnv EmptySE = True
isEmptySpecEnv _       = False
\end{code}

@lookupSpecEnv@ looks up in a @SpecEnv@, using a one-way match.  Since the env is kept
ordered, the first match must be the only one.
The thing we are looking up can have an
arbitrary "flexi" part.

\begin{code}
lookupSpecEnv :: SDoc		-- For error report
	      -> SpecEnv value	-- The envt
	      -> [Type]		-- Key
	      -> Maybe (TyVarEnv Type, value)
		     
lookupSpecEnv doc EmptySE key = Nothing
lookupSpecEnv doc (SpecEnv alist) key
  = find alist
  where
    find [] = Nothing
    find ((tpl_tyvars, tpl, val) : rest)
      = case matchTys tpl_tyvars tpl key of
	  Nothing                 -> find rest
	  Just (subst, leftovers) -> ASSERT( null leftovers )
				     Just (subst, val)
\end{code}

@addToSpecEnv@ extends a @SpecEnv@, checking for overlaps.

A boolean flag controls overlap reporting.

True => overlap is permitted, but only if one template matches the other;
        not if they unify but neither is 

\begin{code}
addToSpecEnv :: Bool                            -- True <=> overlap permitted
             -> SpecEnv value			-- Envt
	     -> [TyVar] -> [Type] -> value	-- New item
	     -> MaybeErr (SpecEnv value)	-- Success...
		          ([Type], value)	-- Failure: Offending overlap

addToSpecEnv overlap_ok spec_env ins_tvs ins_tys value
  = case spec_env of
       EmptySE       -> returnMaB (SpecEnv [ins_item])
       SpecEnv alist -> insert alist    `thenMaB` \ alist' ->
                        returnMaB (SpecEnv alist')
  where
    ins_item = (ins_tvs, ins_tys, value)

    insert [] = returnMaB [ins_item]
    insert alist@(cur_item@(tpl_tvs, tpl_tys, val) : rest)

	-- FAIL if:
	-- (a) they are the same, or
	-- (b) they unify, and any sort of overlap is prohibited,
	-- (c) they unify but neither is more specific than t'other
      |  identical 
      || (unifiable && not overlap_ok)
      || (unifiable && not (ins_item_more_specific || cur_item_more_specific))
      =  failMaB (tpl_tys, val)

	-- New item is an instance of current item, so drop it here
      | ins_item_more_specific	= returnMaB (ins_item : alist)

	-- Otherwise carry on
      | otherwise  = insert rest     `thenMaB` \ rest' ->
                     returnMaB (cur_item : rest')
      where
        unifiable = maybeToBool (unifyTyListsX (ins_tvs ++ tpl_tvs) tpl_tys ins_tys)
        ins_item_more_specific = maybeToBool (matchTys tpl_tvs tpl_tys ins_tys)
        cur_item_more_specific = maybeToBool (matchTys ins_tvs ins_tys tpl_tys)
	identical = ins_item_more_specific && cur_item_more_specific
\end{code}

Finally, during simplification we must apply the current substitution to
the SpecEnv.

\begin{code}
substSpecEnv :: TyVarEnv Type -> IdOrTyVarSet 
	     -> (TyVarEnv Type -> IdOrTyVarSet -> val -> val)
	     -> SpecEnv val -> SpecEnv val
substSpecEnv ty_subst in_scope val_fn EmptySE = EmptySE
substSpecEnv ty_subst in_scope val_fn (SpecEnv alist)
  = SpecEnv (map subst alist)
  where
    subst (tpl_tyvars, tpl_tys, val)
	= (tpl_tyvars', 
	   map (fullSubstTy ty_subst' in_scope') tpl_tys, 
	   val_fn ty_subst' in_scope' val)
	where
	  (ty_subst', in_scope', tpl_tyvars') = go ty_subst in_scope [] tpl_tyvars

	  go s i acc []       = (s, i, reverse acc)
	  go s i acc (tv:tvs) = case substTyVar s i tv of
				  (s', i', tv') -> go s' i' (tv' : acc) tvs
\end{code}
