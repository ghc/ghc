%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1996
%
\section[SpecEnv]{Specialisation info about an @Id@}

\begin{code}
module SpecEnv (
	SpecEnv,
	emptySpecEnv, isEmptySpecEnv, specEnvValues,
	addToSpecEnv, lookupSpecEnv, substSpecEnv
    ) where

#include "HsVersions.h"

import Type		( Type, GenType, mkTyVarTy, matchTys, tyVarsOfTypes, applyToTyVars )
import TyVar		( TyVar, TyVarEnv, tyVarFlexi, setTyVarFlexi, lookupTyVarEnv, tyVarSetToList )
import Unify		( Subst, unifyTyListsX )
import Maybes
import Util		( assertPanic )
\end{code}



%************************************************************************
%*									*
\section{SpecEnv}
%*									*
%************************************************************************

\begin{code}
type TemplateType = GenType Bool
      -- The Bool is True for template type variables;
      -- that is, ones that can be bound

data SpecEnv value 
  = EmptySE 
  | SpecEnv [([TemplateType], value)]

specEnvValues :: SpecEnv value -> [value]
specEnvValues EmptySE         = []
specEnvValues (SpecEnv alist) = map snd alist
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
lookupSpecEnv :: SpecEnv value	-- The envt
	      -> [GenType flexi]		-- Key
	      -> Maybe (TyVarEnv (GenType flexi), value)
		     
lookupSpecEnv EmptySE key = Nothing
lookupSpecEnv (SpecEnv alist) key
  = find alist
  where
    find [] = Nothing
    find ((tpl, val) : rest)
      = case matchTys tpl key of
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
	     -> MaybeErr (SpecEnv value)	        -- Success...
		          ([TemplateType], value)	-- Failure: Offending overlap

addToSpecEnv overlap_ok spec_env tvs tys value
  = case spec_env of
       EmptySE       -> returnMaB (SpecEnv [ins_item])
       SpecEnv alist -> insert alist    `thenMaB` \ alist' ->
                        returnMaB (SpecEnv alist')
  where
    ins_item = (ins_tys, value)
    ins_tys  = map (applyToTyVars mk_tv) tys

    mk_tv tv = mkTyVarTy (setTyVarFlexi tv (tv `elem` tvs))
               -- tvs identifies the template variables

    insert [] = returnMaB [ins_item]
    insert alist@(cur_item@(cur_tys, _) : rest)
      | unifiable && not overlap_ok             = failMaB cur_item
      | unifiable && ins_item_more_specific     = returnMaB (ins_item : alist)
      | unifiable && not cur_item_more_specific = failMaB cur_item
      | otherwise                               = -- Less specific, or not unifiable... carry on
                                                  insert rest     `thenMaB` \ rest' ->
                                                  returnMaB (cur_item : rest')
      where
        unifiable = maybeToBool (unifyTyListsX cur_tys ins_tys)
        ins_item_more_specific = maybeToBool (matchTys cur_tys ins_tys)
        cur_item_more_specific = maybeToBool (matchTys ins_tys cur_tys)
\end{code}

Finally, during simplification we must apply the current substitution to
the SpecEnv.

\begin{code}
substSpecEnv :: TyVarEnv Type -> (val -> val) -> SpecEnv val -> SpecEnv val
substSpecEnv ty_env val_fn EmptySE = EmptySE
substSpecEnv ty_env val_fn (SpecEnv alist)
  = SpecEnv [(map ty_fn tys, val_fn val) | (tys, val) <- alist]
  where
    ty_fn = applyToTyVars tyvar_fn

    -- Apply the substitution; but if we ever substitute
    -- we need to convert a Type to a TemplateType
    tyvar_fn tv | tyVarFlexi tv = mkTyVarTy tv
                | otherwise     = case lookupTyVarEnv ty_env tv of
                                    Nothing -> mkTyVarTy tv
                                    Just ty -> applyToTyVars set_non_tpl ty

    set_non_tpl tv = mkTyVarTy (setTyVarFlexi tv False)
\end{code}
