%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1999
%
\section[Variance]{Variance in @Type@ and @TyCon@}

\begin{code}
module Variance(
        calcTyConArgVrcs,
        tyVarVrc
    ) where

#include "HsVersions.h"

import TypeRep          ( Type(..), TyNote(..) )  -- friend
import TyCon            ( TyCon, ArgVrcs, tyConKind, tyConArity, tyConDataCons, tyConTyVars,
                          tyConArgVrcs_maybe, getSynTyConDefn, isSynTyCon, isAlgTyCon )
import DataCon          ( dataConRepArgTys )

import FiniteMap
import Var              ( TyVar )
import VarSet
import Name             ( Name, getName )
import Maybes           ( expectJust )
import Outputable
\end{code}


Computing the tyConArgVrcs info
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@tyConArgVrcs@ gives a list of (occPos,occNeg) flags, one for each
tyvar.  For @AlgTyCon@s and @SynTyCon@s, this info must be precomputed
separately.  Note that this is information about occurrences of type
variables, not usages of term variables.

The function @calcTyConArgVrcs@ must be passed a list of *algebraic or
syntycons only* such that all tycons referred to (by mutual recursion)
appear in the list.  The fixpointing will be done on this set of
tycons as a whole.  It returns a list of @tyconVrcInfo@ data, ready to
be (knot-tyingly?) stuck back into the appropriate fields.

\begin{code}
calcTyConArgVrcs :: [TyCon]
		 -> FiniteMap Name ArgVrcs

calcTyConArgVrcs tycons
  = let oi           = foldl (\fm tc -> addToFM fm tc (initial tc)) emptyFM tycons
        initial tc   = if isAlgTyCon tc && null (tyConDataCons tc) then
                         -- make pessimistic assumption (and warn)
                         take (tyConArity tc) abstractVrcs
                       else
                         replicate (tyConArity tc) (False,False)
        oi''         = tcaoFix oi
        go (tc,vrcs) = (getName tc,vrcs)
    in  listToFM (map go (fmToList oi''))
        
  where

    tcaoFix :: FiniteMap TyCon ArgVrcs   -- initial ArgVrcs per tycon
	    -> FiniteMap TyCon ArgVrcs   -- fixpointed ArgVrcs per tycon

    tcaoFix oi = let (changed,oi') = foldFM (\ tc pms
                                               (changed,oi')
					       -> let pms' = tcaoIter oi' tc  -- seq not simult
					          in  (changed || (pms /= pms'),
						       addToFM oi' tc pms'))
                                            (False,oi)  -- seq not simult for faster fixpting
					    oi
		 in  if changed
		     then tcaoFix oi'
		     else oi'

    tcaoIter :: FiniteMap TyCon ArgVrcs  -- reference ArgVrcs (initial)
	     -> TyCon                    -- tycon to update
	     -> ArgVrcs                  -- new ArgVrcs for tycon

    tcaoIter oi tc | isAlgTyCon tc
      = let cs        = tyConDataCons tc
            vs        = tyConTyVars tc
	    argtys    = concatMap dataConRepArgTys cs
	    myfao tc  = lookupWithDefaultFM oi (expectJust "tcaoIter(Alg)" $
                                                  tyConArgVrcs_maybe tc)
                                               tc
                        -- we use the already-computed result for tycons not in this SCC
        in  map (\v -> anyVrc (\ty -> vrcInTy myfao v ty) argtys)
                vs

    tcaoIter oi tc | isSynTyCon tc
      = let (tyvs,ty) = getSynTyConDefn tc
	    myfao tc  = lookupWithDefaultFM oi (expectJust "tcaoIter(Syn)" $
                                                  tyConArgVrcs_maybe tc)
                                               tc
                        -- we use the already-computed result for tycons not in this SCC
        in  map (\v -> vrcInTy myfao v ty) tyvs


abstractVrcs :: ArgVrcs
-- we pull this out as a CAF so the warning only appears *once*
abstractVrcs = trace ("WARNING: tyConArgVrc info inaccurate due to unavailable constructors.\n"
                      ++ "\tUse -fno-prune-tydecls to fix.") $
                 repeat (True,True)
\end{code}


Variance of tyvars in a type
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A general variance-check function.  We pass a function for determining
the @ArgVrc@s of a tycon; when fixpointing this refers to the current
value; otherwise this should be looked up from the tycon's own
tyConArgVrcs.

\begin{code}
vrcInTy :: (TyCon -> ArgVrcs)  -- function to get argVrcs of a tycon (break out of recursion)
        -> TyVar               -- tyvar to check Vrcs of
        -> Type                -- type to check for occ in
        -> (Bool,Bool)         -- (occurs positively, occurs negatively)

vrcInTy fao v (NoteTy (UsgNote _)   ty) = vrcInTy fao v ty

vrcInTy fao v (NoteTy (UsgForAll _) ty) = vrcInTy fao v ty

vrcInTy fao v (NoteTy (SynNote _)   ty) = vrcInTy fao v ty
    			-- SynTyCon doesn't neccessarily have vrcInfo at this point,
    			-- so don't try and use it

vrcInTy fao v (NoteTy (FTVNote ftv) ty) = if elemVarSet v ftv
    					  then vrcInTy fao v ty
    					  else (False,False)
    			-- note that ftv cannot be calculated as occPos||occNeg,
    			-- since if a tyvar occurs only as unused tyconarg,
    			-- occPos==occNeg==False, but ftv=True

vrcInTy fao v (TyVarTy v')              = if v==v'
    					  then (True,False)
    					  else (False,False)

vrcInTy fao v (AppTy ty1 ty2)           = if vrcInTy fao v ty2 /= (False,False)
                                          then (True,True)
                                          else vrcInTy fao v ty1
                        -- ty1 is probably unknown (or it would have been beta-reduced);
                        -- hence if v occurs in ty2 at all then it could occur with
                        -- either variance.  Otherwise it occurs as it does in ty1.

vrcInTy fao v (FunTy ty1 ty2)           = let (p1,m1) = vrcInTy fao v ty1
    					      (p2,m2) = vrcInTy fao v ty2
    					  in (m1||p2,p1||m2)
					 
vrcInTy fao v (ForAllTy v' ty)          = if v==v'
					  then (False,False)
    					  else vrcInTy fao v ty

vrcInTy fao v (TyConApp tc tys)         = let pms1 = map (vrcInTy fao v) tys
    					      pms2 = fao tc
    				          in  orVrcs (zipWith timesVrc pms1 pms2)
\end{code}


External entry point: assumes tyconargvrcs already computed.

\begin{code}
tyVarVrc :: TyVar               -- tyvar to check Vrc of
         -> Type                -- type to check for occ in
         -> (Bool,Bool)         -- (occurs positively, occurs negatively)

tyVarVrc = vrcInTy (expectJust "tyVarVrcs" . tyConArgVrcs_maybe)
\end{code}


Variance algebra
~~~~~~~~~~~~~~~~

\begin{code}
orVrc :: (Bool,Bool) -> (Bool,Bool) -> (Bool,Bool)
orVrc (p1,m1) (p2,m2) = (p1||p2,m1||m2)

orVrcs :: [(Bool,Bool)] -> (Bool,Bool)
orVrcs = foldl orVrc (False,False)

anyVrc :: (a -> (Bool,Bool)) -> [a] -> (Bool,Bool)
anyVrc p as = foldl (\ pm a -> pm `orVrc` p a)
                    (False,False) as

timesVrc :: (Bool,Bool) -> (Bool,Bool) -> (Bool,Bool)
timesVrc (p1,m1) (p2,m2) = (p1 && p2 || m1 && m2,
    			    p1 && m2 || m1 && p2)
\end{code}
