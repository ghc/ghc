%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
%************************************************************************
%*									*
\section[SATMonad]{The Static Argument Transformation pass Monad}
%*									*
%************************************************************************

\begin{code}
#include "HsVersions.h"

module StgSATMonad (
	getArgLists, saTransform, 

	Id, UniType, SplitUniqSupply, PlainStgExpr(..)
    ) where

import AbsUniType	( mkTyVarTy, mkSigmaTy, TyVarTemplate,
			  extractTyVarsFromTy, splitType, splitTyArgs,
			  glueTyArgs, instantiateTy, TauType(..),
			  Class, ThetaType(..), SigmaType(..),
			  InstTyEnv(..)
			)
import IdEnv
import Id		( mkSysLocal, getIdUniType, eqId )
import Maybes		( Maybe(..) )
import StgSyn
import SATMonad         ( SATEnv(..), SATInfo(..), Arg(..), updSAEnv, insSAEnv,
                          SatM(..), initSAT, thenSAT, thenSAT_,
                          emptyEnvSAT, returnSAT, mapSAT, isStatic, dropStatics,
                          getSATInfo, newSATName )
import SrcLoc		( SrcLoc, mkUnknownSrcLoc )
import SplitUniq
import Unique
import UniqSet		( UniqSet(..), emptyUniqSet )
import Util

\end{code}

%************************************************************************
%*									*
\subsection{Utility Functions}
%*									*
%************************************************************************

\begin{code}
newSATNames :: [Id] -> SatM [Id]
newSATNames [] = returnSAT []
newSATNames (id:ids) = newSATName id (getIdUniType id)	`thenSAT` \ id' ->
                       newSATNames ids			`thenSAT` \ ids' ->
                       returnSAT (id:ids)

getArgLists :: PlainStgRhs -> ([Arg UniType],[Arg Id])
getArgLists (StgRhsCon _ _ _) 
  = ([],[])
getArgLists (StgRhsClosure _ _ _ _ args _)
  = ([], [Static v | v <- args])

\end{code}

\begin{code}
saTransform :: Id -> PlainStgRhs -> SatM PlainStgBinding
saTransform binder rhs
  = getSATInfo binder `thenSAT` \ r ->
    case r of
      Just (_,args) | any isStatic args 
      -- [Andre] test: do it only if we have more than one static argument.
      --Just (_,args) | length (filter isStatic args) > 1
	-> newSATName binder (new_ty args)	`thenSAT` \ binder' ->
           let non_static_args = get_nsa args (snd (getArgLists rhs))
           in
	   newSATNames non_static_args		`thenSAT` \ non_static_args' ->
	   mkNewRhs binder binder' args rhs non_static_args' non_static_args
						`thenSAT` \ new_rhs ->
	   trace ("SAT(STG) "++ show (length (filter isStatic args))) (
           returnSAT (StgNonRec binder new_rhs)
           )
      _ -> returnSAT (StgRec [(binder, rhs)])

  where
    get_nsa :: [Arg a] -> [Arg a] -> [a]
    get_nsa []			_		= []
    get_nsa _			[]		= []
    get_nsa (NotStatic:args)	(Static v:as)	= v:get_nsa args as
    get_nsa (_:args)		(_:as)		=   get_nsa args as

    mkNewRhs binder binder' args rhs@(StgRhsClosure cc bi fvs upd rhsargs body) non_static_args' non_static_args
      = let
	  local_body = StgApp (StgVarAtom binder')
			 [StgVarAtom a | a <- non_static_args] emptyUniqSet

	  rec_body = StgRhsClosure cc bi fvs upd non_static_args'
	               (doStgSubst binder args subst_env body)

	  subst_env = mkIdEnv 
                        ((binder,binder'):zip non_static_args non_static_args')
	in
	returnSAT (
	    StgRhsClosure cc bi fvs upd rhsargs 
	      (StgLet (StgRec [(binder',rec_body)]) {-in-} local_body)
	)

    new_ty args
      = instantiateTy [] (mkSigmaTy [] dict_tys' tau_ty')
      where
	-- get type info for the local function:
	(tv_tmpl, dict_tys, tau_ty) = (splitType . getIdUniType) binder
	(reg_arg_tys, res_type)	    = splitTyArgs tau_ty

	-- now, we drop the ones that are
	-- static, that is, the ones we will not pass to the local function
	l   	     = length dict_tys
	dict_tys'    = dropStatics (take l args) dict_tys
	reg_arg_tys' = dropStatics (drop l args) reg_arg_tys
	tau_ty'	     = glueTyArgs reg_arg_tys' res_type
\end{code}

NOTE: This does not keep live variable/free variable information!!

\begin{code}
doStgSubst binder orig_args subst_env body
  = substExpr body
  where 
    substExpr (StgConApp con args lvs) 
      = StgConApp con (map substAtom args) emptyUniqSet
    substExpr (StgPrimApp op args lvs)
      = StgPrimApp op (map substAtom args) emptyUniqSet
    substExpr expr@(StgApp (StgLitAtom _) [] _) 
      = expr
    substExpr (StgApp atom@(StgVarAtom v)  args lvs)
      | v `eqId` binder
      = StgApp (StgVarAtom (lookupNoFailIdEnv subst_env v))
               (remove_static_args orig_args args) emptyUniqSet
      | otherwise
      = StgApp (substAtom atom) (map substAtom args) lvs
    substExpr (StgCase scrut lv1 lv2 uniq alts)
      = StgCase (substExpr scrut) emptyUniqSet emptyUniqSet uniq (subst_alts alts)
      where
        subst_alts (StgAlgAlts ty alg_alts deflt)
          = StgAlgAlts ty (map subst_alg_alt alg_alts) (subst_deflt deflt)
        subst_alts (StgPrimAlts ty prim_alts deflt)
          = StgPrimAlts ty (map subst_prim_alt prim_alts) (subst_deflt deflt)
        subst_alg_alt (con, args, use_mask, rhs)
          = (con, args, use_mask, substExpr rhs)
        subst_prim_alt (lit, rhs)
          = (lit, substExpr rhs)
        subst_deflt StgNoDefault 
          = StgNoDefault
        subst_deflt (StgBindDefault var used rhs)
          = StgBindDefault var used (substExpr rhs)
    substExpr (StgLetNoEscape fv1 fv2 b body)
      = StgLetNoEscape emptyUniqSet emptyUniqSet (substBinding b) (substExpr body)
    substExpr (StgLet b body)
      = StgLet (substBinding b) (substExpr body)
    substExpr (StgSCC ty cc expr)
      = StgSCC ty cc (substExpr expr)
    substRhs (StgRhsCon cc v args) 
      = StgRhsCon cc v (map substAtom args)
    substRhs (StgRhsClosure cc bi fvs upd args body)
      = StgRhsClosure cc bi [] upd args (substExpr body)
    
    substBinding (StgNonRec binder rhs)
      = StgNonRec binder (substRhs rhs)
    substBinding (StgRec pairs)
      = StgRec (zip binders (map substRhs rhss))
      where
        (binders,rhss) = unzip pairs
    
    substAtom atom@(StgLitAtom lit) = atom
    substAtom atom@(StgVarAtom v) 
      = case lookupIdEnv subst_env v of
          Just v' -> StgVarAtom v'
          Nothing -> atom
    
    remove_static_args _ [] 
      = []
    remove_static_args (Static _:origs) (_:as) 
      = remove_static_args origs as
    remove_static_args (NotStatic:origs) (a:as) 
      = substAtom a:remove_static_args origs as
\end{code}
