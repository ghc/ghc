%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[IdUtils]{Constructing PrimOp Ids}

\begin{code}
#include "HsVersions.h"

module IdUtils ( primOpName ) where

IMP_Ubiq()

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 201
IMPORT_DELOOPER(PrelLoop)		-- here for paranoia checking
IMPORT_DELOOPER(IdLoop) (SpecEnv)
#else
import {-# SOURCE #-} SpecEnv ( SpecEnv )
#endif

import CoreSyn
import CoreUnfold	( UnfoldingGuidance(..), Unfolding, mkUnfolding )
import Id		( mkPrimitiveId, mkTemplateLocals )
import IdInfo		-- quite a few things
import StdIdInfo
import Name		( mkWiredInIdName, Name )
import PrimOp		( primOpInfo, tagOf_PrimOp, primOp_str,
			  PrimOpInfo(..), PrimOpResultInfo(..), PrimOp )
import PrelMods		( gHC__ )
import Type		( mkForAllTys, mkFunTy, mkFunTys, mkTyVarTy, applyTyCon )
import TysWiredIn	( boolTy )
import Unique		( mkPrimOpIdUnique )
import Util		( panic )
\end{code}

\begin{code}
primOpName       :: PrimOp -> Name
primOpName op
  = case (primOpInfo op) of
      Dyadic str ty ->
	mk_prim_name op str [] [ty,ty] (dyadic_fun_ty ty) 2

      Monadic str ty ->
	mk_prim_name op str [] [ty] (monadic_fun_ty ty) 1

      Compare str ty ->
	mk_prim_name op str [] [ty,ty] (compare_fun_ty ty) 2

      Coercing str ty1 ty2 ->
	mk_prim_name op str [] [ty1] (ty1 `mkFunTy` ty2) 1

      PrimResult str tyvars arg_tys prim_tycon kind res_tys ->
	mk_prim_name op str
	    tyvars
	    arg_tys
	    (mkForAllTys tyvars (mkFunTys arg_tys (applyTyCon prim_tycon res_tys)))
	    (length arg_tys) -- arity

      AlgResult str tyvars arg_tys tycon res_tys ->
	mk_prim_name op str
	    tyvars
	    arg_tys
	    (mkForAllTys tyvars (mkFunTys arg_tys (applyTyCon tycon res_tys)))
	    (length arg_tys) -- arity
  where
    mk_prim_name prim_op occ_name tyvar_tmpls arg_tys ty arity
      = name
      where
	key     = mkPrimOpIdUnique (IBOX(tagOf_PrimOp prim_op))
	name    = mkWiredInIdName key gHC__ occ_name the_id
	the_id  = mkPrimitiveId name ty prim_op
\end{code}

\begin{code}
dyadic_fun_ty  ty = mkFunTys [ty, ty] ty
monadic_fun_ty ty = ty `mkFunTy` ty
compare_fun_ty ty = mkFunTys [ty, ty] boolTy
\end{code}
