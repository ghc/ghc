%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[IdUtils]{Constructing PrimOp Ids}

\begin{code}
module IdUtils ( primOpName ) where

#include "HsVersions.h"

import CoreSyn
import CoreUnfold	( Unfolding )
import Id		( mkPrimitiveId )
import IdInfo		-- quite a few things
import StdIdInfo
import Name		( mkWiredInIdName, Name )
import PrimOp		( primOpInfo, tagOf_PrimOp, PrimOpInfo(..), PrimOp )
import PrelMods		( pREL_GHC )
import Type		( mkForAllTys, mkFunTy, mkFunTys, mkTyConApp )
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
	    (mkForAllTys tyvars (mkFunTys arg_tys (mkTyConApp prim_tycon res_tys)))
	    (length arg_tys) -- arity

      AlgResult str tyvars arg_tys tycon res_tys ->
	mk_prim_name op str
	    tyvars
	    arg_tys
	    (mkForAllTys tyvars (mkFunTys arg_tys (mkTyConApp tycon res_tys)))
	    (length arg_tys) -- arity
  where
    mk_prim_name prim_op occ_name tyvar_tmpls arg_tys ty arity
      = name
      where
	key     = mkPrimOpIdUnique (IBOX(tagOf_PrimOp prim_op))
	name    = mkWiredInIdName key pREL_GHC occ_name the_id
	the_id  = mkPrimitiveId name ty prim_op
\end{code}

\begin{code}
dyadic_fun_ty  ty = mkFunTys [ty, ty] ty
monadic_fun_ty ty = ty `mkFunTy` ty
compare_fun_ty ty = mkFunTys [ty, ty] boolTy
\end{code}
