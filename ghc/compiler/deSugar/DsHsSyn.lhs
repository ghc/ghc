%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[DsHsSyn]{Haskell abstract syntax---added things for desugarer}

\begin{code}
#include "HsVersions.h"

module DsHsSyn where

IMP_Ubiq()

import HsSyn		( OutPat(..), HsBinds(..), Bind(..), MonoBinds(..),
			  Sig, HsExpr, GRHSsAndBinds, Match, HsLit )
import TcHsSyn		( SYN_IE(TypecheckedPat), SYN_IE(TypecheckedBind), 
			  SYN_IE(TypecheckedMonoBinds) )

import Id		( idType )
import TysWiredIn	( mkListTy, mkTupleTy, unitTy )
import Util		( panic )
\end{code}

Note: If @outPatType@ doesn't bear a strong resemblance to @coreExprType@,
then something is wrong.
\begin{code}
outPatType :: TypecheckedPat -> Type

outPatType (WildPat ty)		= ty
outPatType (VarPat var)		= idType var
outPatType (LazyPat pat)	= outPatType pat
outPatType (AsPat var pat)	= idType var
outPatType (ConPat _ ty _)	= ty
outPatType (ConOpPat _ _ _ ty)	= ty
outPatType (ListPat ty _)	= mkListTy ty
outPatType (TuplePat pats)	= mkTupleTy (length pats) (map outPatType pats)
outPatType (RecPat _ ty _)      = ty
outPatType (LitPat lit ty)	= ty
outPatType (NPat lit ty _)	= ty
outPatType (DictPat ds ms)      = case (length ds_ms) of
				    0 -> unitTy
				    1 -> idType (head ds_ms)
				    n -> mkTupleTy n (map idType ds_ms)
				   where
				    ds_ms = ds ++ ms
\end{code}


Nota bene: DsBinds relies on the fact that at least for simple
tuple patterns @collectTypedPatBinders@ returns the binders in
the same order as they appear in the tuple.

collectTypedBinders and collectedTypedPatBinders are the exportees.

\begin{code}
collectTypedBinders :: TypecheckedBind -> [Id]
collectTypedBinders EmptyBind	    = []
collectTypedBinders (NonRecBind bs) = collectTypedMonoBinders bs
collectTypedBinders (RecBind    bs) = collectTypedMonoBinders bs

collectTypedMonoBinders :: TypecheckedMonoBinds -> [Id]
collectTypedMonoBinders EmptyMonoBinds	      = []
collectTypedMonoBinders (PatMonoBind pat _ _) = collectTypedPatBinders pat
collectTypedMonoBinders (FunMonoBind f _ _ _) = [f]
collectTypedMonoBinders (VarMonoBind v _)     = [v]
collectTypedMonoBinders (CoreMonoBind v _)     = [v]
collectTypedMonoBinders (AndMonoBinds bs1 bs2)
 = collectTypedMonoBinders bs1 ++ collectTypedMonoBinders bs2

collectTypedPatBinders :: TypecheckedPat -> [Id]
collectTypedPatBinders (VarPat var)	    = [var]
collectTypedPatBinders (LazyPat pat)	    = collectTypedPatBinders pat
collectTypedPatBinders (AsPat a pat)	    = a : collectTypedPatBinders pat
collectTypedPatBinders (ConPat _ _ pats)    = concat (map collectTypedPatBinders pats)
collectTypedPatBinders (ConOpPat p1 _ p2 _) = collectTypedPatBinders p1 ++ collectTypedPatBinders p2
collectTypedPatBinders (ListPat t pats)     = concat (map collectTypedPatBinders pats)
collectTypedPatBinders (TuplePat pats)	    = concat (map collectTypedPatBinders pats)
collectTypedPatBinders (RecPat _ _ fields)  = concat (map (\ (f,pat,_) -> collectTypedPatBinders pat) fields)
collectTypedPatBinders (DictPat ds ms)	    = ds ++ ms
collectTypedPatBinders any_other_pat	    = [ {-no binders-} ]
\end{code}
