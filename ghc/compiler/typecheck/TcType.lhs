%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcType]{Types used in the typechecker}

\begin{code}
module TcType (
  
  TcTyVar, TcBox,
  TcTyVarSet,
  newTcTyVar,
  newTyVarTy,	-- Kind -> NF_TcM s (TcType s)
  newTyVarTys,	-- Int -> Kind -> NF_TcM s [TcType s]

  -----------------------------------------
  TcType, TcMaybe(..),
  TcTauType, TcThetaType, TcRhoType,

	-- Find the type to which a type variable is bound
  tcWriteTyVar,		-- :: TcTyVar s -> TcType s -> NF_TcM (TcType s)
  tcReadTyVar,		-- :: TcTyVar s -> NF_TcM (TcMaybe s)


  tcSplitRhoTy,

  tcInstTyVars,
  tcInstTcType,

  typeToTcType,

  --------------------------------
  TcKind,
  newKindVar, newKindVars,
  kindToTcKind,
  zonkTcKind,

  --------------------------------
  zonkTcTyVar, zonkTcTyVars, zonkTcTyVarBndr,
  zonkTcType, zonkTcTypes, zonkTcThetaType,

  zonkTcTypeToType, zonkTcTyVarToTyVar,
  zonkTcKindToKind

  ) where

#include "HsVersions.h"


-- friends:
import PprType		()
import Type		( Type, Kind, ThetaType, GenType(..), TyNote(..), 
			  mkAppTy,
			  splitDictTy_maybe, splitForAllTys,
			  isTyVarTy, mkTyVarTys, 
			  fullSubstTy, substFlexiTy, 
			  boxedTypeKind, superKind
			)
import VarEnv
import VarSet		( emptyVarSet )
import Var		( TyVar, GenTyVar, tyVarKind, tyVarFlexi, tyVarName,
			  mkFlexiTyVar, removeTyVarFlexi, isFlexiTyVar, isTyVar
			)

-- others:
import TcMonad
import Name		( changeUnique )

import TysWiredIn	( voidTy )

import Name		( NamedThing(..), changeUnique, mkSysLocalName )
import Unique		( Unique )
import Util		( nOfThem )
import Outputable
\end{code}



Data types
~~~~~~~~~~
See TcMonad.lhs

\begin{code}
tcTyVarToTyVar :: TcTyVar s -> TyVar
tcTyVarToTyVar = removeTyVarFlexi
\end{code}

Utility functions
~~~~~~~~~~~~~~~~~
These tcSplit functions are like their non-Tc analogues, but they
follow through bound type variables.

No need for tcSplitForAllTy because a type variable can't be instantiated
to a for-all type.

\begin{code}
tcSplitRhoTy :: TcType s -> NF_TcM s (TcThetaType s, TcType s)
tcSplitRhoTy t
  = go t t []
 where
	-- A type variable is never instantiated to a dictionary type,
	-- so we don't need to do a tcReadVar on the "arg".
    go syn_t (FunTy arg res) ts = case splitDictTy_maybe arg of
					Just pair -> go res res (pair:ts)
					Nothing   -> returnNF_Tc (reverse ts, syn_t)
    go syn_t (NoteTy _ t)    ts = go syn_t t ts
    go syn_t (TyVarTy tv)    ts = tcReadTyVar tv	`thenNF_Tc` \ maybe_ty ->
				  case maybe_ty of
				    BoundTo ty | not (isTyVarTy ty) -> go syn_t ty ts
				    other			    -> returnNF_Tc (reverse ts, syn_t)
    go syn_t t		     ts = returnNF_Tc (reverse ts, syn_t)
\end{code}


New type variables
~~~~~~~~~~~~~~~~~~

\begin{code}
newTcTyVar :: Kind -> NF_TcM s (TcTyVar s)
newTcTyVar kind
  = tcGetUnique 	`thenNF_Tc` \ uniq ->
    tcNewMutVar UnBound	`thenNF_Tc` \ box ->
    let
	name = mkSysLocalName uniq
    in
    returnNF_Tc (mkFlexiTyVar name kind box)

newTyVarTy  :: Kind -> NF_TcM s (TcType s)
newTyVarTy kind
  = newTcTyVar kind	`thenNF_Tc` \ tc_tyvar ->
    returnNF_Tc (TyVarTy tc_tyvar)

newTyVarTys :: Int -> Kind -> NF_TcM s [TcType s]
newTyVarTys n kind = mapNF_Tc newTyVarTy (nOfThem n kind)

newKindVar :: NF_TcM s (TcKind s)
newKindVar = newTyVarTy superKind

newKindVars :: Int -> NF_TcM s [TcKind s]
newKindVars n = mapNF_Tc (\ _ -> newKindVar) (nOfThem n ())
\end{code}

Type instantiation
~~~~~~~~~~~~~~~~~~

Instantiating a bunch of type variables

\begin{code}
tcInstTyVars :: [GenTyVar flexi] 
	     -> NF_TcM s ([TcTyVar s], [TcType s], TyVarEnv (TcType s))

tcInstTyVars tyvars
  = mapNF_Tc inst_tyvar tyvars	`thenNF_Tc` \ tc_tyvars ->
    let
	tys = mkTyVarTys tc_tyvars
    in
    returnNF_Tc (tc_tyvars, tys, zipVarEnv tyvars tys)

inst_tyvar tyvar	-- Could use the name from the tyvar?
  = tcGetUnique 		`thenNF_Tc` \ uniq ->
    tcNewMutVar UnBound		`thenNF_Tc` \ box ->
    let
	name = changeUnique (tyVarName tyvar) uniq
	-- Note that we don't change the print-name
	-- This won't confuse the type checker but there's a chance
	-- that two different tyvars will print the same way 
	-- in an error message.  -dppr-debug will show up the difference
	-- Better watch out for this.  If worst comes to worst, just
	-- use mkSysLocalName.
    in
    returnNF_Tc (mkFlexiTyVar name (tyVarKind tyvar) box)
\end{code}

@tcInstTcType@ instantiates the outer-level for-alls of a TcType with
fresh type variables, returning them and the instantiated body of the for-all.


\begin{code}
tcInstTcType :: TcType s -> NF_TcM s ([TcTyVar s], TcType s)
tcInstTcType ty
  = let
	(tyvars, rho) = splitForAllTys ty
    in
    case tyvars of
	[]    -> returnNF_Tc ([], ty)	-- Nothing to do
	other -> tcInstTyVars tyvars		`thenNF_Tc` \ (tyvars', _, tenv)  ->
		 returnNF_Tc (tyvars', fullSubstTy tenv emptyVarSet rho)
					-- Since the tyvars are freshly made,
					-- they cannot possibly be captured by
					-- any existing for-alls.  Hence emptyVarSet
\end{code}

Sometimes we have to convert a Type to a TcType.  I wonder whether we could
do this less than we do?

\begin{code}
typeToTcType :: Type -> TcType s
typeToTcType t = substFlexiTy emptyVarEnv t

kindToTcKind :: Kind -> TcKind s
kindToTcKind = typeToTcType
\end{code}


Reading and writing TcTyVars
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
tcWriteTyVar :: TcTyVar s -> TcType s -> NF_TcM s ()
tcReadTyVar  :: TcTyVar s -> NF_TcM s (TcMaybe s)
\end{code}

Writing is easy:

\begin{code}
tcWriteTyVar tyvar ty = tcWriteMutVar (tyVarFlexi tyvar) (BoundTo ty)
\end{code}

Reading is more interesting.  The easy thing to do is just to read, thus:
\begin{verbatim}
tcReadTyVar tyvar = tcReadMutVar (tyVarFlexi tyvar)
\end{verbatim}

But it's more fun to short out indirections on the way: If this
version returns a TyVar, then that TyVar is unbound.  If it returns
any other type, then there might be bound TyVars embedded inside it.

We return Nothing iff the original box was unbound.

\begin{code}
tcReadTyVar tyvar
  = tcReadMutVar box	`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	BoundTo ty -> short_out ty			`thenNF_Tc` \ ty' ->
		      tcWriteMutVar box (BoundTo ty')	`thenNF_Tc_`
		      returnNF_Tc (BoundTo ty')

	other	   -> returnNF_Tc other
  where
    box = tyVarFlexi tyvar

short_out :: TcType s -> NF_TcM s (TcType s)
short_out ty@(TyVarTy tyvar)
  = tcReadMutVar box	`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	BoundTo ty' -> short_out ty' 			`thenNF_Tc` \ ty' ->
		       tcWriteMutVar box (BoundTo ty')	`thenNF_Tc_`
		       returnNF_Tc ty'

	other       -> returnNF_Tc ty
  where
    box = tyVarFlexi tyvar

short_out other_ty = returnNF_Tc other_ty
\end{code}


Zonking Tc types to Tc types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
zonkTcTyVars :: [TcTyVar s] -> NF_TcM s [TcType s]
zonkTcTyVars tyvars = mapNF_Tc zonkTcTyVar tyvars

zonkTcTyVar :: TcTyVar s -> NF_TcM s (TcType s)
zonkTcTyVar tyvar 
  | not (isFlexiTyVar tyvar)	-- Not a flexi tyvar.  This can happen when
				-- zonking a forall type, when the bound type variable
				-- needn't be a flexi.
  = ASSERT( isTyVar tyvar )
    returnNF_Tc (TyVarTy tyvar)

  | otherwise	-- Is a flexi tyvar
  = tcReadTyVar tyvar		`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	BoundTo ty@(TyVarTy tyvar') -> returnNF_Tc ty		-- tcReadTyVar never returns a bound tyvar
	BoundTo other		    -> zonkTcType other
	other			    -> returnNF_Tc (TyVarTy tyvar)

zonkTcTyVarBndr :: TcTyVar s -> NF_TcM s (TcTyVar s)
zonkTcTyVarBndr tyvar
  = zonkTcTyVar tyvar	`thenNF_Tc` \ (TyVarTy tyvar') ->
    returnNF_Tc tyvar'
	
zonkTcTypes :: [TcType s] -> NF_TcM s [TcType s]
zonkTcTypes tys = mapNF_Tc zonkTcType tys

zonkTcThetaType :: TcThetaType s -> NF_TcM s (TcThetaType s)
zonkTcThetaType theta = mapNF_Tc zonk theta
		    where
		      zonk (c,ts) = zonkTcTypes ts	`thenNF_Tc` \ new_ts ->
				    returnNF_Tc (c, new_ts)

zonkTcKind :: TcKind s -> NF_TcM s (TcKind s)
zonkTcKind = zonkTcType

zonkTcType :: TcType s -> NF_TcM s (TcType s)

zonkTcType (TyVarTy tyvar) = zonkTcTyVar tyvar

zonkTcType (AppTy ty1 ty2)
  = zonkTcType ty1		`thenNF_Tc` \ ty1' ->
    zonkTcType ty2		`thenNF_Tc` \ ty2' ->
    returnNF_Tc (mkAppTy ty1' ty2')

zonkTcType (TyConApp tc tys)
  = mapNF_Tc zonkTcType tys	`thenNF_Tc` \ tys' ->
    returnNF_Tc (TyConApp tc tys')

zonkTcType (NoteTy (SynNote ty1) ty2)
  = zonkTcType ty1 		`thenNF_Tc` \ ty1' ->
    zonkTcType ty2 		`thenNF_Tc` \ ty2' ->
    returnNF_Tc (NoteTy (SynNote ty1') ty2')

zonkTcType (NoteTy (FTVNote _) ty2) = zonkTcType ty2

zonkTcType (ForAllTy tv ty)
  = zonkTcTyVar tv		`thenNF_Tc` \ tv_ty ->
    zonkTcType ty 		`thenNF_Tc` \ ty' ->
    case tv_ty of	-- Should be a tyvar!
      TyVarTy tv' -> returnNF_Tc (ForAllTy tv' ty')
      _ -> panic "zonkTcType"
	   -- pprTrace "zonkTcType:ForAllTy:" (hsep [ppr tv, ppr tv_ty]) $
	   -- returnNF_Tc (ForAllTy tv{-(tcTyVarToTyVar tv)-} ty')

zonkTcType (FunTy ty1 ty2)
  = zonkTcType ty1 		`thenNF_Tc` \ ty1' ->
    zonkTcType ty2 		`thenNF_Tc` \ ty2' ->
    returnNF_Tc (FunTy ty1' ty2')
\end{code}

Zonking Tc types to Type/Kind
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
zonkTcKindToKind :: TcKind s -> NF_TcM s Kind
zonkTcKindToKind kind = zonkTcToType boxedTypeKind emptyVarEnv kind

zonkTcTypeToType :: TyVarEnv Type -> TcType s -> NF_TcM s Type
zonkTcTypeToType env ty = zonkTcToType voidTy env ty

zonkTcTyVarToTyVar :: TcTyVar s -> NF_TcM s TyVar
zonkTcTyVarToTyVar tv
  = zonkTcTyVarBndr tv	`thenNF_Tc` \ tv' ->
    returnNF_Tc (tcTyVarToTyVar tv')

-- zonkTcToType is used for Kinds as well
zonkTcToType :: Type -> TyVarEnv Type -> TcType s -> NF_TcM s Type
zonkTcToType unbound_var_ty env ty
  = go ty
  where
    go (TyConApp tycon tys)	  = mapNF_Tc go tys	`thenNF_Tc` \ tys' ->
				    returnNF_Tc (TyConApp tycon tys')

    go (NoteTy (SynNote ty1) ty2) = go ty1		`thenNF_Tc` \ ty1' ->
				    go ty2		`thenNF_Tc` \ ty2' ->
				    returnNF_Tc (NoteTy (SynNote ty1') ty2')

    go (NoteTy (FTVNote _) ty2)   = go ty2	-- Discard free-tyvar annotations

    go (FunTy arg res)      	  = go arg		`thenNF_Tc` \ arg' ->
				    go res		`thenNF_Tc` \ res' ->
				    returnNF_Tc (FunTy arg' res')
 
    go (AppTy fun arg)	 	  = go fun		`thenNF_Tc` \ fun' ->
				    go arg		`thenNF_Tc` \ arg' ->
				    returnNF_Tc (mkAppTy fun' arg')

	-- The two interesting cases!
	-- c.f. zonkTcTyVar
    go (TyVarTy tyvar)  
	| not (isFlexiTyVar tyvar) = lookup env tyvar

	| otherwise	=  tcReadTyVar tyvar	`thenNF_Tc` \ maybe_ty ->
			   case maybe_ty of
			      BoundTo (TyVarTy tyvar') -> lookup env tyvar'
			      BoundTo other_ty	       -> go other_ty
			      other		       -> lookup env tyvar

    go (ForAllTy tyvar ty)
	= zonkTcTyVarToTyVar tyvar	`thenNF_Tc` \ tyvar' ->
	  let
	     new_env = extendVarEnv env tyvar (TyVarTy tyvar')
	  in
	  zonkTcToType unbound_var_ty new_env ty	`thenNF_Tc` \ ty' ->
	  returnNF_Tc (ForAllTy tyvar' ty')


    lookup env tyvar = returnNF_Tc (case lookupVarEnv env tyvar of
					  Just ty -> ty
					  Nothing -> unbound_var_ty)
\end{code}


