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


  tcSplitForAllTy, tcSplitRhoTy,

  tcInstTyVars,
  tcInstSigTyVars, 
  tcInstType,
  tcInstSigType, tcInstTcType, tcInstSigTcType,
  tcInstTheta,

  zonkTcTyVars, zonkSigTyVar,
  zonkTcType, zonkTcTypes, zonkTcThetaType,
  zonkTcTypeToType,
  zonkTcTyVar,
  zonkTcTyVarToTyVar

  ) where

#include "HsVersions.h"


-- friends:
import Type		( Type, ThetaType, GenType(..), mkAppTy,
			  tyVarsOfTypes, splitDictTy_maybe,
			  isTyVarTy, instantiateTy
			)
import TyVar		( TyVar, GenTyVar(..), GenTyVarSet, 
			  TyVarEnv, lookupTyVarEnv, addToTyVarEnv,
			  emptyTyVarEnv, zipTyVarEnv, tyVarSetToList
			)

-- others:
import Class		( Class )
import TyCon		( isFunTyCon )
import Kind		( Kind )
import TcMonad

import TysPrim		( voidTy )

import Unique		( Unique )
import UniqFM		( UniqFM )
import BasicTypes	( unused )
import Util		( nOfThem, panic )
\end{code}



Data types
~~~~~~~~~~


\begin{code}
type TcType s = GenType (TcBox s)	-- Used during typechecker
	-- Invariant on ForAllTy in TcTypes:
	-- 	forall a. T
	-- a cannot occur inside a MutTyVar in T; that is,
	-- T is "flattened" before quantifying over a

type TcThetaType s = [(Class, [TcType s])]
type TcRhoType s   = TcType s		-- No ForAllTys
type TcTauType s   = TcType s		-- No DictTys or ForAllTys

type TcBox s = TcRef s (TcMaybe s)

data TcMaybe s = UnBound
	       | BoundTo (TcType s)

-- Interestingly, you can't use (Maybe (TcType s)) instead of (TcMaybe s),
-- because you get a synonym loop if you do!

type TcTyVar s    = GenTyVar (TcBox s)
type TcTyVarSet s = GenTyVarSet (TcBox s)
\end{code}

\begin{code}
tcTyVarToTyVar :: TcTyVar s -> TyVar
tcTyVarToTyVar (TyVar uniq kind name _) = TyVar uniq kind name unused
\end{code}

Utility functions
~~~~~~~~~~~~~~~~~
These tcSplit functions are like their non-Tc analogues, but they
follow through bound type variables.

\begin{code}
tcSplitForAllTy :: TcType s -> NF_TcM s ([TcTyVar s], TcType s)
tcSplitForAllTy t 
  = go t t []
  where
    go syn_t (ForAllTy tv t) tvs = go t t (tv:tvs)
    go syn_t (SynTy _ t)     tvs = go syn_t t tvs
    go syn_t (TyVarTy tv)    tvs = tcReadTyVar tv	`thenNF_Tc` \ maybe_ty ->
				   case maybe_ty of
					BoundTo ty | not (isTyVarTy ty) -> go syn_t ty tvs
					other				-> returnNF_Tc (reverse tvs, syn_t)
    go syn_t t	             tvs = returnNF_Tc (reverse tvs, syn_t)

tcSplitRhoTy :: TcType s -> NF_TcM s (TcThetaType s, TcType s)
tcSplitRhoTy t
  = go t t []
 where
	-- A type variable is never instantiated to a dictionary type,
	-- so we don't need to do a tcReadVar on the "arg".
    go syn_t (FunTy arg res) ts = case splitDictTy_maybe arg of
					Just pair -> go res res (pair:ts)
					Nothing   -> returnNF_Tc (reverse ts, syn_t)
    go syn_t (SynTy _ t)     ts = go syn_t t ts
    go syn_t (TyVarTy tv)    ts = tcReadTyVar tv	`thenNF_Tc` \ maybe_ty ->
				  case maybe_ty of
				    BoundTo ty | not (isTyVarTy ty) -> go syn_t ty ts
				    other			    -> returnNF_Tc (reverse ts, syn_t)
    go syn_t t		     ts = returnNF_Tc (reverse ts, syn_t)
\end{code}


Type instantiation
~~~~~~~~~~~~~~~~~~

\begin{code}
newTcTyVar :: Kind -> NF_TcM s (TcTyVar s)
newTcTyVar kind
  = tcGetUnique 	`thenNF_Tc` \ uniq ->
    tcNewMutVar UnBound	`thenNF_Tc` \ box ->
    returnNF_Tc (TyVar uniq kind Nothing box)

newTyVarTy  :: Kind -> NF_TcM s (TcType s)
newTyVarTy kind
  = newTcTyVar kind	`thenNF_Tc` \ tc_tyvar ->
    returnNF_Tc (TyVarTy tc_tyvar)

newTyVarTys :: Int -> Kind -> NF_TcM s [TcType s]
newTyVarTys n kind = mapNF_Tc newTyVarTy (nOfThem n kind)


-- For signature type variables, use the user name for the type variable
tcInstTyVars, tcInstSigTyVars
	:: [GenTyVar flexi] 
  	-> NF_TcM s ([TcTyVar s], [TcType s], TyVarEnv (TcType s))

tcInstTyVars    tyvars = inst_tyvars inst_tyvar     tyvars
tcInstSigTyVars tyvars = inst_tyvars inst_sig_tyvar tyvars

inst_tyvars inst tyvars
  = mapNF_Tc inst tyvars	`thenNF_Tc` \ tc_tyvars ->
    let
	tys = map TyVarTy tc_tyvars
    in
    returnNF_Tc (tc_tyvars, tys, zipTyVarEnv tyvars tys)

inst_tyvar (TyVar _ kind name _) 
  = tcGetUnique 		`thenNF_Tc` \ uniq ->
    tcNewMutVar UnBound		`thenNF_Tc` \ box ->
    returnNF_Tc (TyVar uniq kind Nothing box)
	-- The "Nothing" means that it'll always print with its 
	-- unique (or something similar).  If we leave the original (Just Name)
	-- in there then error messages will say "can't match (T a) against (T a)"

inst_sig_tyvar (TyVar _ kind name _) 
  = tcGetUnique 		`thenNF_Tc` \ uniq ->

    tcNewMutVar UnBound		`thenNF_Tc` \ box ->
	-- Was DontBind, but we've nuked that "optimisation"

    returnNF_Tc (TyVar uniq kind name box)
	-- We propagate the name of the sigature type variable
\end{code}

@tcInstType@ and @tcInstSigType@ both create a fresh instance of a
type, returning a @TcType@. All inner for-alls are instantiated with
fresh TcTyVars.

The difference is that tcInstType instantiates all forall'd type
variables (and their bindees) with anonymous type variables, whereas
tcInstSigType instantiates them with named type variables.
@tcInstSigType@ also doesn't take an environment.

On the other hand, @tcInstTcType@ instantiates a TcType. It uses
instantiateTy which could take advantage of sharing some day.

\begin{code}
tcInstTcType :: TcType s -> NF_TcM s ([TcTyVar s], TcType s)
tcInstTcType ty
  = tcSplitForAllTy ty		`thenNF_Tc` \ (tyvars, rho) -> 
    case tyvars of
	[]    -> returnNF_Tc ([], ty)	-- Nothing to do
	other -> tcInstTyVars tyvars		`thenNF_Tc` \ (tyvars', _, tenv)  ->
		 returnNF_Tc (tyvars', instantiateTy tenv rho)

tcInstSigTcType :: TcType s -> NF_TcM s ([TcTyVar s], TcType s)
tcInstSigTcType ty
  = tcSplitForAllTy ty		`thenNF_Tc` \ (tyvars, rho) ->
    case tyvars of
	[]    -> returnNF_Tc ([], ty)	-- Nothing to do
	other -> tcInstSigTyVars tyvars		`thenNF_Tc` \ (tyvars', _, tenv)  ->
		 returnNF_Tc (tyvars', instantiateTy tenv rho)
    
tcInstType :: TyVarEnv (TcType s)
	   -> GenType flexi
	   -> NF_TcM s (TcType s)
tcInstType tenv ty_to_inst
  = tcConvert bind_fn occ_fn tenv ty_to_inst
  where
    bind_fn = inst_tyvar
    occ_fn env tyvar = case lookupTyVarEnv env tyvar of
			 Just ty -> returnNF_Tc ty
			 Nothing -> panic "tcInstType:1" --(vcat [ppr ty_to_inst, 
							--	      ppr tyvar])

tcInstSigType :: GenType flexi -> NF_TcM s (TcType s)
tcInstSigType ty_to_inst
  = tcConvert bind_fn occ_fn emptyTyVarEnv ty_to_inst
  where
    bind_fn = inst_sig_tyvar	-- Note: inst_sig_tyvar, not inst_tyvar
				-- I don't think that can lead to strange error messages
				-- of the form can't match (T a) against (T a)
				-- See notes with inst_tyvar

    occ_fn env tyvar = case lookupTyVarEnv env tyvar of
			 Just ty -> returnNF_Tc ty
			 Nothing -> panic "tcInstType:2"-- (vcat [ppr ty_to_inst, 
							--	      ppr tyvar])

zonkTcTyVarToTyVar :: TcTyVar s -> NF_TcM s TyVar
zonkTcTyVarToTyVar tv
  = zonkTcTyVar tv	`thenNF_Tc` \ tv_ty ->
    case tv_ty of	-- Should be a tyvar!

      TyVarTy tv' ->    returnNF_Tc (tcTyVarToTyVar tv')

      _ -> --pprTrace "zonkTcTyVarToTyVar:" (hsep [ppr tv, ppr tv_ty]) $
	   returnNF_Tc (tcTyVarToTyVar tv)


zonkTcTypeToType :: TyVarEnv Type -> TcType s -> NF_TcM s Type
zonkTcTypeToType env ty 
  = tcConvert zonkTcTyVarToTyVar occ_fn env ty
  where
    occ_fn env tyvar 
      =  tcReadTyVar tyvar	`thenNF_Tc` \ maybe_ty ->
	 case maybe_ty of
	   BoundTo (TyVarTy tyvar') -> lookup env tyvar'
	   BoundTo other_ty	    -> tcConvert zonkTcTyVarToTyVar occ_fn env other_ty
	   other		    -> lookup env tyvar

    lookup env tyvar = case lookupTyVarEnv env tyvar of
			  Just ty -> returnNF_Tc ty
			  Nothing -> returnNF_Tc voidTy	-- Unbound type variables go to Void


tcConvert bind_fn occ_fn env ty_to_convert
  = doo env ty_to_convert
  where
    doo env (TyConApp tycon tys) = mapNF_Tc (doo env) tys	`thenNF_Tc` \ tys' ->
				   returnNF_Tc (TyConApp tycon tys')

    doo env (SynTy ty1 ty2)      = doo env ty1			`thenNF_Tc` \ ty1' ->
				   doo env ty2			`thenNF_Tc` \ ty2' ->
				   returnNF_Tc (SynTy ty1' ty2')

    doo env (FunTy arg res)      = doo env arg		`thenNF_Tc` \ arg' ->
				   doo env res		`thenNF_Tc` \ res' ->
				   returnNF_Tc (FunTy arg' res')
 
    doo env (AppTy fun arg)	 = doo env fun		`thenNF_Tc` \ fun' ->
				   doo env arg		`thenNF_Tc` \ arg' ->
				   returnNF_Tc (mkAppTy fun' arg')

	-- The two interesting cases!
    doo env (TyVarTy tv) 	 = occ_fn env tv

    doo env (ForAllTy tyvar ty)
	= bind_fn tyvar		`thenNF_Tc` \ tyvar' ->
	  let
		new_env = addToTyVarEnv env tyvar (TyVarTy tyvar')
	  in
	  doo new_env ty		`thenNF_Tc` \ ty' ->
	  returnNF_Tc (ForAllTy tyvar' ty')


tcInstTheta :: TyVarEnv (TcType s) -> ThetaType -> NF_TcM s (TcThetaType s)
tcInstTheta tenv theta
  = mapNF_Tc go theta
  where
    go (clas,tys) = mapNF_Tc (tcInstType tenv) tys 	`thenNF_Tc` \ tc_tys ->
		    returnNF_Tc (clas, tc_tys)
\end{code}

Reading and writing TcTyVars
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
tcWriteTyVar :: TcTyVar s -> TcType s -> NF_TcM s ()
tcReadTyVar  :: TcTyVar s -> NF_TcM s (TcMaybe s)
\end{code}

Writing is easy:

\begin{code}
tcWriteTyVar (TyVar uniq kind name box) ty = tcWriteMutVar box (BoundTo ty)
\end{code}

Reading is more interesting.  The easy thing to do is just to read, thus:
\begin{verbatim}
tcReadTyVar (TyVar uniq kind name box) = tcReadMutVar box
\end{verbatim}

But it's more fun to short out indirections on the way: If this
version returns a TyVar, then that TyVar is unbound.  If it returns
any other type, then there might be bound TyVars embedded inside it.

We return Nothing iff the original box was unbound.

\begin{code}
tcReadTyVar (TyVar uniq kind name box)
  = tcReadMutVar box	`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	BoundTo ty -> short_out ty			`thenNF_Tc` \ ty' ->
		      tcWriteMutVar box (BoundTo ty')	`thenNF_Tc_`
		      returnNF_Tc (BoundTo ty')

	other	   -> returnNF_Tc other

short_out :: TcType s -> NF_TcM s (TcType s)
short_out ty@(TyVarTy (TyVar uniq kind name box))
  = tcReadMutVar box	`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	BoundTo ty' -> short_out ty' 			`thenNF_Tc` \ ty' ->
		       tcWriteMutVar box (BoundTo ty')	`thenNF_Tc_`
		       returnNF_Tc ty'

	other       -> returnNF_Tc ty

short_out other_ty = returnNF_Tc other_ty
\end{code}


Zonking
~~~~~~~
\begin{code}
zonkTcTyVars :: TcTyVarSet s -> NF_TcM s (TcTyVarSet s)
zonkTcTyVars tyvars
  = mapNF_Tc zonkTcTyVar (tyVarSetToList tyvars)	`thenNF_Tc` \ tys ->
    returnNF_Tc (tyVarsOfTypes tys)

zonkTcTyVar :: TcTyVar s -> NF_TcM s (TcType s)
zonkTcTyVar tyvar 
  = tcReadTyVar tyvar		`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	BoundTo ty@(TyVarTy tyvar') -> returnNF_Tc ty		-- tcReadTyVar never returns a bound tyvar
	BoundTo other		    -> zonkTcType other
	other			    -> returnNF_Tc (TyVarTy tyvar)

-- Signature type variables only get bound to each other,
-- never to a type
zonkSigTyVar :: TcTyVar s -> NF_TcM s (TcTyVar s)
zonkSigTyVar tyvar 
  = tcReadTyVar tyvar		`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	BoundTo ty@(TyVarTy tyvar') -> returnNF_Tc tyvar'	-- tcReadTyVar never returns a bound tyvar
	BoundTo other		    -> panic "zonkSigTyVar"	-- Should only be bound to another tyvar
	other			    -> returnNF_Tc tyvar

zonkTcTypes :: [TcType s] -> NF_TcM s [TcType s]
zonkTcTypes tys = mapNF_Tc zonkTcType tys

zonkTcThetaType :: TcThetaType s -> NF_TcM s (TcThetaType s)
zonkTcThetaType theta = mapNF_Tc zonk theta
		    where
		      zonk (c,ts) = zonkTcTypes ts	`thenNF_Tc` \ new_ts ->
				    returnNF_Tc (c, new_ts)

zonkTcType :: TcType s -> NF_TcM s (TcType s)

zonkTcType (TyVarTy tyvar) = zonkTcTyVar tyvar

zonkTcType (AppTy ty1 ty2)
  = zonkTcType ty1		`thenNF_Tc` \ ty1' ->
    zonkTcType ty2		`thenNF_Tc` \ ty2' ->
    returnNF_Tc (mkAppTy ty1' ty2')

zonkTcType (TyConApp tc tys)
  = mapNF_Tc zonkTcType tys	`thenNF_Tc` \ tys' ->
    returnNF_Tc (TyConApp tc tys')

zonkTcType (SynTy ty1 ty2)
  = zonkTcType ty1 		`thenNF_Tc` \ ty1' ->
    zonkTcType ty2 		`thenNF_Tc` \ ty2' ->
    returnNF_Tc (SynTy ty1' ty2')

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
