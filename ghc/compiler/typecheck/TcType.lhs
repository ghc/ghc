\begin{code}
#include "HsVersions.h"

module TcType (

  SYN_IE(TcTyVar),
  newTcTyVar,
  newTyVarTy,	-- Kind -> NF_TcM s (TcType s)
  newTyVarTys,	-- Int -> Kind -> NF_TcM s [TcType s]


  SYN_IE(TcTyVarSet),

  -----------------------------------------
  SYN_IE(TcType), TcMaybe(..),
  SYN_IE(TcTauType), SYN_IE(TcThetaType), SYN_IE(TcRhoType),

	-- Find the type to which a type variable is bound
  tcWriteTyVar,		-- :: TcTyVar s -> TcType s -> NF_TcM (TcType s)
  tcReadTyVar,		-- :: TcTyVar s -> NF_TcM (TcMaybe s)


  tcInstTyVars,
  tcInstSigTyVars, 
  tcInstType, tcInstSigType, tcInstTcType, tcInstSigTcType,
  tcInstTheta, tcInstId,

  zonkTcTyVars,
  zonkTcType,
  zonkTcTypeToType,
  zonkTcTyVar,
  zonkTcTyVarToTyVar

  ) where



-- friends:
import Type	( SYN_IE(Type), SYN_IE(ThetaType), GenType(..),
		  tyVarsOfTypes, getTyVar_maybe,
		  splitForAllTy, splitRhoTy,
		  mkForAllTys, instantiateTy
		)
import TyVar	( SYN_IE(TyVar), GenTyVar(..), SYN_IE(TyVarSet), SYN_IE(GenTyVarSet), 
		  SYN_IE(TyVarEnv), lookupTyVarEnv, addOneToTyVarEnv,
		  nullTyVarEnv, mkTyVarEnv,
		  tyVarSetToList
		)

-- others:
import Class	( GenClass )
import Id	( idType )
import Kind	( Kind )
import TcKind	( TcKind )
import TcMonad	hiding ( rnMtoTcM )
import Usage	( SYN_IE(Usage), GenUsage, SYN_IE(UVar), duffUsage )

import TysPrim		( voidTy )

IMP_Ubiq()
import Unique		( Unique )
import UniqFM		( UniqFM )
import Maybes		( assocMaybe )
import Util		( zipEqual, nOfThem, panic, pprPanic, pprTrace{-ToDo:rm-} )

import Outputable	( Outputable(..) )	-- Debugging messages
import PprType		( GenTyVar, GenType )
import Pretty					-- ditto
import PprStyle		( PprStyle(..) )	-- ditto
\end{code}



Data types
~~~~~~~~~~

\begin{code}
type TcType s = GenType (TcTyVar s) UVar	-- Used during typechecker
	-- Invariant on ForAllTy in TcTypes:
	-- 	forall a. T
	-- a cannot occur inside a MutTyVar in T; that is,
	-- T is "flattened" before quantifying over a

type TcThetaType s = [(Class, TcType s)]
type TcRhoType s   = TcType s		-- No ForAllTys
type TcTauType s   = TcType s		-- No DictTys or ForAllTys

type Box s = MutableVar s (TcMaybe s)

data TcMaybe s = UnBound
	       | BoundTo (TcType s)
	       | DontBind		-- This variant is used for tyvars
					-- arising from type signatures, or
					-- existentially quantified tyvars;
					-- The idea is that we must not unify
					-- such tyvars with anything except
					-- themselves.

-- Interestingly, you can't use (Maybe (TcType s)) instead of (TcMaybe s),
-- because you get a synonym loop if you do!

type TcTyVar s    = GenTyVar (Box s)
type TcTyVarSet s = GenTyVarSet (Box s)
\end{code}

\begin{code}
tcTyVarToTyVar :: TcTyVar s -> TyVar
tcTyVarToTyVar (TyVar uniq kind name _) = TyVar uniq kind name duffUsage
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


-- For signature type variables, mark them as "DontBind"
tcInstTyVars, tcInstSigTyVars
	:: [GenTyVar flexi] 
  	-> NF_TcM s ([TcTyVar s], [TcType s], [(GenTyVar flexi, TcType s)])

tcInstTyVars    tyvars = inst_tyvars UnBound  tyvars
tcInstSigTyVars tyvars = inst_tyvars DontBind tyvars

inst_tyvars initial_cts tyvars
  = mapNF_Tc (inst_tyvar initial_cts) tyvars	`thenNF_Tc` \ tc_tyvars ->
    let
	tys = map TyVarTy tc_tyvars
    in
    returnNF_Tc (tc_tyvars, tys, zipEqual "inst_tyvars" tyvars tys)

inst_tyvar initial_cts (TyVar _ kind name _) 
  = tcGetUnique 		`thenNF_Tc` \ uniq ->
    tcNewMutVar initial_cts	`thenNF_Tc` \ box ->
    returnNF_Tc (TyVar uniq kind name box)
\end{code}

@tcInstType@ and @tcInstSigType@ both create a fresh instance of a
type, returning a @TcType@. All inner for-alls are instantiated with
fresh TcTyVars.

The difference is that tcInstType instantiates all forall'd type
variables (and their bindees) with UnBound type variables, whereas
tcInstSigType instantiates them with DontBind types variables.
@tcInstSigType@ also doesn't take an environment.

On the other hand, @tcInstTcType@ instantiates a TcType. It uses
instantiateTy which could take advantage of sharing some day.

\begin{code}
tcInstTcType :: TcType s -> NF_TcM s ([TcTyVar s], TcType s)
tcInstTcType ty
  = case tyvars of
	[]    -> returnNF_Tc ([], ty)	-- Nothing to do
	other -> tcInstTyVars tyvars		`thenNF_Tc` \ (tyvars', _, tenv)  ->
		 returnNF_Tc (tyvars', instantiateTy tenv rho)
  where
    (tyvars, rho) = splitForAllTy ty

tcInstSigTcType :: TcType s -> NF_TcM s ([TcTyVar s], TcType s)
tcInstSigTcType ty
  = case tyvars of
	[]    -> returnNF_Tc ([], ty)	-- Nothing to do
	other -> tcInstSigTyVars tyvars		`thenNF_Tc` \ (tyvars', _, tenv)  ->
		 returnNF_Tc (tyvars', instantiateTy tenv rho)
  where
    (tyvars, rho) = splitForAllTy ty

tcInstType :: [(GenTyVar flexi,TcType s)] 
	   -> GenType (GenTyVar flexi) UVar 
	   -> NF_TcM s (TcType s)
tcInstType tenv ty_to_inst
  = tcConvert bind_fn occ_fn (mkTyVarEnv tenv) ty_to_inst
  where
    bind_fn = inst_tyvar UnBound
    occ_fn env tyvar = case lookupTyVarEnv env tyvar of
			 Just ty -> returnNF_Tc ty
			 Nothing -> pprPanic "tcInstType:" (ppAboves [ppr PprDebug ty_to_inst, 
								      ppr PprDebug tyvar])

tcInstSigType :: GenType (GenTyVar flexi) UVar -> NF_TcM s (TcType s)
tcInstSigType ty_to_inst
  = tcConvert bind_fn occ_fn nullTyVarEnv ty_to_inst
  where
    bind_fn = inst_tyvar DontBind
    occ_fn env tyvar = case lookupTyVarEnv env tyvar of
			 Just ty -> returnNF_Tc ty
			 Nothing -> pprPanic "tcInstType:" (ppAboves [ppr PprDebug ty_to_inst, 
								      ppr PprDebug tyvar])

zonkTcTyVarToTyVar :: TcTyVar s -> NF_TcM s TyVar
zonkTcTyVarToTyVar tv
  = zonkTcTyVar tv	`thenNF_Tc` \ tv_ty ->
    case tv_ty of	-- Should be a tyvar!

      TyVarTy tv' ->    returnNF_Tc (tcTyVarToTyVar tv')

      _ -> pprTrace "zonkTcTyVarToTyVar:" (ppCat [ppr PprDebug tv, ppr PprDebug tv_ty]) $
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
    doo env (TyConTy tycon usage) = returnNF_Tc (TyConTy tycon usage)

    doo env (SynTy tycon tys ty)  = mapNF_Tc (doo env) tys	`thenNF_Tc` \ tys' ->
				   doo env ty			`thenNF_Tc` \ ty' ->
				   returnNF_Tc (SynTy tycon tys' ty')

    doo env (FunTy arg res usage) = doo env arg		`thenNF_Tc` \ arg' ->
				   doo env res		`thenNF_Tc` \ res' ->
				   returnNF_Tc (FunTy arg' res' usage)

    doo env (AppTy fun arg)	 = doo env fun		`thenNF_Tc` \ fun' ->
				   doo env arg		`thenNF_Tc` \ arg' ->
				   returnNF_Tc (AppTy fun' arg')

    doo env (DictTy clas ty usage)= doo env ty		`thenNF_Tc` \ ty' ->
				   returnNF_Tc (DictTy clas ty' usage)

    doo env (ForAllUsageTy u us ty) = doo env ty	`thenNF_Tc` \ ty' ->
				     returnNF_Tc (ForAllUsageTy u us ty')

	-- The two interesting cases!
    doo env (TyVarTy tv) 	 = occ_fn env tv

    doo env (ForAllTy tyvar ty)
	= bind_fn tyvar		`thenNF_Tc` \ tyvar' ->
	  let
		new_env = addOneToTyVarEnv env tyvar (TyVarTy tyvar')
	  in
	  doo new_env ty		`thenNF_Tc` \ ty' ->
	  returnNF_Tc (ForAllTy tyvar' ty')


tcInstTheta :: [(TyVar,TcType s)] -> ThetaType -> NF_TcM s (TcThetaType s)
tcInstTheta tenv theta
  = mapNF_Tc go theta
  where
    go (clas,ty) = tcInstType tenv ty 	`thenNF_Tc` \ tc_ty ->
		   returnNF_Tc (clas, tc_ty)

-- A useful function that takes an occurrence of a global thing
-- and instantiates its type with fresh type variables
tcInstId :: Id
	 -> NF_TcM s ([TcTyVar s], 	-- It's instantiated type
		      TcThetaType s,	--
		      TcType s)		--

tcInstId id
  = let
      (tyvars, rho) = splitForAllTy (idType id)
    in
    tcInstTyVars tyvars		`thenNF_Tc` \ (tyvars', arg_tys, tenv) ->
    tcInstType tenv rho		`thenNF_Tc` \ rho' ->
    let
	(theta', tau') = splitRhoTy rho'
    in
    returnNF_Tc (tyvars', theta', tau')
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
	BoundTo ty@(TyVarTy tyvar') -> returnNF_Tc ty
	BoundTo other		    -> zonkTcType other
	other			    -> returnNF_Tc (TyVarTy tyvar)

zonkTcType :: TcType s -> NF_TcM s (TcType s)

zonkTcType (TyVarTy tyvar) = zonkTcTyVar tyvar

zonkTcType (AppTy ty1 ty2)
  = zonkTcType ty1		`thenNF_Tc` \ ty1' ->
    zonkTcType ty2		`thenNF_Tc` \ ty2' ->
    returnNF_Tc (AppTy ty1' ty2')

zonkTcType (TyConTy tc u)
  = returnNF_Tc (TyConTy tc u)

zonkTcType (SynTy tc tys ty)
  = mapNF_Tc zonkTcType tys	`thenNF_Tc` \ tys' ->
    zonkTcType ty 		`thenNF_Tc` \ ty' ->
    returnNF_Tc (SynTy tc tys' ty')

zonkTcType (ForAllTy tv ty)
  = zonkTcTyVar tv		`thenNF_Tc` \ tv_ty ->
    zonkTcType ty 		`thenNF_Tc` \ ty' ->
    case tv_ty of	-- Should be a tyvar!
      TyVarTy tv' -> 
		     returnNF_Tc (ForAllTy tv' ty')
      _ -> pprTrace "zonkTcType:ForAllTy:" (ppCat [ppr PprDebug tv, ppr PprDebug tv_ty]) $
	   
	   returnNF_Tc (ForAllTy tv{-(tcTyVarToTyVar tv)-} ty')

zonkTcType (ForAllUsageTy uv uvs ty)
  = panic "zonk:ForAllUsageTy"

zonkTcType (FunTy ty1 ty2 u)
  = zonkTcType ty1 		`thenNF_Tc` \ ty1' ->
    zonkTcType ty2 		`thenNF_Tc` \ ty2' ->
    returnNF_Tc (FunTy ty1' ty2' u)

zonkTcType (DictTy c ty u)
  = zonkTcType ty 		`thenNF_Tc` \ ty' ->
    returnNF_Tc (DictTy c ty' u)
\end{code}
