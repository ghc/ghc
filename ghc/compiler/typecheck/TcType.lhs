\begin{code}
module TcType (

  TcTyVar(..),
  newTcTyVar,
  newTyVarTy,	-- Kind -> NF_TcM s (TcType s)
  newTyVarTys,	-- Int -> Kind -> NF_TcM s [TcType s]


  TcTyVarSet(..),

  -----------------------------------------
  TcType(..), TcMaybe(..),
  TcTauType(..), TcThetaType(..), TcRhoType(..),

	-- Find the type to which a type variable is bound
  tcWriteTyVar,		-- :: TcTyVar s -> TcType s -> NF_TcM (TcType s)
  tcReadTyVar,		-- :: TcTyVar s -> NF_TcM (TcMaybe s)


  tcInstTyVars,    -- TyVar -> NF_TcM s (TcTyVar s)
  tcInstSigTyVars, 
  tcInstType, tcInstTcType, tcInstTheta, tcInstId,

    zonkTcTyVars,	-- TcTyVarSet s -> NF_TcM s (TcTyVarSet s)
    zonkTcType,		-- TcType s -> NF_TcM s (TcType s)
    zonkTcTypeToType,	-- TcType s -> NF_TcM s Type
    zonkTcTyVarToTyVar	-- TcTyVar s -> NF_TcM s TyVar

  ) where



-- friends:
import Type	( Type(..), ThetaType(..), GenType(..),
		  tyVarsOfTypes, getTyVar_maybe,
		  splitForAllTy, splitRhoTy
		)
import TyVar	( TyVar(..), GenTyVar(..), TyVarSet(..), GenTyVarSet(..), 
		  tyVarSetToList
		)

-- others:
import Class	( GenClass )
import Id	( idType )
import Kind	( Kind )
import TcKind	( TcKind )
import TcMonad
import Usage	( Usage(..), GenUsage, UVar(..), duffUsage )

import Ubiq
import Unique		( Unique )
import UniqFM		( UniqFM )
import Maybes		( assocMaybe )
import Util		( panic, pprPanic )

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
newTyVarTys n kind = mapNF_Tc newTyVarTy (take n (repeat kind))



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
    returnNF_Tc (tc_tyvars, tys, tyvars `zip` tys)

inst_tyvar initial_cts (TyVar _ kind name _) 
  = tcGetUnique 		`thenNF_Tc` \ uniq ->
    tcNewMutVar initial_cts	`thenNF_Tc` \ box ->
    returnNF_Tc (TyVar uniq kind name box)
\end{code}

@tcInstType@ and @tcInstTcType@ both create a fresh instance of a
type, returning a @TcType@. All inner for-alls are instantiated with
fresh TcTyVars.

There are two versions, one for instantiating a @Type@, and one for a @TcType@.
The former must instantiate everything; all tyvars must be bound either
by a forall or by an environment passed in.  The latter can do some sharing,
and is happy with free tyvars (which is vital when instantiating the type
of local functions).  In the future @tcInstType@ may try to be clever about not
instantiating constant sub-parts.

\begin{code}
tcInstType :: [(TyVar,TcType s)] -> Type  -> NF_TcM s (TcType s)
tcInstType tenv ty_to_inst
  = do [(uniq,ty) | (TyVar uniq _ _ _, ty) <- tenv] ty_to_inst
  where
    do env (TyConTy tycon usage) = returnNF_Tc (TyConTy tycon usage)

    do env (SynTy tycon tys ty)  = mapNF_Tc (do env) tys	`thenNF_Tc` \ tys' ->
				   do env ty			`thenNF_Tc` \ ty' ->
				   returnNF_Tc (SynTy tycon tys' ty')

    do env (FunTy arg res usage) = do env arg		`thenNF_Tc` \ arg' ->
				   do env res		`thenNF_Tc` \ res' ->
				   returnNF_Tc (FunTy arg' res' usage)

    do env (AppTy fun arg)	 = do env fun		`thenNF_Tc` \ fun' ->
				   do env arg		`thenNF_Tc` \ arg' ->
				   returnNF_Tc (AppTy fun' arg')

    do env (DictTy clas ty usage)= do env ty		`thenNF_Tc` \ ty' ->
				   returnNF_Tc (DictTy clas ty' usage)

    do env (TyVarTy tv@(TyVar uniq kind name _))
	= case assocMaybe env uniq of
		Just tc_ty -> returnNF_Tc tc_ty
		Nothing    -> pprPanic "tcInstType:" (ppAboves [ppr PprDebug tenv, 
					      ppr PprDebug ty_to_inst, ppr PprDebug tv])

    do env (ForAllTy tyvar@(TyVar uniq kind name _) ty)
	= inst_tyvar DontBind tyvar 	`thenNF_Tc` \ tc_tyvar ->
	  let
		new_env = (uniq, TyVarTy tc_tyvar) : env
	  in
	  do new_env ty	`thenNF_Tc` \ ty' ->
	  returnNF_Tc (ForAllTy tc_tyvar ty')

   -- ForAllUsage impossible


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


tcInstTcType ::  [(TcTyVar s,TcType s)] -> TcType s -> NF_TcM s (TcType s)
tcInstTcType tenv ty_to_inst
  = do [(uniq,ty) | (TyVar uniq _ _ _, ty) <- tenv] ty_to_inst
  where
    do env ty@(TyConTy tycon usage) = returnNF_Tc ty

-- Could do clever stuff here to avoid instantiating constant types
    do env (SynTy tycon tys ty)  = mapNF_Tc (do env) tys	`thenNF_Tc` \ tys' ->
				   do env ty			`thenNF_Tc` \ ty' ->
				   returnNF_Tc (SynTy tycon tys' ty')

    do env (FunTy arg res usage)  = do env arg		`thenNF_Tc` \ arg' ->
				    do env res		`thenNF_Tc` \ res' ->
				    returnNF_Tc (FunTy arg' res' usage)

    do env (AppTy fun arg)	  = do env fun		`thenNF_Tc` \ fun' ->
				    do env arg		`thenNF_Tc` \ arg' ->
				    returnNF_Tc (AppTy fun' arg')

    do env (DictTy clas ty usage)= do env ty		`thenNF_Tc` \ ty' ->
				   returnNF_Tc (DictTy clas ty' usage)

    do env ty@(TyVarTy (TyVar uniq kind name _))
	= case assocMaybe env uniq of
		Just tc_ty -> returnNF_Tc tc_ty
		Nothing    -> returnNF_Tc ty

    do env (ForAllTy (TyVar uniq kind name _) ty) = panic "tcInstTcType"

   -- ForAllUsage impossible

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
@zonkTcTypeToType@ converts from @TcType@ to @Type@.  It follows through all
the substitutions of course.

\begin{code}
zonkTcTypeToType :: TcType s -> NF_TcM s Type
zonkTcTypeToType ty = zonk tcTyVarToTyVar ty

zonkTcType :: TcType s -> NF_TcM s (TcType s)
zonkTcType ty = zonk (\tyvar -> tyvar) ty

zonkTcTyVars :: TcTyVarSet s -> NF_TcM s (TcTyVarSet s)
zonkTcTyVars tyvars
  = mapNF_Tc (zonk_tv (\tyvar -> tyvar)) 
	     (tyVarSetToList tyvars)		`thenNF_Tc` \ tys ->
    returnNF_Tc (tyVarsOfTypes tys)

zonkTcTyVarToTyVar :: TcTyVar s -> NF_TcM s TyVar
zonkTcTyVarToTyVar tyvar
  = zonk_tv_to_tv tcTyVarToTyVar tyvar


zonk tyvar_fn (TyVarTy tyvar)
  = zonk_tv tyvar_fn tyvar

zonk tyvar_fn (AppTy ty1 ty2)
  = zonk tyvar_fn ty1		`thenNF_Tc` \ ty1' ->
    zonk tyvar_fn ty2		`thenNF_Tc` \ ty2' ->
    returnNF_Tc (AppTy ty1' ty2')

zonk tyvar_fn (TyConTy tc u)
  = returnNF_Tc (TyConTy tc u)

zonk tyvar_fn (SynTy tc tys ty)
  = mapNF_Tc (zonk tyvar_fn) tys `thenNF_Tc` \ tys' ->
    zonk tyvar_fn ty 		 `thenNF_Tc` \ ty' ->
    returnNF_Tc (SynTy tc tys' ty')

zonk tyvar_fn (ForAllTy tv ty)
  = zonk_tv_to_tv tyvar_fn tv	`thenNF_Tc` \ tv' ->
    zonk tyvar_fn ty 		`thenNF_Tc` \ ty' ->
    returnNF_Tc (ForAllTy tv' ty')

zonk tyvar_fn (ForAllUsageTy uv uvs ty)
  = panic "zonk:ForAllUsageTy"

zonk tyvar_fn (FunTy ty1 ty2 u)
  = zonk tyvar_fn ty1 		`thenNF_Tc` \ ty1' ->
    zonk tyvar_fn ty2 		`thenNF_Tc` \ ty2' ->
    returnNF_Tc (FunTy ty1' ty2' u)

zonk tyvar_fn (DictTy c ty u)
  = zonk tyvar_fn ty 		`thenNF_Tc` \ ty' ->
    returnNF_Tc (DictTy c ty' u)


zonk_tv tyvar_fn tyvar
  = tcReadTyVar tyvar		`thenNF_Tc` \ maybe_ty ->
    case maybe_ty of
	BoundTo ty -> zonk tyvar_fn ty
	other      -> returnNF_Tc (TyVarTy (tyvar_fn tyvar))


zonk_tv_to_tv tyvar_fn tyvar
  = zonk_tv tyvar_fn tyvar	`thenNF_Tc` \ ty ->
    case getTyVar_maybe ty of
	Nothing    -> panic "zonk_tv_to_tv"
	Just tyvar -> returnNF_Tc tyvar
\end{code}
