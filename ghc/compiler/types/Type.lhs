\begin{code}
#include "HsVersions.h"

module Type (
	GenType(..), SYN_IE(Type), SYN_IE(TauType),
	mkTyVarTy, mkTyVarTys,
	getTyVar, getTyVar_maybe, isTyVarTy,
	mkAppTy, mkAppTys, splitAppTy,
	mkFunTy, mkFunTys,
	splitFunTy, splitFunTyExpandingDicts, splitFunTyExpandingDictsAndPeeking,
	getFunTy_maybe, getFunTyExpandingDicts_maybe,
	mkTyConTy, getTyCon_maybe, applyTyCon,
	mkSynTy,
	mkForAllTy, mkForAllTys, getForAllTy_maybe, getForAllTyExpandingDicts_maybe, splitForAllTy,
	mkForAllUsageTy, getForAllUsageTy,
	applyTy,
#ifdef DEBUG
	expandTy, -- only let out for debugging (ToDo: rm?)
#endif
	isPrimType, isUnboxedType, typePrimRep,

	SYN_IE(RhoType), SYN_IE(SigmaType), SYN_IE(ThetaType),
	mkDictTy,
	mkRhoTy, splitRhoTy, mkTheta, isDictTy,
	mkSigmaTy, splitSigmaTy,

	maybeAppTyCon, getAppTyCon,
	maybeAppDataTyCon, getAppDataTyCon, getAppSpecDataTyCon,
	maybeAppDataTyConExpandingDicts, maybeAppSpecDataTyConExpandingDicts,
	getAppDataTyConExpandingDicts,  getAppSpecDataTyConExpandingDicts,
	maybeBoxedPrimType,

	matchTy, matchTys, eqTy, eqSimpleTy, eqSimpleTheta,

	instantiateTy, instantiateTauTy, instantiateUsage,
	applyTypeEnvToTy,

	isTauTy,

	tyVarsOfType, tyVarsOfTypes, typeKind
    ) where

IMP_Ubiq()
--IMPORT_DELOOPER(IdLoop)	 -- for paranoia checking
IMPORT_DELOOPER(TyLoop)
--IMPORT_DELOOPER(PrelLoop)  -- for paranoia checking

-- friends:
import Class	( classSig, classOpLocalType, GenClass{-instances-} )
import Kind	( mkBoxedTypeKind, resultKind, notArrowKind, Kind )
import TyCon	( mkFunTyCon, mkTupleTyCon, isFunTyCon,
		  isPrimTyCon, isDataTyCon, isSynTyCon, maybeNewTyCon, isNewTyCon,
		  tyConKind, tyConDataCons, getSynTyConDefn, TyCon )
import TyVar	( tyVarKind, GenTyVar{-instances-}, SYN_IE(GenTyVarSet),
		  emptyTyVarSet, unionTyVarSets, minusTyVarSet,
		  unitTyVarSet, nullTyVarEnv, lookupTyVarEnv, delFromTyVarEnv,
		  addOneToTyVarEnv, SYN_IE(TyVarEnv), SYN_IE(TyVar) )
import Usage	( usageOmega, GenUsage, SYN_IE(Usage), SYN_IE(UVar), SYN_IE(UVarEnv),
		  nullUVarEnv, addOneToUVarEnv, lookupUVarEnv, eqUVar,
		  eqUsage )

-- others
import Maybes	( maybeToBool, assocMaybe )
import PrimRep	( PrimRep(..) )
import Unique	-- quite a few *Keys
import Util	( thenCmp, zipEqual, assoc,
		  panic, panic#, assertPanic,
		  Ord3(..){-instances-}
		)
-- ToDo:rm all these
--import	{-mumble-}
--	Pretty
--import  {-mumble-}
--	PprStyle
--import	{-mumble-}
--	PprType --(pprType )
--import  {-mumble-}
--	UniqFM (ufmToList )
--import {-mumble-}
--	Outputable
\end{code}

Data types
~~~~~~~~~~

\begin{code}
type Type  = GenType TyVar UVar	-- Used after typechecker

data GenType tyvar uvar	-- Parameterised over type and usage variables
  = TyVarTy tyvar

  | AppTy
	(GenType tyvar uvar)
	(GenType tyvar uvar)

  | TyConTy 	-- Constants of a specified kind
	TyCon 	-- Must *not* be a SynTyCon
	(GenUsage uvar)	-- Usage gives uvar of the full application,
			-- iff the full application is of kind Type
			-- c.f. the Usage field in TyVars

  | SynTy 	-- Synonyms must be saturated, and contain their expansion
	TyCon	-- Must be a SynTyCon
	[GenType tyvar uvar]
	(GenType tyvar uvar)	-- Expansion!

  | ForAllTy
	tyvar
	(GenType tyvar uvar)	-- TypeKind

  | ForAllUsageTy
	uvar			-- Quantify over this
	[uvar]			-- Bounds; the quantified var must be
				-- less than or equal to all these
	(GenType tyvar uvar)

	-- Two special cases that save a *lot* of administrative
	-- overhead:

  | FunTy			-- BoxedTypeKind
	(GenType tyvar uvar)	-- Both args are of TypeKind
	(GenType tyvar uvar)
	(GenUsage uvar)

  | DictTy			-- TypeKind
	Class			-- Class
	(GenType tyvar uvar)	-- Arg has kind TypeKind
	(GenUsage uvar)
\end{code}

\begin{code}
type RhoType   = Type
type TauType   = Type
type ThetaType = [(Class, Type)]
type SigmaType = Type
\end{code}


Expand abbreviations
~~~~~~~~~~~~~~~~~~~~
Removes just the top level of any abbreviations.

\begin{code}
expandTy :: Type -> Type	-- Restricted to Type due to Dict expansion

expandTy (FunTy t1 t2 u) = AppTy (AppTy (TyConTy mkFunTyCon u) t1) t2
expandTy (SynTy _  _  t) = expandTy t
expandTy (DictTy clas ty u)
  = case all_arg_tys of

	[]	 -> voidTy		-- Empty dictionary represented by Void

	[arg_ty] -> expandTy arg_ty	-- just the <whatever> itself

		-- The extra expandTy is to make sure that
		-- the result isn't still a dict, which it might be
		-- if the original guy was a dict with one superdict and
		-- no methods!

	other -> ASSERT(not (null all_arg_tys))
	    	foldl AppTy (TyConTy (mkTupleTyCon (length all_arg_tys)) u) all_arg_tys

		-- A tuple of 'em
		-- Note: length of all_arg_tys can be 0 if the class is
		--       CCallable, CReturnable (and anything else
		--       *really weird* that the user writes).
  where
    (tyvar, super_classes, ops) = classSig clas
    super_dict_tys = map mk_super_ty super_classes
    class_op_tys   = map mk_op_ty ops
    all_arg_tys    = super_dict_tys ++ class_op_tys
    mk_super_ty sc = DictTy sc ty usageOmega
    mk_op_ty	op = instantiateTy [(tyvar,ty)] (classOpLocalType op)

expandTy ty = ty
\end{code}

Simple construction and analysis functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
mkTyVarTy  :: t   -> GenType t u
mkTyVarTys :: [t] -> [GenType t y]
mkTyVarTy  = TyVarTy
mkTyVarTys = map mkTyVarTy -- a common use of mkTyVarTy

getTyVar :: String -> GenType t u -> t
getTyVar msg (TyVarTy tv)   = tv
getTyVar msg (SynTy _ _ t)  = getTyVar msg t
getTyVar msg other	    = panic ("getTyVar: " ++ msg)

getTyVar_maybe :: GenType t u -> Maybe t
getTyVar_maybe (TyVarTy tv)  = Just tv
getTyVar_maybe (SynTy _ _ t) = getTyVar_maybe t
getTyVar_maybe other	     = Nothing

isTyVarTy :: GenType t u -> Bool
isTyVarTy (TyVarTy tv)  = True
isTyVarTy (SynTy _ _ t) = isTyVarTy t
isTyVarTy other = False
\end{code}

\begin{code}
mkAppTy = AppTy

mkAppTys :: GenType t u -> [GenType t u] -> GenType t u
mkAppTys t ts = foldl AppTy t ts

splitAppTy :: GenType t u -> (GenType t u, [GenType t u])
splitAppTy t = go t []
  where
    go (AppTy t arg)     ts = go t (arg:ts)
    go (FunTy fun arg u) ts = (TyConTy mkFunTyCon u, fun:arg:ts)
    go (SynTy _ _ t)     ts = go t ts
    go t		 ts = (t,ts)
\end{code}

\begin{code}
-- NB mkFunTy, mkFunTys puts in Omega usages, for now at least
mkFunTy arg res = FunTy arg res usageOmega

mkFunTys :: [GenType t u] -> GenType t u -> GenType t u
mkFunTys ts t = foldr (\ f a -> FunTy f a usageOmega) t ts

  -- getFunTy_maybe and splitFunTy *must* have the general type given, which
  -- means they *can't* do the DictTy jiggery-pokery that
  -- *is* sometimes required.  Hence we also have the ExpandingDicts variants
  -- The relationship between these
  -- two functions is like that between eqTy and eqSimpleTy.
  -- ToDo: NUKE when we do dicts via newtype

getFunTy_maybe :: GenType t u -> Maybe (GenType t u, GenType t u)
getFunTy_maybe (FunTy arg result _) = Just (arg,result)
getFunTy_maybe (AppTy (AppTy (TyConTy tycon _) arg) res)
	       	 | isFunTyCon tycon = Just (arg, res)
getFunTy_maybe (SynTy _ _ t)        = getFunTy_maybe t
getFunTy_maybe other		    = Nothing

getFunTyExpandingDicts_maybe :: Bool -- True <=> peek inside newtype applicatons
			     -> Type
			     -> Maybe (Type, Type)

getFunTyExpandingDicts_maybe peek (FunTy arg result _) = Just (arg,result)
getFunTyExpandingDicts_maybe peek
	(AppTy (AppTy (TyConTy tycon _) arg) res) | isFunTyCon tycon = Just (arg, res)
getFunTyExpandingDicts_maybe peek (SynTy _ _ t)	    = getFunTyExpandingDicts_maybe peek t
getFunTyExpandingDicts_maybe peek ty@(DictTy _ _ _) = getFunTyExpandingDicts_maybe peek (expandTy ty)
getFunTyExpandingDicts_maybe peek other
  | not peek = Nothing -- that was easy
  | otherwise
  = case (maybeAppTyCon other) of
      Nothing -> Nothing
      Just (tc, arg_tys)
        | not (isNewTyCon tc) -> Nothing
	| otherwise ->
	  let
	     [newtype_con] = tyConDataCons tc -- there must be exactly one...
	     [inside_ty]   = dataConArgTys newtype_con arg_tys
	  in
	  getFunTyExpandingDicts_maybe peek inside_ty

splitFunTy			   :: GenType t u -> ([GenType t u], GenType t u)
splitFunTyExpandingDicts	   :: Type	  -> ([Type], Type)
splitFunTyExpandingDictsAndPeeking :: Type	  -> ([Type], Type)

splitFunTy		           t = split_fun_ty getFunTy_maybe			 t
splitFunTyExpandingDicts           t = split_fun_ty (getFunTyExpandingDicts_maybe False) t
splitFunTyExpandingDictsAndPeeking t = split_fun_ty (getFunTyExpandingDicts_maybe True)  t

split_fun_ty get t = go t []
  where
    go t ts = case (get t) of
		Just (arg,res) -> go res (arg:ts)
		Nothing	       -> (reverse ts, t)
\end{code}

\begin{code}
-- NB applyTyCon puts in usageOmega, for now at least
mkTyConTy tycon
  = ASSERT(not (isSynTyCon tycon))
    TyConTy tycon usageOmega

applyTyCon :: TyCon -> [GenType t u] -> GenType t u
applyTyCon tycon tys
  = ASSERT (not (isSynTyCon tycon))
    --(if (not (isSynTyCon tycon)) then \x->x else pprTrace "applyTyCon:" (pprTyCon PprDebug tycon)) $
    foldl AppTy (TyConTy tycon usageOmega) tys

getTyCon_maybe		     :: GenType t u -> Maybe TyCon
--getTyConExpandingDicts_maybe :: Type        -> Maybe TyCon

getTyCon_maybe (TyConTy tycon _) = Just tycon
getTyCon_maybe (SynTy _ _ t)     = getTyCon_maybe t
getTyCon_maybe other_ty		 = Nothing

--getTyConExpandingDicts_maybe (TyConTy tycon _) = Just tycon
--getTyConExpandingDicts_maybe (SynTy _ _ t)     = getTyConExpandingDicts_maybe t
--getTyConExpandingDicts_maybe ty@(DictTy _ _ _) = getTyConExpandingDicts_maybe (expandTy ty)
--getTyConExpandingDicts_maybe other_ty	       = Nothing
\end{code}

\begin{code}
mkSynTy syn_tycon tys
  = ASSERT(isSynTyCon syn_tycon)
    SynTy syn_tycon tys (instantiateTauTy (zipEqual "mkSynTy" tyvars tys) body)
  where
    (tyvars, body) = getSynTyConDefn syn_tycon
\end{code}

Tau stuff
~~~~~~~~~
\begin{code}
isTauTy :: GenType t u -> Bool
isTauTy (TyVarTy v)        = True
isTauTy (TyConTy _ _)      = True
isTauTy (AppTy a b)        = isTauTy a && isTauTy b
isTauTy (FunTy a b _)      = isTauTy a && isTauTy b
isTauTy (SynTy _ _ ty)     = isTauTy ty
isTauTy other		   = False
\end{code}

Rho stuff
~~~~~~~~~
NB mkRhoTy and mkDictTy put in usageOmega, for now at least

\begin{code}
mkDictTy :: Class -> GenType t u -> GenType t u
mkDictTy clas ty = DictTy clas ty usageOmega

mkRhoTy :: [(Class, GenType t u)] -> GenType t u -> GenType t u
mkRhoTy theta ty =
  foldr (\(c,t) r -> FunTy (DictTy c t usageOmega) r usageOmega) ty theta

splitRhoTy :: GenType t u -> ([(Class,GenType t u)], GenType t u)
splitRhoTy t =
  go t []
 where
  go (FunTy (DictTy c t _) r _) ts = go r ((c,t):ts)
  go (AppTy (AppTy (TyConTy tycon _) (DictTy c t _)) r) ts
	| isFunTyCon tycon
	= go r ((c,t):ts)
  go (SynTy _ _ t) ts = go t ts
  go t ts = (reverse ts, t)


mkTheta :: [Type] -> ThetaType
    -- recover a ThetaType from the types of some dictionaries
mkTheta dict_tys
  = map cvt dict_tys
  where
    cvt (DictTy clas ty _) = (clas, ty)
    cvt other		   = panic "Type.mkTheta" -- pprPanic "mkTheta:" (pprType PprDebug other)

isDictTy (DictTy _ _ _) = True
isDictTy (SynTy  _ _ t) = isDictTy t
isDictTy _		= False
\end{code}


Forall stuff
~~~~~~~~~~~~
\begin{code}
mkForAllTy = ForAllTy

mkForAllTys :: [t] -> GenType t u -> GenType t u
mkForAllTys tyvars ty = foldr ForAllTy ty tyvars

getForAllTy_maybe :: GenType t u -> Maybe (t,GenType t u)
getForAllTy_maybe (SynTy _ _ t)	     = getForAllTy_maybe t
getForAllTy_maybe (ForAllTy tyvar t) = Just(tyvar,t)
getForAllTy_maybe _		     = Nothing

getForAllTyExpandingDicts_maybe :: Type -> Maybe (TyVar, Type)
getForAllTyExpandingDicts_maybe (SynTy _ _ t)	   = getForAllTyExpandingDicts_maybe t
getForAllTyExpandingDicts_maybe (ForAllTy tyvar t) = Just(tyvar,t)
getForAllTyExpandingDicts_maybe ty@(DictTy _ _ _)  = getForAllTyExpandingDicts_maybe (expandTy ty)
getForAllTyExpandingDicts_maybe _		   = Nothing

splitForAllTy :: GenType t u-> ([t], GenType t u)
splitForAllTy t = go t []
	       where
		    go (ForAllTy tv t) tvs = go t (tv:tvs)
		    go (SynTy _ _ t)   tvs = go t tvs
		    go t	       tvs = (reverse tvs, t)
\end{code}

\begin{code}
mkForAllUsageTy :: u -> [u] -> GenType t u -> GenType t u
mkForAllUsageTy = ForAllUsageTy

getForAllUsageTy :: GenType t u -> Maybe (u,[u],GenType t u)
getForAllUsageTy (ForAllUsageTy uvar bounds t) = Just(uvar,bounds,t)
getForAllUsageTy (SynTy _ _ t) = getForAllUsageTy t
getForAllUsageTy _ = Nothing
\end{code}

Applied tycons (includes FunTyCons)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
maybeAppTyCon
	:: GenType tyvar uvar
	-> Maybe (TyCon,		-- the type constructor
		  [GenType tyvar uvar])	-- types to which it is applied

maybeAppTyCon ty
  = case (getTyCon_maybe app_ty) of
	Nothing    -> Nothing
	Just tycon -> Just (tycon, arg_tys)
  where
    (app_ty, arg_tys) = splitAppTy ty


getAppTyCon
	:: GenType tyvar uvar
	-> (TyCon,			-- the type constructor
	    [GenType tyvar uvar])	-- types to which it is applied

getAppTyCon ty
  = case maybeAppTyCon ty of
      Just stuff -> stuff
#ifdef DEBUG
      Nothing    -> panic "Type.getAppTyCon" -- (ppr PprShowAll ty)
#endif
\end{code}

Applied data tycons (give back constrs)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
maybeAppDataTyCon
	:: GenType (GenTyVar any) uvar
	-> Maybe (TyCon,		-- the type constructor
		  [GenType (GenTyVar any) uvar],	-- types to which it is applied
		  [Id])			-- its family of data-constructors
maybeAppDataTyConExpandingDicts, maybeAppSpecDataTyConExpandingDicts
	:: Type -> Maybe (TyCon, [Type], [Id])

maybeAppDataTyCon		    ty = maybe_app_data_tycon (\x->x) ty
maybeAppDataTyConExpandingDicts     ty = maybe_app_data_tycon expandTy ty
maybeAppSpecDataTyConExpandingDicts ty = maybe_app_data_tycon expandTy ty


maybe_app_data_tycon expand ty
  = let
	expanded_ty       = expand ty
	(app_ty, arg_tys) = splitAppTy expanded_ty
    in
    case (getTyCon_maybe app_ty) of
	Just tycon |  --pprTrace "maybe_app:" (ppCat [ppr PprDebug (isDataTyCon tycon), ppr PprDebug (notArrowKind (typeKind expanded_ty))]) $
		      isDataTyCon tycon && 
		      notArrowKind (typeKind expanded_ty)
			-- Must be saturated for ty to be a data type
		   -> Just (tycon, arg_tys, tyConDataCons tycon)

	other      -> Nothing

getAppDataTyCon, getAppSpecDataTyCon
	:: GenType (GenTyVar any) uvar
	-> (TyCon,			-- the type constructor
	    [GenType (GenTyVar any) uvar],	-- types to which it is applied
	    [Id])			-- its family of data-constructors
getAppDataTyConExpandingDicts, getAppSpecDataTyConExpandingDicts
	:: Type -> (TyCon, [Type], [Id])

getAppDataTyCon               ty = get_app_data_tycon maybeAppDataTyCon ty
getAppDataTyConExpandingDicts ty = --pprTrace "getAppDataTyConEx...:" (pprType PprDebug ty) $
				   get_app_data_tycon maybeAppDataTyConExpandingDicts ty

-- these should work like the UniTyFuns.getUniDataSpecTyCon* things of old (ToDo)
getAppSpecDataTyCon               = getAppDataTyCon
getAppSpecDataTyConExpandingDicts = getAppDataTyConExpandingDicts

get_app_data_tycon maybe ty
  = case maybe ty of
      Just stuff -> stuff
#ifdef DEBUG
      Nothing    -> panic "Type.getAppDataTyCon" -- (pprGenType PprShowAll ty)
#endif


maybeBoxedPrimType :: Type -> Maybe (Id, Type)

maybeBoxedPrimType ty
  = case (maybeAppDataTyCon ty) of		-- Data type,
      Just (tycon, tys_applied, [data_con]) 	-- with exactly one constructor
        -> case (dataConArgTys data_con tys_applied) of
	     [data_con_arg_ty]		    	-- Applied to exactly one type,
	        | isPrimType data_con_arg_ty 	-- which is primitive
	        -> Just (data_con, data_con_arg_ty)
	     other_cases -> Nothing
      other_cases -> Nothing
\end{code}

\begin{code}
splitSigmaTy :: GenType t u -> ([t], [(Class,GenType t u)], GenType t u)
splitSigmaTy ty =
  (tyvars, theta, tau)
 where
  (tyvars,rho) = splitForAllTy ty
  (theta,tau)  = splitRhoTy rho

mkSigmaTy tyvars theta tau = mkForAllTys tyvars (mkRhoTy theta tau)
\end{code}


Finding the kind of a type
~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
typeKind :: GenType (GenTyVar any) u -> Kind

typeKind (TyVarTy tyvar) 	= tyVarKind tyvar
typeKind (TyConTy tycon usage)	= tyConKind tycon
typeKind (SynTy _ _ ty)		= typeKind ty
typeKind (FunTy fun arg _)	= mkBoxedTypeKind
typeKind (DictTy clas arg _)	= mkBoxedTypeKind
typeKind (AppTy fun arg)	= resultKind (typeKind fun)
typeKind (ForAllTy _ _)		= mkBoxedTypeKind
typeKind (ForAllUsageTy _ _ _)	= mkBoxedTypeKind
\end{code}


Free variables of a type
~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
tyVarsOfType :: GenType (GenTyVar flexi) uvar -> GenTyVarSet flexi

tyVarsOfType (TyVarTy tv)		= unitTyVarSet tv
tyVarsOfType (TyConTy tycon usage)	= emptyTyVarSet
tyVarsOfType (SynTy _ tys ty)		= tyVarsOfTypes tys
tyVarsOfType (FunTy arg res _)		= tyVarsOfType arg `unionTyVarSets` tyVarsOfType res
tyVarsOfType (AppTy fun arg)		= tyVarsOfType fun `unionTyVarSets` tyVarsOfType arg
tyVarsOfType (DictTy clas ty _)		= tyVarsOfType ty
tyVarsOfType (ForAllTy tyvar ty)	= tyVarsOfType ty `minusTyVarSet` unitTyVarSet tyvar
tyVarsOfType (ForAllUsageTy _ _ ty)	= tyVarsOfType ty

tyVarsOfTypes :: [GenType (GenTyVar flexi) uvar] -> GenTyVarSet flexi
tyVarsOfTypes tys = foldr (unionTyVarSets.tyVarsOfType) emptyTyVarSet tys
\end{code}


Instantiating a type
~~~~~~~~~~~~~~~~~~~~
\begin{code}
applyTy :: GenType (GenTyVar flexi) uvar 
	-> GenType (GenTyVar flexi) uvar 
	-> GenType (GenTyVar flexi) uvar

applyTy (SynTy _ _ fun)  arg = applyTy fun arg
applyTy (ForAllTy tv ty) arg = instantiateTy [(tv,arg)] ty
applyTy other		 arg = panic "applyTy"
\end{code}

\begin{code}
instantiateTy	:: [(GenTyVar flexi, GenType (GenTyVar flexi) uvar)] 
		-> GenType (GenTyVar flexi) uvar 
		-> GenType (GenTyVar flexi) uvar

instantiateTauTy :: Eq tv =>
		   [(tv, GenType tv' u)]
		-> GenType tv u
		-> GenType tv' u

applyTypeEnvToTy :: TyVarEnv Type -> SigmaType -> SigmaType

-- instantiateTauTy works only (a) on types with no ForAlls,
-- 	and when	       (b) all the type variables are being instantiated
-- In return it is more polymorphic than instantiateTy

instant_help ty lookup_tv deflt_tv choose_tycon
		if_usage if_forall bound_forall_tv_BAD deflt_forall_tv
  = go ty
  where
    go (TyVarTy tv)		   = case (lookup_tv tv) of
				       Nothing -> deflt_tv tv
				       Just ty -> ty
    go ty@(TyConTy tycon usage)	   = choose_tycon ty tycon usage
    go (SynTy tycon tys ty)	   = SynTy tycon (map go tys) (go ty)
    go (FunTy arg res usage)	   = FunTy (go arg) (go res) usage
    go (AppTy fun arg)		   = AppTy (go fun) (go arg)
    go (DictTy clas ty usage)	   = DictTy clas (go ty) usage
    go (ForAllUsageTy uvar bds ty) = if_usage $
				     ForAllUsageTy uvar bds (go ty)
    go (ForAllTy tv ty)		   = if_forall $
				     (if (bound_forall_tv_BAD && maybeToBool (lookup_tv tv)) then
					trace "instantiateTy: unexpected forall hit"
				     else
				        \x->x) ForAllTy (deflt_forall_tv tv) (go ty)

instantiateTy tenv ty
  = instant_help ty lookup_tv deflt_tv choose_tycon
		    if_usage if_forall bound_forall_tv_BAD deflt_forall_tv
  where
    lookup_tv tv = case [ty | (tv',ty) <- tenv, tv == tv'] of
		     []   -> Nothing
		     [ty] -> Just ty
		     _	  -> panic "instantiateTy:lookup_tv"

    deflt_tv tv = TyVarTy tv
    choose_tycon ty _ _ = ty
    if_usage ty = ty
    if_forall ty = ty
    bound_forall_tv_BAD = True
    deflt_forall_tv tv  = tv

instantiateTauTy tenv ty
  = instant_help ty lookup_tv deflt_tv choose_tycon
		    if_usage if_forall bound_forall_tv_BAD deflt_forall_tv
  where
    lookup_tv tv = case [ty | (tv',ty) <- tenv, tv == tv'] of
		     []   -> Nothing
		     [ty] -> Just ty
		     _	  -> panic "instantiateTauTy:lookup_tv"

    deflt_tv tv = panic "instantiateTauTy"
    choose_tycon _ tycon usage = TyConTy tycon usage
    if_usage ty = panic "instantiateTauTy:ForAllUsageTy"
    if_forall ty = panic "instantiateTauTy:ForAllTy"
    bound_forall_tv_BAD = panic "instantiateTauTy:bound_forall_tv"
    deflt_forall_tv tv  = panic "instantiateTauTy:deflt_forall_tv"


-- applyTypeEnv applies a type environment to a type.
-- It can handle shadowing; for example:
--	f = /\ t1 t2 -> \ d ->
--	   letrec f' = /\ t1 -> \x -> ...(f' t1 x')...
--         in f' t1
-- Here, when we clone t1 to t1', say, we'll come across shadowing
-- when applying the clone environment to the type of f'.
--
-- As a sanity check, we should also check that name capture 
-- doesn't occur, but that means keeping track of the free variables of the
-- range of the TyVarEnv, which I don't do just yet.
--
-- We don't use instant_help because we need to carry in the environment

applyTypeEnvToTy tenv ty
  = go tenv ty
  where
    go tenv ty@(TyVarTy tv)	   	= case (lookupTyVarEnv tenv tv) of
				       	     Nothing -> ty
				       	     Just ty -> ty
    go tenv ty@(TyConTy tycon usage)	= ty
    go tenv (SynTy tycon tys ty)	= SynTy tycon (map (go tenv) tys) (go tenv ty)
    go tenv (FunTy arg res usage)	= FunTy (go tenv arg) (go tenv res) usage
    go tenv (AppTy fun arg)		= AppTy (go tenv fun) (go tenv arg)
    go tenv (DictTy clas ty usage)	= DictTy clas (go tenv ty) usage
    go tenv (ForAllUsageTy uvar bds ty) = ForAllUsageTy uvar bds (go tenv ty)
    go tenv (ForAllTy tv ty)		= ForAllTy tv (go tenv' ty)
					where
					  tenv' = case lookupTyVarEnv tenv tv of
						    Nothing -> tenv
						    Just _  -> delFromTyVarEnv tenv tv
\end{code}

\begin{code}
instantiateUsage
	:: Ord3 u => [(u, GenType t u')] -> GenType t u -> GenType t u'

instantiateUsage = panic "instantiateUsage: not implemented"
\end{code}


At present there are no unboxed non-primitive types, so
isUnboxedType is the same as isPrimType.

We're a bit cavalier about finding out whether something is
primitive/unboxed or not.  Rather than deal with the type
arguemnts we just zoom into the function part of the type.
That is, given (T a) we just recurse into the "T" part,
ignoring "a".

\begin{code}
isPrimType, isUnboxedType :: Type -> Bool

isPrimType (AppTy ty _)      = isPrimType ty
isPrimType (SynTy _ _ ty)    = isPrimType ty
isPrimType (TyConTy tycon _) = case maybeNewTyCon tycon of
				  Just (tyvars, ty) -> isPrimType ty
				  Nothing 	    -> isPrimTyCon tycon

isPrimType _ 		     = False

isUnboxedType = isPrimType
\end{code}

This is *not* right: it is a placeholder (ToDo 96/03 WDP):
\begin{code}
typePrimRep :: Type -> PrimRep

typePrimRep (SynTy _ _ ty)  = typePrimRep ty
typePrimRep (AppTy ty _)    = typePrimRep ty
typePrimRep (TyConTy tc _)  
  | isPrimTyCon tc	    = case (assocMaybe tc_primrep_list (uniqueOf tc)) of
				   Just xx -> xx
				   Nothing -> panic "Type.typePrimRep" -- pprPanic "typePrimRep:" (pprTyCon PprDebug tc)

  | otherwise		    = case maybeNewTyCon tc of
				  Just (tyvars, ty) | isPrimType ty -> typePrimRep ty
				  _ -> PtrRep 	-- Default

typePrimRep _		    = PtrRep -- the "default"

tc_primrep_list
  = [(addrPrimTyConKey,	     	    AddrRep)
    ,(arrayPrimTyConKey,     	    ArrayRep)
    ,(byteArrayPrimTyConKey, 	    ByteArrayRep)
    ,(charPrimTyConKey,	     	    CharRep)
    ,(doublePrimTyConKey,    	    DoubleRep)
    ,(floatPrimTyConKey,     	    FloatRep)
    ,(foreignObjPrimTyConKey,	    ForeignObjRep)
    ,(intPrimTyConKey,	     	    IntRep)
    ,(mutableArrayPrimTyConKey,     ArrayRep)
    ,(mutableByteArrayPrimTyConKey, ByteArrayRep)
    ,(stablePtrPrimTyConKey, 	    StablePtrRep)
    ,(statePrimTyConKey,	    VoidRep)
    ,(synchVarPrimTyConKey,	    PtrRep)
    ,(voidTyConKey,	     	    VoidRep)
    ,(wordPrimTyConKey,	     	    WordRep)
    ]
\end{code}

%************************************************************************
%*									*
\subsection{Matching on types}
%*									*
%************************************************************************

Matching is a {\em unidirectional} process, matching a type against a
template (which is just a type with type variables in it).  The
matcher assumes that there are no repeated type variables in the
template, so that it simply returns a mapping of type variables to
types.  It also fails on nested foralls.

@matchTys@ matches corresponding elements of a list of templates and
types.

\begin{code}
matchTy :: GenType t1 u1		-- Template
	-> GenType t2 u2		-- Proposed instance of template
	-> Maybe [(t1,GenType t2 u2)]	-- Matching substitution
					

matchTys :: [GenType t1 u1]		-- Templates
	 -> [GenType t2 u2]		-- Proposed instance of template
	 -> Maybe ([(t1,GenType t2 u2)],-- Matching substitution
		   [GenType t2 u2])	-- Left over instance types

matchTy  ty1  ty2  = match  ty1 ty2 (\s -> Just s) []
matchTys tys1 tys2 = go [] tys1 tys2
		   where
		     go s [] 	    tys2        = Just (s,tys2)
		     go s (ty1:tys1) []	        = trace "matchTys" Nothing
		     go s (ty1:tys1) (ty2:tys2) = match ty1 ty2 (\s' -> go s' tys1 tys2) s
\end{code}

@match@ is the main function.

\begin{code}
match :: GenType t1 u1 -> GenType t2 u2			-- Current match pair
      -> ([(t1, GenType t2 u2)] -> Maybe result)	-- Continuation
      -> [(t1, GenType t2 u2)]				-- Current substitution
      -> Maybe result

match (TyVarTy v) 	   ty		        k = \s -> k ((v,ty) : s)
match (FunTy fun1 arg1 _)  (FunTy fun2 arg2 _)  k = match fun1 fun2 (match arg1 arg2 k)
match (AppTy fun1 arg1)    (AppTy fun2 arg2)    k = match fun1 fun2 (match arg1 arg2 k)
match (TyConTy con1 _)     (TyConTy con2 _)     k | con1  == con2  = k
match (DictTy clas1 ty1 _) (DictTy clas2 ty2 _) k | clas1 == clas2 = match ty1 ty2 k
match (SynTy _ _ ty1)      ty2		        k = match ty1 ty2 k
match ty1		       (SynTy _ _ ty2)  k = match ty1 ty2 k

	-- With type synonyms, we have to be careful for the exact
	-- same reasons as in the unifier.  Please see the
	-- considerable commentary there before changing anything
	-- here! (WDP 95/05)

-- Catch-all fails
match _ _ _ = \s -> Nothing
\end{code}

%************************************************************************
%*									*
\subsection{Equality on types}
%*									*
%************************************************************************

The functions eqSimpleTy and eqSimpleTheta are polymorphic in the types t
and u, but ONLY WORK FOR SIMPLE TYPES (ie. they panic if they see
dictionaries or polymorphic types).  The function eqTy has a more
specific type, but does the `right thing' for all types.

\begin{code}
eqSimpleTheta :: (Eq t,Eq u) =>
    [(Class,GenType t u)] -> [(Class,GenType t u)] -> Bool

eqSimpleTheta [] [] = True
eqSimpleTheta ((c1,t1):th1) ((c2,t2):th2) =
  c1==c2 && t1 `eqSimpleTy` t2 && th1 `eqSimpleTheta` th2
eqSimpleTheta other1 other2 = False
\end{code}

\begin{code}
eqSimpleTy :: (Eq t,Eq u) => GenType t u -> GenType t u -> Bool

(TyVarTy tv1) `eqSimpleTy` (TyVarTy tv2) =
  tv1 == tv2
(AppTy f1 a1)  `eqSimpleTy` (AppTy f2 a2) =
  f1 `eqSimpleTy` f2 && a1 `eqSimpleTy` a2
(TyConTy tc1 u1) `eqSimpleTy` (TyConTy tc2 u2) =
  tc1 == tc2 --ToDo: later: && u1 == u2

(FunTy f1 a1 u1) `eqSimpleTy` (FunTy f2 a2 u2) =
  f1 `eqSimpleTy` f2 && a1 `eqSimpleTy` a2 && u1 == u2
(FunTy f1 a1 u1) `eqSimpleTy` t2 =
  -- Expand t1 just in case t2 matches that version
  (AppTy (AppTy (TyConTy mkFunTyCon u1) f1) a1) `eqSimpleTy` t2
t1 `eqSimpleTy` (FunTy f2 a2 u2) =
  -- Expand t2 just in case t1 matches that version
  t1 `eqSimpleTy` (AppTy (AppTy (TyConTy mkFunTyCon u2) f2) a2)

(SynTy tc1 ts1 t1) `eqSimpleTy` (SynTy tc2 ts2 t2) =
  (tc1 == tc2 && and (zipWith eqSimpleTy ts1 ts2) && length ts1 == length ts2)
  || t1 `eqSimpleTy` t2
(SynTy _ _ t1) `eqSimpleTy` t2 =
  t1 `eqSimpleTy` t2  -- Expand the abbrevation and try again
t1 `eqSimpleTy` (SynTy _ _ t2) =
  t1 `eqSimpleTy` t2  -- Expand the abbrevation and try again

(DictTy _ _ _) `eqSimpleTy` _ = panic "eqSimpleTy: got DictTy"
_ `eqSimpleTy` (DictTy _ _ _) = panic "eqSimpleTy: got DictTy"

(ForAllTy _ _) `eqSimpleTy` _ = panic "eqSimpleTy: got ForAllTy"
_ `eqSimpleTy` (ForAllTy _ _) = panic "eqSimpleTy: got ForAllTy"

(ForAllUsageTy _ _ _) `eqSimpleTy` _ = panic "eqSimpleTy: got ForAllUsageTy"
_ `eqSimpleTy` (ForAllUsageTy _ _ _) = panic "eqSimpleTy: got ForAllUsageTy"

_ `eqSimpleTy` _ = False
\end{code}

Types are ordered so we can sort on types in the renamer etc.  DNT: Since
this class is also used in CoreLint and other such places, we DO expand out
Fun/Syn/Dict types (if necessary).

\begin{code}
eqTy :: Type -> Type -> Bool

eqTy t1 t2 =
  eq nullTyVarEnv nullUVarEnv t1 t2
 where
  eq tve uve (TyVarTy tv1) (TyVarTy tv2) =
    tv1 == tv2 ||
    case (lookupTyVarEnv tve tv1) of
      Just tv -> tv == tv2
      Nothing -> False
  eq tve uve (AppTy f1 a1) (AppTy f2 a2) =
    eq tve uve f1 f2 && eq tve uve a1 a2
  eq tve uve (TyConTy tc1 u1) (TyConTy tc2 u2) =
    tc1 == tc2 -- ToDo: LATER: && eqUsage uve u1 u2

  eq tve uve (FunTy f1 a1 u1) (FunTy f2 a2 u2) =
    eq tve uve f1 f2 && eq tve uve a1 a2 && eqUsage uve u1 u2
  eq tve uve (FunTy f1 a1 u1) t2 =
    -- Expand t1 just in case t2 matches that version
    eq tve uve (AppTy (AppTy (TyConTy mkFunTyCon u1) f1) a1) t2
  eq tve uve t1 (FunTy f2 a2 u2) =
    -- Expand t2 just in case t1 matches that version
    eq tve uve t1 (AppTy (AppTy (TyConTy mkFunTyCon u2) f2) a2)

  eq tve uve (DictTy c1 t1 u1) (DictTy c2 t2 u2) 
    | c1 == c2 
    = eq tve uve t1 t2 && eqUsage uve u1 u2
	-- NB we use a guard for c1==c2 so that if they aren't equal we
	-- fall through into expanding the type.  Why?  Because brain-dead
	-- people might write
	--	class Foo a => Baz a where {}
	-- and that means that a Foo dictionary and a Baz dictionary are identical
	-- Sigh.  Let's hope we don't spend too much time in here!

  eq tve uve t1@(DictTy _ _ _) t2 =
    eq tve uve (expandTy t1) t2  -- Expand the dictionary and try again
  eq tve uve t1 t2@(DictTy _ _ _) =
    eq tve uve t1 (expandTy t2)  -- Expand the dictionary and try again

  eq tve uve (SynTy tc1 ts1 t1) (SynTy tc2 ts2 t2) =
    (tc1 == tc2 && and (zipWith (eq tve uve) ts1 ts2) && length ts1 == length ts2)
    || eq tve uve t1 t2
  eq tve uve (SynTy _ _ t1) t2 =
    eq tve uve t1 t2  -- Expand the abbrevation and try again
  eq tve uve t1 (SynTy _ _ t2) =
    eq tve uve t1 t2  -- Expand the abbrevation and try again

  eq tve uve (ForAllTy tv1 t1) (ForAllTy tv2 t2) =
    eq (addOneToTyVarEnv tve tv1 tv2) uve t1 t2
  eq tve uve (ForAllUsageTy u1 b1 t1) (ForAllUsageTy u2 b2 t2) =
    eqBounds uve b1 b2 && eq tve (addOneToUVarEnv uve u1 u2) t1 t2

  eq _ _ _ _ = False

  eqBounds uve [] [] = True
  eqBounds uve (u1:b1) (u2:b2) = eqUVar uve u1 u2 && eqBounds uve b1 b2
  eqBounds uve _ _ = False
\end{code}
