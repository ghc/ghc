\begin{code}
module TcEnv(
	TcId, TcIdSet, tcInstId,
	tcLookupDataCon,

	TcEnv, ValueEnv, TcTyThing(..),

	initEnv, getEnvTyCons, getEnvClasses, getAllEnvTyCons,
	
        tcExtendUVarEnv, tcLookupUVar,

	tcExtendTyVarEnv, tcExtendTyVarEnvForMeths, tcExtendTypeEnv, tcGetInScopeTyVars,

	tcLookupTy,
	tcLookupTyCon, tcLookupTyConByKey, 
	tcLookupClass, tcLookupClassByKey, tcLookupClassByKey_maybe,

	tcExtendGlobalValEnv, tcExtendLocalValEnv,
	tcGetValueEnv,        tcSetValueEnv, 
	tcAddImportedIdInfo,

	tcLookupValue,      tcLookupValueMaybe, 
	tcLookupValueByKey, tcLookupValueByKeyMaybe,
	explicitLookupValueByKey, explicitLookupValue,
	valueEnvIds,

	newLocalId, newSpecPragmaId,
	tcGetGlobalTyVars, tcExtendGlobalTyVars,

	badCon, badPrimOp
  ) where

#include "HsVersions.h"

import HsTypes	( HsTyVar, getTyVarName )
import Id	( mkUserLocal, isDataConWrapId_maybe )
import MkId 	( mkSpecPragmaId )
import Var	( TyVar, Id, setVarName,
		  idType, lazySetIdInfo, idInfo, tyVarKind, UVar,
		)
import TcType	( TcType, TcTyVar, TcTyVarSet, TcThetaType,
		  tcInstTyVars, zonkTcTyVars,
		  TcKind, kindToTcKind
		)
import VarEnv
import VarSet
import Type	( Kind, superKind,
		  tyVarsOfType, tyVarsOfTypes, mkTyVarTy,
		  splitForAllTys, splitRhoTy, splitFunTys,
		  splitAlgTyConApp_maybe, getTyVar
		)
import Subst	( substTy )
import UsageSPUtils ( unannotTy )
import DataCon	( DataCon )
import TyCon	( TyCon, tyConKind, tyConArity, isSynTyCon )
import Class	( Class, classTyCon )

import TcMonad

import BasicTypes	( Arity )
import IdInfo		( vanillaIdInfo )
import Name		( Name, OccName, nameOccName, getSrcLoc,
			  maybeWiredInTyConName, maybeWiredInIdName, isLocallyDefined,
			  NamedThing(..)
			)
import Unique		( pprUnique10, Unique, Uniquable(..) )
import FiniteMap	( lookupFM, addToFM )
import UniqFM
import Unique		( Uniquable(..) )
import Util		( zipEqual, zipWith3Equal, mapAccumL )
import Bag		( bagToList )
import Maybes		( maybeToBool, catMaybes )
import SrcLoc		( SrcLoc )
import FastString	( FastString )
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection{TcId}
%*									*
%************************************************************************


\begin{code}
type TcId    = Id 			-- Type may be a TcType
type TcIdSet = IdSet

tcLookupDataCon :: Name -> TcM s (DataCon, [TcType], TcType)
tcLookupDataCon con_name
  = tcLookupValue con_name		`thenNF_Tc` \ con_id ->
    case isDataConWrapId_maybe con_id of {
	Nothing -> failWithTc (badCon con_id);
 	Just data_con ->

    tcInstId con_id			`thenNF_Tc` \ (_, _, con_tau) ->
	     -- Ignore the con_theta; overloaded constructors only
	     -- behave differently when called, not when used for
	     -- matching.
    let
	(arg_tys, result_ty) = splitFunTys con_tau
    in
    ASSERT( maybeToBool (splitAlgTyConApp_maybe result_ty) )
    returnTc (data_con, arg_tys, result_ty) }

-- A useful function that takes an occurrence of a global thing
-- and instantiates its type with fresh type variables
tcInstId :: Id
	 -> NF_TcM s ([TcTyVar], 	-- It's instantiated type
		      TcThetaType,	--
		      TcType)		--
tcInstId id
  = let
      (tyvars, rho) = splitForAllTys (unannotTy (idType id))
    in
    tcInstTyVars tyvars		`thenNF_Tc` \ (tyvars', arg_tys, tenv) ->
    let
	rho'	       = substTy tenv rho
	(theta', tau') = splitRhoTy rho' 
    in
    returnNF_Tc (tyvars', theta', tau')
\end{code}

Between the renamer and the first invocation of the UsageSP inference,
identifiers read from interface files will have usage information in
their types, whereas other identifiers will not.  The unannotTy here
in @tcInstId@ prevents this information from pointlessly propagating
further prior to the first usage inference.


%************************************************************************
%*									*
\subsection{TcEnv}
%*									*
%************************************************************************

Data type declarations
~~~~~~~~~~~~~~~~~~~~~

\begin{code}
data TcEnv = TcEnv
                  UsageEnv
		  TypeEnv
		  ValueEnv 
		  (TcTyVarSet,		-- The in-scope TyVars
		   TcRef TcTyVarSet)	-- Free type variables of the value env
					-- ...why mutable? see notes with tcGetGlobalTyVars
					-- Includes the in-scope tyvars

type NameEnv val = UniqFM val		-- Keyed by Names

type UsageEnv   = NameEnv UVar
type TypeEnv	= NameEnv (TcKind, Maybe Arity, TcTyThing)
type ValueEnv	= NameEnv Id	

valueEnvIds :: ValueEnv -> [Id]
valueEnvIds ve = eltsUFM ve

data TcTyThing = ATyVar TcTyVar		-- Mutable only so that the kind can be mutable
					-- if the kind is mutable, the tyvar must be so that
					-- zonking works
	       | ATyCon TyCon
	       | AClass Class


initEnv :: TcRef TcTyVarSet -> TcEnv
initEnv mut = TcEnv emptyUFM emptyUFM emptyUFM (emptyVarSet, mut)

getEnvTyCons  (TcEnv _ te _ _) = [tc | (_, _, ATyCon tc) <- eltsUFM te]
getEnvClasses (TcEnv _ te _ _) = [cl | (_, _, AClass cl) <- eltsUFM te]
getAllEnvTyCons (TcEnv _ te _ _) = catMaybes (map gettc (eltsUFM te))
    where                          
      gettc (_,_, ATyCon tc) = Just tc
      gettc (_,_, AClass cl) = Just (classTyCon cl)
      gettc _                = Nothing
\end{code}

The UsageEnv
~~~~~~~~~~~~

Extending the usage environment.

\begin{code}
tcExtendUVarEnv :: Name -> UVar -> TcM s r -> TcM s r
tcExtendUVarEnv uv_name uv scope
  = tcGetEnv                                                 `thenNF_Tc` \ (TcEnv ue te ve gtvs) ->
    tcSetEnv (TcEnv (addToUFM ue uv_name uv) te ve gtvs) scope
\end{code}

Looking up in the environments.

\begin{code}
tcLookupUVar :: Name -> NF_TcM s UVar
tcLookupUVar uv_name
  = tcGetEnv	`thenNF_Tc` \ (TcEnv ue te ve gtvs) ->
    case lookupUFM ue uv_name of
      Just uv -> returnNF_Tc uv
      Nothing -> failWithTc (uvNameOutOfScope uv_name)
\end{code}	


The TypeEnv
~~~~~~~~~~~~

Extending the type environment. 

\begin{code}
tcExtendTyVarEnv :: [TyVar] -> TcM s r -> TcM s r
tcExtendTyVarEnv tyvars scope
  = tcGetEnv				`thenNF_Tc` \ (TcEnv ue te ve (in_scope_tvs, gtvs)) ->
    let
	extend_list = [ (getName tv, (kindToTcKind (tyVarKind tv), Nothing, ATyVar tv))
		      | tv <- tyvars
		      ]
 	te'           = addListToUFM te extend_list
	new_tv_set    = mkVarSet tyvars
	in_scope_tvs' = in_scope_tvs `unionVarSet` new_tv_set
    in
	-- It's important to add the in-scope tyvars to the global tyvar set
	-- as well.  Consider
	--	f (x::r) = let g y = y::r in ...
	-- Here, g mustn't be generalised.  This is also important during
	-- class and instance decls, when we mustn't generalise the class tyvars
	-- when typechecking the methods.
    tc_extend_gtvs gtvs new_tv_set		`thenNF_Tc` \ gtvs' ->
    tcSetEnv (TcEnv ue te' ve (in_scope_tvs', gtvs')) scope

-- This variant, tcExtendTyVarEnvForMeths, takes *two* bunches of tyvars:
--	the signature tyvars contain the original names
--	the instance  tyvars are what those names should be mapped to
-- It's needed when typechecking the method bindings of class and instance decls
-- It does *not* extend the global tyvars; tcMethodBind does that for itself

tcExtendTyVarEnvForMeths :: [TyVar] -> [TcTyVar] -> TcM s r -> TcM s r
tcExtendTyVarEnvForMeths sig_tyvars inst_tyvars thing_inside
  = tcGetEnv					`thenNF_Tc` \ (TcEnv ue te ve gtvs) ->
    let
	te' = addListToUFM te stuff
    in
    tcSetEnv (TcEnv ue te' ve gtvs) thing_inside
  where
    stuff = [ (getName sig_tv, (kindToTcKind (tyVarKind inst_tv), Nothing, ATyVar inst_tv))
	    | (sig_tv, inst_tv) <- zipEqual "tcMeth" sig_tyvars inst_tyvars
	    ]

tcExtendGlobalTyVars extra_global_tvs scope
  = tcGetEnv					`thenNF_Tc` \ (TcEnv ue te ve (in_scope,gtvs)) ->
    tc_extend_gtvs gtvs	extra_global_tvs	`thenNF_Tc` \ gtvs' ->
    tcSetEnv (TcEnv ue te ve (in_scope,gtvs')) scope

tc_extend_gtvs gtvs extra_global_tvs
  = tcReadMutVar gtvs			`thenNF_Tc` \ global_tvs ->
    let
	new_global_tyvars = global_tvs `unionVarSet` extra_global_tvs
    in
    tcNewMutVar new_global_tyvars
\end{code}

@tcGetGlobalTyVars@ returns a fully-zonked set of tyvars free in the environment.
To improve subsequent calls to the same function it writes the zonked set back into
the environment.

\begin{code}
tcGetGlobalTyVars :: NF_TcM s TcTyVarSet
tcGetGlobalTyVars
  = tcGetEnv 						`thenNF_Tc` \ (TcEnv ue te ve (_,gtvs)) ->
    tcReadMutVar gtvs					`thenNF_Tc` \ global_tvs ->
    zonkTcTyVars (varSetElems global_tvs)		`thenNF_Tc` \ global_tys' ->
    let
	global_tvs' = (tyVarsOfTypes global_tys')
    in
    tcWriteMutVar gtvs global_tvs'			`thenNF_Tc_` 
    returnNF_Tc global_tvs'

tcGetInScopeTyVars :: NF_TcM s [TcTyVar]
tcGetInScopeTyVars
  = tcGetEnv		`thenNF_Tc` \ (TcEnv ue te ve (in_scope_tvs, gtvs)) ->
    returnNF_Tc (varSetElems in_scope_tvs)
\end{code}


Type constructors and classes

\begin{code}
tcExtendTypeEnv :: [(Name, (TcKind, Maybe Arity, TcTyThing))] -> TcM s r -> TcM s r
tcExtendTypeEnv bindings scope
  = ASSERT( null [tv | (_, (_,_,ATyVar tv)) <- bindings] )
	-- Not for tyvars; use tcExtendTyVarEnv
    tcGetEnv					`thenNF_Tc` \ (TcEnv ue te ve gtvs) ->
    let
	te' = addListToUFM te bindings
    in
    tcSetEnv (TcEnv ue te' ve gtvs) scope
\end{code}


Looking up in the environments.

\begin{code}
tcLookupTy :: Name ->  NF_TcM s (TcKind, Maybe Arity, TcTyThing)
tcLookupTy name
  = tcGetEnv	`thenNF_Tc` \ (TcEnv ue te ve gtvs) ->
    case lookupUFM te name of {
	Just thing -> returnNF_Tc thing ;
 	Nothing    -> 

    case maybeWiredInTyConName name of
	Just tc -> returnNF_Tc (kindToTcKind (tyConKind tc), maybe_arity, ATyCon tc)
		where
		   maybe_arity | isSynTyCon tc = Just (tyConArity tc)
			       | otherwise     = Nothing 

	Nothing -> 	-- This can happen if an interface-file
			-- unfolding is screwed up
		   failWithTc (tyNameOutOfScope name)
    }
	
tcLookupClass :: Name -> NF_TcM s Class
tcLookupClass name
  = tcLookupTy name	`thenNF_Tc` \ (_, _, AClass clas) ->
    returnNF_Tc clas

tcLookupTyCon :: Name -> NF_TcM s TyCon
tcLookupTyCon name
  = tcLookupTy name	`thenNF_Tc` \ (_, _, ATyCon tycon) ->
    returnNF_Tc tycon

tcLookupClassByKey :: Unique -> NF_TcM s Class
tcLookupClassByKey key
  = tcGetEnv		`thenNF_Tc` \ (TcEnv ue te ve gtvs) ->
    case lookupUFM_Directly te key of
	Just (_, _, AClass cl) -> returnNF_Tc cl
	other		       -> pprPanic "tcLookupClassByKey:" (pprUnique10 key)

tcLookupClassByKey_maybe :: Unique -> NF_TcM s (Maybe Class)
tcLookupClassByKey_maybe key
  = tcGetEnv		`thenNF_Tc` \ (TcEnv ue te ve gtvs) ->
    case lookupUFM_Directly te key of
	Just (_, _, AClass cl) -> returnNF_Tc (Just cl)
	other		       -> returnNF_Tc Nothing

tcLookupTyConByKey :: Unique -> NF_TcM s TyCon
tcLookupTyConByKey key
  = tcGetEnv		`thenNF_Tc` \ (TcEnv ue te ve gtvs) ->
    case lookupUFM_Directly te key of
	Just (_, _, ATyCon tc) -> returnNF_Tc tc
	other		       -> pprPanic "tcLookupTyConByKey:" (pprUnique10 key)
\end{code}




%************************************************************************
%*									*
\subsection{The value environment}
%*									*
%************************************************************************

\begin{code}
tcExtendGlobalValEnv :: [Id] -> TcM s a -> TcM s a
tcExtendGlobalValEnv ids scope
  = tcGetEnv		`thenNF_Tc` \ (TcEnv ue te ve gtvs) ->
    let
	ve' = addListToUFM_Directly ve [(getUnique id, id) | id <- ids]
    in
    tcSetEnv (TcEnv ue te ve' gtvs) scope

tcExtendLocalValEnv :: [(Name,TcId)] -> TcM s a -> TcM s a
tcExtendLocalValEnv names_w_ids scope
  = tcGetEnv		`thenNF_Tc` \ (TcEnv ue te ve (in_scope_tvs,gtvs)) ->
    tcReadMutVar gtvs	`thenNF_Tc` \ global_tvs ->
    let
	ve'		    = addListToUFM ve names_w_ids
	extra_global_tyvars = tyVarsOfTypes (map (idType . snd) names_w_ids)
    in
    tc_extend_gtvs gtvs extra_global_tyvars	`thenNF_Tc` \ gtvs' ->
    tcSetEnv (TcEnv ue te ve' (in_scope_tvs,gtvs')) scope
\end{code}


\begin{code}
tcLookupValue :: Name -> NF_TcM s Id	-- Panics if not found
tcLookupValue name
  = case maybeWiredInIdName name of
	Just id -> returnNF_Tc id
	Nothing -> tcGetEnv 		`thenNF_Tc` \ (TcEnv ue te ve gtvs) ->
		   returnNF_Tc (lookupWithDefaultUFM ve def name)
  where
    def = pprPanic "tcLookupValue:" (ppr name)

tcLookupValueMaybe :: Name -> NF_TcM s (Maybe Id)
tcLookupValueMaybe name
  = case maybeWiredInIdName name of
	Just id -> returnNF_Tc (Just id)
	Nothing -> tcGetEnv 		`thenNF_Tc` \ (TcEnv ue te ve gtvs) ->
		   returnNF_Tc (lookupUFM ve name)

tcLookupValueByKey :: Unique -> NF_TcM s Id	-- Panics if not found
tcLookupValueByKey key
  = tcGetEnv 		`thenNF_Tc` \ (TcEnv ue te ve gtvs) ->
    returnNF_Tc (explicitLookupValueByKey ve key)

tcLookupValueByKeyMaybe :: Unique -> NF_TcM s (Maybe Id)
tcLookupValueByKeyMaybe key
  = tcGetEnv 		`thenNF_Tc` \ (TcEnv ue te ve gtvs) ->
    returnNF_Tc (lookupUFM_Directly ve key)

tcGetValueEnv :: NF_TcM s ValueEnv
tcGetValueEnv
  = tcGetEnv 		`thenNF_Tc` \ (TcEnv ue te ve gtvs) ->
    returnNF_Tc ve

tcSetValueEnv :: ValueEnv -> TcM s a -> TcM s a
tcSetValueEnv ve scope
  = tcGetEnv		`thenNF_Tc` \ (TcEnv ue te _ gtvs) ->
    tcSetEnv (TcEnv ue te ve gtvs) scope

-- Non-monadic version, environment given explicitly
explicitLookupValueByKey :: ValueEnv -> Unique -> Id
explicitLookupValueByKey ve key
  = lookupWithDefaultUFM_Directly ve def key
  where
    def = pprPanic "lookupValueByKey:" (pprUnique10 key)

explicitLookupValue :: ValueEnv -> Name -> Maybe Id
explicitLookupValue ve name
  = case maybeWiredInIdName name of
	Just id -> Just id
	Nothing -> lookupUFM ve name

	-- Extract the IdInfo from an IfaceSig imported from an interface file
tcAddImportedIdInfo :: ValueEnv -> Id -> Id
tcAddImportedIdInfo unf_env id
  | isLocallyDefined id		-- Don't look up locally defined Ids, because they
				-- have explicit local definitions, so we get a black hole!
  = id
  | otherwise
  = id `lazySetIdInfo` new_info
	-- The Id must be returned without a data dependency on maybe_id
  where
    new_info = -- pprTrace "tcAdd" (ppr id) $
	       case explicitLookupValue unf_env (getName id) of
		     Nothing	      -> vanillaIdInfo
		     Just imported_id -> idInfo imported_id
		-- ToDo: could check that types are the same
\end{code}


%************************************************************************
%*									*
\subsection{Constructing new Ids}
%*									*
%************************************************************************

\begin{code}
newLocalId :: OccName -> TcType -> SrcLoc -> NF_TcM s TcId
newLocalId name ty loc
  = tcGetUnique		`thenNF_Tc` \ uniq ->
    returnNF_Tc (mkUserLocal name uniq ty loc)

newSpecPragmaId :: Name -> TcType -> NF_TcM s TcId
newSpecPragmaId name ty 
  = tcGetUnique		`thenNF_Tc` \ uniq ->
    returnNF_Tc (mkSpecPragmaId (nameOccName name) uniq ty (getSrcLoc name))
\end{code}


%************************************************************************
%*									*
\subsection{Errors}
%*									*
%************************************************************************

\begin{code}
badCon con_id
  = quotes (ppr con_id) <+> ptext SLIT("is not a data constructor")
badPrimOp op
  = quotes (ppr op) <+> ptext SLIT("is not a primop")

uvNameOutOfScope name
  = ptext SLIT("UVar") <+> quotes (ppr name) <+> ptext SLIT("is not in scope")

tyNameOutOfScope name
  = quotes (ppr name) <+> ptext SLIT("is not in scope")
\end{code}
