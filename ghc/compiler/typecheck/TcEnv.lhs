\begin{code}
module TcEnv(
	TcIdOcc(..), TcIdBndr, TcIdSet, tcIdType, tcIdTyVars, tcInstId,
	tcLookupDataCon,

	TcEnv, GlobalValueEnv,

	initEnv, getEnv_TyCons, getEnv_Classes,
	
	tcExtendTyVarEnv, tcLookupTyVar, tcLookupTyVarBndrs,

	tcExtendTyConEnv, tcLookupTyCon, tcLookupTyConByKey, 
	tcExtendClassEnv, tcLookupClass, tcLookupClassByKey,
	tcGetTyConsAndClasses,

	tcExtendGlobalValEnv, tcExtendLocalValEnv, tcExtendEnvWithPat,
	tcGetGlobalValEnv, tcSetGlobalValEnv, lookupGlobalByKey,
	tcLookupLocalValue, tcLookupLocalValueOK, tcLookupLocalValueByKey, 
	tcLookupGlobalValue, tcLookupGlobalValueByKey, tcLookupGlobalValueMaybe,
	tcAddImportedIdInfo, tcExplicitLookupGlobal,
	tcLookupGlobalValueByKeyMaybe, 

	newLocalIds, newLocalId, newSpecPragmaId,
	tcGetGlobalTyVars, tcExtendGlobalTyVars,

	tidyType, tidyTypes, tidyTyVar,

	badCon, badPrimOp
  ) where

#include "HsVersions.h"

import HsTypes	( getTyVarName )
import Id	( mkUserLocal, isDataConId_maybe )
import MkId 	( mkSpecPragmaId )
import Var	( TyVar, Id, GenId, setVarName,
		  idType, setIdInfo, idInfo
		)
import TcType	( TcType, TcTyVar, TcTyVarSet, TcThetaType, TcBox,
		  tcInstTyVars, zonkTcTyVars,
		  TcKind, kindToTcKind
		)
import VarEnv
import VarSet
import Type	( Kind,
		  tyVarsOfType, tyVarsOfTypes, mkTyVarTy, substTy,
		  splitForAllTys, splitRhoTy, splitFunTys, substFlexiTy,
		  splitAlgTyConApp_maybe, getTyVar
		)
import DataCon	( DataCon )
import TyCon	( TyCon, tyConKind, tyConArity, isSynTyCon )
import Class	( Class )

import TcMonad

import BasicTypes	( Arity )
import IdInfo		( noIdInfo )
import Name		( Name, OccName(..), nameOccName, occNameString, mkLocalName,
			  maybeWiredInTyConName, maybeWiredInIdName, isLocallyDefined,
			  isSysLocalName,
			  NamedThing(..)
			)
import Unique		( pprUnique10, Unique, Uniquable(..) )
import FiniteMap	( lookupFM, addToFM )
import UniqFM
import Unique		( Uniquable(..) )
import Util		( zipEqual, zipWith3Equal, mapAccumL )
import Bag		( bagToList )
import Maybes		( maybeToBool )
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection{TcId, TcIdOcc}
%*									*
%************************************************************************


\begin{code}
type TcIdBndr s = GenId  (TcBox s)	-- Binders are all TcTypes
data TcIdOcc  s = TcId   (TcIdBndr s)	-- Bindees may be either
		| RealId Id

type TcIdSet s  = GenIdSet (TcBox s)

instance Eq (TcIdOcc s) where
  (TcId id1)   == (TcId id2)   = id1 == id2
  (RealId id1) == (RealId id2) = id1 == id2
  _	       == _	       = False

instance Ord (TcIdOcc s) where
  (TcId id1)   `compare` (TcId id2)   = id1 `compare` id2
  (RealId id1) `compare` (RealId id2) = id1 `compare` id2
  (TcId _)     `compare` (RealId _)   = LT
  (RealId _)   `compare` (TcId _)     = GT

instance Outputable (TcIdOcc s) where
  ppr (TcId id)   = ppr id
  ppr (RealId id) = ppr id

instance NamedThing (TcIdOcc s) where
  getName (TcId id)   = getName id
  getName (RealId id) = getName id


tcIdType :: TcIdOcc s -> TcType s
tcIdType (TcId   id) = idType id
tcIdType (RealId id) = pprPanic "tcIdType:" (ppr id)

tcIdTyVars (TcId id)  = tyVarsOfType (idType id)
tcIdTyVars (RealId _) = emptyVarSet		-- Top level Ids have no free type variables


tcLookupDataCon :: Name -> TcM s (DataCon, [TcType s], TcType s)
tcLookupDataCon con_name
  = tcLookupGlobalValue con_name		`thenNF_Tc` \ con_id ->
    case isDataConId_maybe con_id of {
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
	 -> NF_TcM s ([TcTyVar s], 	-- It's instantiated type
		      TcThetaType s,	--
		      TcType s)		--

tcInstId id
  = let
      (tyvars, rho) = splitForAllTys (idType id)
    in
    tcInstTyVars tyvars		`thenNF_Tc` \ (tyvars', arg_tys, tenv) ->
    let
	rho'	       = substFlexiTy tenv rho
	(theta', tau') = splitRhoTy rho' 
    in
    returnNF_Tc (tyvars', theta', tau')
\end{code}

tidyTy tidies up a type for printing in an error message.

\begin{code}
tidyType :: TidyTypeEnv s -> TcType s -> (TidyTypeEnv s, TcType s)
tidyType env ty
  = (env', substTy subst' ty)
  where
    env'@(_, subst') = foldl go env (varSetElems (tyVarsOfType ty))
    go env tyvar     = fst (tidyTyVar env tyvar)

tidyTypes :: TidyTypeEnv s -> [TcType s] -> (TidyTypeEnv s, [TcType s])
tidyTypes env tys = mapAccumL tidyType env tys

tidyTyVar :: TidyTypeEnv s -> TcTyVar s -> (TidyTypeEnv s, TcTyVar s)
tidyTyVar (supply,subst) tyvar
  = case lookupVarEnv subst tyvar of
	Just ty -> 	-- Already substituted
		   ((supply,subst), getTyVar "tidyTyVar" ty)
	Nothing -> 	-- Make a new nice name for it
		   ((addToFM supply str next,
		     extendVarEnv subst tyvar (mkTyVarTy new_tyvar)),
		    new_tyvar)
  where
    tyvar_name = getName tyvar
    is_sys     = isSysLocalName tyvar_name

    str | is_sys    = SLIT("$")
        | otherwise = occNameString (nameOccName tyvar_name)

    next = case lookupFM supply str of
		Nothing -> 0
		Just n  -> n+1

    new_tyvar = mkNewTv str is_sys next tyvar
			
mkNewTv :: FastString -> Bool -> Int -> TcTyVar s -> TcTyVar s
mkNewTv str False  0 tv = tv	-- Leave first non-sys thing alone
mkNewTv str is_sys n tv = setVarName tv (mkLocalName (getUnique tv) 
					             (TvOcc (_PK_ ((_UNPK_ str) ++ show n))))
\end{code}


%************************************************************************
%*									*
\subsection{TcEnv}
%*									*
%************************************************************************

Data type declarations
~~~~~~~~~~~~~~~~~~~~~

\begin{code}
data TcEnv s = TcEnv
		  (TcTyVarEnv s)
		  (TyConEnv s)
		  (ClassEnv s)
		  GlobalValueEnv
		  (ValueEnv (TcIdBndr s))	-- Locals
		  (TcRef s (TcTyVarSet s))	-- Free type variables of locals
						-- ...why mutable? see notes with tcGetGlobalTyVars

type TcTyVarEnv s = UniqFM (TcKind s, TyVar)
type TyConEnv s   = UniqFM (TcKind s, Maybe Arity, TyCon)	-- Arity present for Synonyms only
type ClassEnv s   = UniqFM ([TcKind s], Class)		-- The kinds are the kinds of the args
							-- to the class
type ValueEnv id = UniqFM id
type GlobalValueEnv = ValueEnv Id			-- Globals

initEnv :: TcRef s (TcTyVarSet s) -> TcEnv s
initEnv mut = TcEnv emptyUFM emptyUFM emptyUFM emptyUFM emptyUFM mut 

getEnv_TyCons   (TcEnv _ ts _ _ _ _) = [tycon | (_, _, tycon) <- eltsUFM ts]
getEnv_Classes  (TcEnv _ _ cs _ _ _) = [clas  | (_, clas)     <- eltsUFM cs]
\end{code}

Type variable env
~~~~~~~~~~~~~~~~~
\begin{code}
tcExtendTyVarEnv :: [Name] -> [(TcKind s, TyVar)] -> TcM s r -> TcM s r
tcExtendTyVarEnv names kinds_w_types scope
  = tcGetEnv					`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
    let
	tve' = addListToUFM tve (zipEqual "tcTyVarScope" names kinds_w_types)
    in
    tcSetEnv (TcEnv tve' tce ce gve lve gtvs) scope
\end{code}

The Kind, TyVar, Class and TyCon envs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Extending the environments. 

\begin{code}
tcExtendTyConEnv :: [(Name, (TcKind s, Maybe Arity, TyCon))] -> TcM s r -> TcM s r

tcExtendTyConEnv bindings scope
  = tcGetEnv					`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
    let
	tce' = addListToUFM tce bindings
    in
    tcSetEnv (TcEnv tve tce' ce gve lve gtvs) scope


tcExtendClassEnv :: [(Name, ([TcKind s], Class))] -> TcM s r -> TcM s r
tcExtendClassEnv bindings scope
  = tcGetEnv				`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
    let
	ce' = addListToUFM ce bindings
    in
    tcSetEnv (TcEnv tve tce ce' gve lve gtvs) scope
\end{code}


Looking up in the environments.

\begin{code}
tcLookupTyVarBndrs tyvar_bndrs		-- [HsTyVar name]
  = mapAndUnzipNF_Tc (tcLookupTyVar . getTyVarName) tyvar_bndrs

tcLookupTyVar name
  = tcGetEnv		`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
    returnNF_Tc (lookupWithDefaultUFM tve (pprPanic "tcLookupTyVar:" (ppr name)) name)


tcLookupTyCon name
  =	-- Try for a wired-in tycon
    case maybeWiredInTyConName name of {
	Just tc | isSynTyCon tc -> returnTc (kind, Just (tyConArity tc), tc)
		| otherwise     -> returnTc (kind, Nothing,              tc)
		where {
		  kind = kindToTcKind (tyConKind tc) 
		};

	Nothing -> 

	    -- Try in the environment
	  tcGetEnv	`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
          case lookupUFM tce name of {
	      Just stuff -> returnTc stuff;

	      Nothing    ->

		-- Could be that he's using a class name as a type constructor
	       case lookupUFM ce name of
		 Just _  -> failWithTc (classAsTyConErr name)
		 Nothing -> pprPanic "tcLookupTyCon:" (ppr name)
	    } } 

tcLookupTyConByKey uniq
  = tcGetEnv		`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
    let 
       (kind, arity, tycon) =  lookupWithDefaultUFM_Directly tce 
					(pprPanic "tcLookupTyConByKey:" (pprUnique10 uniq)) 
					uniq
    in
    returnNF_Tc tycon

tcLookupClass name
  = tcGetEnv		`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
    case lookupUFM ce name of
	Just stuff         -- Common case: it's ok
	  -> returnTc stuff

	Nothing		   -- Could be that he's using a type constructor as a class
	  |  maybeToBool (maybeWiredInTyConName name)
	  || maybeToBool (lookupUFM tce name)
	  -> failWithTc (tyConAsClassErr name)

	  | otherwise      -- Wierd!  Renamer shouldn't let this happen
	  -> pprPanic "tcLookupClass" (ppr name)

tcLookupClassByKey uniq
  = tcGetEnv		`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
    let
	(kind, clas) = lookupWithDefaultUFM_Directly ce 
				(pprPanic "tcLookupClassByKey:" (pprUnique10 uniq))
				uniq
    in
    returnNF_Tc clas

tcGetTyConsAndClasses :: NF_TcM s ([TyCon], [Class])
tcGetTyConsAndClasses
  = tcGetEnv		`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
    returnNF_Tc ([tc | (_, _, tc) <- eltsUFM tce],
	         [c  | (_, c)     <- eltsUFM ce])
\end{code}



Extending and consulting the value environment
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
tcExtendGlobalValEnv ids scope
  = tcGetEnv		`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
    let
	gve' = addListToUFM_Directly gve [(getUnique id, id) | id <- ids]
    in
    tcSetEnv (TcEnv tve tce ce gve' lve gtvs) scope

tcExtendLocalValEnv names ids scope
  = tcGetEnv		`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
    tcReadMutVar gtvs	`thenNF_Tc` \ global_tvs ->
    let
	lve' = addListToUFM lve (zipEqual "tcExtendLocalValEnv" names ids)
	extra_global_tyvars = tyVarsOfTypes (map idType ids)
	new_global_tyvars   = global_tvs `unionVarSet` extra_global_tyvars
    in
    tcNewMutVar new_global_tyvars	`thenNF_Tc` \ gtvs' ->

    tcSetEnv (TcEnv tve tce ce gve lve' gtvs') scope

tcExtendEnvWithPat names_w_ids scope
  = tcGetEnv		`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
    tcReadMutVar gtvs	`thenNF_Tc` \ global_tvs ->
    let
	names_w_ids_list    = bagToList names_w_ids
	lve'		    = addListToUFM lve names_w_ids_list
	extra_global_tyvars = tyVarsOfTypes (map (idType . snd) names_w_ids_list)
	new_global_tyvars   = global_tvs `unionVarSet` extra_global_tyvars
    in
    tcNewMutVar new_global_tyvars	`thenNF_Tc` \ gtvs' ->

    tcSetEnv (TcEnv tve tce ce gve lve' gtvs') scope
\end{code}

@tcGetGlobalTyVars@ returns a fully-zonked set of tyvars free in the environment.
To improve subsequent calls to the same function it writes the zonked set back into
the environment.

\begin{code}
tcGetGlobalTyVars :: NF_TcM s (TcTyVarSet s)
tcGetGlobalTyVars
  = tcGetEnv 						`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
    tcReadMutVar gtvs					`thenNF_Tc` \ global_tvs ->
    zonkTcTyVars (varSetElems global_tvs)		`thenNF_Tc` \ global_tys' ->
    let
	global_tvs' = (tyVarsOfTypes global_tys')
    in
    tcWriteMutVar gtvs global_tvs'			`thenNF_Tc_` 
    returnNF_Tc global_tvs'

tcExtendGlobalTyVars extra_global_tvs scope
  = tcGetEnv				`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
    tcReadMutVar gtvs			`thenNF_Tc` \ global_tvs ->
    let
	new_global_tyvars = global_tvs `unionVarSet` extra_global_tvs
    in
    tcNewMutVar new_global_tyvars	`thenNF_Tc` \ gtvs' ->
    tcSetEnv (TcEnv tve tce ce gve lve gtvs') scope
\end{code}

\begin{code}
tcLookupLocalValue :: Name -> NF_TcM s (Maybe (TcIdBndr s))
tcLookupLocalValue name
  = tcGetEnv 		`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
    returnNF_Tc (lookupUFM lve name)

tcLookupLocalValueByKey :: Unique -> NF_TcM s (Maybe (TcIdBndr s))
tcLookupLocalValueByKey uniq
  = tcGetEnv 		`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
    returnNF_Tc (lookupUFM_Directly lve uniq)

tcLookupLocalValueOK :: String -> Name -> NF_TcM s (TcIdBndr s)
tcLookupLocalValueOK err name
  = tcGetEnv 		`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
    returnNF_Tc (lookupWithDefaultUFM lve (panic err) name)


tcLookupGlobalValue :: Name -> NF_TcM s Id
tcLookupGlobalValue name
  = case maybeWiredInIdName name of
	Just id -> returnNF_Tc id
	Nothing -> tcGetEnv 		`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
		   returnNF_Tc (lookupWithDefaultUFM gve def name)
  where
    def = pprPanic "tcLookupGlobalValue:" (ppr name)

tcLookupGlobalValueMaybe :: Name -> NF_TcM s (Maybe Id)
tcLookupGlobalValueMaybe name
  = case maybeWiredInIdName name of
	Just id -> returnNF_Tc (Just id)
	Nothing -> tcGetEnv 		`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
		   returnNF_Tc (lookupUFM gve name)


tcLookupGlobalValueByKey :: Unique -> NF_TcM s Id
tcLookupGlobalValueByKey uniq
  = tcGetEnv 		`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
    returnNF_Tc (lookupGlobalByKey gve uniq)

lookupGlobalByKey :: GlobalValueEnv -> Unique -> Id
lookupGlobalByKey gve uniq
  = lookupWithDefaultUFM_Directly gve def uniq
  where
#ifdef DEBUG
    def = pprPanic "tcLookupGlobalValueByKey:" (pprUnique10 uniq)
#else
    def = panic "tcLookupGlobalValueByKey"
#endif

tcLookupGlobalValueByKeyMaybe :: Unique -> NF_TcM s (Maybe Id)
tcLookupGlobalValueByKeyMaybe uniq
  = tcGetEnv 		`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
    returnNF_Tc (lookupUFM_Directly gve uniq)

tcGetGlobalValEnv :: NF_TcM s GlobalValueEnv
tcGetGlobalValEnv
  = tcGetEnv 		`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
    returnNF_Tc gve

tcSetGlobalValEnv :: GlobalValueEnv -> TcM s a -> TcM s a
tcSetGlobalValEnv gve scope
  = tcGetEnv		`thenNF_Tc` \ (TcEnv tve tce ce _ lve gtvs) ->
    tcSetEnv (TcEnv tve tce ce gve lve gtvs) scope


-- Non-monadic version, environment given explicitly
tcExplicitLookupGlobal :: GlobalValueEnv -> Name -> Maybe Id
tcExplicitLookupGlobal gve name
  = case maybeWiredInIdName name of
	Just id -> Just id
	Nothing -> lookupUFM gve name

	-- Extract the IdInfo from an IfaceSig imported from an interface file
tcAddImportedIdInfo :: GlobalValueEnv -> Id -> Id
tcAddImportedIdInfo unf_env id
  | isLocallyDefined id		-- Don't look up locally defined Ids, because they
				-- have explicit local definitions, so we get a black hole!
  = id
  | otherwise
  = id `setIdInfo` new_info
	-- The Id must be returned without a data dependency on maybe_id
  where
    new_info = -- pprTrace "tcAdd" (ppr id) $
	       case tcExplicitLookupGlobal unf_env (getName id) of
		     Nothing	      -> noIdInfo
		     Just imported_id -> idInfo imported_id
		-- ToDo: could check that types are the same
\end{code}


Constructing new Ids
~~~~~~~~~~~~~~~~~~~~

\begin{code}
newLocalId :: OccName -> TcType s -> NF_TcM s (TcIdBndr s)
newLocalId name ty
  = tcGetUnique		`thenNF_Tc` \ uniq ->
    returnNF_Tc (mkUserLocal name uniq ty)

newLocalIds :: [OccName] -> [TcType s] -> NF_TcM s [TcIdBndr s]
newLocalIds names tys
  = tcGetUniques (length names) `thenNF_Tc` \ uniqs ->
    let
	new_ids            = zipWith3Equal "newLocalIds" mk_id names uniqs tys
	mk_id name uniq ty = mkUserLocal name uniq ty
    in
    returnNF_Tc new_ids

newSpecPragmaId :: Name -> TcType s -> NF_TcM s (TcIdBndr s)
newSpecPragmaId name ty 
  = tcGetUnique		`thenNF_Tc` \ uniq ->
    returnNF_Tc (mkSpecPragmaId (nameOccName name) uniq ty)
\end{code}


\begin{code}
classAsTyConErr name
  = ptext SLIT("Class used as a type constructor:") <+> ppr name

tyConAsClassErr name
  = ptext SLIT("Type constructor used as a class:") <+> ppr name

badCon con_id
  = quotes (ppr con_id) <+> ptext SLIT("is not a data constructor")
badPrimOp op
  = quotes (ppr op) <+> ptext SLIT("is not a primop")
\end{code}
