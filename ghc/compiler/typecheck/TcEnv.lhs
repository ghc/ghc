\begin{code}
module TcEnv(
	TcIdOcc(..), TcIdBndr, tcIdType, tcIdTyVars, tcInstId,

	TcEnv, 

	initEnv, getEnv_LocalIds, getEnv_TyCons, getEnv_Classes,
	
	tcExtendTyVarEnv, tcLookupTyVar, 

	tcExtendTyConEnv, tcLookupTyCon, tcLookupTyConByKey, 
	tcExtendClassEnv, tcLookupClass, tcLookupClassByKey,
	tcGetTyConsAndClasses,

	tcExtendGlobalValEnv, tcExtendLocalValEnv,
	tcLookupLocalValue, tcLookupLocalValueOK, tcLookupLocalValueByKey, 
	tcLookupGlobalValue, tcLookupGlobalValueByKey, tcLookupGlobalValueMaybe,
	tcAddImportedIdInfo, tcExplicitLookupGlobal,
	tcLookupGlobalValueByKeyMaybe, 

	newMonoIds, newLocalIds, newLocalId, newSpecPragmaId,
	tcGetGlobalTyVars, tcExtendGlobalTyVars
  ) where

#include "HsVersions.h"

import MkId	( mkUserLocal, mkUserId, mkSpecPragmaId )
import Id	( Id, GenId, idType, replaceIdInfo, idInfo )
import TcKind	( TcKind, kindToTcKind, Kind )
import TcType	( TcType, TcMaybe, TcTyVar, TcTyVarSet, TcThetaType,
		  newTyVarTys, tcInstTyVars, zonkTcTyVars, tcInstType
		)
import TyVar	( mkTyVarSet, unionTyVarSets, emptyTyVarSet, TyVar )
import Type	( tyVarsOfType, tyVarsOfTypes, splitForAllTys, splitRhoTy )
import TyCon	( TyCon, tyConKind, tyConArity, isSynTyCon, Arity )
import Class	( Class )

import TcMonad

import IdInfo		( noIdInfo )
import Name		( Name, OccName(..), nameOccName,
			  maybeWiredInTyConName, maybeWiredInIdName, isLocallyDefined,
			  NamedThing(..)
			)
import Unique		( pprUnique10{-, pprUnique ToDo:rm-}, Unique, Uniquable(..) )
import UniqFM	     
import Util		( zipEqual, zipWithEqual, zipWith3Equal )
import Maybes		( maybeToBool )
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection{TcId, TcIdOcc}
%*									*
%************************************************************************


\begin{code}
type TcIdBndr s = GenId  (TcType s)	-- Binders are all TcTypes
data TcIdOcc  s = TcId   (TcIdBndr s)	-- Bindees may be either
		| RealId Id

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
tcIdTyVars (RealId _) = emptyTyVarSet		-- Top level Ids have no free type variables


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
    tcInstType tenv rho		`thenNF_Tc` \ rho' ->
    let
	(theta', tau') = splitRhoTy rho'
    in
    returnNF_Tc (tyvars', theta', tau')
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
		  (TyVarEnv s)
		  (TyConEnv s)
		  (ClassEnv s)
		  (ValueEnv Id)			-- Globals
		  (ValueEnv (TcIdBndr s))	-- Locals
		  (TcRef s (TcTyVarSet s))	-- Free type variables of locals
						-- ...why mutable? see notes with tcGetGlobalTyVars

type TyVarEnv s  = UniqFM (TcKind s, TyVar)
type TyConEnv s  = UniqFM (TcKind s, Maybe Arity, TyCon)	-- Arity present for Synonyms only
type ClassEnv s  = UniqFM ([TcKind s], Class)		-- The kinds are the kinds of the args
							-- to the class
type ValueEnv id = UniqFM id

initEnv :: TcRef s (TcTyVarSet s) -> TcEnv s
initEnv mut = TcEnv emptyUFM emptyUFM emptyUFM emptyUFM emptyUFM mut 

getEnv_LocalIds (TcEnv _ _ _ _ ls _) = eltsUFM ls
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
	gve' = addListToUFM_Directly gve [(uniqueOf id, id) | id <- ids]
    in
    tcSetEnv (TcEnv tve tce ce gve' lve gtvs) scope

tcExtendLocalValEnv names ids scope
  = tcGetEnv		`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
    tcReadMutVar gtvs	`thenNF_Tc` \ global_tvs ->
    let
	lve' = addListToUFM lve (zipEqual "tcExtendLocalValEnv" names ids)
	extra_global_tyvars = tyVarsOfTypes (map idType ids)
	new_global_tyvars   = global_tvs `unionTyVarSets` extra_global_tyvars
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
  = tcGetEnv 				`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
    tcReadMutVar gtvs			`thenNF_Tc` \ global_tvs ->
    zonkTcTyVars global_tvs		`thenNF_Tc` \ global_tvs' ->
    tcWriteMutVar gtvs global_tvs'	`thenNF_Tc_` 
    returnNF_Tc global_tvs'

tcExtendGlobalTyVars extra_global_tvs scope
  = tcGetEnv				`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
    tcReadMutVar gtvs			`thenNF_Tc` \ global_tvs ->
    let
	new_global_tyvars = global_tvs `unionTyVarSets` mkTyVarSet extra_global_tvs
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
    returnNF_Tc (lookupWithDefaultUFM_Directly gve def uniq)
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


-- Non-monadic version, environment given explicitly
tcExplicitLookupGlobal :: TcEnv s -> Name -> Maybe Id
tcExplicitLookupGlobal (TcEnv tve tce ce gve lve gtvs) name
  = case maybeWiredInIdName name of
	Just id -> Just id
	Nothing -> lookupUFM gve name

	-- Extract the IdInfo from an IfaceSig imported from an interface file
tcAddImportedIdInfo :: TcEnv s -> Id -> Id
tcAddImportedIdInfo unf_env id
  | isLocallyDefined id		-- Don't look up locally defined Ids, because they
				-- have explicit local definitions, so we get a black hole!
  = id
  | otherwise
  = id `replaceIdInfo` new_info
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
-- Uses the Name as the Name of the Id
newMonoIds :: [Name] -> Kind -> ([TcIdBndr s] -> TcM s a) -> TcM s a

newMonoIds names kind m
  = newTyVarTys no_of_names kind	`thenNF_Tc` \ tys ->
    let
	new_ids       = zipWithEqual "newMonoIds" mk_id names tys
	mk_id name ty = mkUserId name ty
    in
    tcExtendLocalValEnv names new_ids (m new_ids)
  where
    no_of_names = length names

newLocalId :: OccName -> TcType s -> NF_TcM s (TcIdBndr s)
newLocalId name ty
  = tcGetSrcLoc		`thenNF_Tc` \ loc ->
    tcGetUnique		`thenNF_Tc` \ uniq ->
    returnNF_Tc (mkUserLocal name uniq ty loc)

newLocalIds :: [OccName] -> [TcType s] -> NF_TcM s [TcIdBndr s]
newLocalIds names tys
  = tcGetSrcLoc			`thenNF_Tc` \ loc ->
    tcGetUniques (length names) `thenNF_Tc` \ uniqs ->
    let
	new_ids            = zipWith3Equal "newLocalIds" mk_id names uniqs tys
	mk_id name uniq ty = mkUserLocal name uniq ty loc
    in
    returnNF_Tc new_ids

newSpecPragmaId :: Name -> TcType s -> NF_TcM s (TcIdBndr s)
newSpecPragmaId name ty 
  = tcGetSrcLoc		`thenNF_Tc` \ loc ->
    tcGetUnique		`thenNF_Tc` \ uniq ->
    returnNF_Tc (mkSpecPragmaId (nameOccName name) uniq ty loc)
\end{code}


\begin{code}
classAsTyConErr name
  = ptext SLIT("Class used as a type constructor:") <+> ppr name

tyConAsClassErr name
  = ptext SLIT("Type constructor used as a class:") <+> ppr name
\end{code}
