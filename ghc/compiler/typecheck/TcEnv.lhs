\begin{code}
#include "HsVersions.h"

module TcEnv(
	TcEnv, 

	initEnv, getEnv_LocalIds, getEnv_TyCons, getEnv_Classes,
	
	tcExtendTyVarEnv, tcLookupTyVar, 

	tcExtendTyConEnv, tcLookupTyCon, tcLookupTyConByKey, 
	tcExtendClassEnv, tcLookupClass, tcLookupClassByKey,
	tcGetTyConsAndClasses,

	tcExtendGlobalValEnv, tcExtendLocalValEnv,
	tcLookupLocalValue, tcLookupLocalValueOK, tcLookupLocalValueByKey, 
	tcLookupGlobalValue, tcLookupGlobalValueByKey, tcLookupGlobalValueMaybe,
	tcAddImportedIdInfo,
	tcLookupGlobalValueByKeyMaybe, 

	newMonoIds, newLocalIds, newLocalId,
	tcGetGlobalTyVars, tcExtendGlobalTyVars
  ) where


IMP_Ubiq()
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 201
IMPORT_DELOOPER(TcMLoop)  -- for paranoia checking
#else
import {-# SOURCE #-} TcType
#endif

import HsTypes	( HsTyVar(..) )
import Id	( SYN_IE(Id), GenId, idType, mkUserLocal, mkUserId, replaceIdInfo, getIdInfo )
import PragmaInfo ( PragmaInfo(..) )
import TcHsSyn	( SYN_IE(TcIdBndr), TcIdOcc(..) )
import TcKind	( TcKind, newKindVars, newKindVar, tcDefaultKind, kindToTcKind, Kind )
import TcType	( SYN_IE(TcType), TcMaybe, SYN_IE(TcTyVar), SYN_IE(TcTyVarSet),
		  newTyVarTys, tcInstTyVars, zonkTcTyVars
		)
import TyVar	( unionTyVarSets, emptyTyVarSet, tyVarSetToList, SYN_IE(TyVar) )
import PprType	( GenTyVar )
import Type	( tyVarsOfTypes, splitForAllTy )
import TyCon	( TyCon, tyConKind, synTyConArity, SYN_IE(Arity) )
import Class	( SYN_IE(Class), GenClass, classSig )

import TcMonad

import IdInfo		( noIdInfo )
import Name		( Name, OccName(..), getSrcLoc, occNameString,
			  maybeWiredInTyConName, maybeWiredInIdName,
			  NamedThing(..)
			)
import Pretty
import Unique		( pprUnique10{-, pprUnique ToDo:rm-}, Unique, Uniquable(..) )
import UniqFM	     
import Util		( zipEqual, zipWithEqual, zipWith3Equal, zipLazy,
			  panic, pprPanic, pprTrace
			)
import Outputable
\end{code}

Data type declarations
~~~~~~~~~~~~~~~~~~~~~

\begin{code}
data TcEnv s = TcEnv
		  (TyVarEnv s)
		  (TyConEnv s)
		  (ClassEnv s)
		  (ValueEnv Id)			-- Globals
		  (ValueEnv (TcIdBndr s))	-- Locals
		  (MutableVar s (TcTyVarSet s))	-- Free type variables of locals
						-- ...why mutable? see notes with tcGetGlobalTyVars

type TyVarEnv s  = UniqFM (TcKind s, TyVar)
type TyConEnv s  = UniqFM (TcKind s, Maybe Arity, TyCon)	-- Arity present for Synonyms only
type ClassEnv s  = UniqFM (TcKind s, Class)
type ValueEnv id = UniqFM id

initEnv :: MutableVar s (TcTyVarSet s) -> TcEnv s
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

Extending the environments.  Notice the uses of @zipLazy@, which makes sure
that the knot-tied TyVars, TyCons and Classes aren't looked at too early.

\begin{code}
tcExtendTyConEnv :: [(Name,Maybe Arity)] -> [TyCon] -> TcM s r -> TcM s r

tcExtendTyConEnv names_w_arities tycons scope
  = newKindVars (length names_w_arities)	`thenNF_Tc` \ kinds ->
    tcGetEnv					`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
    let
	tce' = addListToUFM tce [ (name, (kind, arity, tycon)) 
				| ((name,arity), (kind,tycon))
				  <- zipEqual "tcExtendTyConEnv" names_w_arities (kinds `zipLazy` tycons)
				]
    in
    tcSetEnv (TcEnv tve tce' ce gve lve gtvs) scope	`thenTc` \ result ->
    mapNF_Tc tcDefaultKind kinds			`thenNF_Tc_`
    returnTc result 


tcExtendClassEnv :: [Name] -> [Class] -> TcM s r -> TcM s r
tcExtendClassEnv names classes scope
  = newKindVars (length names)	`thenNF_Tc` \ kinds ->
    tcGetEnv			`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
    let
	ce' = addListToUFM ce (zipEqual "tcExtendClassEnv" names (kinds `zipLazy` classes))
    in
    tcSetEnv (TcEnv tve tce ce' gve lve gtvs) scope	`thenTc` \ result ->
    mapNF_Tc tcDefaultKind kinds			`thenNF_Tc_`
    returnTc result 
\end{code}


Looking up in the environments.

\begin{code}
tcLookupTyVar name
  = tcGetEnv		`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
    returnNF_Tc (lookupWithDefaultUFM tve (pprPanic "tcLookupTyVar:" (ppr PprShowAll name)) name)


tcLookupTyCon name
  = case maybeWiredInTyConName name of
	Just tc -> returnTc (kindToTcKind (tyConKind tc), synTyConArity tc, tc)
	Nothing -> tcGetEnv		`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
		   case lookupUFM tce name of
			Just stuff -> returnTc stuff
			Nothing    -> 	-- Could be that he's using a class name as a type constructor
				      case lookupUFM ce name of
					Just _  -> failTc (classAsTyConErr name)
					Nothing -> pprPanic "tcLookupTyCon:" (ppr PprDebug name)

tcLookupTyConByKey uniq
  = tcGetEnv		`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
    let 
       (kind, arity, tycon) =  lookupWithDefaultUFM_Directly tce 
					(pprPanic "tcLookupTyCon:" (pprUnique10 uniq)) 
					uniq
    in
    returnNF_Tc tycon

tcLookupClass name
  = tcGetEnv		`thenNF_Tc` \ (TcEnv tve tce ce gve lve gtvs) ->
--  pprTrace "tcLookupClass:" (hsep [text "Uniq:", pprUnique10 (uniqueOf name), text "; avail:", hsep (map (pprUnique10 . fst) (ufmToList ce))]) $
--  pprTrace "tcLookupClass:" (hsep [text "Uniq:", pprUnique (uniqueOf name), text "; avail:", hsep (map (pprUnique . fst) (ufmToList ce))]) $
    case lookupUFM ce name of
	Just stuff -> returnTc stuff
	Nothing	   -> 	-- Could be that he's using a type constructor as a class
			case lookupUFM tce name of
			  Just _ ->  failTc (tyConAsClassErr name)
			  Nothing -> pprPanic "tcLookupClass:" (ppr PprShowAll name)

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
	new_global_tyvars = global_tvs `unionTyVarSets` extra_global_tvs
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
    def = pprPanic "tcLookupGlobalValue:" (ppr PprDebug name)

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

	-- Extract the IdInfo from an IfaceSig imported from an interface file
tcAddImportedIdInfo :: Id -> NF_TcM s Id
tcAddImportedIdInfo id
  = tcLookupGlobalValueMaybe (getName id)	`thenNF_Tc` \ maybe_id ->
    let 
	new_info = case maybe_id of
		     Nothing	      -> noIdInfo
		     Just imported_id -> getIdInfo imported_id
		-- ToDo: could check that types are the same
    in
    returnNF_Tc (id `replaceIdInfo` new_info)
	-- The Id must be returned without a data dependency on maybe_id
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
	mk_id name ty = mkUserId name ty NoPragmaInfo
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
\end{code}

\begin{code}
classAsTyConErr name sty
  = hcat [ptext SLIT("Class used as a type constructor: "), ppr sty name]

tyConAsClassErr name sty
  = hcat [ptext SLIT("Type constructor used as a class: "), ppr sty name]
\end{code}
