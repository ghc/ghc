\begin{code}
#include "HsVersions.h"

module TcEnv(
	TcEnv, 

	initEnv, getEnv_LocalIds, getEnv_TyCons, getEnv_Classes,
	
	tcExtendKindEnv, tcExtendTyVarEnv, tcExtendTyConEnv, tcExtendClassEnv,
	tcLookupTyVar, tcLookupTyCon, tcLookupClass, tcLookupClassByKey,

	tcExtendGlobalValEnv, tcExtendLocalValEnv,
	tcLookupLocalValue, tcLookupLocalValueOK,
	tcLookupGlobalValue, tcLookupGlobalValueByKey,

	tcTyVarScope, newMonoIds, newLocalIds,
	tcGetGlobalTyVars
  ) where


import Ubiq
import TcMLoop  -- for paranoia checking

import Id	( Id(..), GenId, idType, mkUserLocal )
import TcHsSyn	( TcIdBndr(..) )
import TcKind	( TcKind, newKindVars, tcKindToKind, kindToTcKind )
import TcType	( TcType(..), TcMaybe, TcTyVar(..), TcTyVarSet(..), newTyVarTys, zonkTcTyVars )
import TyVar	( mkTyVar, getTyVarKind, unionTyVarSets, emptyTyVarSet )
import Type	( tyVarsOfTypes )
import TyCon	( TyCon, getTyConKind )
import Class	( Class(..), GenClass, getClassSig )

import TcMonad

import Name	( Name(..), getNameShortName )
import PprStyle
import Pretty
import Unique	( Unique )
import UniqFM
import Util	( zipWithEqual, zipWith3Equal, zipLazy, panic )
\end{code}

Data type declarations
~~~~~~~~~~~~~~~~~~~~~

\begin{code}
data TcEnv s = TcEnv
		  (TyVarEnv s)
		  (ValueEnv Id)			-- Globals
		  (ValueEnv (TcIdBndr s))	-- Locals
		  (MutableVar s (TcTyVarSet s))	-- Free type variables of locals
						-- ...why mutable? see notes with tcGetGlobalTyVars
		  (KindEnv s) 			-- Gives TcKinds of TyCons and Classes
		  TyConEnv
		  ClassEnv

type TyVarEnv s  = UniqFM (TcKind s, TyVar)
type TyConEnv    = UniqFM TyCon
type KindEnv s   = UniqFM (TcKind s)
type ClassEnv    = UniqFM Class
type ValueEnv id = UniqFM id

initEnv :: MutableVar s (TcTyVarSet s) -> TcEnv s
initEnv mut = TcEnv emptyUFM emptyUFM emptyUFM mut emptyUFM emptyUFM emptyUFM 

getEnv_LocalIds (TcEnv _ _ ls _ _ _ _) = ls
getEnv_TyCons   (TcEnv _ _ _ _ _ ts _) = ts
getEnv_Classes  (TcEnv _ _ _ _ _ _ cs) = cs
\end{code}

Making new TcTyVars, with knot tying!
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
tcTyVarScope :: [Name]			-- Names of some type variables
	     -> ([TyVar] -> TcM s a)	-- Thing to type check in their scope
	     -> TcM s a			-- Result

tcTyVarScope tyvar_names thing_inside
  = newKindVars (length tyvar_names)	`thenNF_Tc` \ tyvar_kinds ->

    fixTc (\ ~(tyvars, _) ->
		-- Ok to look at kinds, but not tyvars!
      tcExtendTyVarEnv tyvar_names (tyvar_kinds `zipLazy` tyvars) (

		-- Do the thing inside
	thing_inside tyvars			`thenTc` \ result ->
 
		-- Get the tyvar's Kinds from their TcKinds
	mapNF_Tc tcKindToKind tyvar_kinds	`thenNF_Tc` \ tyvar_kinds' ->

		-- Construct the real TyVars
	let
	  tyvars	     = zipWithEqual mk_tyvar tyvar_names tyvar_kinds'
	  mk_tyvar name kind = mkTyVar (getNameShortName name) (getItsUnique name) kind
	in
	returnTc (tyvars, result)
    ))					`thenTc` \ (_,result) ->
    returnTc result
\end{code}


The Kind, TyVar, Class and TyCon envs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Extending the environments

\begin{code}
tcExtendKindEnv :: [Name] -> [TcKind s] -> TcM s r -> TcM s r
tcExtendKindEnv names kinds scope
  = tcGetEnv		`thenNF_Tc` \ (TcEnv tve gve lve gtvs ke tce ce) ->
    let
	ke' = addListToUFM ke (names `zip` kinds)
    in
    tcSetEnv (TcEnv tve gve lve gtvs ke' tce ce) scope

tcExtendTyVarEnv :: [Name] -> [(TcKind s, TyVar)] -> TcM s r -> TcM s r
tcExtendTyVarEnv tyvar_names kinds_w_tyvars scope
  = tcGetEnv		`thenNF_Tc` \ (TcEnv tve gve lve gtvs ke tce ce) ->
    let
	tve' = addListToUFM tve (tyvar_names `zip` kinds_w_tyvars)
    in
    tcSetEnv (TcEnv tve' gve lve gtvs ke tce ce) scope

tcExtendTyConEnv tycons scope
  = tcGetEnv		`thenNF_Tc` \ (TcEnv tve gve lve gtvs ke tce ce) ->
    let
	tce' = addListToUFM_Directly tce [(getItsUnique tycon, tycon) | tycon <- tycons]
    in
    tcSetEnv (TcEnv tve gve lve gtvs ke tce' ce) scope

tcExtendClassEnv classes scope
  = tcGetEnv		`thenNF_Tc` \ (TcEnv tve gve lve gtvs ke tce ce) ->
    let
	ce' = addListToUFM_Directly ce [(getItsUnique clas, clas) | clas <- classes]
    in
    tcSetEnv (TcEnv tve gve lve gtvs ke tce ce') scope
\end{code}


Looking up in the environments

\begin{code}
tcLookupTyVar name
  = tcGetEnv		`thenNF_Tc` \ (TcEnv tve gve lve gtvs ke tce ce) ->
    returnNF_Tc (lookupWithDefaultUFM tve (panic "tcLookupTyVar") name)


tcLookupTyCon (WiredInTyCon tc)		-- wired in tycons
  = returnNF_Tc (kindToTcKind (getTyConKind tc), tc)

tcLookupTyCon name
  = tcGetEnv		`thenNF_Tc` \ (TcEnv tve gve lve gtvs ke tce ce) ->
    let
	tycon = lookupWithDefaultUFM tce (panic "tcLookupTyCon")	     name
	kind  = lookupWithDefaultUFM ke  (kindToTcKind (getTyConKind tycon)) name
		-- The KE will bind tycon in the current mutually-recursive set.
		-- If the KE doesn't, then the tycon is already defined, and we
		-- can safely grab the kind from the TyCon itself
    in
    returnNF_Tc (kind,tycon)


tcLookupClass name
  = tcGetEnv		`thenNF_Tc` \ (TcEnv tve gve lve gtvs ke tce ce) ->
    let
	clas = lookupWithDefaultUFM ce (panic "tcLookupClass")	           name
	(tyvar, _, _) = getClassSig clas
	kind = lookupWithDefaultUFM ke (kindToTcKind (getTyVarKind tyvar)) name
    in
    returnNF_Tc (kind,clas)

tcLookupClassByKey uniq
  = tcGetEnv		`thenNF_Tc` \ (TcEnv tve gve lve gtvs ke tce ce) ->
    let
	clas = lookupWithDefaultUFM_Directly ce (panic "tcLookupClas") uniq
    in
    returnNF_Tc (clas)
\end{code}



Extending and consulting the value environment
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
tcExtendGlobalValEnv ids scope
  = tcGetEnv		`thenNF_Tc` \ (TcEnv tve gve lve gtvs ke tce ce) ->
    let
	gve' = addListToUFM_Directly gve [(getItsUnique id, id) | id <- ids]
    in
    tcSetEnv (TcEnv tve gve' lve gtvs ke tce ce) scope

tcExtendLocalValEnv names ids scope
  = tcGetEnv		`thenNF_Tc` \ (TcEnv tve gve lve gtvs ke tce ce) ->
    tcReadMutVar gtvs	`thenNF_Tc` \ global_tvs ->
    let
	lve' = addListToUFM lve (names `zip` ids)
	extra_global_tyvars = tyVarsOfTypes (map idType ids)
	new_global_tyvars   = global_tvs `unionTyVarSets` extra_global_tyvars
    in
    tcNewMutVar new_global_tyvars	`thenNF_Tc` \ gtvs' ->

    tcSetEnv (TcEnv tve gve lve' gtvs' ke tce ce) scope
\end{code}

@tcGetGlobalTyVars@ returns a fully-zonked set of tyvars free in the environment.
To improve subsequent calls to the same function it writes the zonked set back into
the environment.

\begin{code}
tcGetGlobalTyVars :: NF_TcM s (TcTyVarSet s)
tcGetGlobalTyVars
  = tcGetEnv 				`thenNF_Tc` \ (TcEnv tve gve lve gtvs ke tce ce) ->
    tcReadMutVar gtvs			`thenNF_Tc` \ global_tvs ->
    zonkTcTyVars global_tvs		`thenNF_Tc` \ global_tvs' ->
    tcWriteMutVar gtvs global_tvs'	`thenNF_Tc_`
    returnNF_Tc global_tvs'
\end{code}

\begin{code}
tcLookupLocalValue :: Name -> NF_TcM s (Maybe (TcIdBndr s))
tcLookupLocalValue name
  = tcGetEnv 		`thenNF_Tc` \ (TcEnv tve gve lve gtvs ke tce ce) ->
    returnNF_Tc (lookupUFM lve name)

tcLookupLocalValueOK :: String -> Name -> NF_TcM s (TcIdBndr s)
tcLookupLocalValueOK err name
  = tcGetEnv 		`thenNF_Tc` \ (TcEnv tve gve lve gtvs ke tce ce) ->
    returnNF_Tc (lookupWithDefaultUFM lve (panic err) name)


tcLookupGlobalValue :: Name -> NF_TcM s Id

tcLookupGlobalValue (WiredInVal id)	-- wired in ids
  = returnNF_Tc id

tcLookupGlobalValue name
  = tcGetEnv 		`thenNF_Tc` \ (TcEnv tve gve lve gtvs ke tce ce) ->
    returnNF_Tc (lookupWithDefaultUFM gve def name)
  where
#ifdef DEBUG
    def = panic ("tcLookupGlobalValue:" ++ ppShow 1000 (ppr PprDebug name))
#else
    def = panic "tcLookupGlobalValue"
#endif


tcLookupGlobalValueByKey :: Unique -> NF_TcM s Id
tcLookupGlobalValueByKey uniq
  = tcGetEnv 		`thenNF_Tc` \ (TcEnv tve gve lve gtvs ke tce ce) ->
    returnNF_Tc (lookupWithDefaultUFM_Directly gve def uniq)
  where
#ifdef DEBUG
    def = panic ("tcLookupGlobalValueByKey:" ++ ppShow 1000 (ppr PprDebug uniq))
#else
    def = panic "tcLookupGlobalValueByKey"
#endif

\end{code}


Constructing new Ids
~~~~~~~~~~~~~~~~~~~~

\begin{code}
newMonoIds :: [Name] -> Kind -> ([TcIdBndr s] -> TcM s a) -> TcM s a
newMonoIds names kind m
  = newTyVarTys no_of_names kind	`thenNF_Tc` \ tys ->
    tcGetUniques no_of_names		`thenNF_Tc` \ uniqs ->
    let
	new_ids            = zipWith3Equal mk_id names uniqs tys
	mk_id name uniq ty = mkUserLocal (getOccurrenceName name) uniq ty
					 (getSrcLoc name)
    in
    tcExtendLocalValEnv names new_ids (m new_ids)
  where
    no_of_names = length names

newLocalIds :: [FAST_STRING] -> [TcType s] -> NF_TcM s [TcIdBndr s]
newLocalIds names tys
  = tcGetSrcLoc			`thenNF_Tc` \ loc ->
    tcGetUniques (length names) `thenNF_Tc` \ uniqs ->
    let
	new_ids            = zipWith3Equal mk_id names uniqs tys
	mk_id name uniq ty = mkUserLocal name uniq ty loc
    in
    returnNF_Tc new_ids
\end{code}


