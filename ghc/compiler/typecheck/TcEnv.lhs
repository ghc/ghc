\begin{code}
module TcEnv(
	TcId, TcIdSet, 
	TyThing(..), TyThingDetails(..),

	-- Getting stuff from the environment
	TcEnv, initTcEnv, 
	tcEnvTyCons, tcEnvClasses, tcEnvIds, tcEnvTcIds,
	
	-- Instance environment
	tcGetInstEnv, tcSetInstEnv, 

	-- Global environment
	tcExtendGlobalEnv, tcExtendGlobalValEnv, 
	tcLookupTy, tcLookupTyCon, tcLookupClass, tcLookupGlobalId, tcLookupDataCon,

	-- Local environment
	tcExtendKindEnv, 
	tcExtendTyVarEnv, tcExtendTyVarEnvForMeths, 
	tcExtendLocalValEnv,

	-- Global type variables
	tcGetGlobalTyVars, tcExtendGlobalTyVars,

	-- Random useful things
	tcAddImportedIdInfo, tcInstId,

	-- New Ids
	newLocalId, newSpecPragmaId,
	newDefaultMethodName, newDFunName
  ) where

#include "HsVersions.h"

import TcMonad
import TcType	( TcKind,  TcType, TcTyVar, TcTyVarSet, TcThetaType,
		  tcInstTyVars, zonkTcTyVars,
		)
import Id	( mkUserLocal, isDataConWrapId_maybe )
import IdInfo	( vanillaIdInfo )
import MkId 	( mkSpecPragmaId )
import Var	( TyVar, Id, setVarName,
		  idType, lazySetIdInfo, idInfo, tyVarKind, UVar,
		)
import VarSet
import VarEnv	( TyVarSubstEnv )
import Type	( Kind, Type, superKind,
		  tyVarsOfType, tyVarsOfTypes,
		  splitForAllTys, splitRhoTy, splitFunTys,
		  splitAlgTyConApp_maybe, getTyVar, getDFunTyKey
		)
import DataCon	( DataCon )
import TyCon	( TyCon, tyConKind, tyConArity, isSynTyCon )
import Class	( Class, ClassOpItem, ClassContext, classTyCon )
import Subst	( substTy )
import Name	( Name, OccName, Provenance(..), ExportFlag(..), NamedThing(..), 
		  nameOccName, nameModule, getSrcLoc, mkGlobalName,
		  maybeWiredInTyConName, maybeWiredInIdName, isLocallyDefined,
		  NameEnv, emptyNameEnv, lookupNameEnv, nameEnvElts, 
		  extendNameEnv, extendNameEnvList
		)
import OccName	( mkDFunOcc, mkDefaultMethodOcc, occNameString )
import Module	( Module )
import Unify	( unifyTyListsX, matchTys )
import HscTypes	( ModDetails(..), InstEnv, lookupTypeEnv )
import Unique	( pprUnique10, Unique, Uniquable(..) )
import UniqFM
import Unique	( Uniquable(..) )
import Util	( zipEqual, zipWith3Equal, mapAccumL )
import SrcLoc	( SrcLoc )
import FastString	( FastString )
import Maybes
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection{TcEnv}
%*									*
%************************************************************************

\begin{code}
type TcId    = Id 			-- Type may be a TcType
type TcIdSet = IdSet

data TcEnv
  = TcEnv {
	tcGST  	 :: GlobalSymbolTable,	-- The symbol table at the moment we began this compilation

	tcInsts	 :: InstEnv,		-- All instances (both imported and in this module)

	tcGEnv	 :: NameEnv TyThing,	-- The global type environment we've accumulated while
					-- compiling this module:
					--	types and classes (both imported and local)
					-- 	imported Ids
					-- (Ids defined in this module are in the local envt)

	tcLEnv 	 :: NameEnv TcTyThing,	-- The local type environment: Ids and TyVars
					-- defined in this module

	tcTyVars :: TcRef TcTyVarSet	-- The "global tyvars"
					-- Namely, the in-scope TyVars bound in tcLEnv, plus the tyvars
					-- mentioned in the types of Ids bound in tcLEnv
					-- Why mutable? see notes with tcGetGlobalTyVars
    }

\end{code}

The Global-Env/Local-Env story
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
During type checking, we keep in the GlobalEnv
	* All types and classes
	* All Ids derived from types and classes (constructors, selectors)
	* Imported Ids

At the end of type checking, we zonk the local bindings,
and as we do so we add to the GlobalEnv
	* Locally defined top-level Ids

Why?  Because they are now Ids not TcIds.  This final GlobalEnv is
used thus:
	a) fed back (via the knot) to typechecking the 
	   unfoldings of interface signatures

	b) used to augment the GlobalSymbolTable


\begin{code}
data TcTyThing
  = AGlobal TyThing	-- Used only in the return type of a lookup
  | ATcId  TcId		-- Ids defined in this module
  | ATyVar TyVar	-- Type variables
  | AThing TcKind	-- Used temporarily, during kind checking
-- Here's an example of how the AThing guy is used
-- Suppose we are checking (forall a. T a Int):
--	1. We first bind (a -> AThink kv), where kv is a kind variable. 
--	2. Then we kind-check the (T a Int) part.
--	3. Then we zonk the kind variable.
--	4. Now we know the kind for 'a', and we add (a -> ATyVar a::K) to the environment

initTcEnv :: GlobalSymbolTable -> InstEnv -> IO TcEnv
initTcEnv gst inst_env
  = do { gtv_var <- newIORef emptyVarSet
	 return (TcEnv { tcGST    = gst,
		      	 tcGEnv   = emptyNameEnv,
		      	 tcInsts  = inst_env,
		      	 tcLEnv   = emptyNameEnv,
		      	 tcTyVars = gtv_var
	 })}

tcEnvClasses env = [cl | AClass cl <- nameEnvElts (tcGEnv env)]
tcEnvTyCons  env = [tc | ATyCon tc <- nameEnvElts (tcGEnv env)] 
tcEnvIds     env = [id | AnId   id <- nameEnvElts (tcGEnv env)] 
tcEnvTyVars  env = [tv | ATyVar tv <- nameEnvElts (tcLEnv env)]
tcEnvTcIds   env = [id | ATcId  id <- nameEnvElts (tcLEnv env)]

-- This data type is used to help tie the knot
-- when type checking type and class declarations
data TyThingDetails = SynTyDetails Type
		    | DataTyDetails ClassContext [DataCon] [Class]
		    | ClassDetails ClassContext [Id] [ClassOpItem] DataCon
\end{code}


%************************************************************************
%*									*
\subsection{Basic lookups}
%*									*
%************************************************************************

\begin{code}
lookup_global :: TcEnv -> Name -> Maybe TyThing
	-- Try the global envt and then the global symbol table
lookup_global env name 
  = case lookupNameEnv (tcGEnv env) name of
	Just thing -> Just thing
	Nothing    -> lookupTypeEnv (tcGST env) name

lookup_local :: TcEnv -> Name -> Maybe TcTyThing
	-- Try the local envt and then try the global
lookup_local env name
  = case lookupNameEnv (tcLEnv env) name of
	Just thing -> Just thing
	Nothing    -> case lookup_global env name of
			Just thing -> AGlobal thing
			Nothing	   -> Nothing

explicitLookupId :: TcEnv -> Name -> Maybe Id
explicitLookupId env name = case lookup_global env name of
				Just (AnId id) -> Just id
				other	       -> Nothing
\end{code}


%************************************************************************
%*									*
\subsection{Random useful functions}
%*									*
%************************************************************************


\begin{code}
-- A useful function that takes an occurrence of a global thing
-- and instantiates its type with fresh type variables
tcInstId :: Id
	 -> NF_TcM ([TcTyVar], 	-- It's instantiated type
		      TcThetaType,	--
		      TcType)		--
tcInstId id
  = let
      (tyvars, rho) = splitForAllTys (idType id)
    in
    tcInstTyVars tyvars		`thenNF_Tc` \ (tyvars', arg_tys, tenv) ->
    let
	rho'	       = substTy tenv rho
	(theta', tau') = splitRhoTy rho' 
    in
    returnNF_Tc (tyvars', theta', tau')

tcAddImportedIdInfo :: TcEnv -> Id -> Id
tcAddImportedIdInfo unf_env id
  | isLocallyDefined id		-- Don't look up locally defined Ids, because they
				-- have explicit local definitions, so we get a black hole!
  = id
  | otherwise
  = id `lazySetIdInfo` new_info
	-- The Id must be returned without a data dependency on maybe_id
  where
    new_info = case explicitLookupId unf_env (getName id) of
		     Nothing	      -> vanillaIdInfo
		     Just imported_id -> idInfo imported_id
		-- ToDo: could check that types are the same
\end{code}


%************************************************************************
%*									*
\subsection{Making new Ids}
%*									*
%************************************************************************

Constructing new Ids

\begin{code}
newLocalId :: OccName -> TcType -> SrcLoc -> NF_TcM TcId
newLocalId name ty loc
  = tcGetUnique		`thenNF_Tc` \ uniq ->
    returnNF_Tc (mkUserLocal name uniq ty loc)

newSpecPragmaId :: Name -> TcType -> NF_TcM TcId
newSpecPragmaId name ty 
  = tcGetUnique		`thenNF_Tc` \ uniq ->
    returnNF_Tc (mkSpecPragmaId (nameOccName name) uniq ty (getSrcLoc name))
\end{code}

Make a name for the dict fun for an instance decl

\begin{code}
newDFunName :: Module -> Class -> [Type] -> SrcLoc -> NF_TcM Name
newDFunName mod clas (ty:_) loc
  = tcGetDFunUniq dfun_string	`thenNF_Tc` \ inst_uniq ->
    tcGetUnique			`thenNF_Tc` \ uniq ->
    returnNF_Tc (mkGlobalName uniq mod
			      (mkDFunOcc dfun_string inst_uniq) 
			      (LocalDef loc Exported))
  where
	-- Any string that is somewhat unique will do
    dfun_string = occNameString (getOccName clas) ++ occNameString (getDFunTyKey ty)

newDefaultMethodName :: Name -> SrcLoc -> NF_TcM Name
newDefaultMethodName op_name loc
  = tcGetUnique			`thenNF_Tc` \ uniq ->
    returnNF_Tc (mkGlobalName uniq (nameModule op_name)
			      (mkDefaultMethodOcc (getOccName op_name))
			      (LocalDef loc Exported))
\end{code}


%************************************************************************
%*									*
\subsection{The global environment}
%*									*
%************************************************************************

\begin{code}
tcExtendGlobalEnv :: [(Name, TyThing)] -> TcM r -> TcM r
tcExtendGlobalEnv bindings thing_inside
  = tcGetEnv				`thenNF_Tc` \ env ->
    let
	ge' = extendNameEnvList (tcGEnv env) bindings
    in
    tcSetEnv (env {tcGEnv = ge'}) thing_inside

tcExtendGlobalValEnv :: [Id] -> TcM a -> TcM a
tcExtendGlobalValEnv ids thing_inside
  = tcExtendGlobalEnv [(getName id, AnId id) | id <- ids] thing_inside
\end{code}


\begin{code}
tcLookupGlobal_maybe :: Name -> NF_TcM (Maybe TyThing)
tcLookupGlobal_maybe name
  = tcGetEnv		`thenNF_Tc` \ env ->
    returnNF_Tc (lookup_global env name)
\end{code}

A variety of global lookups, when we know what we are looking for.

\begin{code}
tcLookupGlobal :: Name -> NF_TcM TyThing
  = tcLookupGlobal_maybe name	`thenNF_Tc` \ maybe_thing ->
    case maybe_thing of
	Just thing -> returnNF_Tc thing
	other	   -> notFound "tcLookupGlobal:" name

tcLookupGlobalId :: Name -> NF_TcM Id
tcLookupGlobalId name
  = tcLookupGlobal_maybe name	`thenNF_Tc` \ maybe_id ->
    case maybe_id of
	Just (AnId clas) -> returnNF_Tc id
	other		 -> notFound "tcLookupGlobalId:" name
	
tcLookupDataCon :: Name -> TcM DataCon
tcLookupDataCon con_name
  = tcLookupGlobalId con_name		`thenNF_Tc` \ con_id ->
    case isDataConWrapId_maybe con_id of
 	Just data_con -> returnTc data_con
	Nothing	      -> failWithTc (badCon con_id)


tcLookupClass :: Name -> NF_TcM Class
tcLookupClass name
  = tcLookupGlobal_maybe name	`thenNF_Tc` \ maybe_clas ->
    case maybe_clas of
	Just (AClass clas) -> returnNF_Tc clas
	other		   -> notFound "tcLookupClass:" name
	
tcLookupTyCon :: Name -> NF_TcM TyCon
tcLookupTyCon name
  = tcLookupGlobal_maybe name	`thenNF_Tc` \ maybe_tc ->
    case maybe_tc of
	Just (ATyCon tc) -> returnNF_Tc tc
	other		 -> notFound "tcLookupTyCon:" name
\end{code}


%************************************************************************
%*									*
\subsection{The local environment}
%*									*
%************************************************************************

\begin{code}
tcLookup_maybe :: Name -> NF_TcM (Maybe TcTyThing)
tcLookup_maybe name
  = tcGetEnv 		`thenNF_Tc` \ env ->
    returnNF_Tc (lookup_local env name)

tcLookup :: Name -> NF_TcM TcTyThing
tcLookup name
  = tcLookup_maybe name		`thenNF_Tc` \ maybe_thing ->
    case maybe_thing of
	Just thing -> returnNF_Tc thing
	other	   -> notFound "tcLookup:" name
	-- Extract the IdInfo from an IfaceSig imported from an interface file
\end{code}


\begin{code}
tcExtendKindEnv :: [(Name,TcKind)] -> TcM r -> TcM r
tcExtendKindEnv pairs thing_inside
  = tcGetEnv				`thenNF_Tc` \ env ->
    let
 	le' = extendNameEnvList (tcLEnv env) [(n, AThing k) | (n,k) <- pairs]
	-- No need to extend global tyvars for kind checking
    in
    tcSetEnv (env {tcLEnv = le'}) thing_inside
    
tcExtendTyVarEnv :: [TyVar] -> TcM r -> TcM r
tcExtendTyVarEnv tyvars thing_inside
  = tcGetEnv			`thenNF_Tc` \ env@(TcEnv {tcLEnv = le, tcTyVars = gtvs}) ->
    let
 	le'        = extendNameEnvList le [ (getName tv, ATyVar tv) | tv <- tyvars]
	new_tv_set = mkVarSet tyvars
    in
	-- It's important to add the in-scope tyvars to the global tyvar set
	-- as well.  Consider
	--	f (x::r) = let g y = y::r in ...
	-- Here, g mustn't be generalised.  This is also important during
	-- class and instance decls, when we mustn't generalise the class tyvars
	-- when typechecking the methods.
    tc_extend_gtvs gtvs new_tv_set		`thenNF_Tc` \ gtvs' ->
    tcSetEnv (env {tcLEnv = le', tcTyVars = gtvs'}) thing_inside

-- This variant, tcExtendTyVarEnvForMeths, takes *two* bunches of tyvars:
--	the signature tyvars contain the original names
--	the instance  tyvars are what those names should be mapped to
-- It's needed when typechecking the method bindings of class and instance decls
-- It does *not* extend the global tyvars; tcMethodBind does that for itself

tcExtendTyVarEnvForMeths :: [TyVar] -> [TcTyVar] -> TcM r -> TcM r
tcExtendTyVarEnvForMeths sig_tyvars inst_tyvars thing_inside
  = tcGetEnv					`thenNF_Tc` \ env ->
    let
	le'   = extendNameEnvList (tcLEnv env) stuff
	stuff = [ (getName sig_tv, ATyVar inst_tv)
		| (sig_tv, inst_tv) <- zipEqual "tcMeth" sig_tyvars inst_tyvars
		]
    in
    tcSetEnv (env {tcLEnv = le'}) thing_inside
\end{code}


\begin{code}
tcExtendLocalValEnv :: [(Name,TcId)] -> TcM a -> TcM a
tcExtendLocalValEnv names_w_ids thing_inside
  = tcGetEnv		`thenNF_Tc` \ env ->
    let
	extra_global_tyvars = tyVarsOfTypes [idType id | (name,id) <- names_w_ids]
	extra_env	    = [(name, ATcId id) | (name,id) <- names_w_ids]
	le'		    = extendNameEnvList (tcLEnv env) extra_env
    in
    tc_extend_gtvs (tcTyVars env) extra_global_tyvars	`thenNF_Tc` \ gtvs' ->
    tcSetEnv (env {tcLEnv = le', tcTyVars = gtvs'}) thing_inside
\end{code}


%************************************************************************
%*									*
\subsection{The global tyvars}
%*									*
%************************************************************************

\begin{code}
tcExtendGlobalTyVars extra_global_tvs thing_inside
  = tcGetEnv						`thenNF_Tc` \ env ->
    tc_extend_gtvs (tcTyVars env) extra_global_tvs	`thenNF_Tc` \ gtvs' ->
    tcSetEnv (env {tcTyVars = gtvs'}) thing_inside

tc_extend_gtvs gtvs extra_global_tvs
  = tcReadMutVar gtvs			`thenNF_Tc` \ global_tvs ->
    tcNewMutVar (global_tvs `unionVarSet` extra_global_tvs)
\end{code}

@tcGetGlobalTyVars@ returns a fully-zonked set of tyvars free in the environment.
To improve subsequent calls to the same function it writes the zonked set back into
the environment.

\begin{code}
tcGetGlobalTyVars :: NF_TcM TcTyVarSet
tcGetGlobalTyVars
  = tcGetEnv 					`thenNF_Tc` \ (TcEnv {tcTyVars = gtv_var}) ->
    tcReadMutVar gtv_var			`thenNF_Tc` \ global_tvs ->
    zonkTcTyVars (varSetElems global_tvs)	`thenNF_Tc` \ global_tys' ->
    let
	global_tvs' = (tyVarsOfTypes global_tys')
    in
    tcWriteMutVar gtv_var global_tvs'		`thenNF_Tc_` 
    returnNF_Tc global_tvs'
\end{code}


%************************************************************************
%*									*
\subsection{The instance environment}
%*									*
%************************************************************************

\begin{code}
tcGetInstEnv :: NF_TcM InstEnv
tcGetInstEnv = tcGetEnv 	`thenNF_Tc` \ env -> 
	       returnNF_Tc (tcInsts env)

tcSetInstEnv :: InstEnv -> TcM a -> TcM a
tcSetInstEnv ie thing_inside
  = tcGetEnv 	`thenNF_Tc` \ env ->
    tcSetEnv (env {tcInsts = ie}) thing_inside
\end{code}    


%************************************************************************
%*									*
\subsection{Errors}
%*									*
%************************************************************************

\begin{code}
badCon con_id = quotes (ppr con_id) <+> ptext SLIT("is not a data constructor")

notFound wheRe name = failWithTc (text wheRe <> colon <+> quotes (ppr name) <+> 
				  ptext SLIT("is not in scope"))
\end{code}
