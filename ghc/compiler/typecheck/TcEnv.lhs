\begin{code}
module TcEnv(
	TcId, TcIdSet, 
	TyThing(..), TyThingDetails(..), TcTyThing(..),

	-- Getting stuff from the environment
	TcEnv, initTcEnv, 
	tcEnvTyCons, tcEnvClasses, tcEnvIds, tcLEnvElts,
	getTcGEnv,
	
	-- Instance environment, and InstInfo type
	tcGetInstEnv, tcSetInstEnv, 
	InstInfo(..), pprInstInfo,
	simpleInstInfoTy, simpleInstInfoTyCon, 

	-- Global environment
	tcExtendGlobalEnv, tcExtendGlobalValEnv, tcExtendGlobalTypeEnv,
	tcLookupTyCon, tcLookupClass, tcLookupGlobalId, tcLookupDataCon,
	tcLookupGlobal_maybe, tcLookupGlobal, 

	-- Local environment
	tcExtendKindEnv,  tcLookupLocalIds, tcInLocalScope,
	tcExtendTyVarEnv, tcExtendTyVarEnvForMeths, 
	tcExtendLocalValEnv, tcLookup, tcLookup_maybe, tcLookupId,

	-- Global type variables
	tcGetGlobalTyVars, tcExtendGlobalTyVars,

	-- Random useful things
	RecTcEnv, tcLookupRecId, tcLookupRecId_maybe, 

	-- New Ids
	newLocalName, newDFunName,

	-- Misc
	isLocalThing, tcSetEnv
  ) where

#include "HsVersions.h"

import RnHsSyn		( RenamedMonoBinds, RenamedSig )
import TcMonad
import TcMType		( zonkTcTyVarsAndFV )
import TcType		( Type, ThetaType, TcKind, TcTyVar, TcTyVarSet, 
			  tyVarsOfTypes, tcSplitDFunTy,
			  getDFunTyKey, tcTyConAppTyCon
			)
import Id		( isDataConWrapId_maybe )
import Var		( TyVar, Id, idType )
import VarSet
import DataCon		( DataCon )
import TyCon		( TyCon, DataConDetails )
import Class		( Class, ClassOpItem )
import Name		( Name, NamedThing(..), 
			  getSrcLoc, mkLocalName, isLocalName, nameIsLocalOrFrom
			)
import NameEnv		( NameEnv, lookupNameEnv, nameEnvElts, elemNameEnv,
			  extendNameEnvList, emptyNameEnv, plusNameEnv )
import OccName		( mkDFunOcc, occNameString )
import HscTypes		( DFunId, 
			  PackageTypeEnv, TypeEnv, 
			  extendTypeEnvList, extendTypeEnvWithIds,
			  typeEnvTyCons, typeEnvClasses, typeEnvIds,
			  HomeSymbolTable
			)
import Module		( Module )
import InstEnv		( InstEnv, emptyInstEnv )
import HscTypes		( lookupType, TyThing(..) )
import Util		( zipEqual )
import SrcLoc		( SrcLoc )
import Outputable

import IOExts		( newIORef )
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
	tcGST  	 :: Name -> Maybe TyThing,	-- The type environment at the moment we began this compilation

	tcInsts	 :: InstEnv,		-- All instances (both imported and in this module)

	tcGEnv	 :: TypeEnv,		-- The global type environment we've accumulated while
		 {- NameEnv TyThing-}	-- compiling this module:
					--	types and classes (both imported and local)
					-- 	imported Ids
					-- (Ids defined in this module start in the local envt, 
					--  though they move to the global envt during zonking)

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
initTcEnv :: HomeSymbolTable -> PackageTypeEnv -> IO TcEnv
initTcEnv hst pte 
  = do { gtv_var <- newIORef emptyVarSet ;
	 return (TcEnv { tcGST    = lookup,
		      	 tcGEnv   = emptyNameEnv,
		      	 tcInsts  = emptyInstEnv,
		      	 tcLEnv   = emptyNameEnv,
		      	 tcTyVars = gtv_var
	 })}
  where
    lookup name | isLocalName name = Nothing
		| otherwise	   = lookupType hst pte name


tcEnvClasses env = typeEnvClasses (tcGEnv env)
tcEnvTyCons  env = typeEnvTyCons  (tcGEnv env) 
tcEnvIds     env = typeEnvIds     (tcGEnv env) 
tcLEnvElts   env = nameEnvElts (tcLEnv env)

getTcGEnv (TcEnv { tcGEnv = genv }) = genv

tcInLocalScope :: TcEnv -> Name -> Bool
tcInLocalScope env v = v `elemNameEnv` (tcLEnv env)
\end{code}

\begin{code}
data TcTyThing
  = AGlobal TyThing		-- Used only in the return type of a lookup
  | ATcId   TcId		-- Ids defined in this module
  | ATyVar  TyVar 		-- Type variables
  | AThing  TcKind		-- Used temporarily, during kind checking
-- Here's an example of how the AThing guy is used
-- Suppose we are checking (forall a. T a Int):
--	1. We first bind (a -> AThink kv), where kv is a kind variable. 
--	2. Then we kind-check the (T a Int) part.
--	3. Then we zonk the kind variable.
--	4. Now we know the kind for 'a', and we add (a -> ATyVar a::K) to the environment

\end{code}

This data type is used to help tie the knot
 when type checking type and class declarations

\begin{code}
data TyThingDetails = SynTyDetails Type
		    | DataTyDetails ThetaType (DataConDetails DataCon) [Id]
		    | ClassDetails ThetaType [Id] [ClassOpItem] DataCon
		    | ForeignTyDetails	-- Nothing yet
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
	Nothing    -> tcGST env name

lookup_local :: TcEnv -> Name -> Maybe TcTyThing
	-- Try the local envt and then try the global
lookup_local env name
  = case lookupNameEnv (tcLEnv env) name of
	Just thing -> Just thing
	Nothing    -> case lookup_global env name of
			Just thing -> Just (AGlobal thing)
			Nothing	   -> Nothing
\end{code}

\begin{code}
type RecTcEnv = TcEnv
-- This environment is used for getting the 'right' IdInfo 
-- on imported things and for looking up Ids in unfoldings
-- The environment doesn't have any local Ids in it

tcLookupRecId_maybe :: RecTcEnv -> Name -> Maybe Id
tcLookupRecId_maybe env name = case lookup_global env name of
				   Just (AnId id) -> Just id
				   other	  -> Nothing

tcLookupRecId ::  RecTcEnv -> Name -> Id
tcLookupRecId env name = case lookup_global env name of
				Just (AnId id) -> id
				Nothing	       -> pprPanic "tcLookupRecId" (ppr name)
\end{code}

%************************************************************************
%*									*
\subsection{Making new Ids}
%*									*
%************************************************************************

Constructing new Ids

\begin{code}
newLocalName :: Name -> NF_TcM Name
newLocalName name	-- Make a clone
  = tcGetUnique		`thenNF_Tc` \ uniq ->
    returnNF_Tc (mkLocalName uniq (getOccName name) (getSrcLoc name))
\end{code}

Make a name for the dict fun for an instance decl.
It's a *local* name for the moment.  The CoreTidy pass
will globalise it.

\begin{code}
newDFunName :: Class -> [Type] -> SrcLoc -> NF_TcM Name
newDFunName clas (ty:_) loc
  = tcGetUnique			`thenNF_Tc` \ uniq ->
    returnNF_Tc (mkLocalName uniq (mkDFunOcc dfun_string) loc)
  where
	-- Any string that is somewhat unique will do
    dfun_string = occNameString (getOccName clas) ++ occNameString (getDFunTyKey ty)

newDFunName clas [] loc = pprPanic "newDFunName" (ppr clas <+> ppr loc)
\end{code}

\begin{code}
isLocalThing :: NamedThing a => Module -> a -> Bool
isLocalThing mod thing = nameIsLocalOrFrom mod (getName thing)
\end{code}

%************************************************************************
%*									*
\subsection{The global environment}
%*									*
%************************************************************************

\begin{code}
tcExtendGlobalEnv :: [TyThing] -> TcM r -> TcM r
tcExtendGlobalEnv things thing_inside
  = tcGetEnv				`thenNF_Tc` \ env ->
    let
	ge' = extendTypeEnvList (tcGEnv env) things
    in
    tcSetEnv (env {tcGEnv = ge'}) thing_inside


tcExtendGlobalTypeEnv :: TypeEnv -> TcM r -> TcM r
tcExtendGlobalTypeEnv extra_env thing_inside
  = tcGetEnv				`thenNF_Tc` \ env ->
    let
	ge' = tcGEnv env `plusNameEnv` extra_env
    in
    tcSetEnv (env {tcGEnv = ge'}) thing_inside

tcExtendGlobalValEnv :: [Id] -> TcM a -> TcM a
tcExtendGlobalValEnv ids thing_inside
  = tcGetEnv				`thenNF_Tc` \ env ->
    let
	ge' = extendTypeEnvWithIds (tcGEnv env) ids
    in
    tcSetEnv (env {tcGEnv = ge'}) thing_inside
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
tcLookupGlobal name
  = tcLookupGlobal_maybe name	`thenNF_Tc` \ maybe_thing ->
    case maybe_thing of
	Just thing -> returnNF_Tc thing
	other	   -> notFound "tcLookupGlobal" name

tcLookupGlobalId :: Name -> NF_TcM Id
tcLookupGlobalId name
  = tcLookupGlobal_maybe name	`thenNF_Tc` \ maybe_id ->
    case maybe_id of
	Just (AnId id) -> returnNF_Tc id
	other	       -> notFound "tcLookupGlobalId" name
	
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
	other		   -> notFound "tcLookupClass" name
	
tcLookupTyCon :: Name -> NF_TcM TyCon
tcLookupTyCon name
  = tcLookupGlobal_maybe name	`thenNF_Tc` \ maybe_tc ->
    case maybe_tc of
	Just (ATyCon tc) -> returnNF_Tc tc
	other		 -> notFound "tcLookupTyCon" name

tcLookupId :: Name -> NF_TcM Id
tcLookupId name
  = tcLookup name	`thenNF_Tc` \ thing -> 
    case thing of
	ATcId tc_id	  -> returnNF_Tc tc_id
	AGlobal (AnId id) -> returnNF_Tc id
	other		  -> pprPanic "tcLookupId" (ppr name)

tcLookupLocalIds :: [Name] -> NF_TcM [TcId]
tcLookupLocalIds ns
  = tcGetEnv 		`thenNF_Tc` \ env ->
    returnNF_Tc (map (lookup (tcLEnv env)) ns)
  where
    lookup lenv name = case lookupNameEnv lenv name of
			Just (ATcId id) -> id
			other		-> pprPanic "tcLookupLocalIds" (ppr name)
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
	other	   -> notFound "tcLookup" name
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
  = tcGetEnv							`thenNF_Tc` \ env ->
    tc_extend_gtvs (tcTyVars env) (mkVarSet extra_global_tvs)	`thenNF_Tc` \ gtvs' ->
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
    tcReadMutVar gtv_var			`thenNF_Tc` \ gbl_tvs ->
    zonkTcTyVarsAndFV (varSetElems gbl_tvs)	`thenNF_Tc` \ gbl_tvs' ->
    tcWriteMutVar gtv_var gbl_tvs'		`thenNF_Tc_` 
    returnNF_Tc gbl_tvs'
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
\subsection{The InstInfo type}
%*									*
%************************************************************************

The InstInfo type summarises the information in an instance declaration

    instance c => k (t tvs) where b

It is used just for *local* instance decls (not ones from interface files).
But local instance decls includes
	- derived ones
	- generic ones
as well as explicit user written ones.

\begin{code}
data InstInfo
  = InstInfo {
      iDFunId :: DFunId,		-- The dfun id
      iBinds  :: RenamedMonoBinds,	-- Bindings, b
      iPrags  :: [RenamedSig]		-- User pragmas recorded for generating specialised instances
    }

  | NewTypeDerived {		-- Used for deriving instances of newtypes, where the
				-- witness dictionary is identical to the argument dictionary
				-- Hence no bindings.
      iDFunId :: DFunId			-- The dfun id
    }

pprInstInfo info = vcat [ptext SLIT("InstInfo:") <+> ppr (idType (iDFunId info))]

simpleInstInfoTy :: InstInfo -> Type
simpleInstInfoTy info = case tcSplitDFunTy (idType (iDFunId info)) of
			  (_, _, _, [ty]) -> ty

simpleInstInfoTyCon :: InstInfo -> TyCon
  -- Gets the type constructor for a simple instance declaration,
  -- i.e. one of the form 	instance (...) => C (T a b c) where ...
simpleInstInfoTyCon inst = tcTyConAppTyCon (simpleInstInfoTy inst)
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
