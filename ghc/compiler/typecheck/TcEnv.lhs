\begin{code}
module TcEnv(
	TyThing(..), TcTyThing(..), TcId,

	-- Instance environment, and InstInfo type
	InstInfo(..), pprInstInfo, pprInstInfoDetails,
	simpleInstInfoTy, simpleInstInfoTyCon, 
	InstBindings(..),

	-- Global environment
	tcExtendGlobalEnv, 
	tcExtendGlobalValEnv,
	tcLookupLocatedGlobal,	tcLookupGlobal, 
	tcLookupGlobalId, tcLookupTyCon, tcLookupClass, tcLookupDataCon,
	tcLookupLocatedGlobalId, tcLookupLocatedTyCon,
	tcLookupLocatedClass, 
	
	-- Local environment
	tcExtendKindEnv,
	tcExtendTyVarEnv, tcExtendTyVarEnv2, 
	tcExtendIdEnv, tcExtendIdEnv1, tcExtendIdEnv2, 
	tcLookup, tcLookupLocated, tcLookupLocalIds,
	tcLookupId, tcLookupTyVar,
	lclEnvElts, getInLocalScope, findGlobals, 
	wrongThingErr, pprBinders,

	tcExtendRecEnv,    	-- For knot-tying

	-- Rules
 	tcExtendRules,

	-- Global type variables
	tcGetGlobalTyVars,

	-- Template Haskell stuff
	checkWellStaged, spliceOK, bracketOK, tcMetaTy, thLevel, 
	topIdLvl, 

	-- New Ids
	newLocalName, newDFunName
  ) where

#include "HsVersions.h"

import HsSyn		( LRuleDecl, LHsBinds, LSig, pprLHsBinds )
import TcIface		( tcImportDecl )
import TcRnTypes	( pprTcTyThingCategory )
import TcRnMonad
import TcMType		( zonkTcType, zonkTcTyVarsAndFV )
import TcType		( Type, TcKind, TcTyVar, TcTyVarSet, TcType,
			  tyVarsOfType, tyVarsOfTypes, tcSplitDFunTy, mkGenTyConApp,
			  getDFunTyKey, tcTyConAppTyCon, tcGetTyVar, mkTyVarTy,
			  tidyOpenType 
			)
import qualified Type	( getTyVar_maybe )
import Id		( idName, isLocalId )
import Var		( TyVar, Id, idType, tyVarName )
import VarSet
import VarEnv
import RdrName		( extendLocalRdrEnv )
import DataCon		( DataCon )
import TyCon		( TyCon )
import Class		( Class )
import Name		( Name, NamedThing(..), getSrcLoc, mkInternalName, nameIsLocalOrFrom )
import NameEnv
import OccName		( mkDFunOcc, occNameString )
import HscTypes		( DFunId, extendTypeEnvList, lookupType,
			  TyThing(..), tyThingId, tyThingDataCon,
			  ExternalPackageState(..) )

import SrcLoc		( SrcLoc, Located(..) )
import Outputable
\end{code}


%************************************************************************
%*									*
%*			tcLookupGlobal					*
%*									*
%************************************************************************

Using the Located versions (eg. tcLookupLocatedGlobal) is preferred,
unless you know that the SrcSpan in the monad is already set to the
span of the Name.

\begin{code}
tcLookupLocatedGlobal :: Located Name -> TcM TyThing
-- c.f. IfaceEnvEnv.tcIfaceGlobal
tcLookupLocatedGlobal name
  = addLocM tcLookupGlobal name

tcLookupGlobal :: Name -> TcM TyThing
tcLookupGlobal name
  = do	{ env <- getGblEnv
	; if nameIsLocalOrFrom (tcg_mod env) name

	  then	-- It's defined in this module
	      case lookupNameEnv (tcg_type_env env) name of
		Just thing -> return thing
		Nothing	   -> notFound  name	-- Panic!
	 
	  else do 		-- It's imported
	{ (eps,hpt) <- getEpsAndHpt
	; case lookupType hpt (eps_PTE eps) name of 
	    Just thing -> return thing 
	    Nothing    -> tcImportDecl name
    }}
\end{code}

\begin{code}
tcLookupGlobalId :: Name -> TcM Id
-- Never used for Haskell-source DataCons, hence no ADataCon case
tcLookupGlobalId name
  = tcLookupGlobal name		`thenM` \ thing ->
    return (tyThingId thing)

tcLookupDataCon :: Name -> TcM DataCon
tcLookupDataCon con_name
  = tcLookupGlobal con_name	`thenM` \ thing ->
    return (tyThingDataCon thing)

tcLookupClass :: Name -> TcM Class
tcLookupClass name
  = tcLookupGlobal name		`thenM` \ thing ->
    case thing of
	AClass cls -> return cls
	other	   -> wrongThingErr "class" (AGlobal thing) name
	
tcLookupTyCon :: Name -> TcM TyCon
tcLookupTyCon name
  = tcLookupGlobal name		`thenM` \ thing ->
    case thing of
	ATyCon tc -> return tc
	other	  -> wrongThingErr "type constructor" (AGlobal thing) name

tcLookupLocatedGlobalId :: Located Name -> TcM Id
tcLookupLocatedGlobalId = addLocM tcLookupId

tcLookupLocatedClass :: Located Name -> TcM Class
tcLookupLocatedClass = addLocM tcLookupClass

tcLookupLocatedTyCon :: Located Name -> TcM TyCon
tcLookupLocatedTyCon = addLocM tcLookupTyCon
\end{code}

%************************************************************************
%*									*
		Extending the global environment
%*									*
%************************************************************************


\begin{code}
tcExtendGlobalEnv :: [TyThing] -> TcM r -> TcM r
  -- Given a mixture of Ids, TyCons, Classes, all from the
  -- module being compiled, extend the global environment
tcExtendGlobalEnv things thing_inside
   = do	{ env <- getGblEnv
	; let ge'  = extendTypeEnvList (tcg_type_env env) things
	; setGblEnv (env {tcg_type_env = ge'}) thing_inside }

tcExtendGlobalValEnv :: [Id] -> TcM a -> TcM a
  -- Same deal as tcExtendGlobalEnv, but for Ids
tcExtendGlobalValEnv ids thing_inside 
  = tcExtendGlobalEnv [AnId id | id <- ids] thing_inside
\end{code}

\begin{code}
tcExtendRecEnv :: [(Name,TyThing)] -> TcM r -> TcM r
-- Extend the global environments for the type/class knot tying game
tcExtendRecEnv gbl_stuff thing_inside
 = updGblEnv upd thing_inside
 where
   upd env = env { tcg_type_env = extend (tcg_type_env env) }
   extend env = extendNameEnvList env gbl_stuff
\end{code}


%************************************************************************
%*									*
\subsection{The local environment}
%*									*
%************************************************************************

\begin{code}
tcLookupLocated :: Located Name -> TcM TcTyThing
tcLookupLocated = addLocM tcLookup

tcLookup :: Name -> TcM TcTyThing
tcLookup name
  = getLclEnv 		`thenM` \ local_env ->
    case lookupNameEnv (tcl_env local_env) name of
	Just thing -> returnM thing
	Nothing    -> tcLookupGlobal name `thenM` \ thing ->
		      returnM (AGlobal thing)

tcLookupTyVar :: Name -> TcM TcTyVar
tcLookupTyVar name
  = tcLookup name	`thenM` \ thing -> 
    case thing of
	ATyVar _ ty -> returnM (tcGetTyVar "tcLookupTyVar" ty)
	other	    -> pprPanic "tcLookupTyVar" (ppr name)

tcLookupId :: Name -> TcM Id
-- Used when we aren't interested in the binding level
-- Never a DataCon. (Why does that matter? see TcExpr.tcId)
tcLookupId name
  = tcLookup name	`thenM` \ thing -> 
    case thing of
	ATcId tc_id _	  -> returnM tc_id
	AGlobal (AnId id) -> returnM id
	other		  -> pprPanic "tcLookupId" (ppr name)

tcLookupLocalIds :: [Name] -> TcM [TcId]
-- We expect the variables to all be bound, and all at
-- the same level as the lookup.  Only used in one place...
tcLookupLocalIds ns
  = getLclEnv 		`thenM` \ env ->
    returnM (map (lookup (tcl_env env) (thLevel (tcl_th_ctxt env))) ns)
  where
    lookup lenv lvl name 
	= case lookupNameEnv lenv name of
		Just (ATcId id lvl1) -> ASSERT( lvl == lvl1 ) id
		other		     -> pprPanic "tcLookupLocalIds" (ppr name)

lclEnvElts :: TcLclEnv -> [TcTyThing]
lclEnvElts env = nameEnvElts (tcl_env env)

getInLocalScope :: TcM (Name -> Bool)
  -- Ids only
getInLocalScope = getLclEnv	`thenM` \ env ->
		  let 
			lcl_env = tcl_env env
		  in
		  return (`elemNameEnv` lcl_env)
\end{code}

\begin{code}
tcExtendKindEnv :: [(Name, TcKind)] -> TcM r -> TcM r
tcExtendKindEnv things thing_inside
  = updLclEnv upd thing_inside
  where
    upd lcl_env = lcl_env { tcl_env = extend (tcl_env lcl_env) }
    extend env  = extendNameEnvList env [(n, AThing k) | (n,k) <- things]

tcExtendTyVarEnv :: [TyVar] -> TcM r -> TcM r
tcExtendTyVarEnv tvs thing_inside
  = tcExtendTyVarEnv2 [(tyVarName tv, mkTyVarTy tv) | tv <- tvs] thing_inside

tcExtendTyVarEnv2 :: [(Name,TcType)] -> TcM r -> TcM r
tcExtendTyVarEnv2 binds thing_inside
  = getLclEnv	   `thenM` \ env@(TcLclEnv {tcl_env = le, 
					    tcl_tyvars = gtvs, 
					    tcl_rdr = rdr_env}) ->
    let
	rdr_env'   = extendLocalRdrEnv rdr_env (map fst binds)
	new_tv_set = tyVarsOfTypes (map snd binds)
 	le'        = extendNameEnvList le [(name, ATyVar name ty) | (name, ty) <- binds]
    in
	-- It's important to add the in-scope tyvars to the global tyvar set
	-- as well.  Consider
	--	f (_::r) = let g y = y::r in ...
	-- Here, g mustn't be generalised.  This is also important during
	-- class and instance decls, when we mustn't generalise the class tyvars
	-- when typechecking the methods.
    tc_extend_gtvs gtvs new_tv_set		`thenM` \ gtvs' ->
    setLclEnv (env {tcl_env = le', tcl_tyvars = gtvs', tcl_rdr = rdr_env'}) thing_inside
\end{code}


\begin{code}
tcExtendIdEnv :: [TcId] -> TcM a -> TcM a
-- Invariant: the TcIds are fully zonked. Reasons:
--	(a) The kinds of the forall'd type variables are defaulted
--	    (see Kind.defaultKind, done in zonkQuantifiedTyVar)
--	(b) There are no via-Indirect occurrences of the bound variables
--	    in the types, because instantiation does not look through such things
--	(c) The call to tyVarsOfTypes is ok without looking through refs
tcExtendIdEnv ids thing_inside = tcExtendIdEnv2 [(idName id, id) | id <- ids] thing_inside

tcExtendIdEnv1 :: Name -> TcId -> TcM a -> TcM a
tcExtendIdEnv1 name id thing_inside = tcExtendIdEnv2 [(name,id)] thing_inside

tcExtendIdEnv2 :: [(Name,TcId)] -> TcM a -> TcM a
-- Invariant: the TcIds are fully zonked (see tcExtendIdEnv above)
tcExtendIdEnv2 names_w_ids thing_inside
  = getLclEnv		`thenM` \ env ->
    let
	extra_global_tyvars = tyVarsOfTypes [idType id | (name,id) <- names_w_ids]
	th_lvl		    = thLevel    (tcl_th_ctxt   env)
	extra_env	    = [(name, ATcId id th_lvl) | (name,id) <- names_w_ids]
	le'		    = extendNameEnvList (tcl_env env) extra_env
	rdr_env'	    = extendLocalRdrEnv (tcl_rdr env) (map fst names_w_ids)
    in
    tc_extend_gtvs (tcl_tyvars env) extra_global_tyvars	`thenM` \ gtvs' ->
    setLclEnv (env {tcl_env = le', tcl_tyvars = gtvs', tcl_rdr = rdr_env'}) thing_inside
\end{code}


\begin{code}
-----------------------
-- findGlobals looks at the value environment and finds values
-- whose types mention the offending type variable.  It has to be 
-- careful to zonk the Id's type first, so it has to be in the monad.
-- We must be careful to pass it a zonked type variable, too.

findGlobals :: TcTyVarSet
            -> TidyEnv 
            -> TcM (TidyEnv, [SDoc])

findGlobals tvs tidy_env
  = getLclEnv		`thenM` \ lcl_env ->
    go tidy_env [] (lclEnvElts lcl_env)
  where
    go tidy_env acc [] = returnM (tidy_env, acc)
    go tidy_env acc (thing : things)
      = find_thing ignore_it tidy_env thing 	`thenM` \ (tidy_env1, maybe_doc) ->
	case maybe_doc of
	  Just d  -> go tidy_env1 (d:acc) things
	  Nothing -> go tidy_env1 acc     things

    ignore_it ty = not (tvs `intersectsVarSet` tyVarsOfType ty)

-----------------------
find_thing ignore_it tidy_env (ATcId id _)
  = zonkTcType  (idType id)	`thenM` \ id_ty ->
    if ignore_it id_ty then
	returnM (tidy_env, Nothing)
    else let
	(tidy_env', tidy_ty) = tidyOpenType tidy_env id_ty
	msg = sep [ppr id <+> dcolon <+> ppr tidy_ty, 
		   nest 2 (parens (ptext SLIT("bound at") <+>
			 	   ppr (getSrcLoc id)))]
    in
    returnM (tidy_env', Just msg)

find_thing ignore_it tidy_env (ATyVar tv ty)
  = zonkTcType ty		`thenM` \ tv_ty ->
    if ignore_it tv_ty then
	returnM (tidy_env, Nothing)
    else let
	-- The name tv is scoped, so we don't need to tidy it
	(tidy_env1, tidy_ty) = tidyOpenType  tidy_env tv_ty
	msg = sep [ptext SLIT("Scoped type variable") <+> quotes (ppr tv) <+> eq_stuff, nest 2 bound_at]

	eq_stuff | Just tv' <- Type.getTyVar_maybe tv_ty, 
		   getOccName tv == getOccName tv' = empty
		 | otherwise = equals <+> ppr tidy_ty
		-- It's ok to use Type.getTyVar_maybe because ty is zonked by now
	bound_at = parens $ ptext SLIT("bound at:") <+> ppr (getSrcLoc tv)
    in
    returnM (tidy_env1, Just msg)
\end{code}


%************************************************************************
%*									*
\subsection{The global tyvars}
%*									*
%************************************************************************

\begin{code}
tc_extend_gtvs gtvs extra_global_tvs
  = readMutVar gtvs		`thenM` \ global_tvs ->
    newMutVar (global_tvs `unionVarSet` extra_global_tvs)
\end{code}

@tcGetGlobalTyVars@ returns a fully-zonked set of tyvars free in the environment.
To improve subsequent calls to the same function it writes the zonked set back into
the environment.

\begin{code}
tcGetGlobalTyVars :: TcM TcTyVarSet
tcGetGlobalTyVars
  = getLclEnv					`thenM` \ (TcLclEnv {tcl_tyvars = gtv_var}) ->
    readMutVar gtv_var				`thenM` \ gbl_tvs ->
    zonkTcTyVarsAndFV (varSetElems gbl_tvs)	`thenM` \ gbl_tvs' ->
    writeMutVar gtv_var gbl_tvs'		`thenM_` 
    returnM gbl_tvs'
\end{code}


%************************************************************************
%*									*
\subsection{Rules}
%*									*
%************************************************************************

\begin{code}
tcExtendRules :: [LRuleDecl Id] -> TcM a -> TcM a
	-- Just pop the new rules into the EPS and envt resp
	-- All the rules come from an interface file, not soruce
	-- Nevertheless, some may be for this module, if we read
	-- its interface instead of its source code
tcExtendRules lcl_rules thing_inside
 = do { env <- getGblEnv
      ; let
	  env' = env { tcg_rules = lcl_rules ++ tcg_rules env }
      ; setGblEnv env' thing_inside }
\end{code}


%************************************************************************
%*									*
		Meta level
%*									*
%************************************************************************

\begin{code}
instance Outputable ThStage where
   ppr Comp	     = text "Comp"
   ppr (Brack l _ _) = text "Brack" <+> int l
   ppr (Splice l)    = text "Splice" <+> int l


thLevel :: ThStage -> ThLevel
thLevel Comp	      = topLevel
thLevel (Splice l)    = l
thLevel (Brack l _ _) = l


checkWellStaged :: SDoc		-- What the stage check is for
		-> ThLevel 	-- Binding level
	        -> ThStage	-- Use stage
		-> TcM ()	-- Fail if badly staged, adding an error
checkWellStaged pp_thing bind_lvl use_stage
  | bind_lvl <= use_lvl 	-- OK!
  = returnM ()	

  | bind_lvl == topLevel	-- GHC restriction on top level splices
  = failWithTc $ 
    sep [ptext SLIT("GHC stage restriction:") <+>  pp_thing,
	 nest 2 (ptext SLIT("is used in a top-level splice, and must be imported, not defined locally"))]

  | otherwise			-- Badly staged
  = failWithTc $ 
    ptext SLIT("Stage error:") <+> pp_thing <+> 
	hsep   [ptext SLIT("is bound at stage") <+> ppr bind_lvl,
		ptext SLIT("but used at stage") <+> ppr use_lvl]
  where
    use_lvl = thLevel use_stage


topIdLvl :: Id -> ThLevel
-- Globals may either be imported, or may be from an earlier "chunk" 
-- (separated by declaration splices) of this module.  The former
--  *can* be used inside a top-level splice, but the latter cannot.
-- Hence we give the former impLevel, but the latter topLevel
-- E.g. this is bad:
--	x = [| foo |]
--	$( f x )
-- By the time we are prcessing the $(f x), the binding for "x" 
-- will be in the global env, not the local one.
topIdLvl id | isLocalId id = topLevel
	    | otherwise    = impLevel

-- Indicates the legal transitions on bracket( [| |] ).
bracketOK :: ThStage -> Maybe ThLevel
bracketOK (Brack _ _ _) = Nothing	-- Bracket illegal inside a bracket
bracketOK stage         = Just (thLevel stage + 1)

-- Indicates the legal transitions on splice($).
spliceOK :: ThStage -> Maybe ThLevel
spliceOK (Splice _) = Nothing	-- Splice illegal inside splice
spliceOK stage      = Just (thLevel stage - 1)

tcMetaTy :: Name -> TcM Type
-- Given the name of a Template Haskell data type, 
-- return the type
-- E.g. given the name "Expr" return the type "Expr"
tcMetaTy tc_name
  = tcLookupTyCon tc_name	`thenM` \ t ->
    returnM (mkGenTyConApp t [])
	-- Use mkGenTyConApp because it might be a synonym
\end{code}


%************************************************************************
%*									*
\subsection{Making new Ids}
%*									*
%************************************************************************

Constructing new Ids

\begin{code}
newLocalName :: Name -> TcM Name
newLocalName name	-- Make a clone
  = newUnique		`thenM` \ uniq ->
    returnM (mkInternalName uniq (getOccName name) (getSrcLoc name))
\end{code}

Make a name for the dict fun for an instance decl.  It's a *local*
name for the moment.  The CoreTidy pass will externalise it.  Even in
--make and ghci stuff, we rebuild the instance environment each time,
so the dfun id is internal to begin with, and external when compiling
other modules

\begin{code}
newDFunName :: Class -> [Type] -> SrcLoc -> TcM Name
newDFunName clas (ty:_) loc
  = do	{ uniq <- newUnique
	; return (mkInternalName uniq (mkDFunOcc dfun_string) loc) }
  where
	-- Any string that is somewhat unique will do
    dfun_string = occNameString (getOccName clas) ++ occNameString (getDFunTyKey ty)

newDFunName clas [] loc = pprPanic "newDFunName" (ppr clas <+> ppr loc)
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
      iDFunId :: DFunId,		-- The dfun id.  Its forall'd type variables 
      iBinds  :: InstBindings		-- scope over the stuff in InstBindings!
    }

data InstBindings
  = VanillaInst 		-- The normal case
	(LHsBinds Name)		-- Bindings
	[LSig Name]		-- User pragmas recorded for generating 
				-- specialised instances

  | NewTypeDerived 		-- Used for deriving instances of newtypes, where the
	[Type]			-- witness dictionary is identical to the argument 
				-- dictionary.  Hence no bindings, no pragmas
	-- The [Type] are the representation types
	-- See notes in TcDeriv

pprInstInfo info = vcat [ptext SLIT("InstInfo:") <+> ppr (idType (iDFunId info))]

pprInstInfoDetails info = pprInstInfo info $$ nest 2 (details (iBinds info))
  where
    details (VanillaInst b _)  = pprLHsBinds b
    details (NewTypeDerived _) = text "Derived from the representation type"

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
pprBinders :: [Name] -> SDoc
-- Used in error messages
-- Use quotes for a single one; they look a bit "busy" for several
pprBinders [bndr] = quotes (ppr bndr)
pprBinders bndrs  = pprWithCommas ppr bndrs

notFound name 
  = failWithTc (ptext SLIT("GHC internal error:") <+> quotes (ppr name) <+> 
		ptext SLIT("is not in scope"))

wrongThingErr expected thing name
  = failWithTc (pprTcTyThingCategory thing <+> quotes (ppr name) <+> 
		ptext SLIT("used as a") <+> text expected)
\end{code}
