%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{Tidying up Core}

\begin{code}
module TidyPgm( mkBootModDetails, tidyProgram ) where

#include "HsVersions.h"

import DynFlags		( DynFlag(..), DynFlags(..), dopt )
import CoreSyn
import CoreUnfold	( noUnfolding, mkTopUnfolding )
import CoreFVs		( ruleLhsFreeIds, exprSomeFreeVars )
import CoreTidy		( tidyExpr, tidyVarOcc, tidyRules )
import PprCore 		( pprRules )
import CoreLint		( showPass, endPass )
import CoreUtils	( exprArity, rhsIsStatic )
import VarEnv
import VarSet
import Var		( Id, Var )
import Id		( idType, idInfo, idName, idCoreRules, isGlobalId,
			  isExportedId, mkVanillaGlobal, isLocalId, isNaughtyRecordSelector,
			  idArity, idCafInfo, idUnfolding, isImplicitId, setIdInfo
			) 
import IdInfo		{- loads of stuff -}
import InstEnv		( Instance, DFunId, instanceDFunId, setInstanceDFunId )
import NewDemand	( isBottomingSig, topSig )
import BasicTypes	( Arity, isNeverActive, isNonRuleLoopBreaker )
import Name		( Name, getOccName, nameOccName, mkInternalName,
		  	  localiseName, isExternalName, nameSrcLoc, nameParent_maybe,
			  isWiredInName, getName
			)
import NameSet		( NameSet, elemNameSet )
import IfaceEnv		( allocateGlobalBinder )
import NameEnv		( filterNameEnv, mapNameEnv )
import OccName		( TidyOccEnv, initTidyOccEnv, tidyOccName )
import Type		( tidyTopType )
import TcType		( isFFITy )
import DataCon		( dataConName, dataConFieldLabels, dataConWrapId_maybe )
import TyCon		( TyCon, makeTyConAbstract, tyConDataCons, isNewTyCon, 
			  newTyConRep, tyConSelIds, isAlgTyCon,
			  isEnumerationTyCon, isOpenTyCon )
import Class		( classSelIds )
import Module		( Module )
import HscTypes		( HscEnv(..), NameCache( nsUniqs ), CgGuts(..),
			  TypeEnv, typeEnvIds, typeEnvElts, typeEnvTyCons, 
			  extendTypeEnvWithIds, lookupTypeEnv,
			  ModGuts(..), TyThing(..), ModDetails(..),
			  Dependencies(..)
			)
import Maybes		( orElse, mapCatMaybes )
import ErrUtils		( showPass, dumpIfSet_core )
import PackageConfig	( PackageId )
import UniqSupply	( splitUniqSupply, uniqFromSupply )
import List		( partition )
import Maybe		( isJust )
import Outputable
import DATA_IOREF	( IORef, readIORef, writeIORef )
import FastTypes  hiding ( fastOr )
\end{code}


Constructing the TypeEnv, Instances, Rules from which the ModIface is
constructed, and which goes on to subsequent modules in --make mode.

Most of the interface file is obtained simply by serialising the
TypeEnv.  One important consequence is that if the *interface file*
has pragma info if and only if the final TypeEnv does. This is not so
important for *this* module, but it's essential for ghc --make:
subsequent compilations must not see (e.g.) the arity if the interface
file does not contain arity If they do, they'll exploit the arity;
then the arity might change, but the iface file doesn't change =>
recompilation does not happen => disaster. 

For data types, the final TypeEnv will have a TyThing for the TyCon,
plus one for each DataCon; the interface file will contain just one
data type declaration, but it is de-serialised back into a collection
of TyThings.

%************************************************************************
%*				 					*
		Plan A: simpleTidyPgm
%*				 					* 
%************************************************************************


Plan A: mkBootModDetails: omit pragmas, make interfaces small
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Ignore the bindings

* Drop all WiredIn things from the TypeEnv 
	(we never want them in interface files)

* Retain all TyCons and Classes in the TypeEnv, to avoid
	having to find which ones are mentioned in the
	types of exported Ids

* Trim off the constructors of non-exported TyCons, both
	from the TyCon and from the TypeEnv

* Drop non-exported Ids from the TypeEnv

* Tidy the types of the DFunIds of Instances, 
  make them into GlobalIds, (they already have External Names)
  and add them to the TypeEnv

* Tidy the types of the (exported) Ids in the TypeEnv,
  make them into GlobalIds (they already have External Names)

* Drop rules altogether

* Tidy the bindings, to ensure that the Caf and Arity
  information is correct for each top-level binder; the 
  code generator needs it. And to ensure that local names have
  distinct OccNames in case of object-file splitting

\begin{code}
mkBootModDetails :: HscEnv -> ModGuts -> IO ModDetails
-- This is Plan A: make a small type env when typechecking only,
-- or when compiling a hs-boot file, or simply when not using -O
--
-- We don't look at the bindings at all -- there aren't any
-- for hs-boot files

mkBootModDetails hsc_env (ModGuts { mg_module    = mod
				  , mg_exports   = exports
				  , mg_types     = type_env
				  , mg_insts     = insts
				  , mg_fam_insts = fam_insts })
  = do	{ let dflags = hsc_dflags hsc_env 
	; showPass dflags "Tidy [hoot] type env"

	; let { insts'     = tidyInstances tidyExternalId insts
	      ; type_env1  = filterNameEnv (not . isWiredInThing) type_env
	      ; type_env2  = mapNameEnv tidyBootThing type_env1
	      ; type_env'  = extendTypeEnvWithIds type_env2
				(map instanceDFunId insts')
	      }
	; return (ModDetails { md_types     = type_env'
			     , md_insts     = insts'
			     , md_fam_insts = fam_insts
			     , md_rules     = []
			     , md_exports   = exports })
	}
  where

isWiredInThing :: TyThing -> Bool
isWiredInThing thing = isWiredInName (getName thing)

tidyBootThing :: TyThing -> TyThing
-- Just externalise the Ids; keep everything
tidyBootThing (AnId id) | isLocalId id = AnId (tidyExternalId id)
tidyBootThing thing		       = thing

tidyExternalId :: Id -> Id
-- Takes an LocalId with an External Name, 
-- makes it into a GlobalId with VanillaIdInfo, and tidies its type
-- (NB: vanillaIdInfo makes a conservative assumption about Caf-hood.)
tidyExternalId id 
  = ASSERT2( isLocalId id && isExternalName (idName id), ppr id )
    mkVanillaGlobal (idName id) (tidyTopType (idType id)) vanillaIdInfo
\end{code}


%************************************************************************
%*				 					*
	Plan B: tidy bindings, make TypeEnv full of IdInfo
%*				 					* 
%************************************************************************

Plan B: include pragmas, make interfaces 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Figure out which Ids are externally visible

* Tidy the bindings, externalising appropriate Ids

* Drop all Ids from the TypeEnv, and add all the External Ids from 
  the bindings.  (This adds their IdInfo to the TypeEnv; and adds
  floated-out Ids that weren't even in the TypeEnv before.)

Step 1: Figure out external Ids
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
First we figure out which Ids are "external" Ids.  An
"external" Id is one that is visible from outside the compilation
unit.  These are
	a) the user exported ones
	b) ones mentioned in the unfoldings, workers, 
	   or rules of externally-visible ones 
This exercise takes a sweep of the bindings bottom to top.  Actually,
in Step 2 we're also going to need to know which Ids should be
exported with their unfoldings, so we produce not an IdSet but an
IdEnv Bool


Step 2: Tidy the program
~~~~~~~~~~~~~~~~~~~~~~~~
Next we traverse the bindings top to bottom.  For each *top-level*
binder

 1. Make it into a GlobalId; its IdDetails becomes VanillaGlobal, 
    reflecting the fact that from now on we regard it as a global, 
    not local, Id

 2. Give it a system-wide Unique.
    [Even non-exported things need system-wide Uniques because the
    byte-code generator builds a single Name->BCO symbol table.]

    We use the NameCache kept in the HscEnv as the
    source of such system-wide uniques.

    For external Ids, use the original-name cache in the NameCache
    to ensure that the unique assigned is the same as the Id had 
    in any previous compilation run.
  
 3. If it's an external Id, make it have a External Name, otherwise
    make it have an Internal Name.
    This is used by the code generator to decide whether
    to make the label externally visible

 4. Give external Ids a "tidy" OccName.  This means
    we can print them in interface files without confusing 
    "x" (unique 5) with "x" (unique 10).
  
 5. Give it its UTTERLY FINAL IdInfo; in ptic, 
  	* its unfolding, if it should have one
	
	* its arity, computed from the number of visible lambdas

	* its CAF info, computed from what is free in its RHS

		
Finally, substitute these new top-level binders consistently
throughout, including in unfoldings.  We also tidy binders in
RHSs, so that they print nicely in interfaces.

\begin{code}
tidyProgram :: HscEnv -> ModGuts -> IO (CgGuts, ModDetails)
tidyProgram hsc_env
	    mod_impl@(ModGuts {	mg_module = mod, mg_exports = exports, 
				mg_types = type_env, 
				mg_insts = insts, mg_fam_insts = fam_insts,
				mg_binds = binds, 
				mg_rules = imp_rules,
				mg_dir_imps = dir_imps, mg_deps = deps, 
				mg_foreign = foreign_stubs })

  = do	{ let dflags = hsc_dflags hsc_env
	; showPass dflags "Tidy Core"

	; let { omit_prags = dopt Opt_OmitInterfacePragmas dflags
	      ; ext_ids = findExternalIds omit_prags binds
	      ; ext_rules 
		   | omit_prags = []
		   | otherwise  = findExternalRules binds imp_rules ext_ids
		-- findExternalRules filters imp_rules to avoid binders that 
		-- aren't externally visible; but the externally-visible binders 
		-- are computed (by findExternalIds) assuming that all orphan
		-- rules are exported (they get their Exported flag set in the desugarer)
		-- So in fact we may export more than we need. 
		-- (It's a sort of mutual recursion.)
  	}

	; (tidy_env, tidy_binds) <- tidyTopBinds hsc_env mod type_env ext_ids 
						 binds

	; let { tidy_type_env = tidyTypeEnv omit_prags exports type_env 
					    tidy_binds
	      ; tidy_insts    = tidyInstances (lookup_dfun tidy_type_env) insts
		-- A DFunId will have a binding in tidy_binds, and so
		-- will now be in final_env, replete with IdInfo
		-- Its name will be unchanged since it was born, but
		-- we want Global, IdInfo-rich (or not) DFunId in the
		-- tidy_insts

	      ; tidy_rules = tidyRules tidy_env ext_rules
		-- You might worry that the tidy_env contains IdInfo-rich stuff
		-- and indeed it does, but if omit_prags is on, ext_rules is
		-- empty

	      ; implicit_binds = getImplicitBinds type_env
	      ; all_tidy_binds = implicit_binds ++ tidy_binds
	      ; alg_tycons = filter isAlgTyCon (typeEnvTyCons type_env)
	      }

   	; endPass dflags "Tidy Core" Opt_D_dump_simpl all_tidy_binds
	; dumpIfSet_core dflags Opt_D_dump_simpl
		"Tidy Core Rules"
		(pprRules tidy_rules)

	; return (CgGuts { cg_module   = mod, 
			   cg_tycons   = alg_tycons,
			   cg_binds    = all_tidy_binds,
			   cg_dir_imps = dir_imps,
			   cg_foreign  = foreign_stubs,
			   cg_dep_pkgs = dep_pkgs deps }, 

		   ModDetails { md_types     = tidy_type_env,
				md_rules     = tidy_rules,
				md_insts     = tidy_insts,
				md_fam_insts = fam_insts,
				md_exports   = exports })
	}

lookup_dfun type_env dfun_id
  = case lookupTypeEnv type_env (idName dfun_id) of
	Just (AnId dfun_id') -> dfun_id'
	other -> pprPanic "lookup_dfun" (ppr dfun_id)

tidyTypeEnv :: Bool -> NameSet -> TypeEnv -> [CoreBind] -> TypeEnv

-- The competed type environment is gotten from
--	Dropping any wired-in things, and then
-- 	a) keeping the types and classes
--	b) removing all Ids, 
--	c) adding Ids with correct IdInfo, including unfoldings,
--		gotten from the bindings
-- From (c) we keep only those Ids with External names;
--	    the CoreTidy pass makes sure these are all and only
--	    the externally-accessible ones
-- This truncates the type environment to include only the 
-- exported Ids and things needed from them, which saves space

tidyTypeEnv omit_prags exports type_env tidy_binds
  = let type_env1 = filterNameEnv keep_it type_env
	type_env2 = extendTypeEnvWithIds type_env1 final_ids
	type_env3 | omit_prags = mapNameEnv trim_thing type_env2
		  | otherwise  = type_env2
    in 
    type_env3
  where
    final_ids  = [ id | id <- bindersOfBinds tidy_binds, 
			isExternalName (idName id)]

   	-- We keep GlobalIds, because they won't appear 
	-- in the bindings from which final_ids are derived!
	-- (The bindings bind LocalIds.)
    keep_it thing | isWiredInThing thing = False
    keep_it (AnId id) = isGlobalId id	-- Keep GlobalIds (e.g. class ops)
    keep_it other     = True		-- Keep all TyCons, DataCons, and Classes

    trim_thing thing
	= case thing of
	    ATyCon tc | mustExposeTyCon exports tc -> thing
		      | otherwise -> ATyCon (makeTyConAbstract tc)

	    AnId id | isImplicitId id -> thing
		    | otherwise	      -> AnId (id `setIdInfo` vanillaIdInfo)

	    other -> thing

mustExposeTyCon :: NameSet	-- Exports
		-> TyCon	-- The tycon
		-> Bool 	-- Can its rep be hidden?
-- We are compiling without -O, and thus trying to write as little as 
-- possible into the interface file.  But we must expose the details of
-- any data types whose constructors or fields are exported
mustExposeTyCon exports tc
  | not (isAlgTyCon tc) 	-- Synonyms
  = True
  | isEnumerationTyCon tc	-- For an enumeration, exposing the constructors
  = True			-- won't lead to the need for further exposure
				-- (This includes data types with no constructors.)
  | isOpenTyCon tc		-- open type family
  = True
  | otherwise			-- Newtype, datatype
  = any exported_con (tyConDataCons tc)
	-- Expose rep if any datacon or field is exported

  || (isNewTyCon tc && isFFITy (snd (newTyConRep tc)))
	-- Expose the rep for newtypes if the rep is an FFI type.  
	-- For a very annoying reason.  'Foreign import' is meant to
	-- be able to look through newtypes transparently, but it
	-- can only do that if it can "see" the newtype representation
  where
    exported_con con = any (`elemNameSet` exports) 
			   (dataConName con : dataConFieldLabels con)

tidyInstances :: (DFunId -> DFunId) -> [Instance] -> [Instance]
tidyInstances tidy_dfun ispecs
  = map tidy ispecs
  where
    tidy ispec = setInstanceDFunId ispec $
		 tidy_dfun (instanceDFunId ispec)

getImplicitBinds :: TypeEnv -> [CoreBind]
getImplicitBinds type_env
  = map get_defn (concatMap implicit_con_ids (typeEnvTyCons type_env)
		  ++ concatMap other_implicit_ids (typeEnvElts type_env))
	-- Put the constructor wrappers first, because
	-- other implicit bindings (notably the fromT functions arising 
	-- from generics) use the constructor wrappers.  At least that's
	-- what External Core likes
  where
    implicit_con_ids tc = mapCatMaybes dataConWrapId_maybe (tyConDataCons tc)
    
    other_implicit_ids (ATyCon tc) = filter (not . isNaughtyRecordSelector) (tyConSelIds tc)
	-- The "naughty" ones are not real functions at all
	-- They are there just so we can get decent error messages
	-- See Note  [Naughty record selectors] in MkId.lhs
    other_implicit_ids (AClass cl) = classSelIds cl
    other_implicit_ids other       = []
    
    get_defn :: Id -> CoreBind
    get_defn id = NonRec id (tidyExpr emptyTidyEnv rhs)
	where
	  rhs = unfoldingTemplate (idUnfolding id)
	-- Don't forget to tidy the body !  Otherwise you get silly things like
	--	\ tpl -> case tpl of tpl -> (tpl,tpl) -> tpl
\end{code}


%************************************************************************
%*				 					*
\subsection{Step 1: finding externals}
%*				 					* 
%************************************************************************

\begin{code}
findExternalIds :: Bool
		-> [CoreBind]
		-> IdEnv Bool	-- In domain => external
				-- Range = True <=> show unfolding
	-- Step 1 from the notes above
findExternalIds omit_prags binds
  | omit_prags
  = mkVarEnv [ (id,False) | id <- bindersOfBinds binds, isExportedId id ]

  | otherwise
  = foldr find emptyVarEnv binds
  where
    find (NonRec id rhs) needed
	| need_id needed id = addExternal (id,rhs) needed
	| otherwise 	    = needed
    find (Rec prs) needed   = find_prs prs needed

	-- For a recursive group we have to look for a fixed point
    find_prs prs needed	
	| null needed_prs = needed
	| otherwise	  = find_prs other_prs new_needed
	where
	  (needed_prs, other_prs) = partition (need_pr needed) prs
	  new_needed = foldr addExternal needed needed_prs

	-- The 'needed' set contains the Ids that are needed by earlier
	-- interface file emissions.  If the Id isn't in this set, and isn't
	-- exported, there's no need to emit anything
    need_id needed_set id       = id `elemVarEnv` needed_set || isExportedId id 
    need_pr needed_set (id,rhs)	= need_id needed_set id

addExternal :: (Id,CoreExpr) -> IdEnv Bool -> IdEnv Bool
-- The Id is needed; extend the needed set
-- with it and its dependents (free vars etc)
addExternal (id,rhs) needed
  = extendVarEnv (foldVarSet add_occ needed new_needed_ids)
		 id show_unfold
  where
    add_occ id needed | id `elemVarEnv` needed = needed
		      | otherwise	       = extendVarEnv needed id False
	-- "False" because we don't know we need the Id's unfolding
	-- Don't override existing bindings; we might have already set it to True

    new_needed_ids = worker_ids	`unionVarSet`
		     unfold_ids	`unionVarSet`
		     spec_ids

    idinfo	   = idInfo id
    dont_inline	   = isNeverActive (inlinePragInfo idinfo)
    loop_breaker   = isNonRuleLoopBreaker (occInfo idinfo)
    bottoming_fn   = isBottomingSig (newStrictnessInfo idinfo `orElse` topSig)
    spec_ids	   = specInfoFreeVars (specInfo idinfo)
    worker_info	   = workerInfo idinfo

	-- Stuff to do with the Id's unfolding
	-- The simplifier has put an up-to-date unfolding
	-- in the IdInfo, but the RHS will do just as well
    unfolding	 = unfoldingInfo idinfo
    rhs_is_small = not (neverUnfold unfolding)

	-- We leave the unfolding there even if there is a worker
	-- In GHCI the unfolding is used by importers
	-- When writing an interface file, we omit the unfolding 
	-- if there is a worker
    show_unfold = not bottoming_fn	 &&	-- Not necessary
		  not dont_inline	 &&
		  not loop_breaker	 &&
		  rhs_is_small		 	-- Small enough

    unfold_ids | show_unfold = exprSomeFreeVars isLocalId rhs
	       | otherwise   = emptyVarSet

    worker_ids = case worker_info of
		   HasWorker work_id _ -> unitVarSet work_id
		   otherwise	       -> emptyVarSet
\end{code}


\begin{code}
findExternalRules :: [CoreBind]
		  -> [CoreRule]	-- Non-local rules (i.e. ones for imported fns)
	          -> IdEnv a	-- Ids that are exported, so we need their rules
	          -> [CoreRule]
  -- The complete rules are gotten by combining
  --	a) the non-local rules
  --	b) rules embedded in the top-level Ids
findExternalRules binds non_local_rules ext_ids
  = filter (not . internal_rule) (non_local_rules ++ local_rules)
  where
    local_rules  = [ rule
 		   | id <- bindersOfBinds binds,
		     id `elemVarEnv` ext_ids,
		     rule <- idCoreRules id
		   ]

    internal_rule rule
	=  any internal_id (varSetElems (ruleLhsFreeIds rule))
		-- Don't export a rule whose LHS mentions a locally-defined
		--  Id that is completely internal (i.e. not visible to an
		-- importing module)

    internal_id id = not (id `elemVarEnv` ext_ids)
\end{code}



%************************************************************************
%*									*
\subsection{Step 2: top-level tidying}
%*									*
%************************************************************************


\begin{code}
-- TopTidyEnv: when tidying we need to know
--   * nc_var: The NameCache, containing a unique supply and any pre-ordained Names.  
--	  These may have arisen because the
--	  renamer read in an interface file mentioning M.$wf, say,
--	  and assigned it unique r77.  If, on this compilation, we've
--	  invented an Id whose name is $wf (but with a different unique)
--	  we want to rename it to have unique r77, so that we can do easy
--	  comparisons with stuff from the interface file
--
--   * occ_env: The TidyOccEnv, which tells us which local occurrences 
--     are 'used'
--
--   * subst_env: A Var->Var mapping that substitutes the new Var for the old

tidyTopBinds :: HscEnv
	     -> Module
	     -> TypeEnv
	     -> IdEnv Bool	-- Domain = Ids that should be external
				-- True <=> their unfolding is external too
	     -> [CoreBind]
	     -> IO (TidyEnv, [CoreBind])

tidyTopBinds hsc_env mod type_env ext_ids binds
  = tidy init_env binds
  where
    nc_var = hsc_NC hsc_env 

	-- We also make sure to avoid any exported binders.  Consider
	--	f{-u1-} = 1	-- Local decl
	--	...
	--	f{-u2-} = 2	-- Exported decl
	--
	-- The second exported decl must 'get' the name 'f', so we
	-- have to put 'f' in the avoids list before we get to the first
	-- decl.  tidyTopId then does a no-op on exported binders.
    init_env = (initTidyOccEnv avoids, emptyVarEnv)
    avoids   = [getOccName name | bndr <- typeEnvIds type_env,
				  let name = idName bndr,
				  isExternalName name]
		-- In computing our "avoids" list, we must include
		--	all implicit Ids
		--	all things with global names (assigned once and for
		--					all by the renamer)
		-- since their names are "taken".
		-- The type environment is a convenient source of such things.

    this_pkg = thisPackage (hsc_dflags hsc_env)

    tidy env []     = return (env, [])
    tidy env (b:bs) = do { (env1, b')  <- tidyTopBind this_pkg mod nc_var ext_ids env b
			 ; (env2, bs') <- tidy env1 bs
			 ; return (env2, b':bs') }

------------------------
tidyTopBind  :: PackageId
	     -> Module
	     -> IORef NameCache	-- For allocating new unique names
	     -> IdEnv Bool	-- Domain = Ids that should be external
				-- True <=> their unfolding is external too
	     -> TidyEnv -> CoreBind
	     -> IO (TidyEnv, CoreBind)

tidyTopBind this_pkg mod nc_var ext_ids tidy_env1@(occ_env1,subst1) (NonRec bndr rhs)
  = do	{ (occ_env2, name') <- tidyTopName mod nc_var ext_ids occ_env1 bndr
	; let	{ (bndr', rhs') = tidyTopPair ext_ids tidy_env2 caf_info name' (bndr, rhs)
		; subst2        = extendVarEnv subst1 bndr bndr'
		; tidy_env2     = (occ_env2, subst2) }
	; return (tidy_env2, NonRec bndr' rhs') }
  where
    caf_info = hasCafRefs this_pkg subst1 (idArity bndr) rhs

tidyTopBind this_pkg mod nc_var ext_ids tidy_env1@(occ_env1,subst1) (Rec prs)
  = do	{ (occ_env2, names') <- tidyTopNames mod nc_var ext_ids occ_env1 bndrs
	; let	{ prs'      = zipWith (tidyTopPair ext_ids tidy_env2 caf_info)
				      names' prs
		; subst2    = extendVarEnvList subst1 (bndrs `zip` map fst prs')
		; tidy_env2 = (occ_env2, subst2) }
	; return (tidy_env2, Rec prs') }
  where
    bndrs = map fst prs

	-- the CafInfo for a recursive group says whether *any* rhs in
	-- the group may refer indirectly to a CAF (because then, they all do).
    caf_info 
	| or [ mayHaveCafRefs (hasCafRefs this_pkg subst1 (idArity bndr) rhs)
	     | (bndr,rhs) <- prs ] = MayHaveCafRefs
	| otherwise 		   = NoCafRefs

--------------------------------------------------------------------
--		tidyTopName
-- This is where we set names to local/global based on whether they really are 
-- externally visible (see comment at the top of this module).  If the name
-- was previously local, we have to give it a unique occurrence name if
-- we intend to externalise it.
tidyTopNames mod nc_var ext_ids occ_env [] = return (occ_env, [])
tidyTopNames mod nc_var ext_ids occ_env (id:ids)
  = do	{ (occ_env1, name)  <- tidyTopName  mod nc_var ext_ids occ_env id
  	; (occ_env2, names) <- tidyTopNames mod nc_var ext_ids occ_env1 ids
	; return (occ_env2, name:names) }

tidyTopName :: Module -> IORef NameCache -> VarEnv Bool -> TidyOccEnv
	    -> Id -> IO (TidyOccEnv, Name)
tidyTopName mod nc_var ext_ids occ_env id
  | global && internal = return (occ_env, localiseName name)

  | global && external = return (occ_env, name)
	-- Global names are assumed to have been allocated by the renamer,
	-- so they already have the "right" unique
	-- And it's a system-wide unique too

  -- Now we get to the real reason that all this is in the IO Monad:
  -- we have to update the name cache in a nice atomic fashion

  | local  && internal = do { nc <- readIORef nc_var
			    ; let (nc', new_local_name) = mk_new_local nc
			    ; writeIORef nc_var nc'
			    ; return (occ_env', new_local_name) }
	-- Even local, internal names must get a unique occurrence, because
	-- if we do -split-objs we externalise the name later, in the code generator
	--
	-- Similarly, we must make sure it has a system-wide Unique, because
	-- the byte-code generator builds a system-wide Name->BCO symbol table

  | local  && external = do { nc <- readIORef nc_var
			    ; let (nc', new_external_name) = mk_new_external nc
			    ; writeIORef nc_var nc'
			    ; return (occ_env', new_external_name) }
  where
    name	= idName id
    external    = id `elemVarEnv` ext_ids
    global	= isExternalName name
    local	= not global
    internal	= not external
    mb_parent   = nameParent_maybe name
    loc		= nameSrcLoc name

    (occ_env', occ') = tidyOccName occ_env (nameOccName name)

    mk_new_local nc = (nc { nsUniqs = us2 }, mkInternalName uniq occ' loc)
 		    where
		      (us1, us2) = splitUniqSupply (nsUniqs nc)
		      uniq	 = uniqFromSupply us1

    mk_new_external nc = allocateGlobalBinder nc mod occ' mb_parent loc
	-- If we want to externalise a currently-local name, check
	-- whether we have already assigned a unique for it.
	-- If so, use it; if not, extend the table.
	-- All this is done by allcoateGlobalBinder.
	-- This is needed when *re*-compiling a module in GHCi; we must
	-- use the same name for externally-visible things as we did before.


-----------------------------------------------------------
tidyTopPair :: VarEnv Bool
	    -> TidyEnv 	-- The TidyEnv is used to tidy the IdInfo
			-- It is knot-tied: don't look at it!
	    -> CafInfo
	    -> Name		-- New name
	    -> (Id, CoreExpr) 	-- Binder and RHS before tidying
	    -> (Id, CoreExpr)
	-- This function is the heart of Step 2
	-- The rec_tidy_env is the one to use for the IdInfo
	-- It's necessary because when we are dealing with a recursive
	-- group, a variable late in the group might be mentioned
	-- in the IdInfo of one early in the group

tidyTopPair ext_ids rhs_tidy_env caf_info name' (bndr, rhs)
  | isGlobalId bndr 		-- Injected binding for record selector, etc
  = (bndr, tidyExpr rhs_tidy_env rhs)
  | otherwise
  = (bndr', rhs')
  where
    bndr'   = mkVanillaGlobal name' ty' idinfo'
    ty'	    = tidyTopType (idType bndr)
    rhs'    = tidyExpr rhs_tidy_env rhs
    idinfo' = tidyTopIdInfo rhs_tidy_env (isJust maybe_external)
			    (idInfo bndr) unfold_info arity
			    caf_info

    -- Expose an unfolding if ext_ids tells us to
    -- Remember that ext_ids maps an Id to a Bool: 
    --	True to show the unfolding, False to hide it
    maybe_external = lookupVarEnv ext_ids bndr
    show_unfold = maybe_external `orElse` False
    unfold_info | show_unfold = mkTopUnfolding rhs'
		| otherwise   = noUnfolding

    -- Usually the Id will have an accurate arity on it, because
    -- the simplifier has just run, but not always. 
    -- One case I found was when the last thing the simplifier
    -- did was to let-bind a non-atomic argument and then float
    -- it to the top level. So it seems more robust just to
    -- fix it here.
    arity = exprArity rhs


-- tidyTopIdInfo creates the final IdInfo for top-level
-- binders.  There are two delicate pieces:
--
--  * Arity.  After CoreTidy, this arity must not change any more.
--	Indeed, CorePrep must eta expand where necessary to make
--	the manifest arity equal to the claimed arity.
--
--  * CAF info.  This must also remain valid through to code generation.
-- 	We add the info here so that it propagates to all
-- 	occurrences of the binders in RHSs, and hence to occurrences in
-- 	unfoldings, which are inside Ids imported by GHCi. Ditto RULES.
--	CoreToStg makes use of this when constructing SRTs.

tidyTopIdInfo tidy_env is_external idinfo unfold_info arity caf_info
  | not is_external	-- For internal Ids (not externally visible)
  = vanillaIdInfo	-- we only need enough info for code generation
			-- Arity and strictness info are enough;
			--	c.f. CoreTidy.tidyLetBndr
	`setCafInfo` 	       caf_info
	`setArityInfo`	       arity
	`setAllStrictnessInfo` newStrictnessInfo idinfo

  | otherwise		-- Externally-visible Ids get the whole lot
  = vanillaIdInfo
	`setCafInfo` 	       caf_info
	`setArityInfo`	       arity
	`setAllStrictnessInfo` newStrictnessInfo idinfo
	`setInlinePragInfo`    inlinePragInfo idinfo
	`setUnfoldingInfo`     unfold_info
	`setWorkerInfo`	       tidyWorker tidy_env (workerInfo idinfo)
		-- NB: we throw away the Rules
		-- They have already been extracted by findExternalRules



------------  Worker  --------------
tidyWorker tidy_env (HasWorker work_id wrap_arity) 
  = HasWorker (tidyVarOcc tidy_env work_id) wrap_arity
tidyWorker tidy_env other
  = NoWorker
\end{code}

%************************************************************************
%*									*
\subsection{Figuring out CafInfo for an expression}
%*									*
%************************************************************************

hasCafRefs decides whether a top-level closure can point into the dynamic heap.
We mark such things as `MayHaveCafRefs' because this information is
used to decide whether a particular closure needs to be referenced
in an SRT or not.

There are two reasons for setting MayHaveCafRefs:
	a) The RHS is a CAF: a top-level updatable thunk.
	b) The RHS refers to something that MayHaveCafRefs

Possible improvement: In an effort to keep the number of CAFs (and 
hence the size of the SRTs) down, we could also look at the expression and 
decide whether it requires a small bounded amount of heap, so we can ignore 
it as a CAF.  In these cases however, we would need to use an additional
CAF list to keep track of non-collectable CAFs.  

\begin{code}
hasCafRefs  :: PackageId -> VarEnv Var -> Arity -> CoreExpr -> CafInfo
hasCafRefs this_pkg p arity expr 
  | is_caf || mentions_cafs = MayHaveCafRefs
  | otherwise 		    = NoCafRefs
 where
  mentions_cafs = isFastTrue (cafRefs p expr)
  is_caf = not (arity > 0 || rhsIsStatic this_pkg expr)
  -- NB. we pass in the arity of the expression, which is expected
  -- to be calculated by exprArity.  This is because exprArity
  -- knows how much eta expansion is going to be done by 
  -- CorePrep later on, and we don't want to duplicate that
  -- knowledge in rhsIsStatic below.

cafRefs p (Var id)
	-- imported Ids first:
  | not (isLocalId id) = fastBool (mayHaveCafRefs (idCafInfo id))
	-- now Ids local to this module:
  | otherwise =
     case lookupVarEnv p id of
	Just id' -> fastBool (mayHaveCafRefs (idCafInfo id'))
	Nothing  -> fastBool False

cafRefs p (Lit l) 	       = fastBool False
cafRefs p (App f a) 	       = fastOr (cafRefs p f) (cafRefs p) a
cafRefs p (Lam x e) 	       = cafRefs p e
cafRefs p (Let b e) 	       = fastOr (cafRefss p (rhssOfBind b)) (cafRefs p) e
cafRefs p (Case e bndr _ alts) = fastOr (cafRefs p e) (cafRefss p) (rhssOfAlts alts)
cafRefs p (Note n e) 	       = cafRefs p e
cafRefs p (Cast e co)          = cafRefs p e
cafRefs p (Type t) 	       = fastBool False

cafRefss p [] 	  = fastBool False
cafRefss p (e:es) = fastOr (cafRefs p e) (cafRefss p) es

-- hack for lazy-or over FastBool.
fastOr a f x = fastBool (isFastTrue a || isFastTrue (f x))
\end{code}
