%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{Tidying up Core}

\begin{code}
module TidyPgm( tidyCorePgm, tidyCoreExpr ) where

#include "HsVersions.h"

import CmdLineOpts	( DynFlags, DynFlag(..), dopt )
import CoreSyn
import CoreUnfold	( noUnfolding, mkTopUnfolding )
import CoreFVs		( ruleLhsFreeIds, ruleRhsFreeVars, exprSomeFreeVars )
import CoreTidy		( tidyExpr, tidyVarOcc, tidyIdRules )
import PprCore 		( pprIdRules )
import CoreLint		( showPass, endPass )
import CoreUtils	( exprArity, rhsIsStatic )
import VarEnv
import VarSet
import Var		( Id, Var )
import Id		( idType, idInfo, idName, idCoreRules, 
			  isExportedId, mkVanillaGlobal, isLocalId, 
			  isImplicitId, idArity, setIdInfo, idCafInfo
			) 
import IdInfo		{- loads of stuff -}
import NewDemand	( isBottomingSig, topSig )
import BasicTypes	( Arity, isNeverActive )
import Name		( getOccName, nameOccName, mkInternalName,
		  	  localiseName, isExternalName, nameSrcLoc
			)
import RnEnv		( lookupOrigNameCache, newExternalName )
import NameEnv		( lookupNameEnv, filterNameEnv )
import OccName		( TidyOccEnv, initTidyOccEnv, tidyOccName )
import Type		( tidyTopType )
import Module		( Module )
import HscTypes		( PersistentCompilerState( pcs_nc ), 
			  NameCache( nsNames, nsUniqs ),
			  TypeEnv, extendTypeEnvList, typeEnvIds,
			  ModGuts(..), ModGuts, TyThing(..)
			)
import Maybes		( orElse )
import ErrUtils		( showPass, dumpIfSet_core )
import UniqFM		( mapUFM )
import UniqSupply	( splitUniqSupply, uniqFromSupply )
import List		( partition )
import Util		( mapAccumL )
import Maybe		( isJust )
import Outputable
import FastTypes  hiding ( fastOr )
\end{code}


%************************************************************************
%*				 					*
\subsection{What goes on}
%*				 					* 
%************************************************************************

[SLPJ: 19 Nov 00]

The plan is this.  

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

 1. Make it into a GlobalId

 2. Give it a system-wide Unique.
    [Even non-exported things need system-wide Uniques because the
    byte-code generator builds a single Name->BCO symbol table.]

    We use the NameCache kept in the PersistentCompilerState as the
    source of such system-wide uniques.

    For external Ids, use the original-name cache in the NameCache
    to ensure that the unique assigned is the same as the Id had 
    in any previous compilation run.
  
 3. If it's an external Id, make it have a global Name, otherwise
    make it have a local Name.
    This is used by the code generator to decide whether
    to make the label externally visible

 4. Give external Ids a "tidy" occurrence name.  This means
    we can print them in interface files without confusing 
    "x" (unique 5) with "x" (unique 10).
  
 5. Give it its UTTERLY FINAL IdInfo; in ptic, 
	* Its IdDetails becomes VanillaGlobal, reflecting the fact that
	  from now on we regard it as a global, not local, Id

  	* its unfolding, if it should have one
	
	* its arity, computed from the number of visible lambdas

	* its CAF info, computed from what is free in its RHS

		
Finally, substitute these new top-level binders consistently
throughout, including in unfoldings.  We also tidy binders in
RHSs, so that they print nicely in interfaces.

\begin{code}
tidyCorePgm :: DynFlags
	    -> PersistentCompilerState
	    -> ModGuts
	    -> IO (PersistentCompilerState, ModGuts)

tidyCorePgm dflags pcs
	    mod_impl@(ModGuts { mg_module = mod, 
				mg_types = env_tc, mg_insts = insts_tc, 
				mg_binds = binds_in, mg_rules = orphans_in })
  = do	{ showPass dflags "Tidy Core"

	; let omit_iface_prags = dopt Opt_OmitInterfacePragmas dflags
	; let ext_ids   = findExternalSet   omit_iface_prags binds_in orphans_in
	; let ext_rules = findExternalRules omit_iface_prags binds_in orphans_in ext_ids
		-- findExternalRules filters ext_rules to avoid binders that 
		-- aren't externally visible; but the externally-visible binders 
		-- are computed (by findExternalSet) assuming that all orphan
		-- rules are exported.  So in fact we may export more than we
		-- need.  (It's a sort of mutual recursion.)

	-- We also make sure to avoid any exported binders.  Consider
	--	f{-u1-} = 1	-- Local decl
	--	...
	--	f{-u2-} = 2	-- Exported decl
	--
	-- The second exported decl must 'get' the name 'f', so we
	-- have to put 'f' in the avoids list before we get to the first
	-- decl.  tidyTopId then does a no-op on exported binders.
	; let   orig_ns       = pcs_nc pcs
		init_tidy_env = (orig_ns, initTidyOccEnv avoids, emptyVarEnv)
		avoids	      = [getOccName name | bndr <- typeEnvIds env_tc,
						   let name = idName bndr,
						   isExternalName name]
		-- In computing our "avoids" list, we must include
		--	all implicit Ids
		--	all things with global names (assigned once and for
		--					all by the renamer)
		-- since their names are "taken".
		-- The type environment is a convenient source of such things.

	; let ((orig_ns', occ_env, subst_env), tidy_binds) 
	       		= mapAccumL (tidyTopBind mod ext_ids) 
				    init_tidy_env binds_in

	; let tidy_rules = tidyIdRules (occ_env,subst_env) ext_rules

	; let pcs' = pcs { pcs_nc = orig_ns' }

	; let tidy_type_env = mkFinalTypeEnv omit_iface_prags env_tc tidy_binds

		-- Dfuns are local Ids that might have
		-- changed their unique during tidying.  Remember
		-- to lookup the id in the TypeEnv too, because
		-- those Ids have had their IdInfo stripped if
		-- necessary.
	; let lookup_dfun_id id = 
		 case lookupVarEnv subst_env id of
		   Nothing -> dfun_panic
		   Just id -> 
		      case lookupNameEnv tidy_type_env (idName id) of
			Just (AnId id) -> id
			_other -> dfun_panic
	      	where 
		   dfun_panic = pprPanic "lookup_dfun_id" (ppr id)

	      tidy_dfun_ids = map lookup_dfun_id insts_tc

	; let tidy_result = mod_impl { mg_types = tidy_type_env,
				       mg_rules = tidy_rules,
				       mg_insts = tidy_dfun_ids,
				       mg_binds = tidy_binds }

   	; endPass dflags "Tidy Core" Opt_D_dump_simpl tidy_binds
	; dumpIfSet_core dflags Opt_D_dump_simpl
		"Tidy Core Rules"
		(pprIdRules tidy_rules)

	; return (pcs', tidy_result)
	}

tidyCoreExpr :: CoreExpr -> IO CoreExpr
tidyCoreExpr expr = return (tidyExpr emptyTidyEnv expr)
\end{code}


%************************************************************************
%*				 					*
\subsection{Write a new interface file}
%*				 					*
%************************************************************************

\begin{code}
mkFinalTypeEnv :: Bool		-- Omit interface pragmas
	       -> TypeEnv 	-- From typechecker
	       -> [CoreBind]	-- Final Ids
	       -> TypeEnv

-- The competed type environment is gotten from
-- 	a) keeping the types and classes
--	b) removing all Ids, 
--	c) adding Ids with correct IdInfo, including unfoldings,
--		gotten from the bindings
-- From (c) we keep only those Ids with Global names;
--	    the CoreTidy pass makes sure these are all and only
--	    the externally-accessible ones
-- This truncates the type environment to include only the 
-- exported Ids and things needed from them, which saves space
--
-- However, we do keep things like constructors, which should not appear 
-- in interface files, because they are needed by importing modules when
-- using the compilation manager

mkFinalTypeEnv omit_iface_prags type_env tidy_binds
  = extendTypeEnvList (filterNameEnv keep_it type_env) final_ids
  where
    final_ids  = [ AnId (strip_id_info id)
		 | bind <- tidy_binds,
		   id <- bindersOf bind,
		   isExternalName (idName id)]

    strip_id_info id
	  | omit_iface_prags = id `setIdInfo` vanillaIdInfo
	  | otherwise	     = id
	-- If the interface file has no pragma info then discard all
	-- info right here.
	--
	-- This is not so important for *this* module, but it's
	-- vital for ghc --make:
	--   subsequent compilations must not see (e.g.) the arity if
	--   the interface file does not contain arity
	-- If they do, they'll exploit the arity; then the arity might
	-- change, but the iface file doesn't change => recompilation
	-- does not happen => disaster
	--
	-- This IdInfo will live long-term in the Id => vanillaIdInfo makes
	-- a conservative assumption about Caf-hood
	-- 
	-- We're not worried about occurrences of these Ids in unfoldings,
	-- because in OmitInterfacePragmas mode we're stripping all the
	-- unfoldings anyway.

	-- We keep implicit Ids, because they won't appear 
	-- in the bindings from which final_ids are derived!
    keep_it (AnId id) = isImplicitId id	-- Remove all Ids except implicit ones
    keep_it other     = True		-- Keep all TyCons and Classes
\end{code}

\begin{code}
findExternalRules :: Bool	  -- Omit interface pragmas 
		  -> [CoreBind]
		  -> [IdCoreRule] -- Orphan rules
	          -> IdEnv a	  -- Ids that are exported, so we need their rules
	          -> [IdCoreRule]
  -- The complete rules are gotten by combining
  --	a) the orphan rules
  --	b) rules embedded in the top-level Ids
findExternalRules omit_iface_prags binds orphan_rules ext_ids
  | omit_iface_prags = []
  | otherwise
  = filter needed_rule (orphan_rules ++ local_rules)
  where
    local_rules  = [ rule
 		   | id <- bindersOfBinds binds,
		     id `elemVarEnv` ext_ids,
		     rule <- idCoreRules id
		   ]
    needed_rule (id, rule)
	=  not (isBuiltinRule rule)
	 	-- We can't print builtin rules in interface files
		-- Since they are built in, an importing module
		-- will have access to them anyway

	&& not (any internal_id (varSetElems (ruleLhsFreeIds rule)))
		-- Don't export a rule whose LHS mentions an Id that
		-- is completely internal (i.e. not visible to an
		-- importing module)

    internal_id id = isLocalId id && not (id `elemVarEnv` ext_ids)
\end{code}

%************************************************************************
%*				 					*
\subsection{Step 1: finding externals}
%*				 					* 
%************************************************************************

\begin{code}
findExternalSet :: Bool -- omit interface pragmas
		-> [CoreBind] -> [IdCoreRule]
		-> IdEnv Bool	-- In domain => external
				-- Range = True <=> show unfolding
	-- Step 1 from the notes above
findExternalSet omit_iface_prags binds orphan_rules
  = foldr find init_needed binds
  where
    orphan_rule_ids :: IdSet
    orphan_rule_ids = unionVarSets [ ruleRhsFreeVars rule 
				   | (_, rule) <- orphan_rules]
    init_needed :: IdEnv Bool
    init_needed = mapUFM (\_ -> False) orphan_rule_ids
	-- The mapUFM is a bit cheesy.  It is a cheap way
	-- to turn the set of orphan_rule_ids, which we use to initialise
	-- the sweep, into a mapping saying 'don't expose unfolding'	
	-- (When we come to the binding site we may change our mind, of course.)

    find (NonRec id rhs) needed
	| need_id needed id = addExternal omit_iface_prags (id,rhs) needed
	| otherwise 	    = needed
    find (Rec prs) needed   = find_prs prs needed

	-- For a recursive group we have to look for a fixed point
    find_prs prs needed	
	| null needed_prs = needed
	| otherwise	  = find_prs other_prs new_needed
	where
	  (needed_prs, other_prs) = partition (need_pr needed) prs
	  new_needed = foldr (addExternal omit_iface_prags) needed needed_prs

	-- The 'needed' set contains the Ids that are needed by earlier
	-- interface file emissions.  If the Id isn't in this set, and isn't
	-- exported, there's no need to emit anything
    need_id needed_set id       = id `elemVarEnv` needed_set || isExportedId id 
    need_pr needed_set (id,rhs)	= need_id needed_set id

addExternal :: Bool -> (Id,CoreExpr) -> IdEnv Bool -> IdEnv Bool
-- The Id is needed; extend the needed set
-- with it and its dependents (free vars etc)
addExternal omit_iface_prags (id,rhs) needed
  = extendVarEnv (foldVarSet add_occ needed new_needed_ids)
		 id show_unfold
  where
    add_occ id needed = extendVarEnv needed id False
	-- "False" because we don't know we need the Id's unfolding
	-- We'll override it later when we find the binding site

    new_needed_ids | omit_iface_prags = emptyVarSet
	           | otherwise	      = worker_ids	`unionVarSet`
					unfold_ids	`unionVarSet`
					spec_ids

    idinfo	   = idInfo id
    dont_inline	   = isNeverActive (inlinePragInfo idinfo)
    loop_breaker   = isLoopBreaker (occInfo idinfo)
    bottoming_fn   = isBottomingSig (newStrictnessInfo idinfo `orElse` topSig)
    spec_ids	   = rulesRhsFreeVars (specInfo idinfo)
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


%************************************************************************
%*									*
\subsection{Step 2: top-level tidying}
%*									*
%************************************************************************


\begin{code}
type TopTidyEnv = (NameCache, TidyOccEnv, VarEnv Var)

-- TopTidyEnv: when tidying we need to know
--   * ns: The NameCache, containing a unique supply and any pre-ordained Names.  
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
\end{code}


\begin{code}
tidyTopBind :: Module
	    -> IdEnv Bool	-- Domain = Ids that should be external
				-- True <=> their unfolding is external too
	    -> TopTidyEnv -> CoreBind
	    -> (TopTidyEnv, CoreBind)

tidyTopBind mod ext_ids top_tidy_env@(_,_,subst1) (NonRec bndr rhs)
  = ((orig,occ,subst) , NonRec bndr' rhs')
  where
    ((orig,occ,subst), bndr')
	 = tidyTopBinder mod ext_ids caf_info
			 rec_tidy_env rhs rhs' top_tidy_env bndr
    rec_tidy_env = (occ,subst)
    rhs' = tidyExpr rec_tidy_env rhs
    caf_info = hasCafRefs subst1 (idArity bndr') rhs'

tidyTopBind mod ext_ids top_tidy_env@(_,_,subst1) (Rec prs)
  = (final_env, Rec prs')
  where
    (final_env@(_,occ,subst), prs') = mapAccumL do_one top_tidy_env prs
    rec_tidy_env = (occ,subst)

    do_one top_tidy_env (bndr,rhs) 
	= ((orig,occ,subst), (bndr',rhs'))
	where
	((orig,occ,subst), bndr')
	   = tidyTopBinder mod ext_ids caf_info
		rec_tidy_env rhs rhs' top_tidy_env bndr

        rhs' = tidyExpr rec_tidy_env rhs

	-- the CafInfo for a recursive group says whether *any* rhs in
	-- the group may refer indirectly to a CAF (because then, they all do).
    caf_info 
	| or [ mayHaveCafRefs (hasCafRefs subst1 (idArity bndr) rhs)
	     | (bndr,rhs) <- prs ] = MayHaveCafRefs
	| otherwise = NoCafRefs

tidyTopBinder :: Module -> IdEnv Bool -> CafInfo
	      -> TidyEnv 	-- The TidyEnv is used to tidy the IdInfo
	      -> CoreExpr	-- RHS *before* tidying
	      -> CoreExpr	-- RHS *after* tidying
			-- The TidyEnv and the after-tidying RHS are
			-- both are knot-tied: don't look at them!
	      -> TopTidyEnv -> Id -> (TopTidyEnv, Id)
  -- NB: tidyTopBinder doesn't affect the unique supply

tidyTopBinder mod ext_ids caf_info rec_tidy_env rhs tidy_rhs
	      env@(ns2, occ_env2, subst_env2) id
	-- This function is the heart of Step 2
	-- The rec_tidy_env is the one to use for the IdInfo
	-- It's necessary because when we are dealing with a recursive
	-- group, a variable late in the group might be mentioned
	-- in the IdInfo of one early in the group

	-- The rhs is already tidied

  = ASSERT(isLocalId id)  -- "all Ids defined in this module are local
			  -- until the CoreTidy phase"  --GHC comentary
    ((orig_env', occ_env', subst_env'), id')
  where
    (orig_env', occ_env', name') = tidyTopName mod ns2 occ_env2
					       is_external
					       (idName id)
    ty'	   = tidyTopType (idType id)
    idinfo = tidyTopIdInfo rec_tidy_env is_external 
			   (idInfo id) unfold_info arity
			   caf_info

    id' = mkVanillaGlobal name' ty' idinfo

    subst_env' = extendVarEnv subst_env2 id id'

    maybe_external = lookupVarEnv ext_ids id
    is_external    = isJust maybe_external

    -- Expose an unfolding if ext_ids tells us to
    -- Remember that ext_ids maps an Id to a Bool: 
    --	True to show the unfolding, False to hide it
    show_unfold = maybe_external `orElse` False
    unfold_info | show_unfold = mkTopUnfolding tidy_rhs
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


-- This is where we set names to local/global based on whether they really are 
-- externally visible (see comment at the top of this module).  If the name
-- was previously local, we have to give it a unique occurrence name if
-- we intend to externalise it.
tidyTopName mod ns occ_env external name
  | global && internal = (ns, occ_env, localiseName name)

  | global && external = (ns, occ_env, name)
	-- Global names are assumed to have been allocated by the renamer,
	-- so they already have the "right" unique
	-- And it's a system-wide unique too

  | local  && internal = (ns_w_local, occ_env', new_local_name)
	-- Even local, internal names must get a unique occurrence, because
	-- if we do -split-objs we externalise the name later, in the code generator
	--
	-- Similarly, we must make sure it has a system-wide Unique, because
	-- the byte-code generator builds a system-wide Name->BCO symbol table

  | local  && external = case lookupOrigNameCache ns_names mod occ' of
			   Just orig -> (ns,	      occ_env', orig)
			   Nothing   -> (ns_w_global, occ_env', new_external_name)
	-- If we want to externalise a currently-local name, check
	-- whether we have already assigned a unique for it.
	-- If so, use it; if not, extend the table (ns_w_global).
	-- This is needed when *re*-compiling a module in GHCi; we want to
	-- use the same name for externally-visible things as we did before.

  where
    global	     = isExternalName name
    local	     = not global
    internal	     = not external
    loc		     = nameSrcLoc name

    (occ_env', occ') = tidyOccName occ_env (nameOccName name)

    ns_names	     = nsNames ns
    (us1, us2)	     = splitUniqSupply (nsUniqs ns)
    uniq	     = uniqFromSupply us1
    new_local_name   = mkInternalName uniq occ' loc
    ns_w_local	     = ns { nsUniqs = us2 }

    (ns_w_global, new_external_name) = newExternalName ns mod occ' loc


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
hasCafRefs  :: VarEnv Var -> Arity -> CoreExpr -> CafInfo
hasCafRefs p arity expr 
  | is_caf || mentions_cafs = MayHaveCafRefs
  | otherwise 		    = NoCafRefs
 where
  mentions_cafs = isFastTrue (cafRefs p expr)
  is_caf = not (arity > 0 || rhsIsStatic expr)
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

cafRefs p (Lit l) 	     = fastBool False
cafRefs p (App f a) 	     = fastOr (cafRefs p f) (cafRefs p) a
cafRefs p (Lam x e) 	     = cafRefs p e
cafRefs p (Let b e) 	     = fastOr (cafRefss p (rhssOfBind b)) (cafRefs p) e
cafRefs p (Case e bndr alts) = fastOr (cafRefs p e) (cafRefss p) (rhssOfAlts alts)
cafRefs p (Note n e) 	     = cafRefs p e
cafRefs p (Type t) 	     = fastBool False

cafRefss p [] 	  = fastBool False
cafRefss p (e:es) = fastOr (cafRefs p e) (cafRefss p) es

-- hack for lazy-or over FastBool.
fastOr a f x = fastBool (isFastTrue a || isFastTrue (f x))
\end{code}
