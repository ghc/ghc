%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{Tidying up Core}

\begin{code}
module CoreTidy (
	tidyCorePgm, tidyExpr, tidyCoreExpr,
	tidyBndr, tidyBndrs
    ) where

#include "HsVersions.h"

import CmdLineOpts	( DynFlags, DynFlag(..), opt_OmitInterfacePragmas )
import CoreSyn
import CoreUnfold	( noUnfolding, mkTopUnfolding, okToUnfoldInHiFile )
import CoreFVs		( ruleSomeFreeVars, exprSomeFreeVars )
import PprCore		( pprIdCoreRule )
import CoreLint		( showPass, endPass )
import CoreUtils	( exprArity )
import VarEnv
import VarSet
import Var		( Id, Var )
import Id		( idType, idInfo, idName, isExportedId, 
			  idSpecialisation, idUnique, 
			  mkVanillaGlobal, isLocalId, 
			  isImplicitId, mkUserLocal, setIdInfo
			) 
import IdInfo		{- loads of stuff -}
import NewDemand	( isBottomingSig, topSig )
import BasicTypes	( isNeverActive )
import Name		( getOccName, nameOccName, mkLocalName, mkGlobalName, 
		  	  localiseName, isGlobalName, nameSrcLoc
			)
import NameEnv		( filterNameEnv )
import OccName		( TidyOccEnv, initTidyOccEnv, tidyOccName )
import Type		( tidyTopType, tidyType, tidyTyVarBndr )
import Module		( Module, moduleName )
import HscTypes		( PersistentCompilerState( pcs_PRS ), 
			  PersistentRenamerState( prsOrig ),
			  NameSupply( nsNames, nsUniqs ),
			  TypeEnv, extendTypeEnvList, typeEnvIds,
			  ModDetails(..), TyThing(..)
			)
import FiniteMap	( lookupFM, addToFM )
import Maybes		( orElse )
import ErrUtils		( showPass, dumpIfSet_core )
import SrcLoc		( noSrcLoc )
import UniqFM		( mapUFM )
import UniqSupply	( splitUniqSupply, uniqFromSupply )
import List		( partition )
import Util		( mapAccumL )
import Maybe		( isJust )
import Outputable
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

    We use the NameSupply kept in the PersistentRenamerState as the
    source of such system-wide uniques.

    For external Ids, use the original-name cache in the NameSupply 
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
tidyCorePgm :: DynFlags -> Module
	    -> PersistentCompilerState
	    -> CgInfoEnv		-- Information from the back end,
					-- to be splatted into the IdInfo
	    -> ModDetails
	    -> IO (PersistentCompilerState, ModDetails)

tidyCorePgm dflags mod pcs cg_info_env
	    (ModDetails { md_types = env_tc, md_insts = insts_tc, 
			  md_binds = binds_in, md_rules = orphans_in })
  = do	{ showPass dflags "Tidy Core"

	; let ext_ids   = findExternalSet   binds_in orphans_in
	; let ext_rules = findExternalRules binds_in orphans_in ext_ids

	-- We also make sure to avoid any exported binders.  Consider
	--	f{-u1-} = 1	-- Local decl
	--	...
	--	f{-u2-} = 2	-- Exported decl
	--
	-- The second exported decl must 'get' the name 'f', so we
	-- have to put 'f' in the avoids list before we get to the first
	-- decl.  tidyTopId then does a no-op on exported binders.
	; let   prs	      = pcs_PRS pcs
		orig_ns       = prsOrig prs

		init_tidy_env = (orig_ns, initTidyOccEnv avoids, emptyVarEnv)
		avoids	      = [getOccName name | bndr <- typeEnvIds env_tc,
						   let name = idName bndr,
						   isGlobalName name]
		-- In computing our "avoids" list, we must include
		--	all implicit Ids
		--	all things with global names (assigned once and for
		--					all by the renamer)
		-- since their names are "taken".
		-- The type environment is a convenient source of such things.

	; let ((orig_ns', occ_env, subst_env), tidy_binds) 
	       		= mapAccumL (tidyTopBind mod ext_ids cg_info_env) 
				    init_tidy_env binds_in

	; let tidy_rules = tidyIdRules (occ_env,subst_env) ext_rules

	; let prs' = prs { prsOrig = orig_ns' }
	      pcs' = pcs { pcs_PRS = prs' }

	; let final_ids  = [ id 
			   | bind <- tidy_binds
			   , id <- bindersOf bind
			   , isGlobalName (idName id)]

		-- Dfuns are local Ids that might have
		-- changed their unique during tidying
	; let lookup_dfun_id id = lookupVarEnv subst_env id `orElse` 
				  pprPanic "lookup_dfun_id" (ppr id)


	; let tidy_type_env = mkFinalTypeEnv env_tc final_ids
	      tidy_dfun_ids = map lookup_dfun_id insts_tc

	; let tidy_details = ModDetails { md_types = tidy_type_env,
					  md_rules = tidy_rules,
					  md_insts = tidy_dfun_ids,
					  md_binds = tidy_binds }

   	; endPass dflags "Tidy Core" Opt_D_dump_simpl tidy_binds
	; dumpIfSet_core dflags Opt_D_dump_simpl
		"Tidy Core Rules"
		(vcat (map pprIdCoreRule tidy_rules))

	; return (pcs', tidy_details)
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
mkFinalTypeEnv :: TypeEnv	-- From typechecker
	       -> [Id]		-- Final Ids
	       -> TypeEnv

mkFinalTypeEnv type_env final_ids
  = extendTypeEnvList (filterNameEnv keep_it type_env)
		      (map AnId final_ids)
  where
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

	-- We keep implicit Ids, because they won't appear 
	-- in the bindings from which final_ids are derived!
    keep_it (AnId id) = isImplicitId id	-- Remove all Ids except implicit ones
    keep_it other     = True		-- Keep all TyCons and Classes
\end{code}

\begin{code}
findExternalRules :: [CoreBind]
		  -> [IdCoreRule] -- Orphan rules
	          -> IdEnv a	  -- Ids that are exported, so we need their rules
	          -> [IdCoreRule]
  -- The complete rules are gotten by combining
  --	a) the orphan rules
  --	b) rules embedded in the top-level Ids
findExternalRules binds orphan_rules ext_ids
  | opt_OmitInterfacePragmas = []
  | otherwise
  = orphan_rules ++ local_rules
  where
    local_rules  = [ (id, rule)
 		   | id <- bindersOfBinds binds,
		     id `elemVarEnv` ext_ids,
		     rule <- rulesRules (idSpecialisation id),
		     not (isBuiltinRule rule)
			-- We can't print builtin rules in interface files
			-- Since they are built in, an importing module
			-- will have access to them anyway
		 ]
\end{code}

%************************************************************************
%*				 					*
\subsection{Step 1: finding externals}
%*				 					* 
%************************************************************************

\begin{code}
findExternalSet :: [CoreBind] -> [IdCoreRule]
		-> IdEnv Bool	-- In domain => external
				-- Range = True <=> show unfolding
	-- Step 1 from the notes above
findExternalSet binds orphan_rules
  = foldr find init_needed binds
  where
    orphan_rule_ids :: IdSet
    orphan_rule_ids = unionVarSets [ ruleSomeFreeVars isLocalId rule 
				   | (_, rule) <- orphan_rules]
    init_needed :: IdEnv Bool
    init_needed = mapUFM (\_ -> False) orphan_rule_ids
	-- The mapUFM is a bit cheesy.  It is a cheap way
	-- to turn the set of orphan_rule_ids, which we use to initialise
	-- the sweep, into a mapping saying 'don't expose unfolding'	
	-- (When we come to the binding site we may change our mind, of course.)

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
    add_occ id needed = extendVarEnv needed id False
	-- "False" because we don't know we need the Id's unfolding
	-- We'll override it later when we find the binding site

    new_needed_ids | opt_OmitInterfacePragmas = emptyVarSet
	           | otherwise		      = worker_ids	`unionVarSet`
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
		  rhs_is_small		 &&	-- Small enough
		  okToUnfoldInHiFile rhs 	-- No casms etc

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
type TopTidyEnv = (NameSupply, TidyOccEnv, VarEnv Var)

-- TopTidyEnv: when tidying we need to know
--   * ns: The NameSupply, containing a unique supply and any pre-ordained Names.  
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
	    -> CgInfoEnv
	    -> TopTidyEnv -> CoreBind
	    -> (TopTidyEnv, CoreBind)

tidyTopBind mod ext_ids cg_info_env top_tidy_env (NonRec bndr rhs)
  = ((orig,occ,subst) , NonRec bndr' rhs')
  where
    ((orig,occ,subst), bndr')
	 = tidyTopBinder mod ext_ids cg_info_env 
			 rec_tidy_env rhs rhs' top_tidy_env bndr
    rec_tidy_env = (occ,subst)
    rhs' = tidyExpr rec_tidy_env rhs

tidyTopBind mod ext_ids cg_info_env top_tidy_env (Rec prs)
  = (final_env, Rec prs')
  where
    (final_env@(_,occ,subst), prs') = mapAccumL do_one top_tidy_env prs
    rec_tidy_env = (occ,subst)

    do_one top_tidy_env (bndr,rhs) 
	= ((orig,occ,subst), (bndr',rhs'))
	where
	((orig,occ,subst), bndr')
	   = tidyTopBinder mod ext_ids cg_info_env
		rec_tidy_env rhs rhs' top_tidy_env bndr

        rhs' = tidyExpr rec_tidy_env rhs

tidyTopBinder :: Module -> IdEnv Bool -> CgInfoEnv
	      -> TidyEnv 	-- The TidyEnv is used to tidy the IdInfo
	      -> CoreExpr	-- RHS *before* tidying
	      -> CoreExpr	-- RHS *after* tidying
			-- The TidyEnv and the after-tidying RHS are
			-- both are knot-tied: don't look at them!
	      -> TopTidyEnv -> Id -> (TopTidyEnv, Id)
  -- NB: tidyTopBinder doesn't affect the unique supply

tidyTopBinder mod ext_ids cg_info_env rec_tidy_env rhs tidy_rhs
	      env@(ns2, occ_env2, subst_env2) id
	-- This function is the heart of Step 2
	-- The rec_tidy_env is the one to use for the IdInfo
	-- It's necessary because when we are dealing with a recursive
	-- group, a variable late in the group might be mentioned
	-- in the IdInfo of one early in the group

	-- The rhs is already tidied
	
  = ((orig_env', occ_env', subst_env'), id')
  where
    (orig_env', occ_env', name') = tidyTopName mod ns2 occ_env2
					       is_external
					       (idName id)
    ty'	   = tidyTopType (idType id)
    idinfo = tidyTopIdInfo rec_tidy_env is_external 
			   (idInfo id) unfold_info arity
			   (lookupCgInfo cg_info_env name')

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
-- * CAF info, which comes from the CoreToStg pass via a knot.
-- 	The CAF info will not be looked at by the downstream stuff:
-- 	it *generates* it, and knot-ties it back.  It will only be
-- 	looked at by (a) MkIface when generating an interface file
-- 		     (b) In GHCi, importing modules
-- 	Nevertheless, we add the info here so that it propagates to all
-- 	occurrences of the binders in RHSs, and hence to occurrences in
-- 	unfoldings, which are inside Ids imported by GHCi. Ditto RULES.
--     
-- 	An alterative would be to do a second pass over the unfoldings 
-- 	of Ids, and rules, right at the top, but that would be a pain.

tidyTopIdInfo tidy_env is_external idinfo unfold_info arity cg_info
  | opt_OmitInterfacePragmas || not is_external
	-- Only basic info if the Id isn't external, or if we don't have -O
  = basic_info

  | otherwise	-- Add extra optimisation info
  = basic_info
	`setInlinePragInfo`    inlinePragInfo idinfo
	`setUnfoldingInfo`     unfold_info
	`setWorkerInfo`	       tidyWorker tidy_env (workerInfo idinfo)
		-- NB: we throw away the Rules
		-- They have already been extracted by findExternalRules
  
  where
	-- baasic_info is attached to every top-level binder
    basic_info = vanillaIdInfo 
			`setCgInfo` 	       cg_info
			`setArityInfo`	       arity
			`setNewStrictnessInfo` newStrictnessInfo idinfo

-- This is where we set names to local/global based on whether they really are 
-- externally visible (see comment at the top of this module).  If the name
-- was previously local, we have to give it a unique occurrence name if
-- we intend to globalise it.
tidyTopName mod ns occ_env external name
  | global && internal = (ns, occ_env, localiseName name)

  | global && external = (ns, occ_env, name)
	-- Global names are assumed to have been allocated by the renamer,
	-- so they already have the "right" unique
	-- And it's a system-wide unique too

  | local  && internal = (ns_w_local, occ_env', new_local_name)
	-- Even local, internal names must get a unique occurrence, because
	-- if we do -split-objs we globalise the name later, in the code generator
	--
	-- Similarly, we must make sure it has a system-wide Unique, because
	-- the byte-code generator builds a system-wide Name->BCO symbol table

  | local  && external = case lookupFM ns_names key of
			   Just orig -> (ns,	      occ_env', orig)
			   Nothing   -> (ns_w_global, occ_env', new_global_name)
	-- If we want to globalise a currently-local name, check
	-- whether we have already assigned a unique for it.
	-- If so, use it; if not, extend the table (ns_w_global).
	-- This is needed when *re*-compiling a module in GHCi; we want to
	-- use the same name for externally-visible things as we did before.

  where
    global	     = isGlobalName name
    local	     = not global
    internal	     = not external

    (occ_env', occ') = tidyOccName occ_env (nameOccName name)
    key		     = (moduleName mod, occ')
    ns_names	     = nsNames ns
    ns_uniqs	     = nsUniqs ns
    (us1, us2)	     = splitUniqSupply ns_uniqs
    uniq	     = uniqFromSupply us1
    loc		     = nameSrcLoc name

    new_local_name   = mkLocalName  uniq     occ' loc
    new_global_name  = mkGlobalName uniq mod occ' loc  

    ns_w_local	     = ns { nsUniqs = us2 }
    ns_w_global	     = ns { nsUniqs = us2, nsNames = addToFM ns_names key new_global_name }


------------  Worker  --------------
tidyWorker tidy_env (HasWorker work_id wrap_arity) 
  = HasWorker (tidyVarOcc tidy_env work_id) wrap_arity
tidyWorker tidy_env other
  = NoWorker

------------  Rules  --------------
tidyIdRules :: TidyEnv -> [IdCoreRule] -> [IdCoreRule]
tidyIdRules env [] = []
tidyIdRules env ((fn,rule) : rules)
  = tidyRule env rule  		=: \ rule ->
    tidyIdRules env rules 	=: \ rules ->
     ((tidyVarOcc env fn, rule) : rules)

tidyRule :: TidyEnv -> CoreRule -> CoreRule
tidyRule env rule@(BuiltinRule _ _) = rule
tidyRule env (Rule name act vars tpl_args rhs)
  = tidyBndrs env vars			=: \ (env', vars) ->
    map (tidyExpr env') tpl_args  	=: \ tpl_args ->
     (Rule name act vars tpl_args (tidyExpr env' rhs))
\end{code}

%************************************************************************
%*									*
\subsection{Step 2: inner tidying
%*									*
%************************************************************************

\begin{code}
tidyBind :: TidyEnv
	 -> CoreBind
	 ->  (TidyEnv, CoreBind)

tidyBind env (NonRec bndr rhs)
  = tidyLetBndr env (bndr,rhs)		=: \ (env', bndr') ->
    (env', NonRec bndr' (tidyExpr env' rhs))

tidyBind env (Rec prs)
  = mapAccumL tidyLetBndr env prs	=: \ (env', bndrs') ->
    map (tidyExpr env') (map snd prs)	=: \ rhss' ->
    (env', Rec (zip bndrs' rhss'))


tidyExpr env (Var v)   	=  Var (tidyVarOcc env v)
tidyExpr env (Type ty) 	=  Type (tidyType env ty)
tidyExpr env (Lit lit) 	=  Lit lit
tidyExpr env (App f a) 	=  App (tidyExpr env f) (tidyExpr env a)
tidyExpr env (Note n e) =  Note (tidyNote env n) (tidyExpr env e)

tidyExpr env (Let b e) 
  = tidyBind env b 	=: \ (env', b') ->
    Let b' (tidyExpr env' e)

tidyExpr env (Case e b alts)
  = tidyBndr env b 	=: \ (env', b) ->
    Case (tidyExpr env e) b (map (tidyAlt env') alts)

tidyExpr env (Lam b e)
  = tidyBndr env b 	=: \ (env', b) ->
    Lam b (tidyExpr env' e)


tidyAlt env (con, vs, rhs)
  = tidyBndrs env vs 	=: \ (env', vs) ->
    (con, vs, tidyExpr env' rhs)

tidyNote env (Coerce t1 t2)  = Coerce (tidyType env t1) (tidyType env t2)
tidyNote env note            = note
\end{code}


%************************************************************************
%*									*
\subsection{Tidying up non-top-level binders}
%*									*
%************************************************************************

\begin{code}
tidyVarOcc (_, var_env) v = case lookupVarEnv var_env v of
				  Just v' -> v'
				  Nothing -> v

-- tidyBndr is used for lambda and case binders
tidyBndr :: TidyEnv -> Var -> (TidyEnv, Var)
tidyBndr env var
  | isTyVar var = tidyTyVarBndr env var
  | otherwise   = tidyIdBndr env var

tidyBndrs :: TidyEnv -> [Var] -> (TidyEnv, [Var])
tidyBndrs env vars = mapAccumL tidyBndr env vars

tidyLetBndr :: TidyEnv -> (Id, CoreExpr) -> (TidyEnv, Var)
-- Used for local (non-top-level) let(rec)s
tidyLetBndr env (id,rhs) 
  = ((tidy_env,new_var_env), final_id)
  where
    ((tidy_env,var_env), new_id) = tidyIdBndr env id

	-- We need to keep around any interesting strictness and demand info
	-- because later on we may need to use it when converting to A-normal form.
	-- eg.
	--	f (g x),  where f is strict in its argument, will be converted
	--	into  case (g x) of z -> f z  by CorePrep, but only if f still
	-- 	has its strictness info.
	--
	-- Similarly for the demand info - on a let binder, this tells 
	-- CorePrep to turn the let into a case.
	--
	-- Similarly arity info for eta expansion in CorePrep
    final_id = new_id `setIdInfo` new_info
    idinfo   = idInfo id
    new_info = vanillaIdInfo 
		`setArityInfo`		exprArity rhs
		`setNewStrictnessInfo`	newStrictnessInfo idinfo
		`setNewDemandInfo`	newDemandInfo idinfo

    -- Override the env we get back from tidyId with the new IdInfo
    -- so it gets propagated to the usage sites.
    new_var_env = extendVarEnv var_env id final_id

tidyIdBndr :: TidyEnv -> Id -> (TidyEnv, Id)
tidyIdBndr env@(tidy_env, var_env) id
  = 	-- Non-top-level variables
    let 
	-- Give the Id a fresh print-name, *and* rename its type
	-- The SrcLoc isn't important now, 
	-- though we could extract it from the Id
	-- 
	-- All nested Ids now have the same IdInfo, namely none,
	-- which should save some space.
	(tidy_env', occ') = tidyOccName tidy_env (getOccName id)
        ty'          	  = tidyType env (idType id)
	id'          	  = mkUserLocal occ' (idUnique id) ty' noSrcLoc
	var_env'	  = extendVarEnv var_env id id'
    in
     ((tidy_env', var_env'), id')
\end{code}

\begin{code}
m =: k = m `seq` k m
\end{code}
