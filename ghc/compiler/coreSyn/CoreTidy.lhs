%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{Tidying up Core}

\begin{code}
module CoreTidy (
	tidyCorePgm, tidyExpr, 
	tidyBndr, tidyBndrs
    ) where

#include "HsVersions.h"

import CmdLineOpts	( DynFlags, DynFlag(..), opt_OmitInterfacePragmas )
import CoreSyn
import CoreUnfold	( noUnfolding, mkTopUnfolding, okToUnfoldInHiFile )
import CoreUtils	( exprArity )
import CoreFVs		( ruleSomeFreeVars, exprSomeFreeVars )
import CoreLint		( showPass, endPass )
import VarEnv
import VarSet
import Var		( Id, Var )
import Id		( idType, idInfo, idName, isExportedId,
			  idCafInfo, mkId, isLocalId, isImplicitId,
			  idFlavour, modifyIdInfo, idArity
			) 
import IdInfo		{- loads of stuff -}
import Name		( getOccName, nameOccName, globaliseName, setNameOcc, 
		  	  localiseName, mkLocalName, isGlobalName, isDllName
			)
import OccName		( TidyOccEnv, initTidyOccEnv, tidyOccName )
import Type		( tidyTopType, tidyType, tidyTyVar )
import Module		( Module, moduleName )
import PrimOp		( PrimOp(..), setCCallUnique )
import HscTypes		( PersistentCompilerState( pcs_PRS ), 
			  PersistentRenamerState( prsOrig ),
			  NameSupply( nsNames ), OrigNameCache
			)
import UniqSupply
import DataCon		( DataCon, dataConName )
import Literal		( isLitLitLit )
import FiniteMap	( lookupFM, addToFM )
import Maybes		( maybeToBool, orElse )
import ErrUtils		( showPass )
import PprCore		( pprIdCoreRule )
import SrcLoc		( noSrcLoc )
import UniqFM		( mapUFM )
import Outputable
import FastTypes
import List		( partition )
import Util		( mapAccumL )
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
Next we traverse the bindings top to bottom.  For each top-level
binder

  - Make all external Ids have Global names and vice versa
    This is used by the code generator to decide whether
    to make the label externally visible

  - Give external ids a "tidy" occurrence name.  This means
    we can print them in interface files without confusing 
    "x" (unique 5) with "x" (unique 10).
  
  - Give external Ids the same Unique as they had before
    if the name is in the renamer's name cache
  
  - Clone all local Ids.  This means that Tidy Core has the property
    that all Ids are unique, rather than the weaker guarantee of
    no clashes which the simplifier provides.

  - Give each dynamic CCall occurrence a fresh unique; this is
    rather like the cloning step above.

  - Give the Id its UTTERLY FINAL IdInfo; in ptic, 
	* Its flavour becomes ConstantId, reflecting the fact that
	  from now on we regard it as a constant, not local, Id

  	* its unfolding, if it should have one
	
	* its arity, computed from the number of visible lambdas

	* its CAF info, computed from what is free in its RHS

		
Finally, substitute these new top-level binders consistently
throughout, including in unfoldings.  We also tidy binders in
RHSs, so that they print nicely in interfaces.

\begin{code}
tidyCorePgm :: DynFlags -> Module
	    -> PersistentCompilerState
	    -> [CoreBind] -> [IdCoreRule]
	    -> IO (PersistentCompilerState, [CoreBind], [IdCoreRule])
tidyCorePgm dflags mod pcs binds_in orphans_in
  = do	{ showPass dflags "Tidy Core"

	; let ext_ids = findExternalSet binds_in orphans_in

	; us <- mkSplitUniqSupply 't' -- for "tidy"

	; let ((us1, orig_env', occ_env, subst_env), binds_out) 
	       		= mapAccumL (tidyTopBind mod ext_ids) 
				    (init_tidy_env us) binds_in

	; let (orphans_out, _) 
		   = initUs us1 (tidyIdRules (occ_env,subst_env) orphans_in)

	; let prs' = prs { prsOrig = orig { nsNames = orig_env' } }
	      pcs' = pcs { pcs_PRS = prs' }

	; endPass dflags "Tidy Core" Opt_D_dump_simpl binds_out

	; return (pcs', binds_out, orphans_out)
	}
  where
	-- We also make sure to avoid any exported binders.  Consider
	--	f{-u1-} = 1	-- Local decl
	--	...
	--	f{-u2-} = 2	-- Exported decl
	--
	-- The second exported decl must 'get' the name 'f', so we
	-- have to put 'f' in the avoids list before we get to the first
	-- decl.  tidyTopId then does a no-op on exported binders.
    prs	 	     = pcs_PRS pcs
    orig	     = prsOrig prs
    orig_env 	     = nsNames orig

    init_tidy_env us = (us, orig_env, initTidyOccEnv avoids, emptyVarEnv)
    avoids	     = [getOccName bndr | bndr <- bindersOfBinds binds_in,
				       isGlobalName (idName bndr)]
\end{code}


%************************************************************************
%*				 					*
\subsection{Step 1: finding externals}
%*				 					* 
%************************************************************************

\begin{code}
findExternalSet :: [CoreBind] -> [IdCoreRule]
		-> IdEnv Bool	-- True <=> show unfolding
	-- Step 1 from the notes above
findExternalSet binds orphan_rules
  = pprTrace "fes" (vcat (map pprIdCoreRule orphan_rules) $$ ppr (varSetElems orphan_rule_ids)) $
    foldr find init_needed binds
  where
    orphan_rule_ids :: IdSet
    orphan_rule_ids = unionVarSets [ ruleSomeFreeVars isIdAndLocal rule 
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

isIdAndLocal id = isId id && isLocalId id

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
    dont_inline	   = isNeverInlinePrag (inlinePragInfo idinfo)
    loop_breaker   = isLoopBreaker (occInfo idinfo)
    bottoming_fn   = isBottomingStrictness (strictnessInfo idinfo)
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

    unfold_ids | show_unfold = exprSomeFreeVars isIdAndLocal rhs
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
type TopTidyEnv = (UniqSupply, OrigNameCache, TidyOccEnv, VarEnv Var)

-- TopTidyEnv: when tidying we need to know
--   * orig_env: Any pre-ordained Names.  These may have arisen because the
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
--
--   * uniqsuppy: so we can clone any Ids with non-preordained names.
--
\end{code}


\begin{code}
tidyTopBind :: Module
	    -> IdEnv Bool	-- Domain = Ids that should be external
				-- True <=> their unfolding is external too
	    -> TopTidyEnv -> CoreBind
	    -> (TopTidyEnv, CoreBind)

tidyTopBind mod ext_ids env (NonRec bndr rhs)
  = ((us2,orig,occ,subst) , NonRec bndr' rhs')
  where
    ((us1,orig,occ,subst), bndr')
	 = tidyTopBinder mod ext_ids tidy_env rhs' caf_info env bndr
    tidy_env    = (occ,subst)
    caf_info    = hasCafRefs (const True) rhs'
    (rhs',us2)  = initUs us1 (tidyExpr tidy_env rhs)

tidyTopBind mod ext_ids env (Rec prs)
  = (final_env, Rec prs')
  where
    (final_env@(_,_,occ,subst), prs') = mapAccumL do_one env prs
    final_tidy_env = (occ,subst)

    do_one env (bndr,rhs) 
	= ((us',orig,occ,subst), (bndr',rhs'))
	where
	((us,orig,occ,subst), bndr')
	   = tidyTopBinder mod ext_ids final_tidy_env rhs' caf_info env bndr
        (rhs', us')   = initUs us (tidyExpr final_tidy_env rhs)

	-- the CafInfo for a recursive group says whether *any* rhs in
	-- the group may refer indirectly to a CAF (because then, they all do).
    (bndrs, rhss) = unzip prs'
    caf_info = hasCafRefss pred rhss
    pred v = v `notElem` bndrs


tidyTopBinder :: Module -> IdEnv Bool
	      -> TidyEnv -> CoreExpr -> CafInfo
			-- The TidyEnv is used to tidy the IdInfo
			-- The expr is the already-tided RHS
			-- Both are knot-tied: don't look at them!
	      -> TopTidyEnv -> Id -> (TopTidyEnv, Id)

tidyTopBinder mod ext_ids tidy_env rhs caf_info
	      env@(us, orig_env2, occ_env2, subst_env2) id

  | isImplicitId id	-- Don't mess with constructors, 
  = (env, id)		-- record selectors, and the like

  | otherwise
	-- This function is the heart of Step 2
	-- The second env is the one to use for the IdInfo
	-- It's necessary because when we are dealing with a recursive
	-- group, a variable late in the group might be mentioned
	-- in the IdInfo of one early in the group

	-- The rhs is already tidied
	
  = ((us_r, orig_env', occ_env', subst_env'), id')
  where
    (us_l, us_r)    = splitUniqSupply us

    (orig_env', occ_env', name') = tidyTopName mod orig_env2 occ_env2
					       is_external
					       (idName id)
    ty'	       	    = tidyTopType (idType id)
    idinfo'         = tidyIdInfo us_l tidy_env
			 is_external unfold_info arity_info caf_info id

    id'	       = mkId name' ty' idinfo'
    subst_env' = extendVarEnv subst_env2 id id'

    maybe_external = lookupVarEnv ext_ids id
    is_external    = maybeToBool maybe_external

    -- Expose an unfolding if ext_ids tells us to
    show_unfold = maybe_external `orElse` False
    unfold_info | show_unfold = mkTopUnfolding rhs
		| otherwise   = noUnfolding

    arity_info = exprArity rhs


tidyIdInfo us tidy_env is_external unfold_info arity_info caf_info id
  | opt_OmitInterfacePragmas || not is_external
	-- No IdInfo if the Id isn't external, or if we don't have -O
  = mkIdInfo new_flavour caf_info
	`setStrictnessInfo` strictnessInfo core_idinfo
	`setArityInfo`	    ArityExactly arity_info
	-- Keep strictness, arity and CAF info; it's used by the code generator

  | otherwise
  =  let (rules', _) = initUs us (tidyRules tidy_env (specInfo core_idinfo))
     in
     mkIdInfo new_flavour caf_info
	`setCprInfo`	    cprInfo core_idinfo
	`setStrictnessInfo` strictnessInfo core_idinfo
	`setInlinePragInfo` inlinePragInfo core_idinfo
	`setUnfoldingInfo`  unfold_info
	`setWorkerInfo`	    tidyWorker tidy_env arity_info (workerInfo core_idinfo)
	`setSpecInfo`	    rules'
	`setArityInfo`	    ArityExactly arity_info
		-- this is the final IdInfo, it must agree with the
		-- code finally generated (i.e. NO more transformations
		-- after this!).
  where
    core_idinfo = idInfo id

	-- A DFunId must stay a DFunId, so that we can gather the
	-- DFunIds up later.  Other local things become ConstantIds.
    new_flavour = case flavourInfo core_idinfo of
		    VanillaId  -> ConstantId
		    ExportedId -> ConstantId
		    ConstantId -> ConstantId	-- e.g. Default methods
		    DictFunId  -> DictFunId
		    flavour    -> pprTrace "tidyIdInfo" (ppr id <+> ppFlavourInfo flavour)
				  flavour


-- This is where we set names to local/global based on whether they really are 
-- externally visible (see comment at the top of this module).  If the name
-- was previously local, we have to give it a unique occurrence name if
-- we intend to globalise it.
tidyTopName mod orig_env occ_env external name
  | global && internal = (orig_env, occ_env, localiseName name)

  | local  && internal = (orig_env, occ_env', setNameOcc name occ')
	-- Even local, internal names must get a unique occurrence, because
	-- if we do -split-objs we globalise the name later, n the code generator

  | global && external = (orig_env, occ_env, name)
	-- Global names are assumed to have been allocated by the renamer,
	-- so they already have the "right" unique

  | local  && external = case lookupFM orig_env key of
			   Just orig -> (orig_env,			   occ_env', orig)
			   Nothing   -> (addToFM orig_env key global_name, occ_env', global_name)
	-- If we want to globalise a currently-local name, check
	-- whether we have already assigned a unique for it.
	-- If so, use it; if not, extend the table

  where
    (occ_env', occ') = tidyOccName occ_env (nameOccName name)
    key		     = (moduleName mod, occ')
    global_name      = globaliseName (setNameOcc name occ') mod
    global	     = isGlobalName name
    local	     = not global
    internal	     = not external

------------  Worker  --------------
-- We only treat a function as having a worker if
-- the exported arity (which is now the number of visible lambdas)
-- is the same as the arity at the moment of the w/w split
-- If so, we can safely omit the unfolding inside the wrapper, and
-- instead re-generate it from the type/arity/strictness info
-- But if the arity has changed, we just take the simple path and
-- put the unfolding into the interface file, forgetting the fact
-- that it's a wrapper.  
--
-- How can this happen?  Sometimes we get
--	f = coerce t (\x y -> $wf x y)
-- at the moment of w/w split; but the eta reducer turns it into
--	f = coerce t $wf
-- which is perfectly fine except that the exposed arity so far as
-- the code generator is concerned (zero) differs from the arity
-- when we did the split (2).  
--
-- All this arises because we use 'arity' to mean "exactly how many
-- top level lambdas are there" in interface files; but during the
-- compilation of this module it means "how many things can I apply
-- this to".
tidyWorker tidy_env real_arity (HasWorker work_id wrap_arity) 
  | real_arity == wrap_arity
  = HasWorker (tidyVarOcc tidy_env work_id) wrap_arity
tidyWorker tidy_env real_arity other
  = NoWorker

------------  Rules  --------------
tidyIdRules :: TidyEnv -> [IdCoreRule] -> UniqSM [IdCoreRule]
tidyIdRules env [] = returnUs []
tidyIdRules env ((fn,rule) : rules)
  = tidyRule env rule  		`thenUs` \ rule ->
    tidyIdRules env rules 	`thenUs` \ rules ->
    returnUs ((tidyVarOcc env fn, rule) : rules)

tidyRules :: TidyEnv -> CoreRules -> UniqSM CoreRules
tidyRules env (Rules rules fvs) 
  = mapUs (tidyRule env) rules 		`thenUs` \ rules ->
    returnUs (Rules rules (foldVarSet tidy_set_elem emptyVarSet fvs))
  where
    tidy_set_elem var new_set = extendVarSet new_set (tidyVarOcc env var)

tidyRule :: TidyEnv -> CoreRule -> UniqSM CoreRule
tidyRule env rule@(BuiltinRule _) = returnUs rule
tidyRule env (Rule name vars tpl_args rhs)
  = tidyBndrs env vars			`thenUs` \ (env', vars) ->
    mapUs (tidyExpr env') tpl_args  	`thenUs` \ tpl_args ->
    tidyExpr env' rhs		 	`thenUs` \ rhs ->
    returnUs (Rule name vars tpl_args rhs)
\end{code}

%************************************************************************
%*									*
\subsection{Step 2: inner tidying
%*									*
%************************************************************************

\begin{code}
tidyBind :: TidyEnv
	 -> CoreBind
	 -> UniqSM (TidyEnv, CoreBind)
tidyBind env (NonRec bndr rhs)
  = tidyBndrWithRhs env (bndr,rhs) `thenUs` \ (env', bndr') ->
    tidyExpr env' rhs  		   `thenUs` \ rhs' ->
    returnUs (env', NonRec bndr' rhs')

tidyBind env (Rec prs)
  = mapAccumLUs tidyBndrWithRhs env prs 	`thenUs` \ (env', bndrs') ->
    mapUs (tidyExpr env') (map snd prs)		`thenUs` \ rhss' ->
    returnUs (env', Rec (zip bndrs' rhss'))

tidyExpr env (Var v)   
  = fiddleCCall v  `thenUs` \ v ->
    returnUs (Var (tidyVarOcc env v))

tidyExpr env (Type ty) = returnUs (Type (tidyType env ty))
tidyExpr env (Lit lit) = returnUs (Lit lit)

tidyExpr env (App f a)
  = tidyExpr env f 		`thenUs` \ f ->
    tidyExpr env a 		`thenUs` \ a ->
    returnUs (App f a)

tidyExpr env (Note n e)
  = tidyExpr env e 		`thenUs` \ e ->
    returnUs (Note (tidyNote env n) e)

tidyExpr env (Let b e) 
  = tidyBind env b 		`thenUs` \ (env', b') ->
    tidyExpr env' e 		`thenUs` \ e ->
    returnUs (Let b' e)

tidyExpr env (Case e b alts)
  = tidyExpr env e 		`thenUs` \ e ->
    tidyBndr env b 		`thenUs` \ (env', b) ->
    mapUs (tidyAlt env') alts 	`thenUs` \ alts ->
    returnUs (Case e b alts)

tidyExpr env (Lam b e)
  = tidyBndr env b 		`thenUs` \ (env', b) ->
    tidyExpr env' e		`thenUs` \ e ->
    returnUs (Lam b e)


tidyAlt env (con, vs, rhs)
  = tidyBndrs env vs		`thenUs` \ (env', vs) ->
    tidyExpr env' rhs		`thenUs` \ rhs ->
    returnUs (con, vs, rhs)

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
tidyBndr :: TidyEnv -> Var -> UniqSM (TidyEnv, Var)
tidyBndr env var
  | isTyVar var = returnUs (tidyTyVar env var)
  | otherwise   = tidyId env var vanillaIdInfo

tidyBndrs :: TidyEnv -> [Var] -> UniqSM (TidyEnv, [Var])
tidyBndrs env vars = mapAccumLUs tidyBndr env vars

-- tidyBndrWithRhs is used for let binders
tidyBndrWithRhs :: TidyEnv -> (Var, CoreExpr) -> UniqSM (TidyEnv, Var)
tidyBndrWithRhs env (id,rhs)
   = tidyId env id idinfo
   where
	idinfo = vanillaIdInfo `setArityInfo` ArityExactly (exprArity rhs)
			-- NB: This throws away the IdInfo of the Id, which we
			-- no longer need.  That means we don't need to
			-- run over it with env, nor renumber it.

tidyId :: TidyEnv -> Id -> IdInfo -> UniqSM (TidyEnv, Id)
tidyId env@(tidy_env, var_env) id idinfo
  = 	-- Non-top-level variables
    getUniqueUs   `thenUs` \ uniq ->
    let 
	-- Give the Id a fresh print-name, *and* rename its type
	-- The SrcLoc isn't important now, 
	-- though we could extract it from the Id
	name'        	  = mkLocalName uniq occ' noSrcLoc
	(tidy_env', occ') = tidyOccName tidy_env (getOccName id)
        ty'          	  = tidyType (tidy_env,var_env) (idType id)
	id'          	  = mkId name' ty' idinfo
	var_env'	  = extendVarEnv var_env id id'
    in
    returnUs ((tidy_env', var_env'), id')


fiddleCCall id 
  = case idFlavour id of
         PrimOpId (CCallOp ccall) ->
	    -- Make a guaranteed unique name for a dynamic ccall.
	    getUniqueUs   	`thenUs` \ uniq ->
	    returnUs (modifyIdInfo (`setFlavourInfo` 
			    PrimOpId (CCallOp (setCCallUnique ccall uniq))) id)
	 other_flavour ->
	     returnUs id
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
hasCafRefs  :: (Id -> Bool) -> CoreExpr -> CafInfo
-- Only called for the RHS of top-level lets
hasCafRefss :: (Id -> Bool) -> [CoreExpr] -> CafInfo
	-- predicate returns True for a given Id if we look at this Id when
	-- calculating the result.  Used to *avoid* looking at the CafInfo
 	-- field for an Id that is part of the current recursive group.

hasCafRefs p expr = if isCAF expr || isFastTrue (cafRefs p expr)
			then MayHaveCafRefs
			else NoCafRefs

	-- used for recursive groups.  The whole group is set to
	-- "MayHaveCafRefs" if at least one of the group is a CAF or
	-- refers to any CAFs.
hasCafRefss p exprs = if any isCAF exprs || isFastTrue (cafRefss p exprs)
			then MayHaveCafRefs
			else NoCafRefs

cafRefs p (Var id)
 | p id
 = case idCafInfo id of 
	NoCafRefs      -> fastBool False
	MayHaveCafRefs -> fastBool True
 | otherwise
 = fastBool False

cafRefs p (Lit l) 	     = fastBool False
cafRefs p (App f a) 	     = cafRefs p f `fastOr` cafRefs p a
cafRefs p (Lam x e) 	     = cafRefs p e
cafRefs p (Let b e) 	     = cafRefss p (rhssOfBind b) `fastOr` cafRefs p e
cafRefs p (Case e bndr alts) = cafRefs p e `fastOr` cafRefss p (rhssOfAlts alts)
cafRefs p (Note n e) 	     = cafRefs p e
cafRefs p (Type t) 	     = fastBool False

cafRefss p [] 	  = fastBool False
cafRefss p (e:es) = cafRefs p e `fastOr` cafRefss p es


isCAF :: CoreExpr -> Bool
-- Only called for the RHS of top-level lets
isCAF e = not (rhsIsNonUpd e)
  {- ToDo: check type for onceness, i.e. non-updatable thunks? -}

rhsIsNonUpd :: CoreExpr -> Bool
  -- True => Value-lambda, constructor, PAP
  -- This is a bit like CoreUtils.exprIsValue, with the following differences:
  -- 	a) scc "foo" (\x -> ...) is updatable (so we catch the right SCC)
  --
  --    b) (C x xs), where C is a contructors is updatable if the application is
  --	   dynamic: see isDynConApp
  -- 
  --    c) don't look through unfolding of f in (f x).  I'm suspicious of this one

rhsIsNonUpd (Lam b e)          = isId b || rhsIsNonUpd e
rhsIsNonUpd (Note (SCC _) e)   = False
rhsIsNonUpd (Note _ e)         = rhsIsNonUpd e
rhsIsNonUpd other_expr
  = go other_expr 0 []
  where
    go (Var f) n_args args = idAppIsNonUpd f n_args args
	
    go (App f a) n_args args
	| isTypeArg a = go f n_args args
	| otherwise   = go f (n_args + 1) (a:args)

    go (Note (SCC _) f) n_args args = False
    go (Note _ f) n_args args       = go f n_args args

    go other n_args args = False

idAppIsNonUpd :: Id -> Int -> [CoreExpr] -> Bool
idAppIsNonUpd id n_val_args args
  = case idFlavour id of
	DataConId con | not (isDynConApp con args) -> True
	other -> n_val_args < idArity id

isDynConApp :: DataCon -> [CoreExpr] -> Bool
isDynConApp con args = isDllName (dataConName con) || any isDynArg args
-- Top-level constructor applications can usually be allocated 
-- statically, but they can't if 
-- 	a) the constructor, or any of the arguments, come from another DLL
--	b) any of the arguments are LitLits
-- (because we can't refer to static labels in other DLLs).
-- If this happens we simply make the RHS into an updatable thunk, 
-- and 'exectute' it rather than allocating it statically.
-- All this should match the decision in (see CoreToStg.coreToStgRhs)


isDynArg :: CoreExpr -> Bool
isDynArg (Var v)    = isDllName (idName v)
isDynArg (Note _ e) = isDynArg e
isDynArg (Lit lit)  = isLitLitLit lit
isDynArg (App e _)  = isDynArg e	-- must be a type app
isDynArg (Lam _ e)  = isDynArg e	-- must be a type lam
\end{code}
