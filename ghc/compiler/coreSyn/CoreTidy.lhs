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

import CmdLineOpts	( DynFlags, DynFlag(..), opt_OmitInterfacePragmas, dopt )
import CoreSyn
import CoreUnfold	( noUnfolding, mkTopUnfolding, okToUnfoldInHiFile )
import CoreFVs		( ruleSomeFreeVars, exprSomeFreeVars )
import CoreLint		( showPass, endPass )
import VarEnv
import VarSet
import Var		( Id, Var )
import Id		( idType, idInfo, idName, isExportedId,
			  mkVanillaId, mkId, isLocalId, omitIfaceSigForId,
			  setIdStrictness, setIdDemandInfo,
			) 
import IdInfo		( mkIdInfo,
			  IdFlavour(..), flavourInfo, ppFlavourInfo,
			  specInfo, setSpecInfo, 
			  cprInfo, setCprInfo,
			  inlinePragInfo, setInlinePragInfo, isNeverInlinePrag,
			  strictnessInfo, setStrictnessInfo, isBottomingStrictness,
			  unfoldingInfo, setUnfoldingInfo, 
			  demandInfo, 
			  occInfo, isLoopBreaker,
			  workerInfo, setWorkerInfo, WorkerInfo(..)
			)
import Name		( getOccName, nameOccName, globaliseName, setNameOcc, 
		  	  localiseName, mkLocalName, isGlobalName
			)
import OccName		( TidyOccEnv, initTidyOccEnv, tidyOccName )
import Type		( tidyTopType, tidyType, tidyTyVar )
import Module		( Module, moduleName )
import HscTypes		( PersistentCompilerState( pcs_PRS ), PersistentRenamerState( prsOrig ),
			  OrigNameEnv( origNames ), OrigNameNameEnv
			)
import Unique		( Uniquable(..) )
import FiniteMap	( lookupFM, addToFM )
import Maybes		( maybeToBool, orElse )
import ErrUtils		( showPass )
import SrcLoc		( noSrcLoc )
import UniqFM		( mapUFM )
import Outputable
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
  
  - Give the Id its final IdInfo; in ptic, 
	* Its flavour becomes ConstantId, reflecting the fact that
	  from now on we regard it as a constant, not local, Id
  	* its unfolding, if it should have one
		
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

	; let ((orig_env', occ_env, subst_env), binds_out) 
		  = mapAccumL (tidyTopBind mod ext_ids) init_tidy_env binds_in

	; let orphans_out = tidyIdRules (occ_env,subst_env) orphans_in

	; let pcs' = pcs { pcs_PRS = prs { prsOrig = orig { origNames = orig_env' }}}

	; endPass dflags "Tidy Core" (dopt Opt_D_dump_simpl dflags || 
				      dopt Opt_D_verbose_core2core dflags)
	          binds_out

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
    prs	 	  = pcs_PRS pcs
    orig	  = prsOrig prs
    orig_env 	  = origNames orig

    init_tidy_env = (orig_env, initTidyOccEnv avoids, emptyVarEnv)
    avoids	  = [getOccName bndr | bndr <- bindersOfBinds binds_in,
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
  = foldr find init_needed binds
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
type TopTidyEnv = (OrigNameNameEnv, TidyOccEnv, VarEnv Var)

-- TopTidyEnv: when tidying we need to know
--   * orig_env: Any pre-ordained Names.  These may have arisen because the
--	  renamer read in an interface file mentioning M.$wf, say,
--	  and assigned it unique r77.  If, on this compilation, we've
--	  invented an Id whose name is $wf (but with a different unique)
--	  we want to rename it to have unique r77, so that we can do easy
--	  comparisons with stuff from the interface file

--   * occ_env: The TidyOccEnv, which tells us which local occurrences are 'used'

--   * subst_env: A Var->Var mapping that substitutes the new Var for the old
\end{code}


\begin{code}
tidyTopBind :: Module
	    -> IdEnv Bool	-- Domain = Ids that should be exernal
				-- True <=> their unfolding is external too
	    -> TopTidyEnv -> CoreBind
	    -> (TopTidyEnv, CoreBind)

tidyTopBind mod ext_ids env (NonRec bndr rhs)
  = (env', NonRec bndr' rhs')
  where
    rhs'	  = tidyTopRhs env rhs
    (env', bndr') = tidyTopBinder mod ext_ids env rhs' env bndr

tidyTopBind mod ext_ids env (Rec prs)
  = (final_env, Rec prs')
  where
    (final_env, prs')     = mapAccumL do_one env prs
    do_one env (bndr,rhs) = (env', (bndr', rhs'))
			  where
			    rhs'	  = tidyTopRhs final_env rhs
			    (env', bndr') = tidyTopBinder mod ext_ids final_env
							  rhs env bndr

tidyTopRhs :: TopTidyEnv -> CoreExpr -> CoreExpr
	-- Just an impedence matcher
tidyTopRhs (_, occ_env, subst_env) rhs = tidyExpr (occ_env, subst_env) rhs

tidyTopBinder :: Module -> IdEnv Bool
	      -> TopTidyEnv -> CoreExpr
	      -> TopTidyEnv -> Id -> (TopTidyEnv, Id)
tidyTopBinder mod ext_ids env_idinfo rhs env@(orig_env, occ_env, subst_env) id
  | omitIfaceSigForId id	-- Don't mess with constructors, 
  = (env, id)			-- record selectors, and the like

  | otherwise
	-- This function is the heart of Step 2
	-- The second env is the one to use for the IdInfo
	-- It's necessary because when we are dealing with a recursive
	-- group, a variable late in the group might be mentioned
	-- in the IdInfo of one early in the group

	-- The rhs is already tidied
	
  = ((orig_env', occ_env', subst_env'), id')
  where
    (orig_env', occ_env', name') = tidyTopName mod orig_env occ_env 
					       is_external
					       (idName id)
    ty'	       = tidyTopType (idType id)
    idinfo'    = tidyIdInfo env_idinfo is_external unfold_info id
    id'	       = mkId name' ty' idinfo'
    subst_env' = extendVarEnv subst_env id id'

    maybe_external = lookupVarEnv ext_ids id
    is_external    = maybeToBool maybe_external

    -- Expose an unfolding if ext_ids tells us to
    show_unfold = maybe_external `orElse` False
    unfold_info | show_unfold = mkTopUnfolding rhs
		| otherwise   = noUnfolding

tidyIdInfo (_, occ_env, subst_env) is_external unfold_info id
  | opt_OmitInterfacePragmas || not is_external
	-- No IdInfo if the Id isn't external, or if we don't have -O
  = mkIdInfo new_flavour
	`setStrictnessInfo` strictnessInfo core_idinfo
	-- Keep strictness info; it's used by the code generator

  | otherwise
  = mkIdInfo new_flavour
	`setCprInfo`	    cprInfo core_idinfo
	`setStrictnessInfo` strictnessInfo core_idinfo
	`setInlinePragInfo` inlinePragInfo core_idinfo
	`setUnfoldingInfo`  unfold_info
	`setWorkerInfo`	    tidyWorker tidy_env (workerInfo core_idinfo)
	`setSpecInfo`	    tidyRules  tidy_env (specInfo core_idinfo)
  where
    tidy_env    = (occ_env, subst_env)
    core_idinfo = idInfo id

	-- A DFunId must stay a DFunId, so that we can gather the
	-- DFunIds up later.  Other local things become ConstantIds.
    new_flavour = case flavourInfo core_idinfo of
		    VanillaId  -> ConstantId
		    ExportedId -> ConstantId
		    DictFunId  -> DictFunId
		    flavour    -> pprTrace "tidyIdInfo" (ppr id <+> ppFlavourInfo flavour)
				  flavour

tidyTopName mod orig_env occ_env external name
  | global && internal = (orig_env, occ_env,  localiseName name)
  | local  && internal = (orig_env, occ_env', setNameOcc name occ')
  | global && external = (orig_env, occ_env,  name)
  | local  && external = globalise
  where
	-- If we want to globalise a currently-local name, check
	-- whether we have already assigned a unique for it.
	-- If so, use it; if not, extend the table
    globalise = case lookupFM orig_env key of
		  Just orig -> (orig_env,			  occ_env', orig)
		  Nothing   -> (addToFM orig_env key global_name, occ_env', global_name)

    (occ_env', occ') = tidyOccName occ_env (nameOccName name)
    key		     = (moduleName mod, occ')
    global_name      = globaliseName (setNameOcc name occ') mod
    global	     = isGlobalName name
    local	     = not global
    internal	     = not external

tidyIdRules :: TidyEnv -> [IdCoreRule] -> [IdCoreRule]
tidyIdRules env rules
  = [ (tidyVarOcc env fn, tidyRule env rule) | (fn,rule) <- rules  ]


tidyWorker tidy_env (HasWorker work_id wrap_arity) 
  = HasWorker (tidyVarOcc tidy_env work_id) wrap_arity
tidyWorker tidy_env NoWorker
  = NoWorker

tidyRules :: TidyEnv -> CoreRules -> CoreRules
tidyRules env (Rules rules fvs) 
  = Rules (map (tidyRule env) rules)
	  (foldVarSet tidy_set_elem emptyVarSet fvs)
  where
    tidy_set_elem var new_set = extendVarSet new_set (tidyVarOcc env var)

tidyRule :: TidyEnv -> CoreRule -> CoreRule
tidyRule env rule@(BuiltinRule _) = rule
tidyRule env (Rule name vars tpl_args rhs)
  = (Rule name vars' (map (tidyExpr env') tpl_args) (tidyExpr env' rhs))
  where
    (env', vars') = tidyBndrs env vars
\end{code}


%************************************************************************
%*									*
\subsection{Step 2: inner tidying
%*									*
%************************************************************************

\begin{code}
tidyBind :: TidyEnv
	 -> CoreBind
	 -> (TidyEnv, CoreBind)
tidyBind env (NonRec bndr rhs)
  = let
	(env', bndr') = tidyBndr env bndr
	rhs'	      = tidyExpr env' rhs
	-- We use env' when tidying the RHS even though it's not
	-- strictly necessary; it makes the tidied code pretty 
	-- hard to read if we don't!
    in
    (env', NonRec bndr' rhs')

tidyBind env (Rec prs)
  = (final_env, Rec prs')
  where
    (final_env, prs')     = mapAccumL do_one env prs
    do_one env (bndr,rhs) = (env', (bndr', rhs'))
			  where
			    (env', bndr') = tidyBndr env bndr
			    rhs'          = tidyExpr final_env rhs

tidyExpr env (Type ty)	     = Type (tidyType env ty)
tidyExpr env (Lit lit)	     = Lit lit
tidyExpr env (App f a)       = App (tidyExpr env f) (tidyExpr env a)
tidyExpr env (Note n e)      = Note (tidyNote env n) (tidyExpr env e)

tidyExpr env (Let b e)       = Let b' (tidyExpr env' e)
			     where
			       (env', b') = tidyBind env b

tidyExpr env (Case e b alts) = Case (tidyExpr env e) b' (map (tidyAlt env') alts)
			     where
			       (env', b') = tidyBndr env b

tidyExpr env (Var v)         = Var (tidyVarOcc env v)

tidyExpr env (Lam b e)	     = Lam b' (tidyExpr env' e)
			     where
			       (env', b') = tidyBndr env b

tidyAlt env (con, vs, rhs)   = (con, vs', tidyExpr env' rhs)
			     where
			       (env', vs') = tidyBndrs env vs

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

tidyBndr :: TidyEnv -> Var -> (TidyEnv, Var)
tidyBndr env var | isTyVar var = tidyTyVar env var
		 | otherwise   = tidyId    env var

tidyBndrs :: TidyEnv -> [Var] -> (TidyEnv, [Var])
tidyBndrs env vars = mapAccumL tidyBndr env vars

tidyId :: TidyEnv -> Id -> (TidyEnv, Id)
tidyId env@(tidy_env, var_env) id
  = 	-- Non-top-level variables
    let 
	-- Give the Id a fresh print-name, *and* rename its type
	-- The SrcLoc isn't important now, though we could extract it from the Id
	name'        	  = mkLocalName (getUnique id) occ' noSrcLoc
	(tidy_env', occ') = tidyOccName tidy_env (getOccName id)
        ty'          	  = tidyType env (idType id)
	idinfo		  = idInfo id
	id'          	  = mkVanillaId name' ty'
			    `setIdStrictness` strictnessInfo idinfo
			    `setIdDemandInfo` demandInfo idinfo
			-- NB: This throws away the IdInfo of the Id, which we
			-- no longer need.  That means we don't need to
			-- run over it with env, nor renumber it.
			--
			-- The exception is strictness and demand info, which 
			-- is used to decide whether to use let or case for
			-- function arguments and let bindings

	var_env'	  = extendVarEnv var_env id id'
    in
    ((tidy_env', var_env'), id')
\end{code}
