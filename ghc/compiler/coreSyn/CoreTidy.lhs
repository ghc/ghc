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

import CmdLineOpts	( opt_D_dump_simpl, opt_D_verbose_core2core, opt_UsageSPOn )
import CoreSyn
import CoreUnfold	( noUnfolding )
import CoreLint		( beginPass, endPass )
import Rules		( ProtoCoreRule(..) )
import UsageSPInf       ( doUsageSPInf )
import VarEnv
import VarSet
import Var		( Id, Var )
import Id		( idType, idInfo, idName, 
			  mkVanillaId, mkId, exportWithOrigOccName,
			  idStrictness, setIdStrictness,
			  idDemandInfo, setIdDemandInfo,
			) 
import IdInfo		( specInfo, setSpecInfo, 
			  inlinePragInfo, setInlinePragInfo, InlinePragInfo(..),
			  setUnfoldingInfo, setDemandInfo,
			  workerInfo, setWorkerInfo, WorkerInfo(..)
			)
import Demand		( wwLazy )
import Name		( getOccName, tidyTopName, mkLocalName, isLocallyDefined )
import OccName		( initTidyOccEnv, tidyOccName )
import Type		( tidyTopType, tidyType, tidyTypes, tidyTyVar, tidyTyVars )
import Module		( Module )
import UniqSupply	( UniqSupply )
import Unique		( Uniquable(..) )
import SrcLoc		( noSrcLoc )
import Util		( mapAccumL )
import Outputable
\end{code}



%************************************************************************
%*									*
\subsection{Tidying core}
%*									*
%************************************************************************

Several tasks are done by @tidyCorePgm@

1.  Make certain top-level bindings into Globals. The point is that 
    Global things get externally-visible labels at code generation
    time


2. Give all binders a nice print-name.  Their uniques aren't changed;
   rather we give them lexically unique occ-names, so that we can
   safely print the OccNae only in the interface file.  [Bad idea to
   change the uniques, because the code generator makes global labels
   from the uniques for local thunks etc.]


3. If @opt_UsageSPOn@ then compute usage information (which is
   needed by Core2Stg).  ** NOTE _scc_ HERE **

\begin{code}
tidyCorePgm :: UniqSupply -> Module -> [CoreBind] -> [ProtoCoreRule]
	    -> IO ([CoreBind], [ProtoCoreRule])
tidyCorePgm us module_name binds_in rules
  = do
	beginPass "Tidy Core"

	let (tidy_env1, binds_tidy) = mapAccumL (tidyBind (Just module_name)) init_tidy_env binds_in
	    rules_out	  	    = tidyProtoRules tidy_env1 rules

        binds_out <- if opt_UsageSPOn
                     then _scc_ "CoreUsageSPInf" doUsageSPInf us binds_tidy
                     else return binds_tidy

	endPass "Tidy Core" (opt_D_dump_simpl || opt_D_verbose_core2core) binds_out
	return (binds_out, rules_out)
  where
	-- We also make sure to avoid any exported binders.  Consider
	--	f{-u1-} = 1	-- Local decl
	--	...
	--	f{-u2-} = 2	-- Exported decl
	--
	-- The second exported decl must 'get' the name 'f', so we
	-- have to put 'f' in the avoids list before we get to the first
	-- decl.  tidyTopId then does a no-op on exported binders.
    init_tidy_env = (initTidyOccEnv avoids, emptyVarEnv)
    avoids	  = [getOccName bndr | bndr <- bindersOfBinds binds_in,
				       exportWithOrigOccName bndr]

tidyBind :: Maybe Module		-- (Just m) for top level, Nothing for nested
	 -> TidyEnv
	 -> CoreBind
	 -> (TidyEnv, CoreBind)
tidyBind maybe_mod env (NonRec bndr rhs)
  = let
	(env', bndr') = tidy_bndr maybe_mod env' env bndr
	rhs'	      = tidyExpr env' rhs
	-- We use env' when tidying the RHS even though it's not
	-- strictly necessary; it makes the code pretty hard to read
	-- if we don't!
    in
    (env', NonRec bndr' rhs')

tidyBind maybe_mod env (Rec pairs)
  = let
	-- We use env' when tidying the rhss
	-- When tidying the binder itself we may tidy it's
	-- specialisations; if any of these mention other binders
	-- in the group we should really feed env' to them too;
	-- but that seems (a) unlikely and (b) a bit tiresome.
	-- So I left it out for now

	(bndrs, rhss)  = unzip pairs
	(env', bndrs') = mapAccumL (tidy_bndr maybe_mod env') env bndrs
	rhss'	       = map (tidyExpr env') rhss
  in
  (env', Rec (zip bndrs' rhss'))

tidyExpr env (Type ty)	     = Type (tidyType env ty)
tidyExpr env (Lit lit)	     = Lit lit
tidyExpr env (App f a)       = App (tidyExpr env f) (tidyExpr env a)
tidyExpr env (Note n e)      = Note (tidyNote env n) (tidyExpr env e)

tidyExpr env (Let b e)       = Let b' (tidyExpr env' e)
			     where
			       (env', b') = tidyBind Nothing env b

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

tidyVarOcc (_, var_env) v = case lookupVarEnv var_env v of
				  Just v' -> v'
				  Nothing -> v
\end{code}

\begin{code}
tidy_bndr (Just mod) env_idinfo env var = tidyTopId mod env env_idinfo var
tidy_bndr Nothing    env_idinfo env var = tidyBndr      env            var
\end{code}



%************************************************************************
%*									*
\subsection{Tidying up a binder}
%*									*
%************************************************************************

\begin{code}
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
	id'          	  = mkVanillaId name' ty'
			    `setIdStrictness` idStrictness id
			    `setIdDemandInfo` idDemandInfo id
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

tidyTopId :: Module -> TidyEnv -> TidyEnv -> Id -> (TidyEnv, Id)
	-- The second env is the one to use for the IdInfo
	-- It's necessary because when we are dealing with a recursive
	-- group, a variable late in the group might be mentioned
	-- in the IdInfo of one early in the group
tidyTopId mod env@(tidy_env, var_env) env_idinfo id
  =	-- Top level variables
    let
	(tidy_env', name') | exportWithOrigOccName id = (tidy_env, idName id)
			   | otherwise	              = tidyTopName mod tidy_env (idName id)
	ty'	           = tidyTopType (idType id)
	idinfo'		   = tidyIdInfo env_idinfo (idInfo id)
	id'		   = mkId name' ty' idinfo'
	var_env'	   = extendVarEnv var_env id id'
    in
    ((tidy_env', var_env'), id')
\end{code}

\begin{code}
-- tidyIdInfo does these things:
--	a) tidy the specialisation info and worker info (if any)
--	b) zap the unfolding and demand info
-- The latter two are to avoid space leaks

tidyIdInfo env info
  = info5
  where
    rules = specInfo info

    info2 | isEmptyCoreRules rules = info 
	  | otherwise	           = info `setSpecInfo` tidyRules env rules
		
    info3 = info2 `setUnfoldingInfo` noUnfolding 
    info4 = info3 `setDemandInfo`    wwLazy		-- I don't understand why...

    info5 = case workerInfo info of
		NoWorker -> info4
		HasWorker w a  -> info4 `setWorkerInfo` HasWorker (tidyVarOcc env w) a

tidyProtoRules :: TidyEnv -> [ProtoCoreRule] -> [ProtoCoreRule]
tidyProtoRules env rules
  = [ ProtoCoreRule is_local (tidyVarOcc env fn) (tidyRule env rule)
    | ProtoCoreRule is_local fn rule <- rules
    ]

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
