%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
%************************************************************************
%*									*
\section[OccurAnal]{Occurrence analysis pass}
%*									*
%************************************************************************

The occurrence analyser re-typechecks a core expression, returning a new
core expression with (hopefully) improved usage information.

\begin{code}
#include "HsVersions.h"

module OccurAnal (
	occurAnalyseBinds, occurAnalyseExpr, occurAnalyseGlobalExpr
    ) where

IMP_Ubiq(){-uitous-}

import BinderInfo
import CmdLineOpts	( opt_D_dump_occur_anal, SimplifierSwitch(..) )
import CoreSyn
import Digraph		( stronglyConnComp )
import Id		( idWantsToBeINLINEd, isConstMethodId,
			  emptyIdSet, unionIdSets, mkIdSet,
			  unitIdSet, elementOfIdSet,
			  addOneToIdSet, IdSet(..),
			  nullIdEnv, unitIdEnv, combineIdEnvs,
			  delOneFromIdEnv, delManyFromIdEnv,
			  mapIdEnv, lookupIdEnv, IdEnv(..),
			  GenId{-instance Eq-}
			)
import Maybes		( maybeToBool )
import Name		( isExported )
import Outputable	( Outputable(..){-instance * (,) -} )
import PprCore
import PprStyle		( PprStyle(..) )
import PprType		( GenType{-instance Outputable-}, GenTyVar{-ditto-} )
import Pretty		( ppAboves )
import TyVar		( GenTyVar{-instance Eq-} )
import Unique		( Unique{-instance Eq-} )
import Util		( assoc, zipEqual, pprTrace, panic )

isSpecPragmaId_maybe = panic "OccurAnal.isSpecPragmaId_maybe (ToDo)"
\end{code}


%************************************************************************
%*									*
\subsection[OccurAnal-types]{Data types}
%*									*
%************************************************************************

\begin{code}
data OccEnv =
  OccEnv
    Bool	-- Keep-unused-bindings flag
		-- False <=> OK to chuck away binding
		-- 	     and ignore occurrences within it
    Bool	-- Keep-spec-pragma-ids flag
		-- False <=> OK to chuck away spec pragma bindings
		-- 	     and ignore occurrences within it
    Bool	-- Keep-conjurable flag
		-- False <=> OK to throw away *dead*
		-- "conjurable" Ids; at the moment, that
		-- *only* means constant methods, which
		-- are top-level.  A use of a "conjurable"
		-- Id may appear out of thin air -- e.g.,
		-- specialiser conjuring up refs to const methods.
   Bool		-- IgnoreINLINEPragma flag
		-- False <=> OK to use INLINEPragma information
		-- True  <=> ignore INLINEPragma information
   IdSet	-- Candidates

addNewCands :: OccEnv -> [Id] -> OccEnv
addNewCands (OccEnv kd ks kc ip cands) ids
  = OccEnv kd ks kc ip (cands `unionIdSets` mkIdSet ids)

addNewCand :: OccEnv -> Id -> OccEnv
addNewCand (OccEnv ks kd kc ip cands) id
  = OccEnv kd ks kc ip (addOneToIdSet cands id)

isCandidate :: OccEnv -> Id -> Bool
isCandidate (OccEnv _ _ _ _ cands) id = id `elementOfIdSet` cands

ignoreINLINEPragma :: OccEnv -> Bool
ignoreINLINEPragma (OccEnv _ _ _ ip _) = ip

keepUnusedBinding :: OccEnv -> Id -> Bool
keepUnusedBinding (OccEnv keep_dead keep_spec keep_conjurable _ _) binder
  = keep_dead || (keep_spec && maybeToBool (isSpecPragmaId_maybe binder))

keepBecauseConjurable :: OccEnv -> Id -> Bool
keepBecauseConjurable (OccEnv _ _ keep_conjurable _ _) binder
  = keep_conjurable && isConstMethodId binder

type UsageDetails = IdEnv BinderInfo	-- A finite map from ids to their usage

combineUsageDetails, combineAltsUsageDetails
	:: UsageDetails -> UsageDetails -> UsageDetails

combineUsageDetails usage1 usage2
  = combineIdEnvs addBinderInfo usage1 usage2

combineAltsUsageDetails usage1 usage2
  = combineIdEnvs orBinderInfo usage1 usage2

addOneOcc :: UsageDetails -> Id -> BinderInfo -> UsageDetails
addOneOcc usage id info
  = combineIdEnvs addBinderInfo usage (unitIdEnv id info)
	-- ToDo: make this more efficient

emptyDetails = (nullIdEnv :: UsageDetails)

unitDetails id info = (unitIdEnv id info :: UsageDetails)

tagBinders :: UsageDetails	    -- Of scope
	   -> [Id]		    -- Binders
	   -> (UsageDetails, 	    -- Details with binders removed
	      [(Id,BinderInfo)])    -- Tagged binders

tagBinders usage binders
  = (usage `delManyFromIdEnv` binders,
     [ (binder, usage_of usage binder) | binder <- binders ]
    )

tagBinder :: UsageDetails	    -- Of scope
	  -> Id			    -- Binders
	  -> (UsageDetails, 	    -- Details with binders removed
	      (Id,BinderInfo))	    -- Tagged binders

tagBinder usage binder
  = (usage `delOneFromIdEnv` binder,
     (binder, usage_of usage binder)
    )

usage_of usage binder
  | isExported binder = ManyOcc	0 -- Exported things count as many
  | otherwise
  = case (lookupIdEnv usage binder) of
      Nothing   -> DeadCode
      Just info -> info

isNeeded env usage binder
  = case (usage_of usage binder) of
      DeadCode  -> keepUnusedBinding env binder	-- Maybe keep it anyway
      other     -> True
\end{code}


%************************************************************************
%*									*
\subsection[OccurAnal-main]{Counting occurrences: main function}
%*									*
%************************************************************************

Here's the externally-callable interface:

\begin{code}
occurAnalyseBinds
	:: [CoreBinding]		-- input
	-> (SimplifierSwitch -> Bool)
	-> [SimplifiableCoreBinding]	-- output

occurAnalyseBinds binds simplifier_sw_chkr
  | opt_D_dump_occur_anal = pprTrace "OccurAnal:"
				     (ppAboves (map (ppr PprDebug) binds'))
				     binds'
  | otherwise		  = binds'
  where
    (_, binds') = do initial_env binds

    initial_env = OccEnv (simplifier_sw_chkr KeepUnusedBindings)
			 (simplifier_sw_chkr KeepSpecPragmaIds)
			 (not (simplifier_sw_chkr SimplMayDeleteConjurableIds))
			 (simplifier_sw_chkr IgnoreINLINEPragma)
			 emptyIdSet

    do env [] = (emptyDetails, [])
    do env (bind:binds)
      = (final_usage, new_binds ++ the_rest)
      where
	new_env			 = env `addNewCands` (bindersOf bind)
	(binds_usage, the_rest)  = do new_env binds
	(final_usage, new_binds) = occAnalBind env bind binds_usage
\end{code}

\begin{code}
occurAnalyseExpr :: IdSet 		-- Set of interesting free vars
		 -> CoreExpr
		 -> (IdEnv BinderInfo,	-- Occ info for interesting free vars
		     SimplifiableCoreExpr)

occurAnalyseExpr candidates expr
  = occAnal initial_env expr
  where
    initial_env = OccEnv False {- Drop unused bindings -}
			 False {- Drop SpecPragmaId bindings -}
			 True  {- Keep conjurable Ids -}
			 False {- Do not ignore INLINE Pragma -}
			 candidates

occurAnalyseGlobalExpr :: CoreExpr -> SimplifiableCoreExpr
occurAnalyseGlobalExpr expr
  = 	-- Top level expr, so no interesting free vars, and
	-- discard occurence info returned
    snd (occurAnalyseExpr emptyIdSet expr)
\end{code}

%************************************************************************
%*									*
\subsection[OccurAnal-main]{Counting occurrences: main function}
%*									*
%************************************************************************

Bindings
~~~~~~~~

\begin{code}
occAnalBind :: OccEnv
	    -> CoreBinding
	    -> UsageDetails		-- Usage details of scope
	    -> (UsageDetails,		-- Of the whole let(rec)
		[SimplifiableCoreBinding])

occAnalBind env (NonRec binder rhs) body_usage
  | isNeeded env body_usage binder		-- It's mentioned in body
  = (final_body_usage `combineUsageDetails` rhs_usage,
     [NonRec tagged_binder rhs'])

  | otherwise
  = (body_usage, [])

  where
    (rhs_usage, rhs')		      = occAnalRhs env binder rhs
    (final_body_usage, tagged_binder) = tagBinder body_usage binder
\end{code}

Dropping dead code for recursive bindings is done in a very simple way:

	the entire set of bindings is dropped if none of its binders are
	mentioned in its body; otherwise none are.

This seems to miss an obvious improvement.
@
	letrec  f = ...g...
		g = ...f...
	in
	...g...

===>

	letrec f = ...g...
	       g = ...(...g...)...
	in
	...g...
@

Now @f@ is unused. But dependency analysis will sort this out into a
@letrec@ for @g@ and a @let@ for @f@, and then @f@ will get dropped.
It isn't easy to do a perfect job in one blow.  Consider

@
	letrec f = ...g...
	       g = ...h...
	       h = ...k...
	       k = ...m...
	       m = ...m...
	in
	...m...
@


\begin{code}
occAnalBind env (Rec pairs) body_usage
  = foldr do_final_bind (body_usage, []) sccs
  where

    (binders, rhss) = unzip pairs
    new_env	    = env `addNewCands` binders

    analysed_pairs :: [(Id, (UsageDetails, SimplifiableCoreExpr))]
    analysed_pairs  = [(id, occAnalRhs new_env id rhs) | (id,rhs) <- pairs]

    lookup :: Id -> (UsageDetails, SimplifiableCoreExpr)
    lookup id =  assoc "occAnalBind:lookup" analysed_pairs id


    ---- stuff for dependency analysis of binds -------------------------------

    edges :: [(Id,Id)]		-- (a,b) means a mentions b
    edges = concat [ edges_from binder rhs_usage
		   | (binder, (rhs_usage, _)) <- analysed_pairs]

    edges_from :: Id -> UsageDetails -> [(Id,Id)]
    edges_from id its_rhs_usage
      = [(id,mentioned) | mentioned <- binders,
		 	  maybeToBool (lookupIdEnv its_rhs_usage mentioned)
	]

    sccs :: [[Id]]
    sccs = case binders of
		[_]   -> [binders]	-- Singleton; no need to analyse
		other -> stronglyConnComp (==) edges binders

    ---- stuff to "re-constitute" bindings from dependency-analysis info ------

    do_final_bind sCC@[binder] (body_usage, binds_so_far)
      | isNeeded env body_usage binder
      = (combined_usage, new_bind:binds_so_far)

      | otherwise		-- Dead
      = (body_usage, binds_so_far)
      where
	total_usage       	        = combineUsageDetails body_usage rhs_usage
	(rhs_usage, rhs') 	        = lookup binder
	(combined_usage, tagged_binder) = tagBinder total_usage binder

	new_bind
	  | mentions_itself binder rhs_usage = Rec [(tagged_binder,rhs')]
	  | otherwise		             = NonRec tagged_binder rhs'
	  where
	    mentions_itself binder usage
	      = maybeToBool (lookupIdEnv usage binder)

    do_final_bind sCC (body_usage, binds_so_far)
      | any (isNeeded env body_usage) sCC
      = (combined_usage, new_bind:binds_so_far)

      | otherwise		-- Dead
      = (body_usage, binds_so_far)
      where
	(rhs_usages, rhss')	         = unzip (map lookup sCC)
	total_usage		         = foldr combineUsageDetails body_usage rhs_usages
	(combined_usage, tagged_binders) = tagBinders total_usage sCC

	new_bind = Rec (zipEqual "occAnalBind" tagged_binders rhss')
\end{code}

@occAnalRhs@ deals with the question of bindings where the Id is marked
by an INLINE pragma.  For these we record that anything which occurs
in its RHS occurs many times.  This pessimistically assumes that ths
inlined binder also occurs many times in its scope, but if it doesn't
we'll catch it next time round.  At worst this costs an extra simplifier pass.
ToDo: try using the occurrence info for the inline'd binder.

\begin{code}
occAnalRhs :: OccEnv
	   -> Id	-- Binder
	   -> CoreExpr	-- Rhs
	   -> (UsageDetails, SimplifiableCoreExpr)

occAnalRhs env id rhs
  | idWantsToBeINLINEd id && not (ignoreINLINEPragma env)
  = (mapIdEnv markMany rhs_usage, rhs')

  | otherwise
  = (rhs_usage, rhs')

  where
    (rhs_usage, rhs') = occAnal env rhs
\end{code}

Expressions
~~~~~~~~~~~
\begin{code}
occAnal :: OccEnv
	-> CoreExpr
	-> (UsageDetails,	-- Gives info only about the "interesting" Ids
	    SimplifiableCoreExpr)

occAnal env (Var v)
  | isCandidate env v
  = (unitIdEnv v (funOccurrence 0), Var v)

  | otherwise
  = (emptyDetails, Var v)

occAnal env (Lit lit)	   = (emptyDetails, Lit lit)
occAnal env (Con con args) = (occAnalArgs env args, Con con args)
occAnal env (Prim op args) = (occAnalArgs env args, Prim op args)

occAnal env (SCC cc body)
  = (mapIdEnv markInsideSCC usage, SCC cc body')
  where
    (usage, body') = occAnal env body

occAnal env (Coerce c ty body)
  = (usage, Coerce c ty body')
  where
    (usage, body') = occAnal env body

occAnal env (App fun arg)
  = (fun_usage `combineUsageDetails` arg_usage, App fun' arg)
  where
    (fun_usage, fun') = occAnal    env fun
    arg_usage	      = occAnalArg env arg

occAnal env (Lam (ValBinder binder) body)
  = (mapIdEnv markDangerousToDup final_usage,
     Lam (ValBinder tagged_binder) body')
  where
    (body_usage, body') 	 = occAnal (env `addNewCand` binder) body
    (final_usage, tagged_binder) = tagBinder body_usage binder

-- ANDY: WE MUST THINK ABOUT THIS! (ToDo)
occAnal env (Lam (TyBinder tyvar) body)
  = (mapIdEnv markDangerousToDup body_usage,
     Lam (TyBinder tyvar) body')
  where
    (body_usage, body') = occAnal env body

occAnal env (Lam (UsageBinder _) _) = panic "OccurAnal.occAnal Lam UsageBinder"

occAnal env (Case scrut alts)
  = (scrut_usage `combineUsageDetails` alts_usage,
     Case scrut' alts')
  where
    (scrut_usage, scrut') = occAnal env scrut
    (alts_usage, alts')   = occAnalAlts env alts

occAnal env (Let bind body)
  = (final_usage, foldr Let body' new_binds) -- mkCoLet* wants Core... (sigh)
  where
    new_env		     = env `addNewCands` (bindersOf bind)
    (body_usage, body')      = occAnal new_env body
    (final_usage, new_binds) = occAnalBind env bind body_usage
\end{code}

Case alternatives
~~~~~~~~~~~~~~~~~
\begin{code}
occAnalAlts env (AlgAlts alts deflt)
  = (foldr combineAltsUsageDetails deflt_usage alts_usage,
	-- Note: combine*Alts*UsageDetails...
     AlgAlts alts' deflt')
  where
    (alts_usage,  alts')  = unzip (map do_alt alts)
    (deflt_usage, deflt') = occAnalDeflt env deflt

    do_alt (con, args, rhs)
      = (final_usage, (con, tagged_args, rhs'))
      where
	new_env		   = env `addNewCands` args
	(rhs_usage, rhs')	   = occAnal new_env rhs
	(final_usage, tagged_args) = tagBinders rhs_usage args

occAnalAlts env (PrimAlts alts deflt)
  = (foldr combineAltsUsageDetails deflt_usage alts_usage,
	-- Note: combine*Alts*UsageDetails...
     PrimAlts alts' deflt')
  where
    (alts_usage, alts')   = unzip (map do_alt alts)
    (deflt_usage, deflt') = occAnalDeflt env deflt

    do_alt (lit, rhs)
      = (rhs_usage, (lit, rhs'))
      where
	(rhs_usage, rhs') = occAnal env rhs

occAnalDeflt env NoDefault = (emptyDetails, NoDefault)

occAnalDeflt env (BindDefault binder rhs)
  = (final_usage, BindDefault tagged_binder rhs')
  where
    new_env			 = env `addNewCand` binder
    (rhs_usage, rhs')		 = occAnal new_env rhs
    (final_usage, tagged_binder) = tagBinder rhs_usage binder
\end{code}


Atoms
~~~~~
\begin{code}
occAnalArgs :: OccEnv -> [CoreArg] -> UsageDetails

occAnalArgs env atoms
  = foldr do_one_atom emptyDetails atoms
  where
    do_one_atom (VarArg v) usage
	| isCandidate env v = addOneOcc usage v (argOccurrence 0)
	| otherwise	    = usage
    do_one_atom other_arg  usage = usage


occAnalArg  :: OccEnv -> CoreArg -> UsageDetails

occAnalArg env (VarArg v)
  | isCandidate env v = unitDetails v (argOccurrence 0)
  | otherwise         = emptyDetails
occAnalArg _   _      = emptyDetails
\end{code}
