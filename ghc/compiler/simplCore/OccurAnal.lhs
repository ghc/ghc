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
IMPORT_1_3(List(partition))

import BinderInfo
import CmdLineOpts	( opt_D_dump_occur_anal, SimplifierSwitch(..) )
import CoreSyn
import Digraph		( stronglyConnComp, stronglyConnCompR, SCC(..) )
import Id		( idWantsToBeINLINEd, addNoInlinePragma, nukeNoInlinePragma,
			  idType, idUnique, SYN_IE(Id),
			  emptyIdSet, unionIdSets, mkIdSet,
			  unitIdSet, elementOfIdSet,
			  addOneToIdSet, SYN_IE(IdSet),
			  nullIdEnv, unitIdEnv, combineIdEnvs,
			  delOneFromIdEnv, delManyFromIdEnv, isNullIdEnv, 
			  mapIdEnv, lookupIdEnv, SYN_IE(IdEnv), 
			  GenId{-instance Eq-}
			)
import Name		( isExported, isLocallyDefined )
import Type		( getFunTy_maybe, splitForAllTy )
import Maybes		( maybeToBool )
import Outputable	( PprStyle(..), Outputable(..){-instance * (,) -} )
import PprCore
import PprType		( GenType{-instance Outputable-}, GenTyVar{-ditto-} )
import Pretty		( Doc, vcat, ptext, nest, punctuate, comma, hcat, text )
import TyVar		( GenTyVar{-instance Eq-} )
import Unique		( Unique{-instance Eq-}, u2i )
import UniqFM		( keysUFM ) 
import Util		( assoc, zipEqual, zipWithEqual, Ord3(..)
			, pprTrace, panic 
#ifdef DEBUG
			, assertPanic
#endif
			)

isSpecPragmaId_maybe x = Nothing -- ToDo:!trace "OccurAnal.isSpecPragmaId_maybe"
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
    Bool	-- IgnoreINLINEPragma flag
		-- False <=> OK to use INLINEPragma information
		-- True  <=> ignore INLINEPragma information

    (Id -> IdSet -> Bool)	-- Tells whether an Id occurrence is interesting,
				-- given the set of in-scope variables

    IdSet	-- In-scope Ids


addNewCands :: OccEnv -> [Id] -> OccEnv
addNewCands (OccEnv kd ks kc ip ifun cands) ids
  = OccEnv kd ks kc ip ifun (cands `unionIdSets` mkIdSet ids)

addNewCand :: OccEnv -> Id -> OccEnv
addNewCand (OccEnv ks kd kc ip ifun cands) id
  = OccEnv kd ks kc ip ifun (addOneToIdSet cands id)

isCandidate :: OccEnv -> Id -> Bool
isCandidate (OccEnv _ _ _ _ ifun cands) id = ifun id cands

inlineMe :: OccEnv -> Id -> Bool
inlineMe env id
  = {-	See comments with simplIdWantsToBeINLINEd in SimplUtils.lhs 
	not ignore_inline_prag && 
    -}
    idWantsToBeINLINEd id

keepUnusedBinding :: OccEnv -> Id -> Bool
keepUnusedBinding (OccEnv keep_dead keep_spec keep_conjurable _ _ _) binder
  = keep_dead || (keep_spec && maybeToBool (isSpecPragmaId_maybe binder))

keepBecauseConjurable :: OccEnv -> Id -> Bool
keepBecauseConjurable (OccEnv _ _ keep_conjurable _ _ _) binder
  = False
    {- keep_conjurable && isConstMethodId binder -}

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

tagBinders usage binders =
 let
  usage' = usage `delManyFromIdEnv` binders
  uss    = [ (binder, usage_of usage binder) | binder <- binders ]
 in
 if isNullIdEnv usage' then
    (usage', uss)
 else
    (usage', uss)
{-
  = (usage `delManyFromIdEnv` binders,
     [ (binder, usage_of usage binder) | binder <- binders ]
    )
-}
tagBinder :: UsageDetails	    -- Of scope
	  -> Id			    -- Binders
	  -> (UsageDetails, 	    -- Details with binders removed
	      (Id,BinderInfo))	    -- Tagged binders

tagBinder usage binder =
 let
   usage'  = usage `delOneFromIdEnv` binder
   us      = usage_of usage binder 
   cont =
    if isNullIdEnv usage' then  -- bogus test to force evaluation.
       (usage', (binder, us))
    else
       (usage', (binder, us))
 in
 case us of { DeadCode -> cont; _ -> cont }

--   (binder, usage_of usage binder)


usage_of usage binder
  | isExported binder = ManyOcc 0 -- Visible-elsewhere things count as many
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
				     (vcat (map ppr_bind binds'))
				     binds'
  | otherwise		  = binds'
  where
    (_, binds') = doo initial_env binds

    initial_env = OccEnv (simplifier_sw_chkr KeepUnusedBindings)
			 (simplifier_sw_chkr KeepSpecPragmaIds)
			 (not (simplifier_sw_chkr SimplMayDeleteConjurableIds))
			 (simplifier_sw_chkr IgnoreINLINEPragma)
			 (\id in_scope -> isLocallyDefined id)	-- Anything local is interesting
			 emptyIdSet				-- Not actually used

    doo env [] = (emptyDetails, [])
    doo env (bind:binds)
      = (final_usage, new_binds ++ the_rest)
      where
	new_env			 = env `addNewCands` (bindersOf bind)
	(binds_usage, the_rest)  = doo new_env binds
	(final_usage, new_binds) = occAnalBind env bind binds_usage

	-- This really ought to be done properly by PprCore, but
	-- it isn't.  pprCoreBinding only works on Id binders, and
	-- the general case is complicated by the fact that it has to work
	-- for interface files too.  Sigh

ppr_bind bind@(NonRec binder expr)
  = ppr PprDebug bind

ppr_bind bind@(Rec binds)
  = vcat [ptext SLIT("Rec {"),
	      nest 2 (ppr PprDebug bind),
	      ptext SLIT("end Rec }")]
\end{code}

\begin{code}
occurAnalyseExpr :: (Id -> Bool)	-- Tells if a variable is interesting
		 -> CoreExpr
		 -> (IdEnv BinderInfo,	-- Occ info for interesting free vars
		     SimplifiableCoreExpr)

occurAnalyseExpr interesting expr
  = occAnal initial_env expr
  where
    initial_env = OccEnv False {- Drop unused bindings -}
			 False {- Drop SpecPragmaId bindings -}
			 True  {- Keep conjurable Ids -}
			 False {- Do not ignore INLINE Pragma -}
			 (\id locals -> interesting id || elementOfIdSet id locals)
			 emptyIdSet

occurAnalyseGlobalExpr :: CoreExpr -> SimplifiableCoreExpr
occurAnalyseGlobalExpr expr
  = 	-- Top level expr, so no interesting free vars, and
	-- discard occurence info returned
    snd (occurAnalyseExpr (\_ -> False) expr)
\end{code}

%************************************************************************
%*									*
\subsection[OccurAnal-main]{Counting occurrences: main function}
%*									*
%************************************************************************

Bindings
~~~~~~~~

\begin{code}
type Node details = (details, Int, [Int])	-- The Ints are gotten from the Unique,
						-- which is gotten from the Id.
type Details1	  = (Id, (UsageDetails, SimplifiableCoreExpr))
type Details2	  = ((Id, BinderInfo), SimplifiableCoreExpr)


occAnalBind :: OccEnv
	    -> CoreBinding
	    -> UsageDetails		-- Usage details of scope
	    -> (UsageDetails,		-- Of the whole let(rec)
		[SimplifiableCoreBinding])

occAnalBind env (NonRec binder rhs) body_usage
  | isNeeded env body_usage binder		-- It's mentioned in body
  = (final_body_usage `combineUsageDetails` rhs_usage,
     [NonRec tagged_binder rhs'])

  | otherwise			-- Not mentioned, so drop dead code
  = (body_usage, [])

  where
    binder'			      = nukeNoInlinePragma binder
    (rhs_usage, rhs')		      = occAnalRhs env binder' rhs
    (final_body_usage, tagged_binder) = tagBinder body_usage binder'
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
  = foldr (_scc_ "occAnalBind.dofinal" do_final_bind) (body_usage, []) sccs
  where
    pp_scc (CyclicSCC cycle) = hcat [text "Cyclic ", hcat (punctuate comma (map pp_item cycle))]
    pp_scc (AcyclicSCC item) = hcat [text "Acyclic ", pp_item item]
    pp_item (_, bndr, _)     = ppr PprDebug bndr

    binders = map fst pairs
    new_env = env `addNewCands` binders

    analysed_pairs :: [Details1]
    analysed_pairs  = [(nukeNoInlinePragma bndr, occAnalRhs new_env bndr rhs) | (bndr,rhs) <- pairs]

    sccs :: [SCC (Node Details1)]
    sccs = _scc_ "occAnalBind.scc" stronglyConnCompR edges


    ---- stuff for dependency analysis of binds -------------------------------
    edges :: [Node Details1]
    edges = _scc_ "occAnalBind.assoc"
	    [ (pair, IBOX(u2i (idUnique id)), edges_from rhs_usage)
	    | pair@(id, (rhs_usage, rhs)) <- analysed_pairs
	    ]

	-- (a -> b) means a mentions b
	-- Given the usage details (a UFM that gives occ info for each free var of
	-- the RHS) we can get the list of free vars -- or rather their Int keys --
	-- by just extracting the keys from the finite map.  Grimy, but fast.
	-- Previously we had this:
	-- 	[ bndr | bndr <- bndrs,
	--		 maybeToBool (lookupIdEnv rhs_usage bndr)]
	-- which has n**2 cost, and this meant that edges_from alone 
	-- consumed 10% of total runtime!
    edges_from :: UsageDetails -> [Int]
    edges_from rhs_usage = _scc_ "occAnalBind.edges_from"
			   keysUFM rhs_usage

    ---- stuff to "re-constitute" bindings from dependency-analysis info ------

	-- Non-recursive SCC
    do_final_bind (AcyclicSCC ((bndr, (rhs_usage, rhs')), _, _)) (body_usage, binds_so_far)
      | isNeeded env body_usage bndr
      = (combined_usage, new_bind : binds_so_far)	
      | otherwise
      = (body_usage, binds_so_far)			-- Dead code
      where
	total_usage       	      = combineUsageDetails body_usage rhs_usage
	(combined_usage, tagged_bndr) = tagBinder total_usage bndr
	new_bind		      = NonRec tagged_bndr rhs'

	-- Recursive SCC
    do_final_bind (CyclicSCC cycle) (body_usage, binds_so_far)
      | any (isNeeded env body_usage) bndrs
      = (combined_usage, final_bind:binds_so_far)
      | otherwise
      = (body_usage, binds_so_far)			-- Dead code
      where
	pairs 				 = [pair      | (pair, _, _) <- cycle]
	bndrs				 = [bndr      | (bndr, _)           <- pairs]
	rhs_usages		         = [rhs_usage | (_, (rhs_usage, _)) <- pairs]
	total_usage		         = foldr combineUsageDetails body_usage rhs_usages
	(combined_usage, tagged_binders) = tagBinders total_usage bndrs
	final_bind			 = Rec (reOrderRec env new_cycle)

	new_cycle = CyclicSCC (zipWithEqual "occAnalBind" mk_new_bind tagged_binders cycle)
	mk_new_bind tagged_bndr ((_, (_, rhs')), key, keys) = ((tagged_bndr, rhs'), key, keys)
\end{code}

@reOrderRec@ is applied to the list of (binder,rhs) pairs for a cyclic
strongly connected component (there's guaranteed to be a cycle).  It returns the
same pairs, but 
	a) in a better order,
	b) with some of the Ids having a IMustNotBeINLINEd pragma

The "no-inline" Ids are sufficient to break all cycles in the SCC.  This means
that the simplifier can guarantee not to loop provided it never records an inlining
for these no-inline guys.

Furthermore, the order of the binds is such that if we neglect dependencies
on the no-inline Ids then the binds are topologically sorted.  This means
that the simplifier will generally do a good job if it works from top bottom,
recording inlinings for any Ids which aren't marked as "no-inline" as it goes.

Here's a case that bit me:

	letrec
		a = b
		b = \x. BIG
	in
	...a...a...a....

Re-ordering doesn't change the order of bindings, but there was no loop-breaker.
(The first binding was a var-rhs; the second was a one-occ.)  So the simplifier looped.
My solution was to make a=b bindings record b as Many, rather like INLINE bindings.
Perhaps something cleverer would suffice.

\begin{code}
reOrderRec
	:: OccEnv
	-> SCC (Node Details2)
	-> [Details2]
			-- Sorted into a plausible order.  Enough of the Ids have
			--	dontINLINE pragmas that there are no loops left.

	-- Non-recursive case
reOrderRec env (AcyclicSCC (pair, _, _)) = [pair]

	-- Common case of simple self-recursion
reOrderRec env (CyclicSCC [bind])
  = [((addNoInlinePragma bndr, occ_info), rhs)]
  where
    (((bndr,occ_info), rhs), _, _) = bind

reOrderRec env (CyclicSCC binds)
  = 	-- Choose a loop breaker, mark it no-inline,
	-- do SCC analysis on the rest, and recursively sort them out
    concat (map (reOrderRec env) (stronglyConnCompR unchosen))
    ++ 
    [((addNoInlinePragma bndr, occ_info), rhs)]

  where
    (chosen_pair, unchosen) = choose_loop_breaker binds
    ((bndr,occ_info), rhs)  = chosen_pair

	-- Choosing the loop breaker; heursitic
    choose_loop_breaker (bind@(pair, _, _) : rest)
	|  not (null rest) &&
	   bad_choice pair
	=  (chosen, bind : unchosen)	-- Don't pick it
        | otherwise			-- Pick it
	= (pair,rest)
	where
	  (chosen, unchosen) = choose_loop_breaker rest

    bad_choice ((bndr, occ_info), rhs)
	=    var_rhs rhs 		-- Dont pick var RHS
	  || inlineMe env bndr		-- Dont pick INLINE thing
	  || one_occ occ_info 		-- Dont pick single-occ thing
	  || not_fun_ty (idType bndr)	-- Dont pick data-ty thing

    not_fun_ty ty = not (maybeToBool (getFunTy_maybe rho_ty))
		  where
		    (_, rho_ty) = splitForAllTy ty

	-- A variable RHS
    var_rhs (Var v)   = True
    var_rhs other_rhs = False

	-- One textual occurrence, whether inside lambda or whatever
	-- We stick to just FunOccs because if we're not going to be able
	-- to inline the thing on this round it might be better to pick
	-- this one as the loop breaker.  Real example (the Enum Ordering instance
	-- from PrelBase):
	--	rec	f = \ x -> case d of (p,q,r) -> p x
	--		g = \ x -> case d of (p,q,r) -> q x
	--		d = (v, f, g)
	--
	-- Here, f and g occur just once; but we can't inline them into d.
	-- On the other hand we *could* simplify those case expressions if
	-- we didn't stupidly choose d as the loop breaker.

    one_occ (OneOcc fun_or_arg _ _ _ _) = isFun fun_or_arg
    one_occ other_bind	       	        = False
\end{code}

@occAnalRhs@ deals with the question of bindings where the Id is marked
by an INLINE pragma.  For these we record that anything which occurs
in its RHS occurs many times.  This pessimistically assumes that ths
inlined binder also occurs many times in its scope, but if it doesn't
we'll catch it next time round.  At worst this costs an extra simplifier pass.
ToDo: try using the occurrence info for the inline'd binder.

[March 97] We do the same for atomic RHSs.  Reason: see notes with reOrderRec.

\begin{code}
occAnalRhs :: OccEnv
	   -> Id -> CoreExpr	-- Binder and rhs
	   -> (UsageDetails, SimplifiableCoreExpr)

occAnalRhs env id (Var v)
  | isCandidate env v
  = (unitIdEnv v (markMany (funOccurrence 0)), Var v)

  | otherwise
  = (emptyDetails, Var v)

occAnalRhs env id rhs
  | inlineMe env id
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
occAnal env (Prim op args) = (occAnalArgs env args, Prim op args)
\end{code}

We regard variables that occur as constructor arguments as "dangerousToDup":

\begin{verbatim}
module A where
f x = let y = expensive x in 
      let z = (True,y) in 
      (case z of {(p,q)->q}, case z of {(p,q)->q})
\end{verbatim}

We feel free to duplicate the WHNF (True,y), but that means
that y may be duplicated thereby.

If we aren't careful we duplicate the (expensive x) call!
Constructors are rather like lambdas in this way.

\begin{code}
occAnal env (Con con args) = (mapIdEnv markDangerousToDup (occAnalArgs env args), 
			      Con con args)

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

-- For value lambdas we do a special hack.  Consider
-- 	(\x. \y. ...x...)
-- If we did nothing, x is used inside the \y, so would be marked
-- as dangerous to dup.  But in the common case where the abstraction
-- is applied to two arguments this is over-pessimistic.
-- So instead we don't take account of the \y when dealing with x's usage;
-- instead, the simplifier is careful when partially applying lambdas

occAnal env expr@(Lam (ValBinder binder) body)
  = (mapIdEnv markDangerousToDup final_usage,
     foldr ( \v b -> Lam (ValBinder v) b) body' tagged_binders)
  where
    (binders,body)		  = collectValBinders expr
    (body_usage, body') 	  = occAnal (env `addNewCands` binders) body
    (final_usage, tagged_binders) = tagBinders body_usage binders

-- ANDY: WE MUST THINK ABOUT THIS! (ToDo)
occAnal env (Lam (TyBinder tyvar) body)
  = case occAnal env body of { (body_usage, body') ->
     (mapIdEnv markDangerousToDup body_usage,
      Lam (TyBinder tyvar) body') }
--  where
--    (body_usage, body') = occAnal env body

occAnal env (Lam (UsageBinder _) _) = panic "OccurAnal.occAnal Lam UsageBinder"

occAnal env (Case scrut alts)
  = case occAnalAlts env alts of { (alts_usage, alts')   -> 
     case occAnal env scrut   of { (scrut_usage, scrut') ->
       let
        det = scrut_usage `combineUsageDetails` alts_usage
       in
       if isNullIdEnv det then
          (det, Case scrut' alts')
       else
          (det, Case scrut' alts') }}
{-
       (scrut_usage `combineUsageDetails` alts_usage,
        Case scrut' alts')
  where
    (scrut_usage, scrut') = occAnal env scrut
    (alts_usage, alts')   = occAnalAlts env alts
-}

occAnal env (Let bind body)
  = case occAnal new_env body            of { (body_usage, body') ->
    case occAnalBind env bind body_usage of { (final_usage, new_binds) ->
       (final_usage, foldr Let body' new_binds) }} -- mkCoLet* wants Core... (sigh)
  where
    new_env		     = env `addNewCands` (bindersOf bind)
--    (body_usage, body')      = occAnal new_env body
--    (final_usage, new_binds) = occAnalBind env bind body_usage
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
