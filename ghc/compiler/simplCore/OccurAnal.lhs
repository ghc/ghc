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
module OccurAnal (
	occurAnalyseBinds, occurAnalyseExpr, occurAnalyseGlobalExpr
    ) where

#include "HsVersions.h"

import BinderInfo
import CmdLineOpts	( opt_D_dump_occur_anal, SimplifierSwitch(..) )
import CoreSyn
import CoreUtils	( idSpecVars )
import Digraph		( stronglyConnCompR, SCC(..) )
import Id		( idWantsToBeINLINEd, addNoInlinePragma, nukeNoInlinePragma,
			  omitIfaceSigForId, isSpecPragmaId, getIdSpecialisation,
			  idType, idUnique, Id,
			  emptyIdSet, unionIdSets, mkIdSet,
			  elementOfIdSet,
			  addOneToIdSet, IdSet,

			  IdEnv, nullIdEnv, unitIdEnv, combineIdEnvs,
			  delOneFromIdEnv, delManyFromIdEnv, isNullIdEnv, 
			  mapIdEnv, lookupIdEnv, elemIdEnv, addOneToIdEnv
			)
import SpecEnv		( isEmptySpecEnv )
import Name		( isExported, isLocallyDefined )
import Type		( splitFunTy_maybe, splitForAllTys )
import Maybes		( maybeToBool )
import PprCore
import Unique		( u2i )
import UniqFM		( keysUFM )  
import Util		( zipWithEqual )
import Outputable
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
				     (pprGenericBindings new_binds)
				     new_binds
  | otherwise		  = new_binds
  where
    new_binds	= concat binds'
    (_, _, binds') = occAnalTop initial_env binds

    initial_env = OccEnv (simplifier_sw_chkr IgnoreINLINEPragma)
			 (\id in_scope -> isLocallyDefined id)	-- Anything local is interesting
			 emptyIdSet				-- Not actually used
\end{code}


\begin{code}
occurAnalyseExpr :: (Id -> Bool)	-- Tells if a variable is interesting
		 -> CoreExpr
		 -> (IdEnv BinderInfo,	-- Occ info for interesting free vars
		     SimplifiableCoreExpr)

occurAnalyseExpr interesting expr
  = occAnal initial_env expr
  where
    initial_env = OccEnv False {- Do not ignore INLINE Pragma -}
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
\subsection{Top level stuff}
%*									*
%************************************************************************

In @occAnalTop@ we do indirection-shorting.  That is, if we have this:

	loc = <expression>
	...
	exp = loc

where exp is exported, and loc is not, then we replace it with this:

	loc = exp
	exp = <expression>
	...

Without this we never get rid of the exp = loc thing.
This save a gratuitous jump
(from \tr{x_exported} to \tr{x_local}), and makes strictness
information propagate better.
This used to happen in the final phase, but its tidier to do it here.


If more than one exported thing is equal to a local thing (i.e., the
local thing really is shared), then we do one only:
\begin{verbatim}
	x_local = ....
	x_exported1 = x_local
	x_exported2 = x_local
==>
	x_exported1 = ....

	x_exported2 = x_exported1
\end{verbatim}

We rely on prior eta reduction to simplify things like
\begin{verbatim}
	x_exported = /\ tyvars -> x_local tyvars
==>
	x_exported = x_local
\end{verbatim}
Hence,there's a possibility of leaving unchanged something like this:
\begin{verbatim}
	x_local = ....
	x_exported1 = x_local Int
\end{verbatim}
By the time we've thrown away the types in STG land this 
could be eliminated.  But I don't think it's very common
and it's dangerous to do this fiddling in STG land 
because we might elminate a binding that's mentioned in the
unfolding for something.


\begin{code}
occAnalTop :: OccEnv 			-- What's in scope
	   -> [CoreBinding]
	   -> (IdEnv BinderInfo, 	-- Occurrence info
	       IdEnv Id,	        -- Indirection elimination info
	       [[SimplifiableCoreBinding]]
	      )
occAnalTop env [] = (emptyDetails, nullIdEnv, [])
occAnalTop env (bind : binds)
  = case bind of
	NonRec exported_id (Var local_id)
	  | isExported exported_id &&		-- Only if this is exported

	    isLocallyDefined local_id &&	-- Only if this one is defined in this
						-- 	module, so that we *can* change its
					  	-- 	binding to be the exported thing!

	    not (isExported local_id) &&	-- Only if this one is not itself exported,
						--	since the transformation will nuke it

	    not (omitIfaceSigForId local_id) &&	-- Don't do the transformation if rhs_id is
						-- 	something like a constructor, whose 
						--	definition is implicitly exported and 
						-- 	which must not vanish.
    
		-- To illustrate the preceding check consider
		--	data T = MkT Int
		--	mkT = MkT
		--	f x = MkT (x+1)
		-- Here, we'll make a local, non-exported, defn for MkT, and without the
		-- above condition we'll transform it to:
		--	mkT = \x. MkT [x]
		--	f = \y. mkT (y+1)
		-- This is bad because mkT will get the IdDetails of MkT, and won't
		-- be exported.  Also the code generator won't make a definition for
		-- the MkT constructor.
		-- Slightly gruesome, this.

	    not (maybeToBool (lookupIdEnv ind_env local_id))
						-- Only if not already substituted for
	    -> 	-- Aha!  An indirection; let's eliminate it!
	       (scope_usage, ind_env', binds')
	    where
	        ind_env' = addOneToIdEnv ind_env local_id exported_id

	other 
	    ->  -- The normal case
		(final_usage, ind_env, (new_binds : binds'))
	    where
		(final_usage, new_binds) = occAnalBind env (zap_bind bind) scope_usage
  where
    new_env		           = env `addNewCands` (bindersOf bind)
    (scope_usage, ind_env, binds') = occAnalTop new_env binds

	-- Deal with any indirections
    zap_bind (NonRec bndr rhs) 
	| bndr `elemIdEnv` ind_env 			= Rec (zap (bndr,rhs))
		-- The Rec isn't strictly necessary, but it's convenient
    zap_bind (Rec pairs)
	| or [id `elemIdEnv` ind_env | (id,_) <- pairs] = Rec (concat (map zap pairs))

    zap_bind bind = bind

    zap pair@(bndr,rhs) = case lookupIdEnv ind_env bndr of
			    Nothing          -> [pair]
			    Just exported_id -> [(bndr, Var exported_id),
					         (exported_id, rhs)]

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
type Details1	  = (Id, UsageDetails, SimplifiableCoreExpr)
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
    pp_item (_, bndr, _)     = ppr bndr

    binders = map fst pairs
    new_env = env `addNewCands` binders

    analysed_pairs :: [Details1]
    analysed_pairs  = [ (nukeNoInlinePragma bndr, rhs_usage, rhs')
		      | (bndr, rhs) <- pairs,
			let (rhs_usage, rhs') = occAnalRhs new_env bndr rhs
		      ]

    sccs :: [SCC (Node Details1)]
    sccs = _scc_ "occAnalBind.scc" stronglyConnCompR edges


    ---- stuff for dependency analysis of binds -------------------------------
    edges :: [Node Details1]
    edges = _scc_ "occAnalBind.assoc"
	    [ (details, IBOX(u2i (idUnique id)), edges_from rhs_usage)
	    | details@(id, rhs_usage, rhs) <- analysed_pairs
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
    do_final_bind (AcyclicSCC ((bndr, rhs_usage, rhs'), _, _)) (body_usage, binds_so_far)
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
	details				 = [details   | (details, _, _) <- cycle]
	bndrs				 = [bndr      | (bndr, _, _)      <- details]
	rhs_usages		         = [rhs_usage | (_, rhs_usage, _) <- details]
	total_usage		         = foldr combineUsageDetails body_usage rhs_usages
	(combined_usage, tagged_binders) = tagBinders total_usage bndrs
	final_bind			 = Rec (reOrderRec env new_cycle)

	new_cycle = CyclicSCC (zipWithEqual "occAnalBind" mk_new_bind tagged_binders cycle)
	mk_new_bind (bndr, occ_info) ((_, _, rhs'), key, keys) = (((bndr, occ_info), rhs'), key, keys)
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

My solution was to make a=b bindings record b as Many, rather like INLINE bindings.
Perhaps something cleverer would suffice.

You might think that you can prevent non-termination simply by making
sure that we simplify a recursive binding's RHS in an environment that
simply clones the recursive Id.  But no.  Consider

		letrec f = \x -> let z = f x' in ...

		in
		let n = f y
		in
		case n of { ... }

We bind n to its *simplified* RHS, we then *re-simplify* it when
we inline n.  Then we may well inline f; and then the same thing
happens with z!

I don't think it's possible to prevent non-termination by environment
manipulation in this way.  Apart from anything else, successive
iterations of the simplifier may unroll recursive loops in cases like
that above.  The idea of beaking every recursive loop with an
IMustNotBeINLINEd pragma is much much better.


\begin{code}
reOrderRec
	:: OccEnv
	-> SCC (Node Details2)
	-> [Details2]
			-- Sorted into a plausible order.  Enough of the Ids have
			--	dontINLINE pragmas that there are no loops left.

	-- Non-recursive case
reOrderRec env (AcyclicSCC (bind, _, _)) = [bind]

	-- Common case of simple self-recursion
reOrderRec env (CyclicSCC [bind])
  = [((addNoInlinePragma bndr, occ_info), rhs)]
  where
    (((bndr, occ_info), rhs), _, _) = bind

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
    choose_loop_breaker (bind@(details, _, _) : rest)
	|  not (null rest) &&
	   bad_choice details
	=  (chosen, bind : unchosen)	-- Don't pick it
        | otherwise			-- Pick it
	= (details,rest)
	where
	  (chosen, unchosen) = choose_loop_breaker rest

    bad_choice ((bndr, occ_info), rhs)
	=    var_rhs rhs 		-- Dont pick var RHS
	  || inlineMe env bndr		-- Dont pick INLINE thing
	  || isOneFunOcc occ_info	-- Dont pick single-occ thing
	  || not_fun_ty (idType bndr)	-- Dont pick data-ty thing
	  || not (isEmptySpecEnv (getIdSpecialisation bndr))
		-- Avoid things with a SpecEnv; we'd like
		-- to take advantage of the SpecEnv in the subsuequent bindings

	-- isOneFunOcc looks for one textual occurrence, whether inside lambda or whatever.
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

    not_fun_ty ty = not (maybeToBool (splitFunTy_maybe rho_ty))
		  where
		    (_, rho_ty) = splitForAllTys ty

	-- A variable RHS
    var_rhs (Var v)   = True
    var_rhs other_rhs = False
\end{code}

@occAnalRhs@ deals with the question of bindings where the Id is marked
by an INLINE pragma.  For these we record that anything which occurs
in its RHS occurs many times.  This pessimistically assumes that ths
inlined binder also occurs many times in its scope, but if it doesn't
we'll catch it next time round.  At worst this costs an extra simplifier pass.
ToDo: try using the occurrence info for the inline'd binder.

[March 97] We do the same for atomic RHSs.  Reason: see notes with reOrderRec.

[March 98] A new wrinkle is that if the binder has specialisations inside
it then we count the specialised Ids as "extra rhs's".  That way
the "parent" keeps the specialised "children" alive.  If the parent
dies (because it isn't referenced any more), then the children will
die too unless they are already referenced directly.

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
  = (mapIdEnv markMany total_usage, rhs')

  | otherwise
  = (total_usage, rhs')

  where
    (rhs_usage, rhs') = occAnal env rhs
    total_usage = foldr add rhs_usage (idSpecVars id)
    add v u     = addOneOcc u v noBinderInfo	-- Give a non-committal binder info
						-- (i.e manyOcc) because many copies
						-- of the specialised thing can appear
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
occAnal env (Con con args)
  = (mapIdEnv markDangerousToDup (occAnalArgs env args), 
     Con con args)

occAnal env (Note note@(SCC cc) body)
  = (mapIdEnv markInsideSCC usage, Note note body')
  where
    (usage, body') = occAnal env body

occAnal env (Note note body)
  = (usage, Note note body')
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


%************************************************************************
%*									*
\subsection[OccurAnal-types]{Data types}
%*									*
%************************************************************************

\begin{code}
data OccEnv =
  OccEnv
    Bool	-- IgnoreINLINEPragma flag
		-- False <=> OK to use INLINEPragma information
		-- True  <=> ignore INLINEPragma information

    (Id -> IdSet -> Bool)	-- Tells whether an Id occurrence is interesting,
				-- given the set of in-scope variables

    IdSet	-- In-scope Ids


addNewCands :: OccEnv -> [Id] -> OccEnv
addNewCands (OccEnv ip ifun cands) ids
  = OccEnv ip ifun (cands `unionIdSets` mkIdSet ids)

addNewCand :: OccEnv -> Id -> OccEnv
addNewCand (OccEnv ip ifun cands) id
  = OccEnv ip ifun (addOneToIdSet cands id)

isCandidate :: OccEnv -> Id -> Bool
isCandidate (OccEnv _ ifun cands) id = ifun id cands

inlineMe :: OccEnv -> Id -> Bool
inlineMe env id
  = {-	See comments with simplIdWantsToBeINLINEd in SimplUtils.lhs 
	not ignore_inline_prag && 
    -}
    idWantsToBeINLINEd id


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
    if isNullIdEnv usage' then  -- Bogus test to force evaluation.
       (usage', (binder, us))
    else
       (usage', (binder, us))
 in
 if isDeadOcc us then		-- Ditto 
	cont
 else 
	cont


usage_of usage binder
  | isExported binder || isSpecPragmaId binder
  = noBinderInfo	-- Visible-elsewhere things count as many
  | otherwise
  = case (lookupIdEnv usage binder) of
      Nothing   -> deadOccurrence
      Just info -> info

isNeeded env usage binder = not (isDeadOcc (usage_of usage binder))
\end{code}


