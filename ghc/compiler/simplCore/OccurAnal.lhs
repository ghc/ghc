%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
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
	occurAnalysePgm, occurAnalyseGlobalExpr, occurAnalyseRule, 
    ) where

#include "HsVersions.h"

import CoreSyn
import CoreFVs		( idRuleVars )
import CoreUtils	( exprIsTrivial )
import Id		( isDataConWorkId, isOneShotBndr, setOneShotLambda, 
			  idOccInfo, setIdOccInfo, isLocalId,
			  isExportedId, idArity, idSpecialisation, 
			  idType, idUnique, Id
			)
import BasicTypes	( OccInfo(..), isOneOcc )

import VarSet
import VarEnv

import Type		( isFunTy, dropForAlls )
import Maybes		( orElse )
import Digraph		( stronglyConnCompR, SCC(..) )
import PrelNames	( buildIdKey, foldrIdKey, runSTRepIdKey, augmentIdKey )
import Unique		( Unique )
import UniqFM		( keysUFM )  
import Util		( zipWithEqual, mapAndUnzip )
import Outputable
\end{code}


%************************************************************************
%*									*
\subsection[OccurAnal-main]{Counting occurrences: main function}
%*									*
%************************************************************************

Here's the externally-callable interface:

\begin{code}
occurAnalysePgm :: [CoreBind] -> [CoreBind]
occurAnalysePgm binds
  = snd (go initOccEnv binds)
  where
    go :: OccEnv -> [CoreBind] -> (UsageDetails, [CoreBind])
    go env [] 
	= (emptyDetails, [])
    go env (bind:binds) 
	= (final_usage, bind' ++ binds')
	where
	   (bs_usage, binds')   = go env binds
	   (final_usage, bind') = occAnalBind env bind bs_usage

occurAnalyseGlobalExpr :: CoreExpr -> CoreExpr
occurAnalyseGlobalExpr expr
  = 	-- Top level expr, so no interesting free vars, and
	-- discard occurence info returned
    snd (occAnal initOccEnv expr)

occurAnalyseRule :: CoreRule -> CoreRule
occurAnalyseRule rule@(BuiltinRule _ _) = rule
occurAnalyseRule (Rule str act tpl_vars tpl_args rhs)
		-- Add occ info to tpl_vars, rhs
  = Rule str act tpl_vars' tpl_args rhs'
  where
    (rhs_uds, rhs') = occAnal initOccEnv rhs
    (_, tpl_vars')  = tagBinders rhs_uds tpl_vars
\end{code}


%************************************************************************
%*									*
\subsection[OccurAnal-main]{Counting occurrences: main function}
%*									*
%************************************************************************

Bindings
~~~~~~~~

\begin{code}
type IdWithOccInfo = Id			-- An Id with fresh PragmaInfo attached

type Node details = (details, Unique, [Unique])	-- The Ints are gotten from the Unique,
						-- which is gotten from the Id.
type Details1	  = (Id, UsageDetails, CoreExpr)
type Details2	  = (IdWithOccInfo, CoreExpr)


occAnalBind :: OccEnv
	    -> CoreBind
	    -> UsageDetails		-- Usage details of scope
	    -> (UsageDetails,		-- Of the whole let(rec)
		[CoreBind])

occAnalBind env (NonRec binder rhs) body_usage
  | not (binder `usedIn` body_usage)		-- It's not mentioned
  = (body_usage, [])

  | otherwise			-- It's mentioned in the body
  = (final_body_usage `combineUsageDetails` rhs_usage,
     [NonRec tagged_binder rhs'])

  where
    (final_body_usage, tagged_binder) = tagBinder body_usage binder
    (rhs_usage, rhs')		      = occAnalRhs env tagged_binder rhs
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
    binders = map fst pairs

    analysed_pairs :: [Details1]
    analysed_pairs  = [ (bndr, rhs_usage, rhs')
		      | (bndr, rhs) <- pairs,
			let (rhs_usage, rhs') = occAnalRhs env bndr rhs
		      ]

    sccs :: [SCC (Node Details1)]
    sccs = _scc_ "occAnalBind.scc" stronglyConnCompR edges


    ---- stuff for dependency analysis of binds -------------------------------
    edges :: [Node Details1]
    edges = _scc_ "occAnalBind.assoc"
	    [ (details, idUnique id, edges_from rhs_usage)
	    | details@(id, rhs_usage, rhs) <- analysed_pairs
	    ]

	-- (a -> b) means a mentions b
	-- Given the usage details (a UFM that gives occ info for each free var of
	-- the RHS) we can get the list of free vars -- or rather their Int keys --
	-- by just extracting the keys from the finite map.  Grimy, but fast.
	-- Previously we had this:
	-- 	[ bndr | bndr <- bndrs,
	--		 maybeToBool (lookupVarEnv rhs_usage bndr)]
	-- which has n**2 cost, and this meant that edges_from alone 
	-- consumed 10% of total runtime!
    edges_from :: UsageDetails -> [Unique]
    edges_from rhs_usage = _scc_ "occAnalBind.edges_from"
			   keysUFM rhs_usage

    ---- stuff to "re-constitute" bindings from dependency-analysis info ------

	-- Non-recursive SCC
    do_final_bind (AcyclicSCC ((bndr, rhs_usage, rhs'), _, _)) (body_usage, binds_so_far)
      | not (bndr `usedIn` body_usage)
      = (body_usage, binds_so_far)			-- Dead code
      | otherwise
      = (combined_usage, new_bind : binds_so_far)	
      where
	total_usage       	      = combineUsageDetails body_usage rhs_usage
	(combined_usage, tagged_bndr) = tagBinder total_usage bndr
	new_bind		      = NonRec tagged_bndr rhs'

	-- Recursive SCC
    do_final_bind (CyclicSCC cycle) (body_usage, binds_so_far)
      | not (any (`usedIn` body_usage) bndrs)		-- NB: look at body_usage, not total_usage
      = (body_usage, binds_so_far)			-- Dead code
      | otherwise
      = (combined_usage, final_bind:binds_so_far)
      where
	details			       = [details   | (details, _, _) <- cycle]
	bndrs			       = [bndr      | (bndr, _, _)      <- details]
	rhs_usages		       = [rhs_usage | (_, rhs_usage, _) <- details]
	total_usage		       = foldr combineUsageDetails body_usage rhs_usages
	(combined_usage, tagged_bndrs) = tagBinders total_usage bndrs
	final_bind		       = Rec (reOrderRec env new_cycle)

	new_cycle = CyclicSCC (zipWithEqual "occAnalBind" mk_new_bind tagged_bndrs cycle)
	mk_new_bind tagged_bndr ((_, _, rhs'), key, keys) = ((tagged_bndr, rhs'), key, keys)
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

==============
[June 98: I don't understand the following paragraphs, and I've 
	  changed the a=b case again so that it isn't a special case any more.]

Here's a case that bit me:

	letrec
		a = b
		b = \x. BIG
	in
	...a...a...a....

Re-ordering doesn't change the order of bindings, but there was no loop-breaker.

My solution was to make a=b bindings record b as Many, rather like INLINE bindings.
Perhaps something cleverer would suffice.
===============

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
  = [(setIdOccInfo tagged_bndr IAmALoopBreaker, rhs)]
  where
    ((tagged_bndr, rhs), _, _) = bind

reOrderRec env (CyclicSCC (bind : binds))
  = 	-- Choose a loop breaker, mark it no-inline,
	-- do SCC analysis on the rest, and recursively sort them out
    concat (map (reOrderRec env) (stronglyConnCompR unchosen))
    ++ 
    [(setIdOccInfo tagged_bndr IAmALoopBreaker, rhs)]

  where
    (chosen_pair, unchosen) = choose_loop_breaker bind (score bind) [] binds
    (tagged_bndr, rhs)      = chosen_pair

	-- This loop looks for the bind with the lowest score
	-- to pick as the loop  breaker.  The rest accumulate in 
    choose_loop_breaker (details,_,_) loop_sc acc []
	= (details, acc)	-- Done

    choose_loop_breaker loop_bind loop_sc acc (bind : binds)
	| sc < loop_sc	-- Lower score so pick this new one
	= choose_loop_breaker bind sc (loop_bind : acc) binds

	| otherwise	-- No lower so don't pick it
	= choose_loop_breaker loop_bind loop_sc (bind : acc) binds
	where
	  sc = score bind
	  
    score :: Node Details2 -> Int	-- Higher score => less likely to be picked as loop breaker
    score ((bndr, rhs), _, _)
	| exprIsTrivial rhs 	   = 4	-- Practically certain to be inlined
		-- Used to have also: && not (isExportedId bndr)
		-- But I found this sometimes cost an extra iteration when we have
		--	rec { d = (a,b); a = ...df...; b = ...df...; df = d }
		-- where df is the exported dictionary. Then df makes a really
		-- bad choice for loop breaker
	  
	| not_fun_ty (idType bndr) = 3	-- Data types help with cases
		-- This used to have a lower score than inlineCandidate, but
		-- it's *really* helpful if dictionaries get inlined fast,
		-- so I'm experimenting with giving higher priority to data-typed things

	| inlineCandidate bndr rhs = 2	-- Likely to be inlined

	| not (isEmptyCoreRules (idSpecialisation bndr)) = 1
		-- Avoid things with specialisations; we'd like
		-- to take advantage of them in the subsequent bindings

	| otherwise = 0

    inlineCandidate :: Id -> CoreExpr -> Bool
    inlineCandidate id (Note InlineMe _) = True
    inlineCandidate id rhs	         = isOneOcc (idOccInfo id)

	-- Real example (the Enum Ordering instance from PrelBase):
	--	rec	f = \ x -> case d of (p,q,r) -> p x
	--		g = \ x -> case d of (p,q,r) -> q x
	--		d = (v, f, g)
	--
	-- Here, f and g occur just once; but we can't inline them into d.
	-- On the other hand we *could* simplify those case expressions if
	-- we didn't stupidly choose d as the loop breaker.
	-- But we won't because constructor args are marked "Many".

    not_fun_ty ty = not (isFunTy (dropForAlls ty))
\end{code}

@occAnalRhs@ deals with the question of bindings where the Id is marked
by an INLINE pragma.  For these we record that anything which occurs
in its RHS occurs many times.  This pessimistically assumes that ths
inlined binder also occurs many times in its scope, but if it doesn't
we'll catch it next time round.  At worst this costs an extra simplifier pass.
ToDo: try using the occurrence info for the inline'd binder.

[March 97] We do the same for atomic RHSs.  Reason: see notes with reOrderRec.
[June 98, SLPJ]  I've undone this change; I don't understand it.  See notes with reOrderRec.


\begin{code}
occAnalRhs :: OccEnv
	   -> Id -> CoreExpr	-- Binder and rhs
				-- For non-recs the binder is alrady tagged
				-- with occurrence info
	   -> (UsageDetails, CoreExpr)

occAnalRhs env id rhs
  = (final_usage, rhs')
  where
    (rhs_usage, rhs') = occAnal ctxt rhs
    ctxt | certainly_inline id = env
	 | otherwise	       = rhsCtxt
	-- Note that we generally use an rhsCtxt.  This tells the occ anal n
	-- that it's looking at an RHS, which has an effect in occAnalApp
	--
	-- But there's a problem.  Consider
	--	x1 = a0 : []
	--	x2 = a1 : x1
	--	x3 = a2 : x2
	--	g  = f x3
	-- First time round, it looks as if x1 and x2 occur as an arg of a 
	-- let-bound constructor ==> give them a many-occurrence.
	-- But then x3 is inlined (unconditionally as it happens) and
	-- next time round, x2 will be, and the next time round x1 will be
	-- Result: multiple simplifier iterations.  Sigh.  
	-- Crude solution: use rhsCtxt for things that occur just once...

    certainly_inline id = case idOccInfo id of
			    OneOcc in_lam one_br -> not in_lam && one_br
			    other		 -> False

	-- [March 98] A new wrinkle is that if the binder has specialisations inside
	-- it then we count the specialised Ids as "extra rhs's".  That way
	-- the "parent" keeps the specialised "children" alive.  If the parent
	-- dies (because it isn't referenced any more), then the children will
	-- die too unless they are already referenced directly.

    final_usage = addRuleUsage rhs_usage id

addRuleUsage :: UsageDetails -> Id -> UsageDetails
-- Add the usage from RULES in Id to the usage
addRuleUsage usage id
  = foldVarSet add usage (idRuleVars id)
  where
    add v u = addOneOcc u v NoOccInfo		-- Give a non-committal binder info
						-- (i.e manyOcc) because many copies
						-- of the specialised thing can appear
\end{code}

Expressions
~~~~~~~~~~~
\begin{code}
occAnal :: OccEnv
	-> CoreExpr
	-> (UsageDetails,	-- Gives info only about the "interesting" Ids
	    CoreExpr)

occAnal env (Type t)  = (emptyDetails, Type t)

occAnal env (Var v) 
  = (var_uds, Var v)
  where
    var_uds | isLocalId v = unitVarEnv v oneOcc
	    | otherwise	 = emptyDetails

    -- At one stage, I gathered the idRuleVars for v here too,
    -- which in a way is the right thing to do.
    -- But that went wrong right after specialisation, when
    -- the *occurrences* of the overloaded function didn't have any
    -- rules in them, so the *specialised* versions looked as if they
    -- weren't used at all.
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
occAnal env expr@(Lit lit) = (emptyDetails, expr)
\end{code}

\begin{code}
occAnal env (Note InlineMe body)
  = case occAnal env body of { (usage, body') -> 
    (mapVarEnv markMany usage, Note InlineMe body')
    }

occAnal env (Note note@(SCC cc) body)
  = case occAnal env body of { (usage, body') ->
    (mapVarEnv markInsideSCC usage, Note note body')
    }

occAnal env (Note note body)
  = case occAnal env body of { (usage, body') ->
    (usage, Note note body')
    }
\end{code}

\begin{code}
occAnal env app@(App fun arg)
  = occAnalApp env (collectArgs app) False

-- Ignore type variables altogether
--   (a) occurrences inside type lambdas only not marked as InsideLam
--   (b) type variables not in environment

occAnal env expr@(Lam x body) | isTyVar x
  = case occAnal env body of { (body_usage, body') ->
    (body_usage, Lam x body')
    }

-- For value lambdas we do a special hack.  Consider
-- 	(\x. \y. ...x...)
-- If we did nothing, x is used inside the \y, so would be marked
-- as dangerous to dup.  But in the common case where the abstraction
-- is applied to two arguments this is over-pessimistic.
-- So instead, we just mark each binder with its occurrence
-- info in the *body* of the multiple lambda.
-- Then, the simplifier is careful when partially applying lambdas.

occAnal env expr@(Lam _ _)
  = case occAnal env_body body of { (body_usage, body') ->
    let
        (final_usage, tagged_binders) = tagBinders body_usage binders
	--	URGH!  Sept 99: we don't seem to be able to use binders' here, because
	--	we get linear-typed things in the resulting program that we can't handle yet.
	--	(e.g. PrelShow)  TODO 

	really_final_usage = if linear then
				final_usage
			     else
				mapVarEnv markInsideLam final_usage
    in
    (really_final_usage,
     mkLams tagged_binders body') }
  where
    env_body	    = vanillaCtxt			-- Body is (no longer) an RhsContext
    (binders, body) = collectBinders expr
    binders' 	    = oneShotGroup env binders
    linear	    = all is_one_shot binders'
    is_one_shot b   = isId b && isOneShotBndr b

occAnal env (Case scrut bndr ty alts)
  = case mapAndUnzip (occAnalAlt env bndr) alts of { (alts_usage_s, alts')   -> 
    case occAnal vanillaCtxt scrut		    of { (scrut_usage, scrut') ->
	-- No need for rhsCtxt
    let
	alts_usage  = foldr1 combineAltsUsageDetails alts_usage_s
	alts_usage' = addCaseBndrUsage alts_usage
	(alts_usage1, tagged_bndr) = tagBinder alts_usage' bndr
        total_usage = scrut_usage `combineUsageDetails` alts_usage1
    in
    total_usage `seq` (total_usage, Case scrut' tagged_bndr ty alts') }}
  where
	-- The case binder gets a usage of either "many" or "dead", never "one".
	-- Reason: we like to inline single occurrences, to eliminate a binding,
	-- but inlining a case binder *doesn't* eliminate a binding.
	-- We *don't* want to transform
	--	case x of w { (p,q) -> f w }
	-- into
	--	case x of w { (p,q) -> f (p,q) }
    addCaseBndrUsage usage = case lookupVarEnv usage bndr of
				Nothing  -> usage
				Just occ -> extendVarEnv usage bndr (markMany occ)

occAnal env (Let bind body)
  = case occAnal env body     	         of { (body_usage, body') ->
    case occAnalBind env bind body_usage of { (final_usage, new_binds) ->
       (final_usage, mkLets new_binds body') }}

occAnalArgs env args
  = case mapAndUnzip (occAnal arg_env) args of	{ (arg_uds_s, args') ->
    (foldr combineUsageDetails emptyDetails arg_uds_s, args')}
  where
    arg_env = vanillaCtxt
\end{code}

Applications are dealt with specially because we want
the "build hack" to work.

\begin{code}
-- Hack for build, fold, runST
occAnalApp env (Var fun, args) is_rhs
  = case args_stuff of { (args_uds, args') ->
    let
	-- We mark the free vars of the argument of a constructor or PAP 
	-- as "many", if it is the RHS of a let(rec).
	-- This means that nothing gets inlined into a constructor argument
	-- position, which is what we want.  Typically those constructor
	-- arguments are just variables, or trivial expressions.
	--
	-- This is the *whole point* of the isRhsEnv predicate
        final_args_uds
		| isRhsEnv env,
		  isDataConWorkId fun || valArgCount args < idArity fun
		= mapVarEnv markMany args_uds
		| otherwise = args_uds
    in
    (fun_uds `combineUsageDetails` final_args_uds, mkApps (Var fun) args') }
  where
    fun_uniq = idUnique fun

    fun_uds | isLocalId fun = unitVarEnv fun oneOcc
	    | otherwise	    = emptyDetails

    args_stuff	| fun_uniq == buildIdKey    = appSpecial env 2 [True,True]  args
		| fun_uniq == augmentIdKey  = appSpecial env 2 [True,True]  args
		| fun_uniq == foldrIdKey    = appSpecial env 3 [False,True] args
		| fun_uniq == runSTRepIdKey = appSpecial env 2 [True]	    args
			-- (foldr k z xs) may call k many times, but it never
			-- shares a partial application of k; hence [False,True]
			-- This means we can optimise
			--	foldr (\x -> let v = ...x... in \y -> ...v...) z xs
			-- by floating in the v

		| otherwise = occAnalArgs env args


occAnalApp env (fun, args) is_rhs
  = case occAnal (addAppCtxt env args) fun of	{ (fun_uds, fun') ->
	-- The addAppCtxt is a bit cunning.  One iteration of the simplifier
	-- often leaves behind beta redexs like
	--	(\x y -> e) a1 a2
	-- Here we would like to mark x,y as one-shot, and treat the whole
	-- thing much like a let.  We do this by pushing some True items
	-- onto the context stack.

    case occAnalArgs env args of	{ (args_uds, args') ->
    let
	final_uds = fun_uds `combineUsageDetails` args_uds
    in
    (final_uds, mkApps fun' args') }}
    
appSpecial :: OccEnv 
	   -> Int -> CtxtTy	-- Argument number, and context to use for it
	   -> [CoreExpr]
	   -> (UsageDetails, [CoreExpr])
appSpecial env n ctxt args
  = go n args
  where
    arg_env = vanillaCtxt

    go n [] = (emptyDetails, [])	-- Too few args

    go 1 (arg:args)			-- The magic arg
      = case occAnal (setCtxt arg_env ctxt) arg of	{ (arg_uds, arg') ->
	case occAnalArgs env args of			{ (args_uds, args') ->
	(combineUsageDetails arg_uds args_uds, arg':args') }}
    
    go n (arg:args)
      = case occAnal arg_env arg of	{ (arg_uds, arg') ->
	case go (n-1) args of		{ (args_uds, args') ->
	(combineUsageDetails arg_uds args_uds, arg':args') }}
\end{code}

    
Case alternatives
~~~~~~~~~~~~~~~~~
If the case binder occurs at all, the other binders effectively do too.  
For example
	case e of x { (a,b) -> rhs }
is rather like
	let x = (a,b) in rhs
If e turns out to be (e1,e2) we indeed get something like
	let a = e1; b = e2; x = (a,b) in rhs

\begin{code}
occAnalAlt env case_bndr (con, bndrs, rhs)
  = case occAnal env rhs of { (rhs_usage, rhs') ->
    let
        (final_usage, tagged_bndrs) = tagBinders rhs_usage bndrs
	final_bndrs | case_bndr `elemVarEnv` final_usage = bndrs
		    | otherwise				= tagged_bndrs
		-- Leave the binders untagged if the case 
		-- binder occurs at all; see note above
    in
    (final_usage, (con, final_bndrs, rhs')) }
\end{code}


%************************************************************************
%*									*
\subsection[OccurAnal-types]{OccEnv}
%*									*
%************************************************************************

\begin{code}
data OccEnv
  = OccEnv OccEncl	-- Enclosing context information
	   CtxtTy	-- Tells about linearity

-- OccEncl is used to control whether to inline into constructor arguments
-- For example:
--	x = (p,q)		-- Don't inline p or q
--	y = /\a -> (p a, q a)	-- Still don't inline p or q
--	z = f (p,q)		-- Do inline p,q; it may make a rule fire
-- So OccEncl tells enought about the context to know what to do when
-- we encounter a contructor application or PAP.

data OccEncl
  = OccRhs 		-- RHS of let(rec), albeit perhaps inside a type lambda
			-- Don't inline into constructor args here
  | OccVanilla		-- Argument of function, body of lambda, scruintee of case etc.
			-- Do inline into constructor args here

type CtxtTy = [Bool]
	-- []	 	No info
	--
	-- True:ctxt  	Analysing a function-valued expression that will be
	--			applied just once
	--
	-- False:ctxt	Analysing a function-valued expression that may
	--			be applied many times; but when it is, 
	--			the CtxtTy inside applies

initOccEnv :: OccEnv
initOccEnv = OccEnv OccRhs []

vanillaCtxt = OccEnv OccVanilla []
rhsCtxt     = OccEnv OccRhs     []

isRhsEnv (OccEnv OccRhs     _) = True
isRhsEnv (OccEnv OccVanilla _) = False

setCtxt :: OccEnv -> CtxtTy -> OccEnv
setCtxt (OccEnv encl _) ctxt = OccEnv encl ctxt

oneShotGroup :: OccEnv -> [CoreBndr] -> [CoreBndr]
	-- The result binders have one-shot-ness set that they might not have had originally.
	-- This happens in (build (\cn -> e)).  Here the occurrence analyser
	-- linearity context knows that c,n are one-shot, and it records that fact in
	-- the binder. This is useful to guide subsequent float-in/float-out tranformations

oneShotGroup (OccEnv encl ctxt) bndrs 
  = go ctxt bndrs []
  where
    go ctxt [] rev_bndrs = reverse rev_bndrs

    go (lin_ctxt:ctxt) (bndr:bndrs) rev_bndrs
	| isId bndr = go ctxt bndrs (bndr':rev_bndrs)
	where
	  bndr' | lin_ctxt  = setOneShotLambda bndr
		| otherwise = bndr

    go ctxt (bndr:bndrs) rev_bndrs = go ctxt bndrs (bndr:rev_bndrs)

addAppCtxt (OccEnv encl ctxt) args 
  = OccEnv encl (replicate (valArgCount args) True ++ ctxt)
\end{code}

%************************************************************************
%*									*
\subsection[OccurAnal-types]{OccEnv}
%*									*
%************************************************************************

\begin{code}
type UsageDetails = IdEnv OccInfo	-- A finite map from ids to their usage

combineUsageDetails, combineAltsUsageDetails
	:: UsageDetails -> UsageDetails -> UsageDetails

combineUsageDetails usage1 usage2
  = plusVarEnv_C addOccInfo usage1 usage2

combineAltsUsageDetails usage1 usage2
  = plusVarEnv_C orOccInfo usage1 usage2

addOneOcc :: UsageDetails -> Id -> OccInfo -> UsageDetails
addOneOcc usage id info
  = plusVarEnv_C addOccInfo usage (unitVarEnv id info)
	-- ToDo: make this more efficient

emptyDetails = (emptyVarEnv :: UsageDetails)

usedIn :: Id -> UsageDetails -> Bool
v `usedIn` details =  isExportedId v || v `elemVarEnv` details

tagBinders :: UsageDetails	    -- Of scope
	   -> [Id]		    -- Binders
	   -> (UsageDetails, 	    -- Details with binders removed
	      [IdWithOccInfo])    -- Tagged binders

tagBinders usage binders
 = let
     usage' = usage `delVarEnvList` binders
     uss    = map (setBinderOcc usage) binders
   in
   usage' `seq` (usage', uss)

tagBinder :: UsageDetails	    -- Of scope
	  -> Id			    -- Binders
	  -> (UsageDetails, 	    -- Details with binders removed
	      IdWithOccInfo)	    -- Tagged binders

tagBinder usage binder
 = let
     usage'  = usage `delVarEnv` binder
     binder' = setBinderOcc usage binder
   in
   usage' `seq` (usage', binder')

setBinderOcc :: UsageDetails -> CoreBndr -> CoreBndr
setBinderOcc usage bndr
  | isTyVar bndr      = bndr
  | isExportedId bndr = case idOccInfo bndr of
			  NoOccInfo -> bndr
			  other     -> setIdOccInfo bndr NoOccInfo
  	    -- Don't use local usage info for visible-elsewhere things
	    -- BUT *do* erase any IAmALoopBreaker annotation, because we're
	    -- about to re-generate it and it shouldn't be "sticky"
			  
  | otherwise = setIdOccInfo bndr occ_info
  where
    occ_info = lookupVarEnv usage bndr `orElse` IAmDead
\end{code}


%************************************************************************
%*									*
\subsection{Operations over OccInfo}
%*									*
%************************************************************************

\begin{code}
oneOcc :: OccInfo
oneOcc = OneOcc False True

markMany, markInsideLam, markInsideSCC :: OccInfo -> OccInfo

markMany IAmDead = IAmDead
markMany other   = NoOccInfo

markInsideSCC occ = markMany occ

markInsideLam (OneOcc _ one_br) = OneOcc True one_br
markInsideLam occ		= occ

addOccInfo, orOccInfo :: OccInfo -> OccInfo -> OccInfo

addOccInfo IAmDead info2 = info2
addOccInfo info1 IAmDead = info1
addOccInfo info1 info2   = NoOccInfo

-- (orOccInfo orig new) is used
-- when combining occurrence info from branches of a case

orOccInfo IAmDead info2 = info2
orOccInfo info1 IAmDead = info1
orOccInfo (OneOcc in_lam1 one_branch1)
	  (OneOcc in_lam2 one_branch2)
  = OneOcc (in_lam1 || in_lam2)
	   False	-- False, because it occurs in both branches

orOccInfo info1 info2 = NoOccInfo
\end{code}
