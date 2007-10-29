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
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module OccurAnal (
	occurAnalysePgm, occurAnalyseExpr
    ) where

#include "HsVersions.h"

import CoreSyn
import CoreFVs		( idRuleVars )
import CoreUtils	( exprIsTrivial, isDefaultAlt )
import Id
import IdInfo
import BasicTypes	( OccInfo(..), isOneOcc, InterestingCxt )

import VarSet
import VarEnv

import Maybes		( orElse )
import Digraph		( stronglyConnCompR, SCC(..) )
import PrelNames	( buildIdKey, foldrIdKey, runSTRepIdKey, augmentIdKey )
import Unique		( Unique )
import UniqFM		( keysUFM, intersectsUFM, intersectUFM_C, foldUFM_Directly )  
import Util		( mapAndUnzip )
import Outputable

import Data.List
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

occurAnalyseExpr :: CoreExpr -> CoreExpr
	-- Do occurrence analysis, and discard occurence info returned
occurAnalyseExpr expr = snd (occAnal initOccEnv expr)
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
	    -> CoreBind
	    -> UsageDetails		-- Usage details of scope
	    -> (UsageDetails,		-- Of the whole let(rec)
		[CoreBind])

occAnalBind env (NonRec binder rhs) body_usage
  | not (binder `usedIn` body_usage)		-- It's not mentioned
  = (body_usage, [])

  | otherwise			-- It's mentioned in the body
  = (body_usage' +++ addRuleUsage rhs_usage binder,	-- Note [Rules are extra RHSs]
     [NonRec tagged_binder rhs'])
  where
    (body_usage', tagged_binder) = tagBinder body_usage binder
    (rhs_usage, rhs')		 = occAnalRhs env tagged_binder rhs
\end{code}

Note [Dead code]
~~~~~~~~~~~~~~~~
Dropping dead code for recursive bindings is done in a very simple way:

	the entire set of bindings is dropped if none of its binders are
	mentioned in its body; otherwise none are.

This seems to miss an obvious improvement.

	letrec  f = ...g...
		g = ...f...
	in
	...g...
===>
	letrec f = ...g...
	       g = ...(...g...)...
	in
	...g...

Now 'f' is unused! But it's OK!  Dependency analysis will sort this
out into a letrec for 'g' and a 'let' for 'f', and then 'f' will get
dropped.  It isn't easy to do a perfect job in one blow.  Consider

	letrec f = ...g...
	       g = ...h...
	       h = ...k...
	       k = ...m...
	       m = ...m...
	in
	...m...


Note [Loop breaking and RULES]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Loop breaking is surprisingly subtle.  First read the section 4 of 
"Secrets of the GHC inliner".  This describes our basic plan.

However things are made quite a bit more complicated by RULES.  Remember

  * Note [Rules are extra RHSs]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~
    A RULE for 'f' is like an extra RHS for 'f'. That way the "parent"
    keeps the specialised "children" alive.  If the parent dies
    (because it isn't referenced any more), then the children will die
    too (unless they are already referenced directly).

    To that end, we build a Rec group for each cyclic strongly
    connected component,
	*treating f's rules as extra RHSs for 'f'*.
 
    So in Example [eftInt], eftInt and eftIntFB will be put in the
    same Rec, even though their 'main' RHSs are both non-recursive.

  * Note [Rules are visible in their own rec group]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    We want the rules for 'f' to be visible in f's right-hand side.
    And we'd like them to be visible in other function in f's Rec
    group.  E.g. in Example [Specialisation rules] we want f' rule
    to be visible in both f's RHS, and fs's RHS.

    This means that we must simplify the RULEs first, before looking
    at any of the definitions.  This is done by Simplify.simplRecBind,
    when it calls addLetIdInfo.

  * Note [Choosing loop breakers]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    We avoid infinite inlinings by choosing loop breakers, and
    ensuring that a loop breaker cuts each loop.  But what is a
    "loop"?  In particular, a RULES is like an equation for 'f' that
    is *always* inlined if it are applicable.  We do *not* disable
    rules for loop-breakers.  It's up to whoever makes the rules to
    make sure that the rules themselves alwasys terminate.  See Note
    [Rules for recursive functions] in Simplify.lhs

    Hence, if 
	f's RHS mentions g, and
	g has a RULE that mentions h, and
	h has a RULE that mentions f

    then we *must* choose f to be a loop breaker.  In general, take the
    free variables of f's RHS, and augment it with all the variables
    reachable by RULES from those starting points.  That is the whole
    reason for computing rule_fv_env in occAnalBind.  (Of course we
    only consider free vars that are also binders in this Rec group.)

    Note that in Example [eftInt], *neither* eftInt *nor* eftIntFB is
    chosen as a loop breaker, because their RHSs don't mention each other.
    And indeed both can be inlined safely.

    Note that the edges of the graph we use for computing loop breakers
    are not the same as the edges we use for computing the Rec blocks.
    That's why we compute 
	rec_edges	   for the Rec block analysis
	loop_breaker_edges for the loop breaker analysis


  * Note [Weak loop breakers]
    ~~~~~~~~~~~~~~~~~~~~~~~~~
    There is a last nasty wrinkle.  Suppose we have

	Rec { f = f_rhs
              RULE f [] = g
            
	      h = h_rhs
              g = h 
	      ...more...
        }

    Remmber that we simplify the RULES before any RHS (see Note
    [Rules are visible in their own rec group] above).

    So we must *not* postInlineUnconditinoally 'g', even though
    its RHS turns out to be trivial.  (I'm assuming that 'g' is
    not choosen as a loop breaker.)

    We "solve" this by making g a "weak" or "rules-only" loop breaker,
    with OccInfo = IAmLoopBreaker True.  A normal "strong" loop breaker
    has IAmLoopBreaker False.  So

				Inline	postInlineUnconditinoally
	IAmLoopBreaker False	no	no
	IAmLoopBreaker True	yes	no
	other			yes 	yes

    The **sole** reason for this kind of loop breaker is so that
    postInlineUnconditioanlly does not fire.  Ugh.


Example [eftInt]
~~~~~~~~~~~~~~~
Example (from GHC.Enum):

  eftInt :: Int# -> Int# -> [Int]
  eftInt x y = ...(non-recursive)...

  {-# INLINE [0] eftIntFB #-}
  eftIntFB :: (Int -> r -> r) -> r -> Int# -> Int# -> r
  eftIntFB c n x y = ...(non-recursive)...

  {-# RULES
  "eftInt"  [~1] forall x y. eftInt x y = build (\ c n -> eftIntFB c n x y)
  "eftIntList"  [1] eftIntFB  (:) [] = eftInt
   #-}

Example [Specialisation rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this group, which is typical of what SpecConstr builds:

   fs a = ....f (C a)....
   f  x = ....f (C a)....
   {-# RULE f (C a) = fs a #-}

So 'f' and 'fs' are in the same Rec group (since f refers to fs via its RULE).

But watch out!  If 'fs' is not chosen as a loop breaker, we may get an infinite loop:
	- the RULE is applied in f's RHS (see Note [Self-recursive rules] in Simplify
	- fs is inlined (say it's small)
	- now there's another opportunity to apply the RULE

This showed up when compiling Control.Concurrent.Chan.getChanContents.


\begin{code}
occAnalBind env (Rec pairs) body_usage
  | not (any (`usedIn` body_usage) bndrs)	-- NB: look at body_usage, not total_usage
  = (body_usage, [])				-- Dead code
  | otherwise
  = (final_usage, map ({-# SCC "occAnalBind.dofinal" #-} do_final_bind) sccs)
  where
    bndrs    = map fst pairs
    bndr_set = mkVarSet bndrs

	---------------------------------------
	-- See Note [Loop breaking]
	---------------------------------------

    -------------Dependency analysis ------------------------------
    occ_anald :: [(Id, (UsageDetails, CoreExpr))]
	-- The UsageDetails here are strictly those arising from the RHS
	-- *not* from any rules in the Id
    occ_anald = [(bndr, occAnalRhs env bndr rhs) | (bndr,rhs) <- pairs]

    total_usage        = foldl add_usage body_usage occ_anald
    add_usage body_usage (bndr, (rhs_usage, _))
	= body_usage +++ addRuleUsage rhs_usage bndr

    (final_usage, tagged_bndrs) = tagBinders total_usage bndrs
    final_bndrs | no_rules  = tagged_bndrs
		| otherwise = map tag_rule_var tagged_bndrs
    tag_rule_var bndr | bndr `elemVarSet` all_rule_fvs = makeLoopBreaker True bndr
	    	      | otherwise		       = bndr

    ---- stuff for dependency analysis of binds -------------------------------
    sccs :: [SCC (Node Details)]
    sccs = {-# SCC "occAnalBind.scc" #-} stronglyConnCompR rec_edges

    rec_edges :: [Node Details]	-- The binders are tagged with correct occ-info
    rec_edges = {-# SCC "occAnalBind.assoc" #-} zipWith make_node final_bndrs occ_anald
    make_node tagged_bndr (_bndr, (rhs_usage, rhs))
	= ((tagged_bndr, rhs, rhs_fvs), idUnique tagged_bndr, out_edges)
	where
	  rhs_fvs = intersectUFM_C (\b _ -> b) bndr_set rhs_usage
	  out_edges = keysUFM (rhs_fvs `unionVarSet` idRuleVars tagged_bndr)
	

	-- (a -> b) means a mentions b
	-- Given the usage details (a UFM that gives occ info for each free var of
	-- the RHS) we can get the list of free vars -- or rather their Int keys --
	-- by just extracting the keys from the finite map.  Grimy, but fast.
	-- Previously we had this:
	-- 	[ bndr | bndr <- bndrs,
	--		 maybeToBool (lookupVarEnv rhs_usage bndr)]
	-- which has n**2 cost, and this meant that edges_from alone 
	-- consumed 10% of total runtime!

    ---- Stuff to "re-constitute" bindings from dependency-analysis info ------
    do_final_bind (AcyclicSCC ((bndr, rhs, _), _, _)) = NonRec bndr rhs
    do_final_bind (CyclicSCC cycle)
	| no_rules  = Rec (reOrderCycle cycle)
	| otherwise = Rec (concatMap reOrderRec (stronglyConnCompR loop_breaker_edges))
	where	-- See Note [Loop breaking for reason for looop_breker_edges]
	  loop_breaker_edges = map mk_node cycle
	  mk_node (details@(bndr, rhs, rhs_fvs), k, _) = (details, k, new_ks)
		where
		  new_ks = keysUFM (extendFvs rule_fv_env rhs_fvs rhs_fvs)

	
    ------------------------------------
    rule_fv_env :: IdEnv IdSet	-- Variables from this group mentioned in RHS of rules
				-- Domain is *subset* of bound vars (others have no rule fvs)
    rule_fv_env = rule_loop init_rule_fvs

    no_rules      = null init_rule_fvs
    all_rule_fvs  = foldr (unionVarSet . snd) emptyVarSet init_rule_fvs
    init_rule_fvs = [(b, rule_fvs)
		    | b <- bndrs 
		    , let rule_fvs = idRuleVars b `intersectVarSet` bndr_set
		    , not (isEmptyVarSet rule_fvs)]

    rule_loop :: [(Id,IdSet)] -> IdEnv IdSet	-- Finds fixpoint
    rule_loop fv_list 
	| no_change = env
	| otherwise = rule_loop new_fv_list
	where
	  env = mkVarEnv init_rule_fvs
	  (no_change, new_fv_list) = mapAccumL bump True fv_list
	  bump no_change (b,fvs) 
		| new_fvs `subVarSet` fvs = (no_change, (b,fvs))
		| otherwise		  = (False,     (b,new_fvs `unionVarSet` fvs))
		where
		  new_fvs = extendFvs env emptyVarSet fvs

extendFvs :: IdEnv IdSet -> IdSet -> IdSet -> IdSet
-- (extendFVs env fvs s) returns (fvs `union` env(s))
extendFvs env fvs id_set
  = foldUFM_Directly add fvs id_set
  where
    add uniq _ fvs 
	= case lookupVarEnv_Directly env uniq  of
	    Just fvs' -> fvs' `unionVarSet` fvs
	    Nothing   -> fvs
\end{code}

@reOrderRec@ is applied to the list of (binder,rhs) pairs for a cyclic
strongly connected component (there's guaranteed to be a cycle).  It returns the
same pairs, but 
	a) in a better order,
	b) with some of the Ids having a IAmALoopBreaker pragma

The "loop-breaker" Ids are sufficient to break all cycles in the SCC.  This means
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


\begin{code}
type Node details = (details, Unique, [Unique])	-- The Ints are gotten from the Unique,
						-- which is gotten from the Id.
type Details = (Id, 		-- Binder
		CoreExpr,	-- RHS
		IdSet)		-- RHS free vars (*not* include rules)

reOrderRec :: SCC (Node Details)
	   -> [(Id,CoreExpr)]
-- Sorted into a plausible order.  Enough of the Ids have
--	IAmALoopBreaker pragmas that there are no loops left.
reOrderRec (AcyclicSCC ((bndr, rhs, _), _, _)) = [(bndr, rhs)]
reOrderRec (CyclicSCC cycle)  		       = reOrderCycle cycle

reOrderCycle :: [Node Details] -> [(Id,CoreExpr)]
reOrderCycle []
  = panic "reOrderCycle"
reOrderCycle [bind]	-- Common case of simple self-recursion
  = [(makeLoopBreaker False bndr, rhs)]
  where
    ((bndr, rhs, _), _, _) = bind

reOrderCycle (bind : binds)
  = 	-- Choose a loop breaker, mark it no-inline,
	-- do SCC analysis on the rest, and recursively sort them out
    concatMap reOrderRec (stronglyConnCompR unchosen) ++
    [(makeLoopBreaker False bndr, rhs)]

  where
    (chosen_bind, unchosen) = choose_loop_breaker bind (score bind) [] binds
    (bndr, rhs, _)  = chosen_bind

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
	  
    score :: Node Details -> Int	-- Higher score => less likely to be picked as loop breaker
    score ((bndr, rhs, _), _, _)
        | workerExists (idWorkerInfo bndr)      = 10
                -- Note [Worker inline loop]

	| exprIsTrivial rhs 	   = 4	-- Practically certain to be inlined
		-- Used to have also: && not (isExportedId bndr)
		-- But I found this sometimes cost an extra iteration when we have
		--	rec { d = (a,b); a = ...df...; b = ...df...; df = d }
		-- where df is the exported dictionary. Then df makes a really
		-- bad choice for loop breaker
	  
	| is_con_app rhs = 2	-- Data types help with cases
                -- Note [conapp]

	| inlineCandidate bndr rhs = 1	-- Likely to be inlined
		-- Note [Inline candidates]

	| otherwise = 0

    inlineCandidate :: Id -> CoreExpr -> Bool
    inlineCandidate id (Note InlineMe _) = True
    inlineCandidate id rhs	         = isOneOcc (idOccInfo id)

        -- Note [conapp]
        --
        -- It's really really important to inline dictionaries.  Real
        -- example (the Enum Ordering instance from GHC.Base):
        --
	--	rec	f = \ x -> case d of (p,q,r) -> p x
	--		g = \ x -> case d of (p,q,r) -> q x
	--		d = (v, f, g)
	--
	-- Here, f and g occur just once; but we can't inline them into d.
	-- On the other hand we *could* simplify those case expressions if
	-- we didn't stupidly choose d as the loop breaker.
	-- But we won't because constructor args are marked "Many".
        -- Inlining dictionaries is really essential to unravelling
        -- the loops in static numeric dictionaries, see GHC.Float.

	-- Cheap and cheerful; the simplifer moves casts out of the way
	-- The lambda case is important to spot x = /\a. C (f a)
	-- which comes up when C is a dictionary constructor and
	-- f is a default method.  
	-- Example: the instance for Show (ST s a) in GHC.ST
	--
	-- However we *also* treat (\x. C p q) as a con-app-like thing, 
	-- 	Note [Closure conversion]
    is_con_app (Var v)    = isDataConWorkId v
    is_con_app (App f _)  = is_con_app f
    is_con_app (Lam b e)  = is_con_app e
    is_con_app (Note _ e) = is_con_app e
    is_con_app other      = False

makeLoopBreaker :: Bool -> Id -> Id
-- Set the loop-breaker flag
-- See Note [Weak loop breakers]
makeLoopBreaker weak bndr = setIdOccInfo bndr (IAmALoopBreaker weak)
\end{code}

Note [Worker inline loop]
~~~~~~~~~~~~~~~~~~~~~~~~
Never choose a wrapper as the loop breaker!  Because
wrappers get auto-generated inlinings when importing, and
that can lead to an infinite inlining loop.  For example:
  rec {
	$wfoo x = ....foo x....
	
	{-loop brk-} foo x = ...$wfoo x...
  }

The interface file sees the unfolding for $wfoo, and sees that foo is
strict (and hence it gets an auto-generated wrapper).  Result: an
infinite inlining in the importing scope.  So be a bit careful if you
change this.  A good example is Tree.repTree in
nofib/spectral/minimax. If the repTree wrapper is chosen as the loop
breaker then compiling Game.hs goes into an infinite loop (this
happened when we gave is_con_app a lower score than inline candidates).

Note [Closure conversion]
~~~~~~~~~~~~~~~~~~~~~~~~~
We treat (\x. C p q) as a high-score candidate in the letrec scoring algorithm.
The immediate motivation came from the result of a closure-conversion transformation
which generated code like this:

    data Clo a b = forall c. Clo (c -> a -> b) c

    ($:) :: Clo a b -> a -> b
    Clo f env $: x = f env x

    rec { plus = Clo plus1 ()

        ; plus1 _ n = Clo plus2 n

	; plus2 Zero     n = n
	; plus2 (Succ m) n = Succ (plus $: m $: n) }

If we inline 'plus' and 'plus1', everything unravels nicely.  But if
we choose 'plus1' as the loop breaker (which is entirely possible
otherwise), the loop does not unravel nicely.


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
  = occAnal ctxt rhs
  where
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
			    OneOcc in_lam one_br _ -> not in_lam && one_br
			    other		   -> False
\end{code}



\begin{code}
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
occAnal env (Var v)   = (mkOneOcc env v False, Var v)
    -- At one stage, I gathered the idRuleVars for v here too,
    -- which in a way is the right thing to do.
    -- Btu that went wrong right after specialisation, when
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

occAnal env (Cast expr co)
  = case occAnal env expr of { (usage, expr') ->
    (markRhsUds env True usage, Cast expr' co)
	-- If we see let x = y `cast` co
	-- then mark y as 'Many' so that we don't
	-- immediately inline y again. 
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
  = case occ_anal_scrut scrut alts		    of { (scrut_usage, scrut') ->
    case mapAndUnzip (occAnalAlt alt_env bndr) alts of { (alts_usage_s, alts')   -> 
    let
	alts_usage  = foldr1 combineAltsUsageDetails alts_usage_s
	alts_usage' = addCaseBndrUsage alts_usage
	(alts_usage1, tagged_bndr) = tagBinder alts_usage' bndr
        total_usage = scrut_usage +++ alts_usage1
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

    alt_env = setVanillaCtxt env
	-- Consider 	x = case v of { True -> (p,q); ... }
	-- Then it's fine to inline p and q

    occ_anal_scrut (Var v) (alt1 : other_alts)
				| not (null other_alts) || not (isDefaultAlt alt1)
			        = (mkOneOcc env v True, Var v)
    occ_anal_scrut scrut alts   = occAnal vanillaCtxt scrut
					-- No need for rhsCtxt

occAnal env (Let bind body)
  = case occAnal env body     	         of { (body_usage, body') ->
    case occAnalBind env bind body_usage of { (final_usage, new_binds) ->
       (final_usage, mkLets new_binds body') }}

occAnalArgs env args
  = case mapAndUnzip (occAnal arg_env) args of	{ (arg_uds_s, args') ->
    (foldr (+++) emptyDetails arg_uds_s, args')}
  where
    arg_env = vanillaCtxt
\end{code}

Applications are dealt with specially because we want
the "build hack" to work.

\begin{code}
occAnalApp env (Var fun, args) is_rhs
  = case args_stuff of { (args_uds, args') ->
    let
        final_args_uds = markRhsUds env is_pap args_uds
    in
    (fun_uds +++ final_args_uds, mkApps (Var fun) args') }
  where
    fun_uniq = idUnique fun
    fun_uds  = mkOneOcc env fun (valArgCount args > 0)
    is_pap = isDataConWorkId fun || valArgCount args < idArity fun

		-- Hack for build, fold, runST
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
	final_uds = fun_uds +++ args_uds
    in
    (final_uds, mkApps fun' args') }}
    

markRhsUds :: OccEnv		-- Check if this is a RhsEnv
	   -> Bool		-- and this is true
	   -> UsageDetails	-- The do markMany on this
	   -> UsageDetails
-- We mark the free vars of the argument of a constructor or PAP 
-- as "many", if it is the RHS of a let(rec).
-- This means that nothing gets inlined into a constructor argument
-- position, which is what we want.  Typically those constructor
-- arguments are just variables, or trivial expressions.
--
-- This is the *whole point* of the isRhsEnv predicate
markRhsUds env is_pap arg_uds
  | isRhsEnv env && is_pap = mapVarEnv markMany arg_uds
  | otherwise 		   = arg_uds


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
	(arg_uds +++ args_uds, arg':args') }}
    
    go n (arg:args)
      = case occAnal arg_env arg of	{ (arg_uds, arg') ->
	case go (n-1) args of		{ (args_uds, args') ->
	(arg_uds +++ args_uds, arg':args') }}
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

Note [Aug 06]: I don't think this is necessary any more, and it helpe
	       to know when binders are unused.  See esp the call to
	       isDeadBinder in Simplify.mkDupableAlt

\begin{code}
occAnalAlt env case_bndr (con, bndrs, rhs)
  = case occAnal env rhs of { (rhs_usage, rhs') ->
    let
        (final_usage, tagged_bndrs) = tagBinders rhs_usage bndrs
	final_bndrs = tagged_bndrs	-- See Note [Aug06] above
{-
	final_bndrs | case_bndr `elemVarEnv` final_usage = bndrs
		    | otherwise				= tagged_bndrs
		-- Leave the binders untagged if the case 
		-- binder occurs at all; see note above
-}
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

setVanillaCtxt :: OccEnv -> OccEnv
setVanillaCtxt (OccEnv OccRhs ctxt_ty) = OccEnv OccVanilla ctxt_ty
setVanillaCtxt other_env	       = other_env

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

(+++), combineAltsUsageDetails
	:: UsageDetails -> UsageDetails -> UsageDetails

(+++) usage1 usage2
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

type IdWithOccInfo = Id

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
mkOneOcc :: OccEnv -> Id -> InterestingCxt -> UsageDetails
mkOneOcc env id int_cxt
  | isLocalId id = unitVarEnv id (OneOcc False True int_cxt)
  | otherwise    = emptyDetails

markMany, markInsideLam, markInsideSCC :: OccInfo -> OccInfo

markMany IAmDead = IAmDead
markMany other   = NoOccInfo

markInsideSCC occ = markMany occ

markInsideLam (OneOcc _ one_br int_cxt) = OneOcc True one_br int_cxt
markInsideLam occ		 	= occ

addOccInfo, orOccInfo :: OccInfo -> OccInfo -> OccInfo

addOccInfo IAmDead info2       = info2
addOccInfo info1 IAmDead       = info1
addOccInfo info1 info2         = NoOccInfo

-- (orOccInfo orig new) is used
-- when combining occurrence info from branches of a case

orOccInfo IAmDead info2 = info2
orOccInfo info1 IAmDead = info1
orOccInfo (OneOcc in_lam1 one_branch1 int_cxt1)
	  (OneOcc in_lam2 one_branch2 int_cxt2)
  = OneOcc (in_lam1 || in_lam2)
	   False	-- False, because it occurs in both branches
	   (int_cxt1 && int_cxt2)
orOccInfo info1 info2 = NoOccInfo
\end{code}
