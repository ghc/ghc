%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
%************************************************************************
%*                                                                      *
\section[OccurAnal]{Occurrence analysis pass}
%*                                                                      *
%************************************************************************

The occurrence analyser re-typechecks a core expression, returning a new
core expression with (hopefully) improved usage information.

\begin{code}
module OccurAnal (
        occurAnalysePgm, occurAnalyseExpr
    ) where

#include "HsVersions.h"

import CoreSyn
import CoreFVs
import Type		( tyVarsOfType )
import CoreUtils        ( exprIsTrivial, isDefaultAlt, mkCoerceI, isExpandableApp )
import Coercion		( CoercionI(..), mkSymCoI )
import Id
import Name		( localiseName )
import BasicTypes

import VarSet
import VarEnv

import Maybes           ( orElse )
import Digraph          ( SCC(..), stronglyConnCompFromEdgedVerticesR )
import PrelNames        ( buildIdKey, foldrIdKey, runSTRepIdKey, augmentIdKey )
import Unique           ( Unique )
import UniqFM           ( keysUFM, intersectUFM_C, foldUFM_Directly )
import Util             ( mapAndUnzip, filterOut )
import Bag
import Outputable
import FastString
import Data.List
\end{code}


%************************************************************************
%*                                                                      *
\subsection[OccurAnal-main]{Counting occurrences: main function}
%*                                                                      *
%************************************************************************

Here's the externally-callable interface:

\begin{code}
occurAnalysePgm :: [CoreBind] -> [CoreRule] -> [CoreBind]
occurAnalysePgm binds rules
  = snd (go initOccEnv binds)
  where
    initial_details = addIdOccs emptyDetails (rulesFreeVars rules)
    -- The RULES keep things alive!

    go :: OccEnv -> [CoreBind] -> (UsageDetails, [CoreBind])
    go _ []
        = (initial_details, [])
    go env (bind:binds)
        = (final_usage, bind' ++ binds')
        where
           (bs_usage, binds')   = go env binds
           (final_usage, bind') = occAnalBind env env bind bs_usage

occurAnalyseExpr :: CoreExpr -> CoreExpr
        -- Do occurrence analysis, and discard occurence info returned
occurAnalyseExpr expr = snd (occAnal initOccEnv expr)
\end{code}


%************************************************************************
%*                                                                      *
\subsection[OccurAnal-main]{Counting occurrences: main function}
%*                                                                      *
%************************************************************************

Bindings
~~~~~~~~

\begin{code}
occAnalBind :: OccEnv 		-- The incoming OccEnv
	    -> OccEnv		-- Same, but trimmed by (binderOf bind)
            -> CoreBind
            -> UsageDetails             -- Usage details of scope
            -> (UsageDetails,           -- Of the whole let(rec)
                [CoreBind])

occAnalBind env _ (NonRec binder rhs) body_usage
  | isTyVar binder			-- A type let; we don't gather usage info
  = (body_usage, [NonRec binder rhs])

  | not (binder `usedIn` body_usage)    -- It's not mentioned
  = (body_usage, [])

  | otherwise                   -- It's mentioned in the body
  = (body_usage' +++ addRuleUsage rhs_usage binder,     -- Note [Rules are extra RHSs]
     [NonRec tagged_binder rhs'])
  where
    (body_usage', tagged_binder) = tagBinder body_usage binder
    (rhs_usage, rhs')            = occAnalRhs env tagged_binder rhs
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

    When we make the Rec groups we include variables free in *either*
    LHS *or* RHS of the rule.  The former might seems silly, but see
    Note [Rule dependency info].

    So in Example [eftInt], eftInt and eftIntFB will be put in the
    same Rec, even though their 'main' RHSs are both non-recursive.

  * Note [Rules are visible in their own rec group]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    We want the rules for 'f' to be visible in f's right-hand side.
    And we'd like them to be visible in other functions in f's Rec
    group.  E.g. in Example [Specialisation rules] we want f' rule
    to be visible in both f's RHS, and fs's RHS.

    This means that we must simplify the RULEs first, before looking
    at any of the definitions.  This is done by Simplify.simplRecBind,
    when it calls addLetIdInfo.

  * Note [Choosing loop breakers]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    We avoid infinite inlinings by choosing loop breakers, and
    ensuring that a loop breaker cuts each loop.  But what is a
    "loop"?  In particular, a RULE is like an equation for 'f' that
    is *always* inlined if it is applicable.  We do *not* disable
    rules for loop-breakers.  It's up to whoever makes the rules to
    make sure that the rules themselves always terminate.  See Note
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

    Note that when we compute this rule_fv_env, we only consider variables
    free in the *RHS* of the rule, in contrast to the way we build the
    Rec group in the first place (Note [Rule dependency info])

    Note that in Example [eftInt], *neither* eftInt *nor* eftIntFB is
    chosen as a loop breaker, because their RHSs don't mention each other.
    And indeed both can be inlined safely.

    Note that the edges of the graph we use for computing loop breakers
    are not the same as the edges we use for computing the Rec blocks.
    That's why we compute
        rec_edges          for the Rec block analysis
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

    So we must *not* postInlineUnconditionally 'g', even though
    its RHS turns out to be trivial.  (I'm assuming that 'g' is
    not choosen as a loop breaker.)  Why not?  Because then we
    drop the binding for 'g', which leaves it out of scope in the
    RULE!

    We "solve" this by making g a "weak" or "rules-only" loop breaker,
    with OccInfo = IAmLoopBreaker True.  A normal "strong" loop breaker
    has IAmLoopBreaker False.  So

                                Inline  postInlineUnconditionally
        IAmLoopBreaker False    no      no
        IAmLoopBreaker True     yes     no
        other                   yes     yes

    The **sole** reason for this kind of loop breaker is so that
    postInlineUnconditionally does not fire.  Ugh.

  * Note [Rule dependency info]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~
    The VarSet in a SpecInfo is used for dependency analysis in the
    occurrence analyser.  We must track free vars in *both* lhs and rhs.  
    Hence use of idRuleVars, rather than idRuleRhsVars in addRuleUsage.  
    Why both? Consider
        x = y
        RULE f x = 4
    Then if we substitute y for x, we'd better do so in the
    rule's LHS too, so we'd better ensure the dependency is respected


  * Note [Inline rules]
    ~~~~~~~~~~~~~~~~~~~
    None of the above stuff about RULES applies to Inline Rules,
    stored in a CoreUnfolding.  The unfolding, if any, is simplified
    at the same time as the regular RHS of the function, so it should
    be treated *exactly* like an extra RHS.


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
occAnalBind _ env (Rec pairs) body_usage
  = foldr occAnalRec (body_usage, []) sccs
	-- For a recursive group, we 
	--	* occ-analyse all the RHSs
	--	* compute strongly-connected components
	--	* feed those components to occAnalRec
  where
    -------------Dependency analysis ------------------------------
    bndr_set = mkVarSet (map fst pairs)

    sccs :: [SCC (Node Details)]
    sccs = {-# SCC "occAnalBind.scc" #-} stronglyConnCompFromEdgedVerticesR rec_edges

    rec_edges :: [Node Details]
    rec_edges = {-# SCC "occAnalBind.assoc" #-}  map make_node pairs
    
    make_node (bndr, rhs)
	= (ND bndr rhs' all_rhs_usage rhs_fvs, idUnique bndr, out_edges)
	where
	  (rhs_usage, rhs') = occAnalRhs env bndr rhs
	  all_rhs_usage = addRuleUsage rhs_usage bndr    -- Note [Rules are extra RHSs]
	  rhs_fvs = intersectUFM_C (\b _ -> b) bndr_set rhs_usage
	  out_edges = keysUFM (rhs_fvs `unionVarSet` idRuleVars bndr)
        -- (a -> b) means a mentions b
        -- Given the usage details (a UFM that gives occ info for each free var of
        -- the RHS) we can get the list of free vars -- or rather their Int keys --
        -- by just extracting the keys from the finite map.  Grimy, but fast.
        -- Previously we had this:
        --      [ bndr | bndr <- bndrs,
        --               maybeToBool (lookupVarEnv rhs_usage bndr)]
        -- which has n**2 cost, and this meant that edges_from alone
        -- consumed 10% of total runtime!

-----------------------------
occAnalRec :: SCC (Node Details) -> (UsageDetails, [CoreBind])
				 -> (UsageDetails, [CoreBind])

	-- The NonRec case is just like a Let (NonRec ...) above
occAnalRec (AcyclicSCC (ND bndr rhs rhs_usage _, _, _)) (body_usage, binds)
  | not (bndr `usedIn` body_usage) 
  = (body_usage, binds)

  | otherwise			-- It's mentioned in the body
  = (body_usage' +++ rhs_usage,	
     NonRec tagged_bndr rhs : binds)
  where
    (body_usage', tagged_bndr) = tagBinder body_usage bndr


	-- The Rec case is the interesting one
	-- See Note [Loop breaking]
occAnalRec (CyclicSCC nodes) (body_usage, binds)
  | not (any (`usedIn` body_usage) bndrs)	-- NB: look at body_usage, not total_usage
  = (body_usage, binds)				-- Dead code

  | otherwise	-- At this point we always build a single Rec
  = (final_usage, Rec pairs : binds)

  where
    bndrs    = [b | (ND b _ _ _, _, _) <- nodes]
    bndr_set = mkVarSet bndrs

	----------------------------
	-- Tag the binders with their occurrence info
    total_usage = foldl add_usage body_usage nodes
    add_usage usage_so_far (ND _ _ rhs_usage _, _, _) = usage_so_far +++ rhs_usage
    (final_usage, tagged_nodes) = mapAccumL tag_node total_usage nodes

    tag_node :: UsageDetails -> Node Details -> (UsageDetails, Node Details)
	-- (a) Tag the binders in the details with occ info
	-- (b) Mark the binder with "weak loop-breaker" OccInfo 
	--	saying "no preInlineUnconditionally" if it is used
	-- 	in any rule (lhs or rhs) of the recursive group
	--      See Note [Weak loop breakers]
    tag_node usage (ND bndr rhs rhs_usage rhs_fvs, k, ks)
      = (usage `delVarEnv` bndr, (ND bndr2 rhs rhs_usage rhs_fvs, k, ks))
      where
	bndr2 | bndr `elemVarSet` all_rule_fvs = makeLoopBreaker True bndr1
	      | otherwise  		       = bndr1
	bndr1 = setBinderOcc usage bndr
    all_rule_fvs = bndr_set `intersectVarSet` foldr (unionVarSet . idRuleVars) 
						    emptyVarSet bndrs

	----------------------------
	-- Now reconstruct the cycle
    pairs | no_rules  = reOrderCycle 0 tagged_nodes []
 	  | otherwise = foldr (reOrderRec 0) [] $
			stronglyConnCompFromEdgedVerticesR loop_breaker_edges

	-- See Note [Choosing loop breakers] for loop_breaker_edges
    loop_breaker_edges = map mk_node tagged_nodes
    mk_node (details@(ND _ _ _ rhs_fvs), k, _) = (details, k, new_ks)
	where
	  new_ks = keysUFM (extendFvs rule_fv_env rhs_fvs rhs_fvs)

    ------------------------------------
    rule_fv_env :: IdEnv IdSet  -- Variables from this group mentioned in RHS of rules
                                -- Domain is *subset* of bound vars (others have no rule fvs)
    rule_fv_env = rule_loop init_rule_fvs

    no_rules      = null init_rule_fvs
    init_rule_fvs = [(b, rule_fvs)
                    | b <- bndrs
                    , let rule_fvs = idRuleRhsVars b `intersectVarSet` bndr_set
                    , not (isEmptyVarSet rule_fvs)]

    rule_loop :: [(Id,IdSet)] -> IdEnv IdSet    -- Finds fixpoint
    rule_loop fv_list
        | no_change = env
        | otherwise = rule_loop new_fv_list
        where
          env = mkVarEnv init_rule_fvs
          (no_change, new_fv_list) = mapAccumL bump True fv_list
          bump no_change (b,fvs)
                | new_fvs `subVarSet` fvs = (no_change, (b,fvs))
                | otherwise               = (False,     (b,new_fvs `unionVarSet` fvs))
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
data Details = ND Id 		-- Binder
		  CoreExpr	-- RHS

		  UsageDetails	-- Full usage from RHS, 
                                -- including *both* RULES *and* InlineRule unfolding

		  IdSet		-- Other binders *from this Rec group* mentioned in
		  		--   * the  RHS
		  		--   * any InlineRule unfolding
				-- but *excluding* any RULES

reOrderRec :: Int -> SCC (Node Details)
           -> [(Id,CoreExpr)] -> [(Id,CoreExpr)]
-- Sorted into a plausible order.  Enough of the Ids have
--      IAmALoopBreaker pragmas that there are no loops left.
reOrderRec _ (AcyclicSCC (ND bndr rhs _ _, _, _)) pairs = (bndr, rhs) : pairs
reOrderRec depth (CyclicSCC cycle)  		  pairs = reOrderCycle depth cycle pairs

reOrderCycle :: Int -> [Node Details] -> [(Id,CoreExpr)] -> [(Id,CoreExpr)]
reOrderCycle _ [] _
  = panic "reOrderCycle"
reOrderCycle _ [bind] pairs    -- Common case of simple self-recursion
  = (makeLoopBreaker False bndr, rhs) : pairs
  where
    (ND bndr rhs _ _, _, _) = bind

reOrderCycle depth (bind : binds) pairs
  =     -- Choose a loop breaker, mark it no-inline,
        -- do SCC analysis on the rest, and recursively sort them out
--    pprTrace "reOrderCycle" (ppr [b | (ND b _ _ _, _, _) <- bind:binds]) $
    foldr (reOrderRec new_depth)
          ([ (makeLoopBreaker False bndr, rhs) 
           | (ND bndr rhs _ _, _, _) <- chosen_binds] ++ pairs)
	  (stronglyConnCompFromEdgedVerticesR unchosen) 
  where
    (chosen_binds, unchosen) = choose_loop_breaker [bind] (score bind) [] binds

    approximate_loop_breaker = depth >= 2
    new_depth | approximate_loop_breaker = 0
	      | otherwise		 = depth+1
	-- After two iterations (d=0, d=1) give up
	-- and approximate, returning to d=0

        -- This loop looks for the bind with the lowest score
        -- to pick as the loop  breaker.  The rest accumulate in
    choose_loop_breaker loop_binds _loop_sc acc []
        = (loop_binds, acc)        -- Done

	-- If approximate_loop_breaker is True, we pick *all*
	-- nodes with lowest score, else just one
	-- See Note [Complexity of loop breaking]
    choose_loop_breaker loop_binds loop_sc acc (bind : binds)
        | sc < loop_sc  -- Lower score so pick this new one
        = choose_loop_breaker [bind] sc (loop_binds ++ acc) binds

	| approximate_loop_breaker && sc == loop_sc
	= choose_loop_breaker (bind : loop_binds) loop_sc acc binds
	
        | otherwise     -- Higher score so don't pick it
        = choose_loop_breaker loop_binds loop_sc (bind : acc) binds
        where
          sc = score bind

    score :: Node Details -> Int        -- Higher score => less likely to be picked as loop breaker
    score (ND bndr rhs _ _, _, _)
        | isDFunId bndr = 9   -- Never choose a DFun as a loop breaker
	   	     	      -- Note [DFuns should not be loop breakers]

        | Just (inl_source, _) <- isInlineRule_maybe (idUnfolding bndr)
	= case inl_source of
	     InlineWrapper {} -> 10  -- Note [INLINE pragmas]
	     _other	      ->  3  -- Data structures are more important than this
	     		             -- so that dictionary/method recursion unravels
		-- Note that this case hits all InlineRule things, so we
		-- never look at 'rhs for InlineRule stuff. That's right, because
		-- 'rhs' is irrelevant for inlining things with an InlineRule
                
        | is_con_app rhs = 5  -- Data types help with cases: Note [Constructor applications]
                
        | exprIsTrivial rhs = 10  -- Practically certain to be inlined
                -- Used to have also: && not (isExportedId bndr)
                -- But I found this sometimes cost an extra iteration when we have
                --      rec { d = (a,b); a = ...df...; b = ...df...; df = d }
                -- where df is the exported dictionary. Then df makes a really
                -- bad choice for loop breaker

	
-- If an Id is marked "never inline" then it makes a great loop breaker
-- The only reason for not checking that here is that it is rare
-- and I've never seen a situation where it makes a difference,
-- so it probably isn't worth the time to test on every binder
--	| isNeverActive (idInlinePragma bndr) = -10

        | isOneOcc (idOccInfo bndr) = 2  -- Likely to be inlined

        | canUnfold (realIdUnfolding bndr) = 1
                -- The Id has some kind of unfolding
		-- Ignore loop-breaker-ness here because that is what we are setting!

        | otherwise = 0

	-- Checking for a constructor application
        -- Cheap and cheerful; the simplifer moves casts out of the way
        -- The lambda case is important to spot x = /\a. C (f a)
        -- which comes up when C is a dictionary constructor and
        -- f is a default method.
        -- Example: the instance for Show (ST s a) in GHC.ST
        --
        -- However we *also* treat (\x. C p q) as a con-app-like thing,
        --      Note [Closure conversion]
    is_con_app (Var v)    = isConLikeId v
    is_con_app (App f _)  = is_con_app f
    is_con_app (Lam _ e)  = is_con_app e
    is_con_app (Note _ e) = is_con_app e
    is_con_app _          = False

makeLoopBreaker :: Bool -> Id -> Id
-- Set the loop-breaker flag: see Note [Weak loop breakers]
makeLoopBreaker weak bndr = setIdOccInfo bndr (IAmALoopBreaker weak)
\end{code}

Note [Complexity of loop breaking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The loop-breaking algorithm knocks out one binder at a time, and 
performs a new SCC analysis on the remaining binders.  That can
behave very badly in tightly-coupled groups of bindings; in the
worst case it can be (N**2)*log N, because it does a full SCC
on N, then N-1, then N-2 and so on.

To avoid this, we switch plans after 2 (or whatever) attempts:
  Plan A: pick one binder with the lowest score, make it
	  a loop breaker, and try again
  Plan B: pick *all* binders with the lowest score, make them
	  all loop breakers, and try again 
Since there are only a small finite number of scores, this will
terminate in a constant number of iterations, rather than O(N)
iterations.

You might thing that it's very unlikely, but RULES make it much
more likely.  Here's a real example from Trac #1969:
  Rec { $dm = \d.\x. op d
	{-# RULES forall d. $dm Int d  = $s$dm1
		  forall d. $dm Bool d = $s$dm2 #-}
	
	dInt = MkD .... opInt ...
	dInt = MkD .... opBool ...
	opInt  = $dm dInt
	opBool = $dm dBool

	$s$dm1 = \x. op dInt
	$s$dm2 = \x. op dBool }
The RULES stuff means that we can't choose $dm as a loop breaker
(Note [Choosing loop breakers]), so we must choose at least (say)
opInt *and* opBool, and so on.  The number of loop breakders is
linear in the number of instance declarations.

Note [INLINE pragmas]
~~~~~~~~~~~~~~~~~~~~~
Avoid choosing a function with an INLINE pramga as the loop breaker!  
If such a function is mutually-recursive with a non-INLINE thing,
then the latter should be the loop-breaker.

Usually this is just a question of optimisation. But a particularly
bad case is wrappers generated by the demand analyser: if you make
then into a loop breaker you may get an infinite inlining loop.  For
example:
  rec {
        $wfoo x = ....foo x....

        {-loop brk-} foo x = ...$wfoo x...
  }
The interface file sees the unfolding for $wfoo, and sees that foo is
strict (and hence it gets an auto-generated wrapper).  Result: an
infinite inlining in the importing scope.  So be a bit careful if you
change this.  A good example is Tree.repTree in
nofib/spectral/minimax. If the repTree wrapper is chosen as the loop
breaker then compiling Game.hs goes into an infinite loop.  This
happened when we gave is_con_app a lower score than inline candidates:

  Tree.repTree
    = __inline_me (/\a. \w w1 w2 -> 
                   case Tree.$wrepTree @ a w w1 w2 of
                    { (# ww1, ww2 #) -> Branch @ a ww1 ww2 })
  Tree.$wrepTree
    = /\a w w1 w2 -> 
      (# w2_smP, map a (Tree a) (Tree.repTree a w1 w) (w w2) #)

Here we do *not* want to choose 'repTree' as the loop breaker.

Note [DFuns should not be loop breakers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's particularly bad to make a DFun into a loop breaker.  See
Note [How instance declarations are translated] in TcInstDcls

We give DFuns a higher score than ordinary CONLIKE things because 
if there's a choice we want the DFun to be the non-looop breker. Eg
 
rec { sc = /\ a \$dC. $fBWrap (T a) ($fCT @ a $dC)

      $fCT :: forall a_afE. (Roman.C a_afE) => Roman.C (Roman.T a_afE)
      {-# DFUN #-}
      $fCT = /\a \$dC. MkD (T a) ((sc @ a $dC) |> blah) ($ctoF @ a $dC)
    }

Here 'sc' (the superclass) looks CONLIKE, but we'll never get to it
if we can't unravel the DFun first.

Note [Constructor applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's really really important to inline dictionaries.  Real
example (the Enum Ordering instance from GHC.Base):

     rec     f = \ x -> case d of (p,q,r) -> p x
             g = \ x -> case d of (p,q,r) -> q x
             d = (v, f, g)

Here, f and g occur just once; but we can't inline them into d.
On the other hand we *could* simplify those case expressions if
we didn't stupidly choose d as the loop breaker.
But we won't because constructor args are marked "Many".
Inlining dictionaries is really essential to unravelling
the loops in static numeric dictionaries, see GHC.Float.

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
           -> Id -> CoreExpr    -- Binder and rhs
                                -- For non-recs the binder is alrady tagged
                                -- with occurrence info
           -> (UsageDetails, CoreExpr)
	      -- Returned usage details includes any INLINE rhs

occAnalRhs env id rhs
  = (addIdOccs rhs_usage (idUnfoldingVars id), rhs')
    	-- Include occurrences for the "extra RHS" from a CoreUnfolding
  where
    (rhs_usage, rhs') = occAnal ctxt rhs
    ctxt | certainly_inline id = env
         | otherwise           = rhsCtxt env
        -- Note that we generally use an rhsCtxt.  This tells the occ anal n
        -- that it's looking at an RHS, which has an effect in occAnalApp
        --
        -- But there's a problem.  Consider
        --      x1 = a0 : []
        --      x2 = a1 : x1
        --      x3 = a2 : x2
        --      g  = f x3
        -- First time round, it looks as if x1 and x2 occur as an arg of a
        -- let-bound constructor ==> give them a many-occurrence.
        -- But then x3 is inlined (unconditionally as it happens) and
        -- next time round, x2 will be, and the next time round x1 will be
        -- Result: multiple simplifier iterations.  Sigh.
        -- Crude solution: use rhsCtxt for things that occur just once...

    certainly_inline id = case idOccInfo id of
                            OneOcc in_lam one_br _ -> not in_lam && one_br
                            _                      -> False
\end{code}



\begin{code}
addRuleUsage :: UsageDetails -> Id -> UsageDetails
-- Add the usage from RULES in Id to the usage
addRuleUsage usage id = addIdOccs usage (idRuleVars id)
        -- idRuleVars here: see Note [Rule dependency info]

addIdOccs :: UsageDetails -> VarSet -> UsageDetails
addIdOccs usage id_set = foldVarSet add usage id_set
  where
    add v u | isId v    = addOneOcc u v NoOccInfo
            | otherwise = u
	-- Give a non-committal binder info (i.e NoOccInfo) because
	--   a) Many copies of the specialised thing can appear
	--   b) We don't want to substitute a BIG expression inside a RULE
	--	even if that's the only occurrence of the thing
	--	(Same goes for INLINE.)
\end{code}

Expressions
~~~~~~~~~~~
\begin{code}
occAnal :: OccEnv
        -> CoreExpr
        -> (UsageDetails,       -- Gives info only about the "interesting" Ids
            CoreExpr)

occAnal _   (Type t)  = (emptyDetails, Type t)
occAnal env (Var v)   = (mkOneOcc env v False, Var v)
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
occAnal _   expr@(Lit _) = (emptyDetails, expr)
\end{code}

\begin{code}
occAnal env (Note note@(SCC _) body)
  = case occAnal env body of { (usage, body') ->
    (mapVarEnv markInsideSCC usage, Note note body')
    }

occAnal env (Note note body)
  = case occAnal env body of { (usage, body') ->
    (usage, Note note body')
    }

occAnal env (Cast expr co)
  = case occAnal env expr of { (usage, expr') ->
      (markManyIf (isRhsEnv env) usage, Cast expr' co)
        -- If we see let x = y `cast` co
        -- then mark y as 'Many' so that we don't
        -- immediately inline y again.
    }
\end{code}

\begin{code}
occAnal env app@(App _ _)
  = occAnalApp env (collectArgs app)

-- Ignore type variables altogether
--   (a) occurrences inside type lambdas only not marked as InsideLam
--   (b) type variables not in environment

occAnal env (Lam x body) | isTyVar x
  = case occAnal env body of { (body_usage, body') ->
    (body_usage, Lam x body')
    }

-- For value lambdas we do a special hack.  Consider
--      (\x. \y. ...x...)
-- If we did nothing, x is used inside the \y, so would be marked
-- as dangerous to dup.  But in the common case where the abstraction
-- is applied to two arguments this is over-pessimistic.
-- So instead, we just mark each binder with its occurrence
-- info in the *body* of the multiple lambda.
-- Then, the simplifier is careful when partially applying lambdas.

occAnal env expr@(Lam _ _)
  = case occAnal env_body body of { (body_usage, body') ->
    let
        (final_usage, tagged_binders) = tagLamBinders body_usage binders'
		      -- Use binders' to put one-shot info on the lambdas

        --      URGH!  Sept 99: we don't seem to be able to use binders' here, because
        --      we get linear-typed things in the resulting program that we can't handle yet.
        --      (e.g. PrelShow)  TODO

        really_final_usage = if linear then
                                final_usage
                             else
                                mapVarEnv markInsideLam final_usage
    in
    (really_final_usage,
     mkLams tagged_binders body') }
  where
    env_body        = vanillaCtxt (trimOccEnv env binders)
		        -- Body is (no longer) an RhsContext
    (binders, body) = collectBinders expr
    binders'        = oneShotGroup env binders
    linear          = all is_one_shot binders'
    is_one_shot b   = isId b && isOneShotBndr b

occAnal env (Case scrut bndr ty alts)
  = case occ_anal_scrut scrut alts     of { (scrut_usage, scrut') ->
    case mapAndUnzip occ_anal_alt alts of { (alts_usage_s, alts')   ->
    let
        alts_usage  = foldr1 combineAltsUsageDetails alts_usage_s
        (alts_usage1, tagged_bndr) = tag_case_bndr alts_usage bndr
        total_usage = scrut_usage +++ alts_usage1
    in
    total_usage `seq` (total_usage, Case scrut' tagged_bndr ty alts') }}
  where
	-- Note [Case binder usage]	
	-- ~~~~~~~~~~~~~~~~~~~~~~~~
        -- The case binder gets a usage of either "many" or "dead", never "one".
        -- Reason: we like to inline single occurrences, to eliminate a binding,
        -- but inlining a case binder *doesn't* eliminate a binding.
        -- We *don't* want to transform
        --      case x of w { (p,q) -> f w }
        -- into
        --      case x of w { (p,q) -> f (p,q) }
    tag_case_bndr usage bndr
      = case lookupVarEnv usage bndr of
          Nothing -> (usage,                  setIdOccInfo bndr IAmDead)
          Just _  -> (usage `delVarEnv` bndr, setIdOccInfo bndr NoOccInfo)

    alt_env      = mkAltEnv env scrut bndr
    occ_anal_alt = occAnalAlt alt_env bndr

    occ_anal_scrut (Var v) (alt1 : other_alts)
        | not (null other_alts) || not (isDefaultAlt alt1)
        = (mkOneOcc env v True, Var v)	-- The 'True' says that the variable occurs
					-- in an interesting context; the case has
					-- at least one non-default alternative
    occ_anal_scrut scrut _alts  
	= occAnal (vanillaCtxt env) scrut    -- No need for rhsCtxt

occAnal env (Let bind body)
  = case occAnal env_body body                    of { (body_usage, body') ->
    case occAnalBind env env_body bind body_usage of { (final_usage, new_binds) ->
       (final_usage, mkLets new_binds body') }}
  where
    env_body = trimOccEnv env (bindersOf bind)

occAnalArgs :: OccEnv -> [CoreExpr] -> (UsageDetails, [CoreExpr])
occAnalArgs env args
  = case mapAndUnzip (occAnal arg_env) args of  { (arg_uds_s, args') ->
    (foldr (+++) emptyDetails arg_uds_s, args')}
  where
    arg_env = vanillaCtxt env
\end{code}

Applications are dealt with specially because we want
the "build hack" to work.

\begin{code}
occAnalApp :: OccEnv
           -> (Expr CoreBndr, [Arg CoreBndr])
           -> (UsageDetails, Expr CoreBndr)
occAnalApp env (Var fun, args)
  = case args_stuff of { (args_uds, args') ->
    let
       final_args_uds = markManyIf (isRhsEnv env && is_exp) args_uds
	  -- We mark the free vars of the argument of a constructor or PAP
	  -- as "many", if it is the RHS of a let(rec).
	  -- This means that nothing gets inlined into a constructor argument
	  -- position, which is what we want.  Typically those constructor
	  -- arguments are just variables, or trivial expressions.
	  --
	  -- This is the *whole point* of the isRhsEnv predicate
    in
    (fun_uds +++ final_args_uds, mkApps (Var fun) args') }
  where
    fun_uniq = idUnique fun
    fun_uds  = mkOneOcc env fun (valArgCount args > 0)
    is_exp = isExpandableApp fun (valArgCount args)
    	   -- See Note [CONLIKE pragma] in BasicTypes
	   -- The definition of is_exp should match that in
	   -- Simplify.prepareRhs

                -- Hack for build, fold, runST
    args_stuff  | fun_uniq == buildIdKey    = appSpecial env 2 [True,True]  args
                | fun_uniq == augmentIdKey  = appSpecial env 2 [True,True]  args
                | fun_uniq == foldrIdKey    = appSpecial env 3 [False,True] args
                | fun_uniq == runSTRepIdKey = appSpecial env 2 [True]       args
                        -- (foldr k z xs) may call k many times, but it never
                        -- shares a partial application of k; hence [False,True]
                        -- This means we can optimise
                        --      foldr (\x -> let v = ...x... in \y -> ...v...) z xs
                        -- by floating in the v

                | otherwise = occAnalArgs env args


occAnalApp env (fun, args)
  = case occAnal (addAppCtxt env args) fun of   { (fun_uds, fun') ->
        -- The addAppCtxt is a bit cunning.  One iteration of the simplifier
        -- often leaves behind beta redexs like
        --      (\x y -> e) a1 a2
        -- Here we would like to mark x,y as one-shot, and treat the whole
        -- thing much like a let.  We do this by pushing some True items
        -- onto the context stack.

    case occAnalArgs env args of        { (args_uds, args') ->
    let
        final_uds = fun_uds +++ args_uds
    in
    (final_uds, mkApps fun' args') }}


markManyIf :: Bool              -- If this is true
           -> UsageDetails      -- Then do markMany on this
           -> UsageDetails
markManyIf True  uds = mapVarEnv markMany uds
markManyIf False uds = uds

appSpecial :: OccEnv
           -> Int -> CtxtTy     -- Argument number, and context to use for it
           -> [CoreExpr]
           -> (UsageDetails, [CoreExpr])
appSpecial env n ctxt args
  = go n args
  where
    arg_env = vanillaCtxt env

    go _ [] = (emptyDetails, [])        -- Too few args

    go 1 (arg:args)                     -- The magic arg
      = case occAnal (setCtxtTy arg_env ctxt) arg of    { (arg_uds, arg') ->
        case occAnalArgs env args of                    { (args_uds, args') ->
        (arg_uds +++ args_uds, arg':args') }}

    go n (arg:args)
      = case occAnal arg_env arg of     { (arg_uds, arg') ->
        case go (n-1) args of           { (args_uds, args') ->
        (arg_uds +++ args_uds, arg':args') }}
\end{code}


Note [Binders in case alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
    case x of y { (a,b) -> f y }
We treat 'a', 'b' as dead, because they don't physically occur in the
case alternative.  (Indeed, a variable is dead iff it doesn't occur in
its scope in the output of OccAnal.)  It really helps to know when
binders are unused.  See esp the call to isDeadBinder in
Simplify.mkDupableAlt

In this example, though, the Simplifier will bring 'a' and 'b' back to
life, beause it binds 'y' to (a,b) (imagine got inlined and
scrutinised y).

\begin{code}
occAnalAlt :: OccEnv
           -> CoreBndr
           -> CoreAlt
           -> (UsageDetails, Alt IdWithOccInfo)
occAnalAlt env case_bndr (con, bndrs, rhs)
  = let 
        env' = trimOccEnv env bndrs
    in 
    case occAnal env' rhs of { (rhs_usage1, rhs1) ->
    let
	proxies = getProxies env' case_bndr 
	(rhs_usage2, rhs2) = foldrBag wrapProxy (rhs_usage1, rhs1) proxies
        (alt_usg, tagged_bndrs) = tagLamBinders rhs_usage2 bndrs
        bndrs' = tagged_bndrs      -- See Note [Binders in case alternatives]
    in
    (alt_usg, (con, bndrs', rhs2)) }

wrapProxy :: ProxyBind -> (UsageDetails, CoreExpr) -> (UsageDetails, CoreExpr)
wrapProxy (bndr, rhs_var, co) (body_usg, body)
  | not (bndr `usedIn` body_usg) 
  = (body_usg, body)
  | otherwise
  = (body_usg' +++ rhs_usg, Let (NonRec tagged_bndr rhs) body)
  where
    (body_usg', tagged_bndr) = tagBinder body_usg bndr
    rhs_usg = unitVarEnv rhs_var NoOccInfo	-- We don't need exact info
    rhs = mkCoerceI co (Var rhs_var)
\end{code}


%************************************************************************
%*                                                                      *
                    OccEnv									
%*                                                                      *
%************************************************************************

\begin{code}
data OccEnv
  = OccEnv { occ_encl  :: !OccEncl      -- Enclosing context information
    	   , occ_ctxt  :: !CtxtTy       -- Tells about linearity
	   , occ_proxy :: ProxyEnv }


-----------------------------
-- OccEncl is used to control whether to inline into constructor arguments
-- For example:
--      x = (p,q)               -- Don't inline p or q
--      y = /\a -> (p a, q a)   -- Still don't inline p or q
--      z = f (p,q)             -- Do inline p,q; it may make a rule fire
-- So OccEncl tells enought about the context to know what to do when
-- we encounter a contructor application or PAP.

data OccEncl
  = OccRhs              -- RHS of let(rec), albeit perhaps inside a type lambda
                        -- Don't inline into constructor args here
  | OccVanilla          -- Argument of function, body of lambda, scruintee of case etc.
                        -- Do inline into constructor args here

instance Outputable OccEncl where
  ppr OccRhs     = ptext (sLit "occRhs")
  ppr OccVanilla = ptext (sLit "occVanilla")

type CtxtTy = [Bool]
        -- []           No info
        --
        -- True:ctxt    Analysing a function-valued expression that will be
        --                      applied just once
        --
        -- False:ctxt   Analysing a function-valued expression that may
        --                      be applied many times; but when it is,
        --                      the CtxtTy inside applies

initOccEnv :: OccEnv
initOccEnv = OccEnv { occ_encl  = OccVanilla
	      	    , occ_ctxt  = []
		    , occ_proxy = PE emptyVarEnv emptyVarSet }

vanillaCtxt :: OccEnv -> OccEnv
vanillaCtxt env = OccEnv { occ_encl = OccVanilla
                         , occ_ctxt = []
	      	         , occ_proxy = occ_proxy env }

rhsCtxt :: OccEnv -> OccEnv
rhsCtxt env = OccEnv { occ_encl = OccRhs, occ_ctxt = []
	      	     , occ_proxy = occ_proxy env }

setCtxtTy :: OccEnv -> CtxtTy -> OccEnv
setCtxtTy env ctxt = env { occ_ctxt = ctxt }

isRhsEnv :: OccEnv -> Bool
isRhsEnv (OccEnv { occ_encl = OccRhs })     = True
isRhsEnv (OccEnv { occ_encl = OccVanilla }) = False

oneShotGroup :: OccEnv -> [CoreBndr] -> [CoreBndr]
        -- The result binders have one-shot-ness set that they might not have had originally.
        -- This happens in (build (\cn -> e)).  Here the occurrence analyser
        -- linearity context knows that c,n are one-shot, and it records that fact in
        -- the binder. This is useful to guide subsequent float-in/float-out tranformations

oneShotGroup (OccEnv { occ_ctxt = ctxt }) bndrs
  = go ctxt bndrs []
  where
    go _ [] rev_bndrs = reverse rev_bndrs

    go (lin_ctxt:ctxt) (bndr:bndrs) rev_bndrs
        | isId bndr = go ctxt bndrs (bndr':rev_bndrs)
        where
          bndr' | lin_ctxt  = setOneShotLambda bndr
                | otherwise = bndr

    go ctxt (bndr:bndrs) rev_bndrs = go ctxt bndrs (bndr:rev_bndrs)

addAppCtxt :: OccEnv -> [Arg CoreBndr] -> OccEnv
addAppCtxt env@(OccEnv { occ_ctxt = ctxt }) args
  = env { occ_ctxt = replicate (valArgCount args) True ++ ctxt }
\end{code}

%************************************************************************
%*                                                                      *
                    ProxyEnv									
%*                                                                      *
%************************************************************************

\begin{code}
data ProxyEnv 
   = PE (IdEnv (Id, [(Id,CoercionI)])) VarSet
     	-- Main env, and its free variables (of both range and domain)
\end{code}

Note [ProxyEnv]
~~~~~~~~~~~~~~~
The ProxyEnv keeps track of the connection between case binders and
scrutinee.  Specifically, if
     sc |-> (sc, [...(cb, co)...])
is a binding in the ProxyEnv, then
     cb = sc |> coi
Typically we add such a binding when encountering the case expression
     case (sc |> coi) of cb { ... }

Things to note:
  * The domain of the ProxyEnv is the variable (or casted variable) 
    scrutinees of enclosing cases.  This is additionally used
    to ensure we gather occurrence info even for GlobalId scrutinees;
    see Note [Binder swap for GlobalId scrutinee]

  * The ProxyEnv is just an optimisation; you can throw away any 
    element without losing correctness.  And we do so when pushing
    it inside a binding (see trimProxyEnv).

  * Once scrutinee might map to many case binders:  Eg
      case sc of cb1 { DEFAULT -> ....case sc of cb2 { ... } .. }

INVARIANTS
 * If sc1 |-> (sc2, [...(cb, co)...]), then sc1==sc2
   It's a UniqFM and we sometimes need the domain Id

 * Any particular case binder 'cb' occurs only once in entire range

 * No loops

The Main Reason for having a ProxyEnv is so that when we encounter
    case e of cb { pi -> ri }
we can find all the in-scope variables derivable from 'cb', 
and effectively add let-bindings for them thus:
    case e of cb { pi -> let { x = ..cb..; y = ...cb.. }
                         in ri }
The function getProxies finds these bindings; then we 
add just the necessary ones, using wrapProxy. 

More info under Note [Binder swap]

Note [Binder swap]
~~~~~~~~~~~~~~~~~~
We do these two transformations right here:

 (1)   case x of b { pi -> ri }
    ==>
      case x of b { pi -> let x=b in ri }

 (2)  case (x |> co) of b { pi -> ri }
    ==>
      case (x |> co) of b { pi -> let x = b |> sym co in ri }

    Why (2)?  See Note [Case of cast]

In both cases, in a particular alternative (pi -> ri), we only 
add the binding if
  (a) x occurs free in (pi -> ri)
	(ie it occurs in ri, but is not bound in pi)
  (b) the pi does not bind b (or the free vars of co)
We need (a) and (b) for the inserted binding to be correct.

For the alternatives where we inject the binding, we can transfer
all x's OccInfo to b.  And that is the point.

Notice that 
  * The deliberate shadowing of 'x'. 
  * That (a) rapidly becomes false, so no bindings are injected.

The reason for doing these transformations here is because it allows
us to adjust the OccInfo for 'x' and 'b' as we go.

  * Suppose the only occurrences of 'x' are the scrutinee and in the
    ri; then this transformation makes it occur just once, and hence
    get inlined right away.

  * If we do this in the Simplifier, we don't know whether 'x' is used
    in ri, so we are forced to pessimistically zap b's OccInfo even
    though it is typically dead (ie neither it nor x appear in the
    ri).  There's nothing actually wrong with zapping it, except that
    it's kind of nice to know which variables are dead.  My nose
    tells me to keep this information as robustly as possible.

The Maybe (Id,CoreExpr) passed to occAnalAlt is the extra let-binding
{x=b}; it's Nothing if the binder-swap doesn't happen.

There is a danger though.  Consider
      let v = x +# y
      in case (f v) of w -> ...v...v...
And suppose that (f v) expands to just v.  Then we'd like to
use 'w' instead of 'v' in the alternative.  But it may be too
late; we may have substituted the (cheap) x+#y for v in the 
same simplifier pass that reduced (f v) to v.

I think this is just too bad.  CSE will recover some of it.

Note [Case of cast]
~~~~~~~~~~~~~~~~~~~
Consider        case (x `cast` co) of b { I# ->
                ... (case (x `cast` co) of {...}) ...
We'd like to eliminate the inner case.  That is the motivation for
equation (2) in Note [Binder swap].  When we get to the inner case, we
inline x, cancel the casts, and away we go.

Note [Binder swap on GlobalId scrutinees]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the scrutinee is a GlobalId we must take care in two ways

 i) In order to *know* whether 'x' occurs free in the RHS, we need its
    occurrence info. BUT, we don't gather occurrence info for
    GlobalIds.  That's one use for the (small) occ_proxy env in OccEnv is
    for: it says "gather occurrence info for these.

 ii) We must call localiseId on 'x' first, in case it's a GlobalId, or
     has an External Name. See, for example, SimplEnv Note [Global Ids in
     the substitution].

Note [getProxies is subtle]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
The code for getProxies isn't all that obvious. Consider

  case v |> cov  of x { DEFAULT ->
  case x |> cox1 of y { DEFAULT ->
  case x |> cox2 of z { DEFAULT -> r

These will give us a ProxyEnv looking like:
  x |-> (x, [(y, cox1), (z, cox2)])
  v |-> (v, [(x, cov)])

From this we want to extract the bindings
    x = z |> sym cox2
    v = x |> sym cov
    y = x |> cox1

Notice that later bindings may mention earlier ones, and that
we need to go "both ways".

Historical note [no-case-of-case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We *used* to suppress the binder-swap in case expressions when 
-fno-case-of-case is on.  Old remarks:
    "This happens in the first simplifier pass,
    and enhances full laziness.  Here's the bad case:
            f = \ y -> ...(case x of I# v -> ...(case x of ...) ... )
    If we eliminate the inner case, we trap it inside the I# v -> arm,
    which might prevent some full laziness happening.  I've seen this
    in action in spectral/cichelli/Prog.hs:
             [(m,n) | m <- [1..max], n <- [1..max]]
    Hence the check for NoCaseOfCase."
However, now the full-laziness pass itself reverses the binder-swap, so this
check is no longer necessary.

Historical note [Suppressing the case binder-swap]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This old note describes a problem that is also fixed by doing the
binder-swap in OccAnal:

    There is another situation when it might make sense to suppress the
    case-expression binde-swap. If we have

        case x of w1 { DEFAULT -> case x of w2 { A -> e1; B -> e2 }
                       ...other cases .... }

    We'll perform the binder-swap for the outer case, giving

        case x of w1 { DEFAULT -> case w1 of w2 { A -> e1; B -> e2 }
                       ...other cases .... }

    But there is no point in doing it for the inner case, because w1 can't
    be inlined anyway.  Furthermore, doing the case-swapping involves
    zapping w2's occurrence info (see paragraphs that follow), and that
    forces us to bind w2 when doing case merging.  So we get

        case x of w1 { A -> let w2 = w1 in e1
                       B -> let w2 = w1 in e2
                       ...other cases .... }

    This is plain silly in the common case where w2 is dead.

    Even so, I can't see a good way to implement this idea.  I tried
    not doing the binder-swap if the scrutinee was already evaluated
    but that failed big-time:

            data T = MkT !Int

            case v of w  { MkT x ->
            case x of x1 { I# y1 ->
            case x of x2 { I# y2 -> ...

    Notice that because MkT is strict, x is marked "evaluated".  But to
    eliminate the last case, we must either make sure that x (as well as
    x1) has unfolding MkT y1.  THe straightforward thing to do is to do
    the binder-swap.  So this whole note is a no-op.

It's fixed by doing the binder-swap in OccAnal because we can do the
binder-swap unconditionally and still get occurrence analysis
information right.

\begin{code}
extendProxyEnv :: ProxyEnv -> Id -> CoercionI -> Id -> ProxyEnv
-- (extendPE x co y) typically arises from 
--		  case (x |> co) of y { ... }
-- It extends the proxy env with the binding 
-- 	               y = x |> co
extendProxyEnv pe scrut co case_bndr
  | scrut == case_bndr = PE env1 fvs1	-- If case_bndr shadows scrut,
  | otherwise          = PE env2 fvs2	--   don't extend
  where
    PE env1 fvs1 = trimProxyEnv pe [case_bndr]
    env2 = extendVarEnv_Acc add single env1 scrut1 (case_bndr,co)
    single cb_co = (scrut1, [cb_co]) 
    add cb_co (x, cb_cos) = (x, cb_co:cb_cos)
    fvs2 = fvs1 `unionVarSet`  freeVarsCoI co
		`extendVarSet` case_bndr
		`extendVarSet` scrut1

    scrut1 = mkLocalId (localiseName (idName scrut)) (idType scrut)
	-- Localise the scrut_var before shadowing it; we're making a 
	-- new binding for it, and it might have an External Name, or
	-- even be a GlobalId; Note [Binder swap on GlobalId scrutinees]
	-- Also we don't want any INLILNE or NOINLINE pragmas!

-----------
type ProxyBind = (Id, Id, CoercionI)

getProxies :: OccEnv -> Id -> Bag ProxyBind
-- Return a bunch of bindings [...(xi,ei)...] 
-- such that  let { ...; xi=ei; ... } binds the xi using y alone
-- See Note [getProxies is subtle]
getProxies (OccEnv { occ_proxy = PE pe _ }) case_bndr
  = -- pprTrace "wrapProxies" (ppr case_bndr) $
    go_fwd case_bndr
  where
    fwd_pe :: IdEnv (Id, CoercionI)
    fwd_pe = foldVarEnv add1 emptyVarEnv pe
           where
             add1 (x,ycos) env = foldr (add2 x) env ycos
             add2 x (y,co) env = extendVarEnv env y (x,co)

    go_fwd :: Id -> Bag ProxyBind
	-- Return bindings derivable from case_bndr
    go_fwd case_bndr = -- pprTrace "go_fwd" (vcat [ppr case_bndr, text "fwd_pe =" <+> ppr fwd_pe, 
                       --                         text "pe =" <+> ppr pe]) $ 
                       go_fwd' case_bndr

    go_fwd' case_bndr
        | Just (scrut, co) <- lookupVarEnv fwd_pe case_bndr
        = unitBag (scrut,  case_bndr, mkSymCoI co)
	  `unionBags` go_fwd scrut
          `unionBags` go_bwd scrut [pr | pr@(cb,_) <- lookup_bwd scrut
                                       , cb /= case_bndr]
        | otherwise 
        = emptyBag

    lookup_bwd :: Id -> [(Id, CoercionI)]
	-- Return case_bndrs that are connected to scrut 
    lookup_bwd scrut = case lookupVarEnv pe scrut of
          		  Nothing          -> []
	  		  Just (_, cb_cos) -> cb_cos

    go_bwd :: Id -> [(Id, CoercionI)] -> Bag ProxyBind
    go_bwd scrut cb_cos = foldr (unionBags . go_bwd1 scrut) emptyBag cb_cos

    go_bwd1 :: Id -> (Id, CoercionI) -> Bag ProxyBind
    go_bwd1 scrut (case_bndr, co) 
       = -- pprTrace "go_bwd1" (ppr case_bndr) $
         unitBag (case_bndr, scrut, co)
	 `unionBags` go_bwd case_bndr (lookup_bwd case_bndr)

-----------
mkAltEnv :: OccEnv -> CoreExpr -> Id -> OccEnv
-- Does two things: a) makes the occ_ctxt = OccVanilla
-- 	    	    b) extends the ProxyEnv if possible
mkAltEnv env scrut cb
  = env { occ_encl  = OccVanilla, occ_proxy = pe' }
  where
    pe  = occ_proxy env
    pe' = case scrut of
             Var v           -> extendProxyEnv pe v IdCo     cb
             Cast (Var v) co -> extendProxyEnv pe v (ACo co) cb
	     _other          -> trimProxyEnv pe [cb]

-----------
trimOccEnv :: OccEnv -> [CoreBndr] -> OccEnv
trimOccEnv env bndrs = env { occ_proxy = trimProxyEnv (occ_proxy env) bndrs }

-----------
trimProxyEnv :: ProxyEnv -> [CoreBndr] -> ProxyEnv
-- We are about to push this ProxyEnv inside a binding for 'bndrs'
-- So dump any ProxyEnv bindings which mention any of the bndrs
trimProxyEnv (PE pe fvs) bndrs 
  | not (bndr_set `intersectsVarSet` fvs) 
  = PE pe fvs
  | otherwise
  = PE pe' (fvs `minusVarSet` bndr_set)
  where
    pe' = mapVarEnv trim pe
    bndr_set = mkVarSet bndrs
    trim (scrut, cb_cos) | scrut `elemVarSet` bndr_set = (scrut, [])
			 | otherwise = (scrut, filterOut discard cb_cos)
    discard (cb,co) = bndr_set `intersectsVarSet` 
                      extendVarSet (freeVarsCoI co) cb
                             
-----------
freeVarsCoI :: CoercionI -> VarSet
freeVarsCoI IdCo     = emptyVarSet
freeVarsCoI (ACo co) = tyVarsOfType co
\end{code}


%************************************************************************
%*                                                                      *
\subsection[OccurAnal-types]{OccEnv}
%*                                                                      *
%************************************************************************

\begin{code}
type UsageDetails = IdEnv OccInfo       -- A finite map from ids to their usage
		-- INVARIANT: never IAmDead
		-- (Deadness is signalled by not being in the map at all)

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

emptyDetails :: UsageDetails
emptyDetails = (emptyVarEnv :: UsageDetails)

localUsedIn, usedIn :: Id -> UsageDetails -> Bool
v `localUsedIn` details = v `elemVarEnv` details
v `usedIn`      details =  isExportedId v || v `localUsedIn` details

type IdWithOccInfo = Id

tagLamBinders :: UsageDetails          -- Of scope
              -> [Id]                  -- Binders
              -> (UsageDetails,        -- Details with binders removed
                 [IdWithOccInfo])    -- Tagged binders
-- Used for lambda and case binders
-- It copes with the fact that lambda bindings can have InlineRule 
-- unfoldings, used for join points
tagLamBinders usage binders = usage' `seq` (usage', bndrs')
  where
    (usage', bndrs') = mapAccumR tag_lam usage binders
    tag_lam usage bndr = (usage2, setBinderOcc usage bndr)
      where
        usage1 = usage `delVarEnv` bndr
        usage2 | isId bndr = addIdOccs usage1 (idUnfoldingVars bndr)
               | otherwise = usage1

tagBinder :: UsageDetails           -- Of scope
          -> Id                     -- Binders
          -> (UsageDetails,         -- Details with binders removed
              IdWithOccInfo)        -- Tagged binders

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
                          _         -> setIdOccInfo bndr NoOccInfo
            -- Don't use local usage info for visible-elsewhere things
            -- BUT *do* erase any IAmALoopBreaker annotation, because we're
            -- about to re-generate it and it shouldn't be "sticky"

  | otherwise = setIdOccInfo bndr occ_info
  where
    occ_info = lookupVarEnv usage bndr `orElse` IAmDead
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Operations over OccInfo}
%*                                                                      *
%************************************************************************

\begin{code}
mkOneOcc :: OccEnv -> Id -> InterestingCxt -> UsageDetails
mkOneOcc env id int_cxt
  | isLocalId id = unitVarEnv id (OneOcc False True int_cxt)
  | PE env _ <- occ_proxy env
  , id `elemVarEnv` env = unitVarEnv id NoOccInfo
  | otherwise           = emptyDetails

markMany, markInsideLam, markInsideSCC :: OccInfo -> OccInfo

markMany _  = NoOccInfo

markInsideSCC occ = markMany occ

markInsideLam (OneOcc _ one_br int_cxt) = OneOcc True one_br int_cxt
markInsideLam occ                       = occ

addOccInfo, orOccInfo :: OccInfo -> OccInfo -> OccInfo

addOccInfo a1 a2  = ASSERT( not (isDeadOcc a1 || isDeadOcc a2) )
		    NoOccInfo	-- Both branches are at least One
				-- (Argument is never IAmDead)

-- (orOccInfo orig new) is used
-- when combining occurrence info from branches of a case

orOccInfo (OneOcc in_lam1 _ int_cxt1)
          (OneOcc in_lam2 _ int_cxt2)
  = OneOcc (in_lam1 || in_lam2)
           False        -- False, because it occurs in both branches
           (int_cxt1 && int_cxt2)
orOccInfo a1 a2 = ASSERT( not (isDeadOcc a1 || isDeadOcc a2) )
		  NoOccInfo
\end{code}
