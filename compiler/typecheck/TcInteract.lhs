\begin{code}
module TcInteract ( 
     solveInteract, solveInteractGiven, solveInteractWanted,
     AtomicInert, tyVarsOfInert, 
     InertSet, emptyInert, updInertSet, extractUnsolved, solveOne,
  ) where  

#include "HsVersions.h"


import BasicTypes 
import TcCanonical
import VarSet
import Type

import Id 
import Var

import TcType
import HsBinds

import Inst( tyVarsOfEvVar )
import Class
import TyCon
import Name

import FunDeps

import Coercion
import Outputable

import TcRnTypes
import TcErrors
import TcSMonad
import Bag
import qualified Data.Map as Map

import Control.Monad( when )

import FastString ( sLit ) 
import DynFlags
\end{code}

Note [InertSet invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
An InertSet is a bag of canonical constraints, with the following invariants:

  1 No two constraints react with each other. 
    
    A tricky case is when there exists a given (solved) dictionary 
    constraint and a wanted identical constraint in the inert set, but do 
    not react because reaction would create loopy dictionary evidence for 
    the wanted. See note [Recursive dictionaries]

  2 Given equalities form an idempotent substitution [none of the
    given LHS's occur in any of the given RHS's or reactant parts]

  3 Wanted equalities also form an idempotent substitution

  4 The entire set of equalities is acyclic.

  5 Wanted dictionaries are inert with the top-level axiom set 

  6 Equalities of the form tv1 ~ tv2 always have a touchable variable
    on the left (if possible).

  7 No wanted constraints tv1 ~ tv2 with tv1 touchable. Such constraints
    will be marked as solved right before being pushed into the inert set. 
    See note [Touchables and givens].

  8 No Given constraint mentions a touchable unification variable,
    except if the
 
Note that 6 and 7 are /not/ enforced by canonicalization but rather by 
insertion in the inert list, ie by TcInteract. 

During the process of solving, the inert set will contain some
previously given constraints, some wanted constraints, and some given
constraints which have arisen from solving wanted constraints. For
now we do not distinguish between given and solved constraints.

Note that we must switch wanted inert items to given when going under an
implication constraint (when in top-level inference mode).

\begin{code}

data CCanMap a = CCanMap { cts_given   :: Map.Map a CanonicalCts
                                          -- Invariant: all Given
                         , cts_derived :: Map.Map a CanonicalCts 
                                          -- Invariant: all Derived
                         , cts_wanted  :: Map.Map a CanonicalCts } 
                                          -- Invariant: all Wanted

cCanMapToBag :: Ord a => CCanMap a -> CanonicalCts 
cCanMapToBag cmap = Map.fold unionBags rest_wder (cts_given cmap)
  where rest_wder = Map.fold unionBags rest_der  (cts_wanted cmap) 
        rest_der  = Map.fold unionBags emptyCCan (cts_derived cmap)

emptyCCanMap :: CCanMap a 
emptyCCanMap = CCanMap { cts_given = Map.empty
                       , cts_derived = Map.empty, cts_wanted = Map.empty } 

updCCanMap:: Ord a => (a,CanonicalCt) -> CCanMap a -> CCanMap a 
updCCanMap (a,ct) cmap 
  = case cc_flavor ct of 
      Wanted {} 
          -> cmap { cts_wanted = Map.insertWith unionBags a this_ct (cts_wanted cmap) } 
      Given {} 
          -> cmap { cts_given = Map.insertWith unionBags a this_ct (cts_given cmap) }
      Derived {}
          -> cmap { cts_derived = Map.insertWith unionBags a this_ct (cts_derived cmap) }
  where this_ct = singleCCan ct 

getRelevantCts :: Ord a => a -> CCanMap a -> (CanonicalCts, CCanMap a) 
-- Gets the relevant constraints and returns the rest of the CCanMap
getRelevantCts a cmap 
    = let relevant = unionManyBags [ Map.findWithDefault emptyCCan a (cts_wanted cmap)
                                   , Map.findWithDefault emptyCCan a (cts_given cmap)
                                   , Map.findWithDefault emptyCCan a (cts_derived cmap) ]
          residual_map = cmap { cts_wanted = Map.delete a (cts_wanted cmap) 
                              , cts_given = Map.delete a (cts_given cmap) 
                              , cts_derived = Map.delete a (cts_derived cmap) }
      in (relevant, residual_map) 

extractUnsolvedCMap :: Ord a => CCanMap a -> (CanonicalCts, CCanMap a)
-- Gets the wanted or derived constraints and returns a residual
-- CCanMap with only givens.
extractUnsolvedCMap cmap =
  let wntd = Map.fold unionBags emptyCCan (cts_wanted cmap)
      derd = Map.fold unionBags emptyCCan (cts_derived cmap)
  in (wntd `unionBags` derd, 
           cmap { cts_wanted = Map.empty, cts_derived = Map.empty })


-- See Note [InertSet invariants]
data InertSet 
  = IS { inert_eqs          :: CanonicalCts               -- Equalities only (CTyEqCan)
       , inert_dicts        :: CCanMap Class              -- Dictionaries only
       , inert_ips          :: CCanMap (IPName Name)      -- Implicit parameters 
       , inert_frozen       :: CanonicalCts
       , inert_funeqs       :: CCanMap TyCon              -- Type family equalities only
               -- This representation allows us to quickly get to the relevant 
               -- inert constraints when interacting a work item with the inert set.
       }

tyVarsOfInert :: InertSet -> TcTyVarSet 
tyVarsOfInert (IS { inert_eqs    = eqs
                  , inert_dicts  = dictmap
                  , inert_ips    = ipmap
                  , inert_frozen = frozen
                  , inert_funeqs = funeqmap }) = tyVarsOfCanonicals cts
  where
    cts = eqs `andCCan` frozen `andCCan` cCanMapToBag dictmap
              `andCCan` cCanMapToBag ipmap `andCCan` cCanMapToBag funeqmap

instance Outputable InertSet where
  ppr is = vcat [ vcat (map ppr (Bag.bagToList $ inert_eqs is))
                , vcat (map ppr (Bag.bagToList $ cCanMapToBag (inert_dicts is)))
                , vcat (map ppr (Bag.bagToList $ cCanMapToBag (inert_ips is))) 
                , vcat (map ppr (Bag.bagToList $ cCanMapToBag (inert_funeqs is)))
                , vcat (map ppr (Bag.bagToList $ inert_frozen is))
                ]
                       
emptyInert :: InertSet
emptyInert = IS { inert_eqs    = Bag.emptyBag
                , inert_frozen = Bag.emptyBag
                , inert_dicts  = emptyCCanMap
                , inert_ips    = emptyCCanMap
                , inert_funeqs = emptyCCanMap }

updInertSet :: InertSet -> AtomicInert -> InertSet 
updInertSet is item 
  | isCTyEqCan item                     -- Other equality 
  = let eqs' = inert_eqs is `Bag.snocBag` item 
    in is { inert_eqs = eqs' } 
  | Just cls <- isCDictCan_Maybe item   -- Dictionary 
  = is { inert_dicts = updCCanMap (cls,item) (inert_dicts is) } 
  | Just x  <- isCIPCan_Maybe item      -- IP 
  = is { inert_ips   = updCCanMap (x,item) (inert_ips is) }  
  | Just tc <- isCFunEqCan_Maybe item   -- Function equality 
  = is { inert_funeqs = updCCanMap (tc,item) (inert_funeqs is) }
  | otherwise 
  = is { inert_frozen = inert_frozen is `Bag.snocBag` item }

extractUnsolved :: InertSet -> (InertSet, CanonicalCts)
-- Postcondition: the returned canonical cts are either Derived, or Wanted.
extractUnsolved is@(IS {inert_eqs = eqs}) 
  = let is_solved  = is { inert_eqs    = solved_eqs
                        , inert_dicts  = solved_dicts
                        , inert_ips    = solved_ips
                        , inert_frozen = emptyCCan
                        , inert_funeqs = solved_funeqs }
    in (is_solved, unsolved)

  where (unsolved_eqs, solved_eqs)       = Bag.partitionBag (not.isGivenCt) eqs
        (unsolved_ips, solved_ips)       = extractUnsolvedCMap (inert_ips is) 
        (unsolved_dicts, solved_dicts)   = extractUnsolvedCMap (inert_dicts is) 
        (unsolved_funeqs, solved_funeqs) = extractUnsolvedCMap (inert_funeqs is) 

        unsolved = unsolved_eqs `unionBags` inert_frozen is `unionBags`
                   unsolved_ips `unionBags` unsolved_dicts `unionBags` unsolved_funeqs
\end{code}

%*********************************************************************
%*                                                                   * 
*                      Main Interaction Solver                       *
*                                                                    *
**********************************************************************

Note [Basic plan] 
~~~~~~~~~~~~~~~~~
1. Canonicalise (unary)
2. Pairwise interaction (binary)
    * Take one from work list 
    * Try all pair-wise interactions with each constraint in inert
   
   As an optimisation, we prioritize the equalities both in the 
   worklist and in the inerts. 

3. Try to solve spontaneously for equalities involving touchables 
4. Top-level interaction (binary wrt top-level)
   Superclass decomposition belongs in (4), see note [Superclasses]

\begin{code}
type AtomicInert = CanonicalCt     -- constraint pulled from InertSet
type WorkItem    = CanonicalCt     -- constraint pulled from WorkList

------------------------
data StopOrContinue 
  = Stop			-- Work item is consumed
  | ContinueWith WorkItem	-- Not consumed

instance Outputable StopOrContinue where
  ppr Stop             = ptext (sLit "Stop")
  ppr (ContinueWith w) = ptext (sLit "ContinueWith") <+> ppr w

-- Results after interacting a WorkItem as far as possible with an InertSet
data StageResult
  = SR { sr_inerts     :: InertSet
           -- The new InertSet to use (REPLACES the old InertSet)
       , sr_new_work   :: WorkList
           -- Any new work items generated (should be ADDED to the old WorkList)
           -- Invariant: 
           --    sr_stop = Just workitem => workitem is *not* in sr_inerts and
           --                               workitem is inert wrt to sr_inerts
       , sr_stop       :: StopOrContinue
       }

instance Outputable StageResult where
  ppr (SR { sr_inerts = inerts, sr_new_work = work, sr_stop = stop })
    = ptext (sLit "SR") <+> 
      braces (sep [ ptext (sLit "inerts =") <+> ppr inerts <> comma
             	  , ptext (sLit "new work =") <+> ppr work <> comma
             	  , ptext (sLit "stop =") <+> ppr stop])

type SubGoalDepth = Int	  -- Starts at zero; used to limit infinite
     		    	  -- recursion of sub-goals
type SimplifierStage = SubGoalDepth -> WorkItem -> InertSet -> TcS StageResult 

-- Combine a sequence of simplifier 'stages' to create a pipeline 
runSolverPipeline :: SubGoalDepth
                  -> [(String, SimplifierStage)]
		  -> InertSet -> WorkItem 
                  -> TcS (InertSet, WorkList)
-- Precondition: non-empty list of stages 
runSolverPipeline depth pipeline inerts workItem
  = do { traceTcS "Start solver pipeline" $ 
            vcat [ ptext (sLit "work item =") <+> ppr workItem
                 , ptext (sLit "inerts    =") <+> ppr inerts]

       ; let itr_in = SR { sr_inerts = inerts
                         , sr_new_work = emptyWorkList
                         , sr_stop = ContinueWith workItem }
       ; itr_out <- run_pipeline pipeline itr_in
       ; let new_inert 
              = case sr_stop itr_out of 
       	          Stop              -> sr_inerts itr_out
                  ContinueWith item -> sr_inerts itr_out `updInertSet` item
       ; return (new_inert, sr_new_work itr_out) }
  where 
    run_pipeline :: [(String, SimplifierStage)]
                 -> StageResult -> TcS StageResult
    run_pipeline [] itr                         = return itr
    run_pipeline _  itr@(SR { sr_stop = Stop }) = return itr

    run_pipeline ((name,stage):stages) 
                 (SR { sr_new_work = accum_work
                     , sr_inerts   = inerts
                     , sr_stop     = ContinueWith work_item })
      = do { itr <- stage depth work_item inerts 
           ; traceTcS ("Stage result (" ++ name ++ ")") (ppr itr)
           ; let itr' = itr { sr_new_work = accum_work `unionWorkList` sr_new_work itr }
           ; run_pipeline stages itr' }
\end{code}

Example 1:
  Inert:   {c ~ d, F a ~ t, b ~ Int, a ~ ty} (all given)
  Reagent: a ~ [b] (given)

React with (c~d)     ==> IR (ContinueWith (a~[b]))  True    []
React with (F a ~ t) ==> IR (ContinueWith (a~[b]))  False   [F [b] ~ t]
React with (b ~ Int) ==> IR (ContinueWith (a~[Int]) True    []

Example 2:
  Inert:  {c ~w d, F a ~g t, b ~w Int, a ~w ty}
  Reagent: a ~w [b]

React with (c ~w d)   ==> IR (ContinueWith (a~[b]))  True    []
React with (F a ~g t) ==> IR (ContinueWith (a~[b]))  True    []    (can't rewrite given with wanted!)
etc.

Example 3:
  Inert:  {a ~ Int, F Int ~ b} (given)
  Reagent: F a ~ b (wanted)

React with (a ~ Int)   ==> IR (ContinueWith (F Int ~ b)) True []
React with (F Int ~ b) ==> IR Stop True []    -- after substituting we re-canonicalize and get nothing

\begin{code}
-- Main interaction solver: we fully solve the worklist 'in one go', 
-- returning an extended inert set.
--
-- See Note [Touchables and givens].
solveInteractGiven :: InertSet -> GivenLoc -> [EvVar] -> TcS InertSet
solveInteractGiven inert gloc evs
  = do { (_, inert_ret) <- solveInteract inert $ listToBag $
                           map mk_given evs
       ; return inert_ret }
  where
    flav = Given gloc
    mk_given ev = mkEvVarX ev flav

solveInteractWanted :: InertSet -> [WantedEvVar] -> TcS InertSet
solveInteractWanted inert wvs
  = do { (_,inert_ret) <- solveInteract inert $ listToBag $
                          map wantedToFlavored wvs
       ; return inert_ret }

solveInteract :: InertSet -> Bag FlavoredEvVar -> TcS (Bool, InertSet)
-- Post: (True,  inert_set) means we managed to discharge all constraints
--                          without actually doing any interactions!
--       (False, inert_set) means some interactions occurred
solveInteract inert ws 
  = do { dyn_flags <- getDynFlags
       ; sctx <- getTcSContext

       ; traceTcS "solveInteract, before clever canonicalization:" $
         vcat [ text "ws = " <+>  ppr (mapBag (\(EvVarX ev ct)
                                                   -> (ct,evVarPred ev)) ws)
              , text "inert = " <+> ppr inert ]

       ; can_ws <- mkCanonicalFEVs ws

       ; (flag, inert_ret)
           <- foldrWorkListM (tryPreSolveAndInteract sctx dyn_flags) (True,inert) can_ws

       ; traceTcS "solveInteract, after clever canonicalization (and interaction):" $
         vcat [ text "No interaction happened = " <+> ppr flag
              , text "inert_ret = " <+> ppr inert_ret ]

       ; return (flag, inert_ret) }

tryPreSolveAndInteract :: SimplContext
                       -> DynFlags
                       -> CanonicalCt
                       -> (Bool, InertSet)
                       -> TcS (Bool, InertSet)
-- Returns: True if it was able to discharge this constraint AND all previous ones
tryPreSolveAndInteract sctx dyn_flags ct (all_previous_discharged, inert)
  = do { let inert_cts = get_inert_cts (evVarPred ev_var)

       ; this_one_discharged <- 
           if isCFrozenErr ct then 
               return False
           else
               dischargeFromCCans inert_cts ev_var fl

       ; if this_one_discharged
         then return (all_previous_discharged, inert)

         else do
       { inert_ret <- solveOneWithDepth (ctxtStkDepth dyn_flags,0,[]) ct inert
       ; return (False, inert_ret) } }

  where
    ev_var = cc_id ct
    fl = cc_flavor ct 

    get_inert_cts (ClassP clas _)
      | simplEqsOnly sctx = emptyCCan
      | otherwise         = fst (getRelevantCts clas (inert_dicts inert))
    get_inert_cts (IParam {})
      = emptyCCan -- We must not do the same thing for IParams, because (contrary
                  -- to dictionaries), work items /must/ override inert items.
                 -- See Note [Overriding implicit parameters] in TcInteract.
    get_inert_cts (EqPred {})
      = inert_eqs inert `unionBags` cCanMapToBag (inert_funeqs inert)

dischargeFromCCans :: CanonicalCts -> EvVar -> CtFlavor -> TcS Bool
-- See if this (pre-canonicalised) work-item is identical to a 
-- one already in the inert set. Reasons:
--    a) Avoid creating superclass constraints for millions of incoming (Num a) constraints
--    b) Termination for improve_eqs in TcSimplify.simpl_loop
dischargeFromCCans cans ev fl
  = Bag.foldrBag discharge_ct (return False) cans
  where 
    the_pred = evVarPred ev

    discharge_ct :: CanonicalCt -> TcS Bool -> TcS Bool
    discharge_ct ct _rest
      | evVarPred (cc_id ct) `tcEqPred` the_pred
      , cc_flavor ct `canSolve` fl
      = do { when (isWanted fl) $ set_ev_bind ev (cc_id ct) 
           	 -- Deriveds need no evidence
    	         -- For Givens, we already have evidence, and we don't need it twice 
           ; return True }
      where 
         set_ev_bind x y
            | EqPred {} <- evVarPred y = setEvBind x (EvCoercion (mkCoVarCoercion y))
            | otherwise                = setEvBind x (EvId y)

    discharge_ct _ct rest = rest
\end{code}

Note [Avoiding the superclass explosion] 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
This note now is not as significant as it used to be because we no
longer add the superclasses of Wanted as Derived, except only if they
have equality superclasses or superclasses with functional
dependencies. The fear was that hundreds of identical wanteds would
give rise each to the same superclass or equality Derived's which
would lead to a blo-up in the number of interactions.

Instead, what we do with tryPreSolveAndCanon, is when we encounter a
new constraint, we very quickly see if it can be immediately
discharged by a class constraint in our inert set or the previous
canonicals. If so, we add nothing to the returned canonical
constraints.

\begin{code}
solveOne :: WorkItem -> InertSet -> TcS InertSet 
solveOne workItem inerts 
  = do { dyn_flags <- getDynFlags
       ; solveOneWithDepth (ctxtStkDepth dyn_flags,0,[]) workItem inerts
       }

-----------------
solveInteractWithDepth :: (Int, Int, [WorkItem])
                       -> WorkList -> InertSet -> TcS InertSet
solveInteractWithDepth ctxt@(max_depth,n,stack) ws inert
  | isEmptyWorkList ws
  = return inert

  | n > max_depth 
  = solverDepthErrorTcS n stack

  | otherwise 
  = do { traceTcS "solveInteractWithDepth" $ 
              vcat [ text "Current depth =" <+> ppr n
                   , text "Max depth =" <+> ppr max_depth
                   , text "ws =" <+> ppr ws ]


       ; foldrWorkListM (solveOneWithDepth ctxt) inert ws }
              -- use foldr to preserve the order

------------------
-- Fully interact the given work item with an inert set, and return a
-- new inert set which has assimilated the new information.
solveOneWithDepth :: (Int, Int, [WorkItem])
                  -> WorkItem -> InertSet -> TcS InertSet
solveOneWithDepth (max_depth, depth, stack) work inert
  = do { traceFireTcS depth (text "Solving {" <+> ppr work)
       ; (new_inert, new_work) <- runSolverPipeline depth thePipeline inert work
         
	 -- Recursively solve the new work generated 
         -- from workItem, with a greater depth
       ; res_inert <- solveInteractWithDepth (max_depth, depth+1, work:stack) new_work new_inert 

       ; traceFireTcS depth (text "Done }" <+> ppr work) 

       ; return res_inert }

thePipeline :: [(String,SimplifierStage)]
thePipeline = [ ("interact with inert eqs", interactWithInertEqsStage)
              , ("interact with inerts",    interactWithInertsStage)
              , ("spontaneous solve",       spontaneousSolveStage)
              , ("top-level reactions",     topReactionsStage) ]
\end{code}

*********************************************************************************
*                                                                               * 
                       The spontaneous-solve Stage
*                                                                               *
*********************************************************************************

Note [Efficient Orientation] 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are two cases where we have to be careful about 
orienting equalities to get better efficiency. 

Case 1: In Rewriting Equalities (function rewriteEqLHS) 

    When rewriting two equalities with the same LHS:
          (a)  (tv ~ xi1) 
          (b)  (tv ~ xi2) 
    We have a choice of producing work (xi1 ~ xi2) (up-to the
    canonicalization invariants) However, to prevent the inert items
    from getting kicked out of the inerts first, we prefer to
    canonicalize (xi1 ~ xi2) if (b) comes from the inert set, or (xi2
    ~ xi1) if (a) comes from the inert set.
    
    This choice is implemented using the WhichComesFromInert flag. 

Case 2: Functional Dependencies 
    Again, we should prefer, if possible, the inert variables on the RHS

Case 3: IP improvement work
    We must always rewrite so that the inert type is on the right. 

\begin{code}
spontaneousSolveStage :: SimplifierStage 
spontaneousSolveStage depth workItem inerts 
  = do { mSolve <- trySpontaneousSolve workItem

       ; case mSolve of 
           SPCantSolve -> -- No spontaneous solution for him, keep going
               return $ SR { sr_new_work   = emptyWorkList
                           , sr_inerts     = inerts
                           , sr_stop       = ContinueWith workItem }

           SPSolved workItem'
               | not (isGivenCt workItem) 
	       	 -- Original was wanted or derived but we have now made him 
                 -- given so we have to interact him with the inerts due to
                 -- its status change. This in turn may produce more work.
		 -- We do this *right now* (rather than just putting workItem'
		 -- back into the work-list) because we've solved 
               -> do { bumpStepCountTcS
	       	     ; traceFireTcS depth (ptext (sLit "Spontaneous (w/d)") <+> ppr workItem)
                     ; (new_inert, new_work) <- runSolverPipeline depth
                             [ ("recursive interact with inert eqs", interactWithInertEqsStage)
                             , ("recursive interact with inerts", interactWithInertsStage)
                             ] inerts workItem'
                     ; return $ SR { sr_new_work = new_work 
                                   , sr_inerts   = new_inert -- will include workItem' 
                                   , sr_stop     = Stop }
                     }
               | otherwise 
                   -> -- Original was given; he must then be inert all right, and
                      -- workList' are all givens from flattening
                      do { bumpStepCountTcS
	       	         ; traceFireTcS depth (ptext (sLit "Spontaneous (g)") <+> ppr workItem)
                         ; return $ SR { sr_new_work = emptyWorkList
                                       , sr_inerts   = inerts `updInertSet` workItem' 
                                       , sr_stop     = Stop } }
           SPError -> -- Return with no new work
               return $ SR { sr_new_work = emptyWorkList
                           , sr_inerts   = inerts
                           , sr_stop     = Stop }
       }

data SPSolveResult = SPCantSolve | SPSolved WorkItem | SPError
-- SPCantSolve means that we can't do the unification because e.g. the variable is untouchable
-- SPSolved workItem' gives us a new *given* to go on 
-- SPError means that it's completely impossible to solve this equality, eg due to a kind error


-- @trySpontaneousSolve wi@ solves equalities where one side is a
-- touchable unification variable.
--     	    See Note [Touchables and givens] 
trySpontaneousSolve :: WorkItem -> TcS SPSolveResult
trySpontaneousSolve workItem@(CTyEqCan { cc_id = cv, cc_flavor = gw, cc_tyvar = tv1, cc_rhs = xi })
  | isGiven gw
  = return SPCantSolve
  | Just tv2 <- tcGetTyVar_maybe xi
  = do { tch1 <- isTouchableMetaTyVar tv1
       ; tch2 <- isTouchableMetaTyVar tv2
       ; case (tch1, tch2) of
           (True,  True)  -> trySpontaneousEqTwoWay cv gw tv1 tv2
           (True,  False) -> trySpontaneousEqOneWay cv gw tv1 xi
           (False, True)  -> trySpontaneousEqOneWay cv gw tv2 (mkTyVarTy tv1)
	   _ -> return SPCantSolve }
  | otherwise
  = do { tch1 <- isTouchableMetaTyVar tv1
       ; if tch1 then trySpontaneousEqOneWay cv gw tv1 xi
                 else do { traceTcS "Untouchable LHS, can't spontaneously solve workitem:" 
                                    (ppr workItem) 
                         ; return SPCantSolve }
       }

  -- No need for 
  --      trySpontaneousSolve (CFunEqCan ...) = ...
  -- See Note [No touchables as FunEq RHS] in TcSMonad
trySpontaneousSolve _ = return SPCantSolve

----------------
trySpontaneousEqOneWay :: CoVar -> CtFlavor -> TcTyVar -> Xi -> TcS SPSolveResult
-- tv is a MetaTyVar, not untouchable
trySpontaneousEqOneWay cv gw tv xi	
  | not (isSigTyVar tv) || isTyVarTy xi 
  = do { let kxi = typeKind xi -- NB: 'xi' is fully rewritten according to the inerts 
                               -- so we have its more specific kind in our hands
       ; if kxi `isSubKind` tyVarKind tv then
             solveWithIdentity cv gw tv xi
         else return SPCantSolve
{-
         else if tyVarKind tv `isSubKind` kxi then
             return SPCantSolve -- kinds are compatible but we can't solveWithIdentity this way
                                -- This case covers the  a_touchable :: * ~ b_untouchable :: ?? 
                                -- which has to be deferred or floated out for someone else to solve 
                                -- it in a scope where 'b' is no longer untouchable.
         else do { addErrorTcS KindError gw (mkTyVarTy tv) xi -- See Note [Kind errors]
                 ; return SPError }
-}
       }
  | otherwise -- Still can't solve, sig tyvar and non-variable rhs
  = return SPCantSolve

----------------
trySpontaneousEqTwoWay :: CoVar -> CtFlavor -> TcTyVar -> TcTyVar -> TcS SPSolveResult
-- Both tyvars are *touchable* MetaTyvars so there is only a chance for kind error here
trySpontaneousEqTwoWay cv gw tv1 tv2
  | k1 `isSubKind` k2
  , nicer_to_update_tv2 = solveWithIdentity cv gw tv2 (mkTyVarTy tv1)
  | k2 `isSubKind` k1 
  = solveWithIdentity cv gw tv1 (mkTyVarTy tv2)
  | otherwise -- None is a subkind of the other, but they are both touchable! 
  = return SPCantSolve
    -- do { addErrorTcS KindError gw (mkTyVarTy tv1) (mkTyVarTy tv2)
    --   ; return SPError }
  where
    k1 = tyVarKind tv1
    k2 = tyVarKind tv2
    nicer_to_update_tv2 = isSigTyVar tv1 || isSystemName (Var.varName tv2)
\end{code}

Note [Kind errors] 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the wanted problem: 
      alpha ~ (# Int, Int #) 
where alpha :: ?? and (# Int, Int #) :: (#). We can't spontaneously solve this constraint, 
but we should rather reject the program that give rise to it. If 'trySpontaneousEqTwoWay' 
simply returns @CantSolve@ then that wanted constraint is going to propagate all the way and 
get quantified over in inference mode. That's bad because we do know at this point that the 
constraint is insoluble. Instead, we call 'recKindErrorTcS' here, which will fail later on.

The same applies in canonicalization code in case of kind errors in the givens. 

However, when we canonicalize givens we only check for compatibility (@compatKind@). 
If there were a kind error in the givens, this means some form of inconsistency or dead code.

You may think that when we spontaneously solve wanteds we may have to look through the 
bindings to determine the right kind of the RHS type. E.g one may be worried that xi is 
@alpha@ where alpha :: ? and a previous spontaneous solving has set (alpha := f) with (f :: *).
But we orient our constraints so that spontaneously solved ones can rewrite all other constraint
so this situation can't happen. 

Note [Spontaneous solving and kind compatibility] 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note that our canonical constraints insist that *all* equalities (tv ~
xi) or (F xis ~ rhs) require the LHS and the RHS to have *compatible*
the same kinds.  ("compatible" means one is a subKind of the other.)

  - It can't be *equal* kinds, because
     b) wanted constraints don't necessarily have identical kinds
               eg   alpha::? ~ Int
     b) a solved wanted constraint becomes a given

  - SPJ thinks that *given* constraints (tv ~ tau) always have that
    tau has a sub-kind of tv; and when solving wanted constraints
    in trySpontaneousEqTwoWay we re-orient to achieve this.

  - Note that the kind invariant is maintained by rewriting.
    Eg wanted1 rewrites wanted2; if both were compatible kinds before,
       wanted2 will be afterwards.  Similarly givens.

Caveat:
  - Givens from higher-rank, such as: 
          type family T b :: * -> * -> * 
          type instance T Bool = (->) 

          f :: forall a. ((T a ~ (->)) => ...) -> a -> ... 
          flop = f (...) True 
     Whereas we would be able to apply the type instance, we would not be able to 
     use the given (T Bool ~ (->)) in the body of 'flop' 


Note [Avoid double unifications] 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The spontaneous solver has to return a given which mentions the unified unification
variable *on the left* of the equality. Here is what happens if not: 
  Original wanted:  (a ~ alpha),  (alpha ~ Int) 
We spontaneously solve the first wanted, without changing the order! 
      given : a ~ alpha      [having unified alpha := a] 
Now the second wanted comes along, but he cannot rewrite the given, so we simply continue.
At the end we spontaneously solve that guy, *reunifying*  [alpha := Int] 

We avoid this problem by orienting the resulting given so that the unification
variable is on the left.  [Note that alternatively we could attempt to
enforce this at canonicalization]

See also Note [No touchables as FunEq RHS] in TcSMonad; avoiding
double unifications is the main reason we disallow touchable
unification variables as RHS of type family equations: F xis ~ alpha.

\begin{code}
----------------

solveWithIdentity :: CoVar -> CtFlavor -> TcTyVar -> Xi -> TcS SPSolveResult
-- Solve with the identity coercion 
-- Precondition: kind(xi) is a sub-kind of kind(tv)
-- Precondition: CtFlavor is Wanted or Derived
-- See [New Wanted Superclass Work] to see why solveWithIdentity 
--     must work for Derived as well as Wanted
-- Returns: workItem where 
--        workItem = the new Given constraint
solveWithIdentity cv wd tv xi 
  = do { traceTcS "Sneaky unification:" $ 
                       vcat [text "Coercion variable:  " <+> ppr wd, 
                             text "Coercion:           " <+> pprEq (mkTyVarTy tv) xi,
                             text "Left  Kind is     : " <+> ppr (typeKind (mkTyVarTy tv)),
                             text "Right Kind is     : " <+> ppr (typeKind xi)
                  ]

       ; setWantedTyBind tv xi
       ; cv_given <- newGivenCoVar (mkTyVarTy tv) xi xi

       ; when (isWanted wd) (setCoBind cv xi)
           -- We don't want to do this for Derived, that's why we use 'when (isWanted wd)'

       ; return $ SPSolved (CTyEqCan { cc_id = cv_given
                                     , cc_flavor = mkGivenFlavor wd UnkSkol
                                     , cc_tyvar  = tv, cc_rhs = xi }) }
\end{code}


*********************************************************************************
*                                                                               * 
                       The interact-with-inert Stage
*                                                                               *
*********************************************************************************

Note [The Solver Invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
We always add Givens first.  So you might think that the solver has
the invariant

   If the work-item is Given, 
   then the inert item must Given

But this isn't quite true.  Suppose we have, 
    c1: [W] beta ~ [alpha], c2 : [W] blah, c3 :[W] alpha ~ Int
After processing the first two, we get
     c1: [G] beta ~ [alpha], c2 : [W] blah
Now, c3 does not interact with the the given c1, so when we spontaneously
solve c3, we must re-react it with the inert set.  So we can attempt a 
reaction between inert c2 [W] and work-item c3 [G].

It *is* true that [Solver Invariant]
   If the work-item is Given, 
   AND there is a reaction
   then the inert item must Given
or, equivalently,
   If the work-item is Given, 
   and the inert item is Wanted/Derived
   then there is no reaction

\begin{code}
-- Interaction result of  WorkItem <~> AtomicInert
data InteractResult
   = IR { ir_stop         :: StopOrContinue
            -- Stop
            --   => Reagent (work item) consumed.
            -- ContinueWith new_reagent
            --   => Reagent transformed but keep gathering interactions. 
            --      The transformed item remains inert with respect 
            --      to any previously encountered inerts.

        , ir_inert_action :: InertAction
            -- Whether the inert item should remain in the InertSet.

        , ir_new_work     :: WorkList
            -- new work items to add to the WorkList

        , ir_fire :: Maybe String    -- Tells whether a rule fired, and if so what
        }

-- What to do with the inert reactant.
data InertAction = KeepInert | DropInert 

mkIRContinue :: String -> WorkItem -> InertAction -> WorkList -> TcS InteractResult
mkIRContinue rule wi keep newWork 
  = return $ IR { ir_stop = ContinueWith wi, ir_inert_action = keep
                , ir_new_work = newWork, ir_fire = Just rule }

mkIRStopK :: String -> WorkList -> TcS InteractResult
mkIRStopK rule newWork
  = return $ IR { ir_stop = Stop, ir_inert_action = KeepInert
                , ir_new_work = newWork, ir_fire = Just rule }

mkIRStopD :: String -> WorkList -> TcS InteractResult
mkIRStopD rule newWork
  = return $ IR { ir_stop = Stop, ir_inert_action = DropInert
                , ir_new_work = newWork, ir_fire = Just rule }

noInteraction :: Monad m => WorkItem -> m InteractResult
noInteraction wi
  = return $ IR { ir_stop = ContinueWith wi, ir_inert_action = KeepInert
                , ir_new_work = emptyWorkList, ir_fire = Nothing }

data WhichComesFromInert = LeftComesFromInert | RightComesFromInert 
     -- See Note [Efficient Orientation] 


---------------------------------------------------
-- Interact a single WorkItem with the equalities of an inert set as
-- far as possible, i.e. until we get a Stop result from an individual
-- reaction (i.e. when the WorkItem is consumed), or until we've
-- interact the WorkItem with the entire equalities of the InertSet

interactWithInertEqsStage :: SimplifierStage 
interactWithInertEqsStage depth workItem inert
  = Bag.foldrBagM (interactNext depth) initITR (inert_eqs inert)
                        -- use foldr to preserve the order          
  where
    initITR = SR { sr_inerts   = inert { inert_eqs = emptyCCan }
                 , sr_new_work = emptyWorkList
                 , sr_stop     = ContinueWith workItem }

---------------------------------------------------
-- Interact a single WorkItem with *non-equality* constraints in the inert set. 
-- Precondition: equality interactions must have already happened, hence we have 
-- to pick up some information from the incoming inert, before folding over the 
-- "Other" constraints it contains!

interactWithInertsStage :: SimplifierStage
interactWithInertsStage depth workItem inert
  = let (relevant, inert_residual) = getISRelevant workItem inert 
        initITR = SR { sr_inerts   = inert_residual
                     , sr_new_work = emptyWorkList
                     , sr_stop     = ContinueWith workItem } 
    in Bag.foldrBagM (interactNext depth) initITR relevant 
                        -- use foldr to preserve the order
  where 
    getISRelevant :: CanonicalCt -> InertSet -> (CanonicalCts, InertSet) 
    getISRelevant (CFrozenErr {}) is = (emptyCCan, is)
                  -- Nothing s relevant; we have alread interacted
                  -- it with the equalities in the inert set

    getISRelevant (CDictCan { cc_class = cls } ) is
      = let (relevant, residual_map) = getRelevantCts cls (inert_dicts is)
        in (relevant, is { inert_dicts = residual_map }) 
    getISRelevant (CFunEqCan { cc_fun = tc } ) is 
      = let (relevant, residual_map) = getRelevantCts tc (inert_funeqs is) 
        in (relevant, is { inert_funeqs = residual_map })
    getISRelevant (CIPCan { cc_ip_nm = nm }) is 
      = let (relevant, residual_map) = getRelevantCts nm (inert_ips is)
        in (relevant, is { inert_ips = residual_map }) 
    -- An equality, finally, may kick everything except equalities out 
    -- because we have already interacted the equalities in interactWithInertEqsStage
    getISRelevant _eq_ct is  -- Equality, everything is relevant for this one 
                             -- TODO: if we were caching variables, we'd know that only 
                             --       some are relevant. Experiment with this for now. 
      = let cts = cCanMapToBag (inert_ips is) `unionBags` 
                    cCanMapToBag (inert_dicts is) `unionBags` cCanMapToBag (inert_funeqs is)
        in (cts, is { inert_dicts  = emptyCCanMap
                    , inert_ips    = emptyCCanMap
                    , inert_funeqs = emptyCCanMap })

interactNext :: SubGoalDepth -> AtomicInert -> StageResult -> TcS StageResult 
interactNext depth inert it
  | ContinueWith work_item <- sr_stop it
  = do { let inerts = sr_inerts it 

       ; IR { ir_new_work = new_work, ir_inert_action = inert_action
            , ir_fire = fire_info, ir_stop = stop } 
            <- interactWithInert inert work_item

       ; let mk_msg rule 
      	       = text rule <+> keep_doc
      	         <+> vcat [ ptext (sLit "Inert =") <+> ppr inert
      	                  , ptext (sLit "Work =")  <+> ppr work_item
      	                  , ppUnless (isEmptyWorkList new_work) $
                            ptext (sLit "New =") <+> ppr new_work ]
             keep_doc = case inert_action of
                 	  KeepInert -> ptext (sLit "[keep]")
                 	  DropInert -> ptext (sLit "[drop]")
       ; case fire_info of
           Just rule -> do { bumpStepCountTcS
                           ; traceFireTcS depth (mk_msg rule) }
           Nothing  -> return ()

       -- New inerts depend on whether we KeepInert or not 
       ; let inerts_new = case inert_action of
                            KeepInert -> inerts `updInertSet` inert
                            DropInert -> inerts

       ; return $ SR { sr_inerts   = inerts_new
                     , sr_new_work = sr_new_work it `unionWorkList` new_work
                     , sr_stop     = stop } }
  | otherwise 
  = return $ it { sr_inerts = (sr_inerts it) `updInertSet` inert }

-- Do a single interaction of two constraints.
interactWithInert :: AtomicInert -> WorkItem -> TcS InteractResult
interactWithInert inert workItem 
  = do { ctxt <- getTcSContext
       ; let is_allowed  = allowedInteraction (simplEqsOnly ctxt) inert workItem 

       ; if is_allowed then 
              doInteractWithInert inert workItem 
          else 
              noInteraction workItem 
       }

allowedInteraction :: Bool -> AtomicInert -> WorkItem -> Bool 
-- Allowed interactions 
allowedInteraction eqs_only (CDictCan {}) (CDictCan {}) = not eqs_only
allowedInteraction eqs_only (CIPCan {})   (CIPCan {})   = not eqs_only
allowedInteraction _ _ _ = True 

--------------------------------------------
doInteractWithInert :: CanonicalCt -> CanonicalCt -> TcS InteractResult
-- Identical class constraints.

doInteractWithInert
  inertItem@(CDictCan { cc_id = d1, cc_flavor = fl1, cc_class = cls1, cc_tyargs = tys1 }) 
   workItem@(CDictCan { cc_id = d2, cc_flavor = fl2, cc_class = cls2, cc_tyargs = tys2 })
  | cls1 == cls2 && (and $ zipWith tcEqType tys1 tys2)
  = solveOneFromTheOther "Cls/Cls" (EvId d1,fl1) workItem 

  | cls1 == cls2 && (not (isGiven fl1 && isGiven fl2))
  = 	 -- See Note [When improvement happens]
    do { let pty1 = ClassP cls1 tys1
             pty2 = ClassP cls2 tys2
             inert_pred_loc     = (pty1, pprFlavorArising fl1)
             work_item_pred_loc = (pty2, pprFlavorArising fl2)
             fd_eqns = improveFromAnother 
                                  inert_pred_loc     -- the template
                                  work_item_pred_loc -- the one we aim to rewrite
                                  -- See Note [Efficient Orientation]

       ; m <- rewriteWithFunDeps fd_eqns tys2 fl2
       ; case m of 
           Nothing -> noInteraction workItem
           Just (rewritten_tys2, cos2, fd_work)
             | tcEqTypes tys1 rewritten_tys2
             -> -- Solve him on the spot in this case
	     	case fl2 of
	          Given   {} -> pprPanic "Unexpected given" (ppr inertItem $$ ppr workItem)
                  Derived {} -> mkIRStopK "Cls/Cls fundep (solved)" fd_work
		  Wanted  {} 
		    | isDerived fl1 
                   -> do { setDictBind d2 (EvCast d1 dict_co)
			 ; let inert_w = inertItem { cc_flavor = fl2 }
			   -- A bit naughty: we take the inert Derived, 
			   -- turn it into a Wanted, use it to solve the work-item
			   -- and put it back into the work-list
			   -- Maybe rather than starting again, we could *replace* the
			   -- inert item, but its safe and simple to restart
                         ; mkIRStopD "Cls/Cls fundep (solved)" $ 
                           workListFromNonEq inert_w `unionWorkList` fd_work }
		    | otherwise 
                    -> do { setDictBind d2 (EvCast d1 dict_co)
                          ; mkIRStopK "Cls/Cls fundep (solved)" fd_work }

             | otherwise
             -> -- We could not quite solve him, but we still rewrite him
	        -- Example: class C a b c | a -> b
		--          Given: C Int Bool x, Wanted: C Int beta y
		--          Then rewrite the wanted to C Int Bool y
		--          but note that is still not identical to the given
		-- The important thing is that the rewritten constraint is
		-- inert wrt the given.
		-- However it is not necessarily inert wrt previous inert-set items.
                --      class C a b c d |  a -> b, b c -> d
		--      Inert: c1: C b Q R S, c2: C P Q a b
		--      Work: C P alpha R beta
		--      Does not react with c1; reacts with c2, with alpha:=Q
		--      NOW it reacts with c1!
		-- So we must stop, and put the rewritten constraint back in the work list
                do { d2' <- newDictVar cls1 rewritten_tys2
                   ; case fl2 of
                       Given {}   -> pprPanic "Unexpected given" (ppr inertItem $$ ppr workItem)
                       Wanted {}  -> setDictBind d2 (EvCast d2' dict_co)
                       Derived {} -> return ()
                   ; let workItem' = workItem { cc_id = d2', cc_tyargs = rewritten_tys2 }
                   ; mkIRStopK "Cls/Cls fundep (partial)" $ 
                     workListFromNonEq workItem' `unionWorkList` fd_work } 

             where
               dict_co = mkTyConCoercion (classTyCon cls1) cos2
  }

-- Class constraint and given equality: use the equality to rewrite
-- the class constraint. 
doInteractWithInert (CTyEqCan { cc_id = cv, cc_flavor = ifl, cc_tyvar = tv, cc_rhs = xi }) 
                    (CDictCan { cc_id = dv, cc_flavor = wfl, cc_class = cl, cc_tyargs = xis }) 
  | ifl `canRewrite` wfl 
  , tv `elemVarSet` tyVarsOfTypes xis
  = do { rewritten_dict <- rewriteDict (cv,tv,xi) (dv,wfl,cl,xis)
            -- Continue with rewritten Dictionary because we can only be in the 
            -- interactWithEqsStage, so the dictionary is inert. 
       ; mkIRContinue "Eq/Cls" rewritten_dict KeepInert emptyWorkList }
    
doInteractWithInert (CDictCan { cc_id = dv, cc_flavor = ifl, cc_class = cl, cc_tyargs = xis }) 
           workItem@(CTyEqCan { cc_id = cv, cc_flavor = wfl, cc_tyvar = tv, cc_rhs = xi })
  | wfl `canRewrite` ifl
  , tv `elemVarSet` tyVarsOfTypes xis
  = do { rewritten_dict <- rewriteDict (cv,tv,xi) (dv,ifl,cl,xis)
       ; mkIRContinue "Cls/Eq" workItem DropInert (workListFromNonEq rewritten_dict) }

-- Class constraint and given equality: use the equality to rewrite
-- the class constraint.
doInteractWithInert (CTyEqCan { cc_id = cv, cc_flavor = ifl, cc_tyvar = tv, cc_rhs = xi }) 
                    (CIPCan { cc_id = ipid, cc_flavor = wfl, cc_ip_nm = nm, cc_ip_ty = ty }) 
  | ifl `canRewrite` wfl
  , tv `elemVarSet` tyVarsOfType ty 
  = do { rewritten_ip <- rewriteIP (cv,tv,xi) (ipid,wfl,nm,ty) 
       ; mkIRContinue "Eq/IP" rewritten_ip KeepInert emptyWorkList } 

doInteractWithInert (CIPCan { cc_id = ipid, cc_flavor = ifl, cc_ip_nm = nm, cc_ip_ty = ty }) 
           workItem@(CTyEqCan { cc_id = cv, cc_flavor = wfl, cc_tyvar = tv, cc_rhs = xi })
  | wfl `canRewrite` ifl
  , tv `elemVarSet` tyVarsOfType ty
  = do { rewritten_ip <- rewriteIP (cv,tv,xi) (ipid,ifl,nm,ty) 
       ; mkIRContinue "IP/Eq" workItem DropInert (workListFromNonEq rewritten_ip) }

-- Two implicit parameter constraints.  If the names are the same,
-- but their types are not, we generate a wanted type equality 
-- that equates the type (this is "improvement").  
-- However, we don't actually need the coercion evidence,
-- so we just generate a fresh coercion variable that isn't used anywhere.
doInteractWithInert (CIPCan { cc_id = id1, cc_flavor = ifl, cc_ip_nm = nm1, cc_ip_ty = ty1 }) 
           workItem@(CIPCan { cc_flavor = wfl, cc_ip_nm = nm2, cc_ip_ty = ty2 })
  | nm1 == nm2 && isGiven wfl && isGiven ifl
  = 	-- See Note [Overriding implicit parameters]
        -- Dump the inert item, override totally with the new one
	-- Do not require type equality
	-- For example, given let ?x::Int = 3 in let ?x::Bool = True in ...
	--              we must *override* the outer one with the inner one
    mkIRContinue "IP/IP override" workItem DropInert emptyWorkList

  | nm1 == nm2 && ty1 `tcEqType` ty2 
  = solveOneFromTheOther "IP/IP" (EvId id1,ifl) workItem 

  | nm1 == nm2
  =  	-- See Note [When improvement happens]
    do { co_var <- newCoVar ty2 ty1 -- See Note [Efficient Orientation]
       ; let flav = Wanted (combineCtLoc ifl wfl) 
       ; cans <- mkCanonical flav co_var 
       ; mkIRContinue "IP/IP fundep" workItem KeepInert cans }

-- Never rewrite a given with a wanted equality, and a type function
-- equality can never rewrite an equality. We rewrite LHS *and* RHS 
-- of function equalities so that our inert set exposes everything that 
-- we know about equalities.

-- Inert: equality, work item: function equality
doInteractWithInert (CTyEqCan { cc_id = cv1, cc_flavor = ifl, cc_tyvar = tv, cc_rhs = xi1 }) 
                    (CFunEqCan { cc_id = cv2, cc_flavor = wfl, cc_fun = tc
                               , cc_tyargs = args, cc_rhs = xi2 })
  | ifl `canRewrite` wfl 
  , tv `elemVarSet` tyVarsOfTypes (xi2:args) -- Rewrite RHS as well
  = do { rewritten_funeq <- rewriteFunEq (cv1,tv,xi1) (cv2,wfl,tc,args,xi2) 
       ; mkIRStopK "Eq/FunEq" (workListFromEq rewritten_funeq) } 
         -- Must Stop here, because we may no longer be inert after the rewritting.

-- Inert: function equality, work item: equality
doInteractWithInert (CFunEqCan {cc_id = cv1, cc_flavor = ifl, cc_fun = tc
                              , cc_tyargs = args, cc_rhs = xi1 }) 
           workItem@(CTyEqCan { cc_id = cv2, cc_flavor = wfl, cc_tyvar = tv, cc_rhs = xi2 })
  | wfl `canRewrite` ifl
  , tv `elemVarSet` tyVarsOfTypes (xi1:args) -- Rewrite RHS as well
  = do { rewritten_funeq <- rewriteFunEq (cv2,tv,xi2) (cv1,ifl,tc,args,xi1) 
       ; mkIRContinue "FunEq/Eq" workItem DropInert (workListFromEq rewritten_funeq) } 
         -- One may think that we could (KeepTransformedInert rewritten_funeq) 
         -- but that is wrong, because it may end up not being inert with respect 
         -- to future inerts. Example: 
         -- Original inert = {    F xis ~  [a], b ~ Maybe Int } 
         -- Work item comes along = a ~ [b] 
         -- If we keep { F xis ~ [b] } in the inert set we will end up with: 
         --      { F xis ~ [b], b ~ Maybe Int, a ~ [Maybe Int] } 
         -- At the end, which is *not* inert. So we should unfortunately DropInert here.

doInteractWithInert (CFunEqCan { cc_id = cv1, cc_flavor = fl1, cc_fun = tc1
                               , cc_tyargs = args1, cc_rhs = xi1 }) 
           workItem@(CFunEqCan { cc_id = cv2, cc_flavor = fl2, cc_fun = tc2
                               , cc_tyargs = args2, cc_rhs = xi2 })
  | fl1 `canSolve` fl2 && lhss_match
  = do { cans <- rewriteEqLHS LeftComesFromInert  (mkCoVarCoercion cv1,xi1) (cv2,fl2,xi2) 
       ; mkIRStopK "FunEq/FunEq" cans } 
  | fl2 `canSolve` fl1 && lhss_match
  = do { cans <- rewriteEqLHS RightComesFromInert (mkCoVarCoercion cv2,xi2) (cv1,fl1,xi1) 
       ; mkIRContinue "FunEq/FunEq" workItem DropInert cans }
  where
    lhss_match = tc1 == tc2 && and (zipWith tcEqType args1 args2) 

doInteractWithInert (CTyEqCan { cc_id = cv1, cc_flavor = fl1, cc_tyvar = tv1, cc_rhs = xi1 }) 
           workItem@(CTyEqCan { cc_id = cv2, cc_flavor = fl2, cc_tyvar = tv2, cc_rhs = xi2 })
-- Check for matching LHS 
  | fl1 `canSolve` fl2 && tv1 == tv2 
  = do { cans <- rewriteEqLHS LeftComesFromInert (mkCoVarCoercion cv1,xi1) (cv2,fl2,xi2) 
       ; mkIRStopK "Eq/Eq lhs" cans } 

  | fl2 `canSolve` fl1 && tv1 == tv2 
  = do { cans <- rewriteEqLHS RightComesFromInert (mkCoVarCoercion cv2,xi2) (cv1,fl1,xi1) 
       ; mkIRContinue "Eq/Eq lhs" workItem DropInert cans }

-- Check for rewriting RHS 
  | fl1 `canRewrite` fl2 && tv1 `elemVarSet` tyVarsOfType xi2 
  = do { rewritten_eq <- rewriteEqRHS (cv1,tv1,xi1) (cv2,fl2,tv2,xi2) 
       ; mkIRStopK "Eq/Eq rhs" rewritten_eq }

  | fl2 `canRewrite` fl1 && tv2 `elemVarSet` tyVarsOfType xi1
  = do { rewritten_eq <- rewriteEqRHS (cv2,tv2,xi2) (cv1,fl1,tv1,xi1) 
       ; mkIRContinue "Eq/Eq rhs" workItem DropInert rewritten_eq } 

doInteractWithInert (CTyEqCan   { cc_id = cv1, cc_flavor = fl1, cc_tyvar = tv1, cc_rhs = xi1 })
                    (CFrozenErr { cc_id = cv2, cc_flavor = fl2 })
  | fl1 `canRewrite` fl2 && tv1 `elemVarSet` tyVarsOfEvVar cv2
  = do { rewritten_frozen <- rewriteFrozen (cv1, tv1, xi1) (cv2, fl2)
       ; mkIRStopK "Frozen/Eq" rewritten_frozen }

doInteractWithInert (CFrozenErr { cc_id = cv2, cc_flavor = fl2 })
           workItem@(CTyEqCan   { cc_id = cv1, cc_flavor = fl1, cc_tyvar = tv1, cc_rhs = xi1 })
  | fl1 `canRewrite` fl2 && tv1 `elemVarSet` tyVarsOfEvVar cv2
  = do { rewritten_frozen <- rewriteFrozen (cv1, tv1, xi1) (cv2, fl2)
       ; mkIRContinue "Frozen/Eq" workItem DropInert rewritten_frozen }

-- Fall-through case for all other situations
doInteractWithInert _ workItem = noInteraction workItem

-------------------------
-- Equational Rewriting 
rewriteDict  :: (CoVar, TcTyVar, Xi) -> (DictId, CtFlavor, Class, [Xi]) -> TcS CanonicalCt
rewriteDict (cv,tv,xi) (dv,gw,cl,xis) 
  = do { let cos  = substTysWith [tv] [mkCoVarCoercion cv] xis -- xis[tv] ~ xis[xi]
             args = substTysWith [tv] [xi] xis
             con  = classTyCon cl 
             dict_co = mkTyConCoercion con cos 
       ; dv' <- newDictVar cl args 
       ; case gw of 
           Wanted {}         -> setDictBind dv (EvCast dv' (mkSymCoercion dict_co))
           Given {}          -> setDictBind dv' (EvCast dv dict_co) 
           Derived {}        -> return () -- Derived dicts we don't set any evidence

       ; return (CDictCan { cc_id = dv'
                          , cc_flavor = gw 
                          , cc_class = cl 
                          , cc_tyargs = args }) } 

rewriteIP :: (CoVar,TcTyVar,Xi) -> (EvVar,CtFlavor, IPName Name, TcType) -> TcS CanonicalCt 
rewriteIP (cv,tv,xi) (ipid,gw,nm,ty) 
  = do { let ip_co = substTyWith [tv] [mkCoVarCoercion cv] ty     -- ty[tv] ~ t[xi] 
             ty'   = substTyWith [tv] [xi] ty
       ; ipid' <- newIPVar nm ty' 
       ; case gw of 
           Wanted {}         -> setIPBind ipid  (EvCast ipid' (mkSymCoercion ip_co))
           Given {}          -> setIPBind ipid' (EvCast ipid ip_co) 
           Derived {}        -> return () -- Derived ips: we don't set any evidence

       ; return (CIPCan { cc_id = ipid'
                        , cc_flavor = gw
                        , cc_ip_nm = nm
                        , cc_ip_ty = ty' }) }
   
rewriteFunEq :: (CoVar,TcTyVar,Xi) -> (CoVar,CtFlavor,TyCon, [Xi], Xi) -> TcS CanonicalCt
rewriteFunEq (cv1,tv,xi1) (cv2,gw, tc,args,xi2)                   -- cv2 :: F args ~ xi2
  = do { let arg_cos = substTysWith [tv] [mkCoVarCoercion cv1] args 
             args'   = substTysWith [tv] [xi1] args 
             fun_co  = mkTyConCoercion tc arg_cos                 -- fun_co :: F args ~ F args'

             xi2'    = substTyWith [tv] [xi1] xi2
             xi2_co  = substTyWith [tv] [mkCoVarCoercion cv1] xi2 -- xi2_co :: xi2 ~ xi2' 

       ; cv2' <- newCoVar (mkTyConApp tc args') xi2'
       ; case gw of 
           Wanted {} -> setCoBind cv2  (fun_co               `mkTransCoercion` 
                                        mkCoVarCoercion cv2' `mkTransCoercion` 
                                        mkSymCoercion xi2_co)
           Given {}  -> setCoBind cv2' (mkSymCoercion fun_co `mkTransCoercion` 
                                        mkCoVarCoercion cv2  `mkTransCoercion` 
                                        xi2_co)
           Derived {} -> return () 

       ; return (CFunEqCan { cc_id = cv2'
                           , cc_flavor = gw
                           , cc_tyargs = args'
                           , cc_fun = tc 
                           , cc_rhs = xi2' }) }


rewriteEqRHS :: (CoVar,TcTyVar,Xi) -> (CoVar,CtFlavor,TcTyVar,Xi) -> TcS WorkList
-- Use the first equality to rewrite the second, flavors already checked. 
-- E.g.          c1 : tv1 ~ xi1   c2 : tv2 ~ xi2
-- rewrites c2 to give
--               c2' : tv2 ~ xi2[xi1/tv1]
-- We must do an occurs check to sure the new constraint is canonical
-- So we might return an empty bag
rewriteEqRHS (cv1,tv1,xi1) (cv2,gw,tv2,xi2) 
  | Just tv2' <- tcGetTyVar_maybe xi2'
  , tv2 == tv2'	 -- In this case xi2[xi1/tv1] = tv2, so we have tv2~tv2
  = do { when (isWanted gw) (setCoBind cv2 (mkSymCoercion co2')) 
       ; return emptyWorkList } 
  | otherwise
  = do { cv2' <- newCoVar (mkTyVarTy tv2) xi2'
       ; case gw of
             Wanted {} -> setCoBind cv2 $ mkCoVarCoercion cv2' `mkTransCoercion` 
                                          mkSymCoercion co2'
             Given {}  -> setCoBind cv2' $ mkCoVarCoercion cv2 `mkTransCoercion` 
                                           co2'
             Derived {} -> return ()
       ; canEqToWorkList gw cv2' (mkTyVarTy tv2) xi2' }
  where 
    xi2' = substTyWith [tv1] [xi1] xi2 
    co2' = substTyWith [tv1] [mkCoVarCoercion cv1] xi2  -- xi2 ~ xi2[xi1/tv1]

rewriteEqLHS :: WhichComesFromInert -> (Coercion,Xi) -> (CoVar,CtFlavor,Xi) -> TcS WorkList
-- Used to ineract two equalities of the following form: 
-- First Equality:   co1: (XXX ~ xi1)  
-- Second Equality:  cv2: (XXX ~ xi2) 
-- Where the cv1 `canRewrite` cv2 equality 
-- We have an option of creating new work (xi1 ~ xi2) OR (xi2 ~ xi1), 
--    See Note [Efficient Orientation] for that 
rewriteEqLHS LeftComesFromInert (co1,xi1) (cv2,gw,xi2) 
  = do { cv2' <- newCoVar xi2 xi1 
       ; case gw of 
           Wanted {} -> setCoBind cv2 $ 
                        co1 `mkTransCoercion` mkSymCoercion (mkCoVarCoercion cv2')
           Given {}  -> setCoBind cv2' $ 
                        mkSymCoercion (mkCoVarCoercion cv2) `mkTransCoercion` co1 
           Derived {} -> return ()
       ; mkCanonical gw cv2' }

rewriteEqLHS RightComesFromInert (co1,xi1) (cv2,gw,xi2) 
  = do { cv2' <- newCoVar xi1 xi2
       ; case gw of
           Wanted {} -> setCoBind cv2 $
                        co1 `mkTransCoercion` mkCoVarCoercion cv2'
           Given {}  -> setCoBind cv2' $
                        mkSymCoercion co1 `mkTransCoercion` mkCoVarCoercion cv2
           Derived {} -> return ()
       ; mkCanonical gw cv2' }

rewriteFrozen :: (CoVar,TcTyVar,Xi) -> (CoVar,CtFlavor) -> TcS WorkList
rewriteFrozen (cv1, tv1, xi1) (cv2, fl2)
  = do { cv2' <- newCoVar ty2a' ty2b'  -- ty2a[xi1/tv1] ~ ty2b[xi1/tv1]
       ; case fl2 of
             Wanted {} -> setCoBind cv2 $ co2a'                `mkTransCoercion`
                       	         	  mkCoVarCoercion cv2' `mkTransCoercion`
                       	         	  mkSymCoercion co2b'

             Given {} -> setCoBind cv2' $ mkSymCoercion co2a'  `mkTransCoercion`
                      	 		  mkCoVarCoercion cv2  `mkTransCoercion`
                      	 		  co2b'

             Derived {} -> return ()

      ; return (workListFromNonEq $ CFrozenErr { cc_id = cv2', cc_flavor = fl2 }) }
  where
    (ty2a, ty2b) = coVarKind cv2          -- cv2 : ty2a ~ ty2b
    ty2a' = substTyWith [tv1] [xi1] ty2a
    ty2b' = substTyWith [tv1] [xi1] ty2b

    co2a' = substTyWith [tv1] [mkCoVarCoercion cv1] ty2a  -- ty2a ~ ty2a[xi1/tv1]
    co2b' = substTyWith [tv1] [mkCoVarCoercion cv1] ty2b  -- ty2b ~ ty2b[xi1/tv1]

solveOneFromTheOther :: String -> (EvTerm, CtFlavor) -> CanonicalCt -> TcS InteractResult
-- First argument inert, second argument work-item. They both represent 
-- wanted/given/derived evidence for the *same* predicate so 
-- we can discharge one directly from the other. 
--
-- Precondition: value evidence only (implicit parameters, classes) 
--               not coercion
solveOneFromTheOther info (ev_term,ifl) workItem
  | isDerived wfl
  = mkIRStopK ("Solved[DW] " ++ info) emptyWorkList

  | isDerived ifl -- The inert item is Derived, we can just throw it away, 
    	      	  -- The workItem is inert wrt earlier inert-set items, 
		  -- so it's safe to continue on from this point
  = mkIRContinue ("Solved[DI] " ++ info) workItem DropInert emptyWorkList
  
  | otherwise
  = ASSERT( ifl `canSolve` wfl )
      -- Because of Note [The Solver Invariant], plus Derived dealt with
    do { when (isWanted wfl) $ setEvBind wid ev_term
           -- Overwrite the binding, if one exists
	   -- If both are Given, we already have evidence; no need to duplicate
       ; mkIRStopK ("Solved " ++ info) emptyWorkList }
  where 
     wfl = cc_flavor workItem
     wid = cc_id workItem
\end{code}

Note [Superclasses and recursive dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Overlaps with Note [SUPERCLASS-LOOP 1]
                  Note [SUPERCLASS-LOOP 2]
                  Note [Recursive instances and superclases]
    ToDo: check overlap and delete redundant stuff

Right before adding a given into the inert set, we must
produce some more work, that will bring the superclasses 
of the given into scope. The superclass constraints go into 
our worklist. 

When we simplify a wanted constraint, if we first see a matching
instance, we may produce new wanted work. To (1) avoid doing this work 
twice in the future and (2) to handle recursive dictionaries we may ``cache'' 
this item as given into our inert set WITHOUT adding its superclass constraints, 
otherwise we'd be in danger of creating a loop [In fact this was the exact reason
for doing the isGoodRecEv check in an older version of the type checker]. 

But now we have added partially solved constraints to the worklist which may 
interact with other wanteds. Consider the example: 

Example 1: 

    class Eq b => Foo a b        --- 0-th selector
    instance Eq a => Foo [a] a   --- fooDFun

and wanted (Foo [t] t). We are first going to see that the instance matches 
and create an inert set that includes the solved (Foo [t] t) but not its superclasses:
       d1 :_g Foo [t] t                 d1 := EvDFunApp fooDFun d3 
Our work list is going to contain a new *wanted* goal
       d3 :_w Eq t 

Ok, so how do we get recursive dictionaries, at all: 

Example 2:

    data D r = ZeroD | SuccD (r (D r));
    
    instance (Eq (r (D r))) => Eq (D r) where
        ZeroD     == ZeroD     = True
        (SuccD a) == (SuccD b) = a == b
        _         == _         = False;
    
    equalDC :: D [] -> D [] -> Bool;
    equalDC = (==);

We need to prove (Eq (D [])). Here's how we go:

	d1 :_w Eq (D [])

by instance decl, holds if
	d2 :_w Eq [D []]
	where 	d1 = dfEqD d2

*BUT* we have an inert set which gives us (no superclasses): 
        d1 :_g Eq (D []) 
By the instance declaration of Eq we can show the 'd2' goal if 
	d3 :_w Eq (D [])
	where	d2 = dfEqList d3
		d1 = dfEqD d2
Now, however this wanted can interact with our inert d1 to set: 
        d3 := d1 
and solve the goal. Why was this interaction OK? Because, if we chase the 
evidence of d1 ~~> dfEqD d2 ~~-> dfEqList d3, so by setting d3 := d1 we 
are really setting
        d3 := dfEqD2 (dfEqList d3) 
which is FINE because the use of d3 is protected by the instance function 
applications. 

So, our strategy is to try to put solved wanted dictionaries into the
inert set along with their superclasses (when this is meaningful,
i.e. when new wanted goals are generated) but solve a wanted dictionary
from a given only in the case where the evidence variable of the
wanted is mentioned in the evidence of the given (recursively through
the evidence binds) in a protected way: more instance function applications 
than superclass selectors.

Here are some more examples from GHC's previous type checker


Example 3: 
This code arises in the context of "Scrap Your Boilerplate with Class"

    class Sat a
    class Data ctx a
    instance  Sat (ctx Char)             => Data ctx Char       -- dfunData1
    instance (Sat (ctx [a]), Data ctx a) => Data ctx [a]        -- dfunData2

    class Data Maybe a => Foo a    

    instance Foo t => Sat (Maybe t)                             -- dfunSat

    instance Data Maybe a => Foo a                              -- dfunFoo1
    instance Foo a        => Foo [a]                            -- dfunFoo2
    instance                 Foo [Char]                         -- dfunFoo3

Consider generating the superclasses of the instance declaration
	 instance Foo a => Foo [a]

So our problem is this
    d0 :_g Foo t
    d1 :_w Data Maybe [t] 

We may add the given in the inert set, along with its superclasses
[assuming we don't fail because there is a matching instance, see 
 tryTopReact, given case ]
  Inert:
    d0 :_g Foo t 
  WorkList 
    d01 :_g Data Maybe t  -- d2 := EvDictSuperClass d0 0 
    d1 :_w Data Maybe [t] 
Then d2 can readily enter the inert, and we also do solving of the wanted
  Inert: 
    d0 :_g Foo t 
    d1 :_s Data Maybe [t]           d1 := dfunData2 d2 d3 
  WorkList
    d2 :_w Sat (Maybe [t])          
    d3 :_w Data Maybe t
    d01 :_g Data Maybe t 
Now, we may simplify d2 more: 
  Inert:
      d0 :_g Foo t 
      d1 :_s Data Maybe [t]           d1 := dfunData2 d2 d3 
      d1 :_g Data Maybe [t] 
      d2 :_g Sat (Maybe [t])          d2 := dfunSat d4 
  WorkList: 
      d3 :_w Data Maybe t 
      d4 :_w Foo [t] 
      d01 :_g Data Maybe t 

Now, we can just solve d3.
  Inert
      d0 :_g Foo t 
      d1 :_s Data Maybe [t]           d1 := dfunData2 d2 d3 
      d2 :_g Sat (Maybe [t])          d2 := dfunSat d4 
  WorkList
      d4 :_w Foo [t] 
      d01 :_g Data Maybe t 
And now we can simplify d4 again, but since it has superclasses we *add* them to the worklist:
  Inert
      d0 :_g Foo t 
      d1 :_s Data Maybe [t]           d1 := dfunData2 d2 d3 
      d2 :_g Sat (Maybe [t])          d2 := dfunSat d4 
      d4 :_g Foo [t]                  d4 := dfunFoo2 d5 
  WorkList:
      d5 :_w Foo t 
      d6 :_g Data Maybe [t]           d6 := EvDictSuperClass d4 0
      d01 :_g Data Maybe t 
Now, d5 can be solved! (and its superclass enter scope) 
  Inert
      d0 :_g Foo t 
      d1 :_s Data Maybe [t]           d1 := dfunData2 d2 d3 
      d2 :_g Sat (Maybe [t])          d2 := dfunSat d4 
      d4 :_g Foo [t]                  d4 := dfunFoo2 d5 
      d5 :_g Foo t                    d5 := dfunFoo1 d7
  WorkList:
      d7 :_w Data Maybe t
      d6 :_g Data Maybe [t]
      d8 :_g Data Maybe t            d8 := EvDictSuperClass d5 0
      d01 :_g Data Maybe t 

Now, two problems: 
   [1] Suppose we pick d8 and we react him with d01. Which of the two givens should 
       we keep? Well, we *MUST NOT* drop d01 because d8 contains recursive evidence 
       that must not be used (look at case interactInert where both inert and workitem
       are givens). So we have several options: 
       - Drop the workitem always (this will drop d8)
              This feels very unsafe -- what if the work item was the "good" one
              that should be used later to solve another wanted?
       - Don't drop anyone: the inert set may contain multiple givens! 
              [This is currently implemented] 

The "don't drop anyone" seems the most safe thing to do, so now we come to problem 2: 
  [2] We have added both d6 and d01 in the inert set, and we are interacting our wanted
      d7. Now the [isRecDictEv] function in the ineration solver 
      [case inert-given workitem-wanted] will prevent us from interacting d7 := d8 
      precisely because chasing the evidence of d8 leads us to an unguarded use of d7. 

      So, no interaction happens there. Then we meet d01 and there is no recursion 
      problem there [isRectDictEv] gives us the OK to interact and we do solve d7 := d01! 
             
Note [SUPERCLASS-LOOP 1]
~~~~~~~~~~~~~~~~~~~~~~~~
We have to be very, very careful when generating superclasses, lest we
accidentally build a loop. Here's an example:

  class S a

  class S a => C a where { opc :: a -> a }
  class S b => D b where { opd :: b -> b }
  
  instance C Int where
     opc = opd
  
  instance D Int where
     opd = opc

From (instance C Int) we get the constraint set {ds1:S Int, dd:D Int}
Simplifying, we may well get:
	$dfCInt = :C ds1 (opd dd)
	dd  = $dfDInt
	ds1 = $p1 dd
Notice that we spot that we can extract ds1 from dd.  

Alas!  Alack! We can do the same for (instance D Int):

	$dfDInt = :D ds2 (opc dc)
	dc  = $dfCInt
	ds2 = $p1 dc

And now we've defined the superclass in terms of itself.
Two more nasty cases are in
	tcrun021
	tcrun033

Solution: 
  - Satisfy the superclass context *all by itself* 
    (tcSimplifySuperClasses)
  - And do so completely; i.e. no left-over constraints
    to mix with the constraints arising from method declarations


Note [SUPERCLASS-LOOP 2]
~~~~~~~~~~~~~~~~~~~~~~~~
We need to be careful when adding "the constaint we are trying to prove".
Suppose we are *given* d1:Ord a, and want to deduce (d2:C [a]) where

	class Ord a => C a where
	instance Ord [a] => C [a] where ...

Then we'll use the instance decl to deduce C [a] from Ord [a], and then add the
superclasses of C [a] to avails.  But we must not overwrite the binding
for Ord [a] (which is obtained from Ord a) with a superclass selection or we'll just
build a loop! 

Here's another variant, immortalised in tcrun020
	class Monad m => C1 m
	class C1 m => C2 m x
	instance C2 Maybe Bool
For the instance decl we need to build (C1 Maybe), and it's no good if
we run around and add (C2 Maybe Bool) and its superclasses to the avails 
before we search for C1 Maybe.

Here's another example 
 	class Eq b => Foo a b
	instance Eq a => Foo [a] a
If we are reducing
	(Foo [t] t)

we'll first deduce that it holds (via the instance decl).  We must not
then overwrite the Eq t constraint with a superclass selection!

At first I had a gross hack, whereby I simply did not add superclass constraints
in addWanted, though I did for addGiven and addIrred.  This was sub-optimal,
becuase it lost legitimate superclass sharing, and it still didn't do the job:
I found a very obscure program (now tcrun021) in which improvement meant the
simplifier got two bites a the cherry... so something seemed to be an Stop
first time, but reducible next time.

Now we implement the Right Solution, which is to check for loops directly 
when adding superclasses.  It's a bit like the occurs check in unification.

Note [Recursive instances and superclases]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this code, which arises in the context of "Scrap Your 
Boilerplate with Class".  

    class Sat a
    class Data ctx a
    instance  Sat (ctx Char)             => Data ctx Char
    instance (Sat (ctx [a]), Data ctx a) => Data ctx [a]

    class Data Maybe a => Foo a

    instance Foo t => Sat (Maybe t)

    instance Data Maybe a => Foo a
    instance Foo a        => Foo [a]
    instance                 Foo [Char]

In the instance for Foo [a], when generating evidence for the superclasses
(ie in tcSimplifySuperClasses) we need a superclass (Data Maybe [a]).
Using the instance for Data, we therefore need
        (Sat (Maybe [a], Data Maybe a)
But we are given (Foo a), and hence its superclass (Data Maybe a).
So that leaves (Sat (Maybe [a])).  Using the instance for Sat means
we need (Foo [a]).  And that is the very dictionary we are bulding
an instance for!  So we must put that in the "givens".  So in this
case we have
	Given:  Foo a, Foo [a]
	Wanted: Data Maybe [a]

BUT we must *not not not* put the *superclasses* of (Foo [a]) in
the givens, which is what 'addGiven' would normally do. Why? Because
(Data Maybe [a]) is the superclass, so we'd "satisfy" the wanted 
by selecting a superclass from Foo [a], which simply makes a loop.

On the other hand we *must* put the superclasses of (Foo a) in
the givens, as you can see from the derivation described above.

Conclusion: in the very special case of tcSimplifySuperClasses
we have one 'given' (namely the "this" dictionary) whose superclasses
must not be added to 'givens' by addGiven.  

There is a complication though.  Suppose there are equalities
      instance (Eq a, a~b) => Num (a,b)
Then we normalise the 'givens' wrt the equalities, so the original
given "this" dictionary is cast to one of a different type.  So it's a
bit trickier than before to identify the "special" dictionary whose
superclasses must not be added. See test
   indexed-types/should_run/EqInInstance

We need a persistent property of the dictionary to record this
special-ness.  Current I'm using the InstLocOrigin (a bit of a hack,
but cool), which is maintained by dictionary normalisation.
Specifically, the InstLocOrigin is
	     NoScOrigin
then the no-superclass thing kicks in.  WATCH OUT if you fiddle
with InstLocOrigin!

Note [MATCHING-SYNONYMS]
~~~~~~~~~~~~~~~~~~~~~~~~
When trying to match a dictionary (D tau) to a top-level instance, or a 
type family equation (F taus_1 ~ tau_2) to a top-level family instance, 
we do *not* need to expand type synonyms because the matcher will do that for us.


Note [RHS-FAMILY-SYNONYMS] 
~~~~~~~~~~~~~~~~~~~~~~~~~~
The RHS of a family instance is represented as yet another constructor which is 
like a type synonym for the real RHS the programmer declared. Eg: 
    type instance F (a,a) = [a] 
Becomes: 
    :R32 a = [a]      -- internal type synonym introduced
    F (a,a) ~ :R32 a  -- instance 

When we react a family instance with a type family equation in the work list 
we keep the synonym-using RHS without expansion. 


*********************************************************************************
*                                                                               * 
                       The top-reaction Stage
*                                                                               *
*********************************************************************************

\begin{code}
-- If a work item has any form of interaction with top-level we get this 
data TopInteractResult 
  = NoTopInt         -- No top-level interaction
                     -- Equivalent to (SomeTopInt emptyWorkList (ContinueWith work_item))
  | SomeTopInt 
      { tir_new_work  :: WorkList	-- Sub-goals or new work (could be given, 
                                        --                        for superclasses)
      , tir_new_inert :: StopOrContinue -- The input work item, ready to become *inert* now: 
      }                       		-- NB: in ``given'' (solved) form if the 
                              		-- original was wanted or given and instance match
                              		-- was found, but may also be in wanted form if we 
                                        -- only reacted with functional dependencies 
					-- arising from top-level instances.

topReactionsStage :: SimplifierStage 
topReactionsStage depth workItem inerts 
  = do { tir <- tryTopReact workItem 
       ; case tir of 
           NoTopInt -> 
               return $ SR { sr_inerts   = inerts 
                           , sr_new_work = emptyWorkList 
                           , sr_stop     = ContinueWith workItem } 
           SomeTopInt tir_new_work tir_new_inert -> 
               do { bumpStepCountTcS
                  ; traceFireTcS depth (ptext (sLit "Top react")
                       <+> vcat [ ptext (sLit "Work =") <+> ppr workItem
                                , ptext (sLit "New =") <+> ppr tir_new_work ])
                  ; return $ SR { sr_inerts   = inerts 
                           	, sr_new_work = tir_new_work
                           	, sr_stop     = tir_new_inert
                           	} }
       }

tryTopReact :: WorkItem -> TcS TopInteractResult 
tryTopReact workitem 
  = do {  -- A flag controls the amount of interaction allowed
          -- See Note [Simplifying RULE lhs constraints]
         ctxt <- getTcSContext
       ; if allowedTopReaction (simplEqsOnly ctxt) workitem 
         then do { traceTcS "tryTopReact / calling doTopReact" (ppr workitem)
                 ; doTopReact workitem }
         else return NoTopInt 
       } 

allowedTopReaction :: Bool -> WorkItem -> Bool
allowedTopReaction eqs_only (CDictCan {}) = not eqs_only
allowedTopReaction _        _             = True

doTopReact :: WorkItem -> TcS TopInteractResult 
-- The work item does not react with the inert set, so try interaction with top-level instances
-- NB: The place to add superclasses in *not* in doTopReact stage. Instead superclasses are 
--     added in the worklist as part of the canonicalisation process. 
-- See Note [Adding superclasses] in TcCanonical.

-- Given dictionary
-- See Note [Given constraint that matches an instance declaration]
doTopReact (CDictCan { cc_flavor = Given {} })
  = return NoTopInt -- NB: Superclasses already added since it's canonical

-- Derived dictionary: just look for functional dependencies
doTopReact workItem@(CDictCan { cc_flavor = fl@(Derived loc)
                              , cc_class = cls, cc_tyargs = xis })
  = do { instEnvs <- getInstEnvs
       ; let fd_eqns = improveFromInstEnv instEnvs
                                                (ClassP cls xis, pprArisingAt loc)
       ; m <- rewriteWithFunDeps fd_eqns xis fl
       ; case m of
           Nothing -> return NoTopInt
           Just (xis',_,fd_work) ->
               let workItem' = workItem { cc_tyargs = xis' }
                   -- Deriveds are not supposed to have identity (cc_id is unused!)
               in return $ SomeTopInt { tir_new_work  = fd_work 
                                      , tir_new_inert = ContinueWith workItem' } }

-- Wanted dictionary
doTopReact workItem@(CDictCan { cc_id = dv, cc_flavor = fl@(Wanted loc)
                              , cc_class = cls, cc_tyargs = xis })
  = do { -- See Note [MATCHING-SYNONYMS]
       ; lkp_inst_res <- matchClassInst cls xis loc
       ; case lkp_inst_res of
           NoInstance ->
             do { traceTcS "doTopReact/ no class instance for" (ppr dv)

                ; instEnvs <- getInstEnvs
                ; let fd_eqns = improveFromInstEnv instEnvs
                                                         (ClassP cls xis, pprArisingAt loc)
                ; m <- rewriteWithFunDeps fd_eqns xis fl
                ; case m of
                    Nothing -> return NoTopInt
                    Just (xis',cos,fd_work) ->
                        do { let dict_co = mkTyConCoercion (classTyCon cls) cos
                           ; dv'<- newDictVar cls xis'
                           ; setDictBind dv (EvCast dv' dict_co)
                           ; let workItem' = CDictCan { cc_id = dv', cc_flavor = fl, 
                                                        cc_class = cls, cc_tyargs = xis' }
                           ; return $ 
                             SomeTopInt { tir_new_work  = workListFromNonEq workItem' `unionWorkList` fd_work
                                        , tir_new_inert = Stop } } }

           GenInst wtvs ev_term -- Solved 
	   	   -- No need to do fundeps stuff here; the instance 
		   -- matches already so we won't get any more info
		   -- from functional dependencies
             | null wtvs
             -> do { traceTcS "doTopReact/ found nullary class instance for" (ppr dv) 
                   ; setDictBind dv ev_term 
                    -- Solved in one step and no new wanted work produced. 
                    -- i.e we directly matched a top-level instance
                    -- No point in caching this in 'inert'; hence Stop
                   ; return $ SomeTopInt { tir_new_work  = emptyWorkList 
                                         , tir_new_inert = Stop } }

             | otherwise
             -> do { traceTcS "doTopReact/ found nullary class instance for" (ppr dv) 
                   ; setDictBind dv ev_term 
                        -- Solved and new wanted work produced, you may cache the 
                        -- (tentatively solved) dictionary as Given! (used to be: Derived)
                   ; let solved   = workItem { cc_flavor = given_fl }
                         given_fl = Given (setCtLocOrigin loc UnkSkol) 
                   ; inst_work <- canWanteds wtvs
                   ; return $ SomeTopInt { tir_new_work  = inst_work
                                         , tir_new_inert = ContinueWith solved } }
       }          

-- Type functions
doTopReact (CFunEqCan { cc_id = cv, cc_flavor = fl
                      , cc_fun = tc, cc_tyargs = args, cc_rhs = xi })
  = ASSERT (isSynFamilyTyCon tc)   -- No associated data families have reached that far 
    do { match_res <- matchFam tc args -- See Note [MATCHING-SYNONYMS]
       ; case match_res of 
           MatchInstNo 
             -> return NoTopInt 
           MatchInstSingle (rep_tc, rep_tys)
             -> do { let Just coe_tc = tyConFamilyCoercion_maybe rep_tc
                         Just rhs_ty = tcView (mkTyConApp rep_tc rep_tys)
			    -- Eagerly expand away the type synonym on the
			    -- RHS of a type function, so that it never
			    -- appears in an error message
                            -- See Note [Type synonym families] in TyCon
                         coe = mkTyConApp coe_tc rep_tys 
                   ; cv' <- case fl of
                              Wanted {} -> do { cv' <- newCoVar rhs_ty xi
                                              ; setCoBind cv $ 
                                                    coe `mkTransCoercion`
                                                      mkCoVarCoercion cv'
                                              ; return cv' }
                              Given {}   -> newGivenCoVar xi rhs_ty $ 
                                            mkSymCoercion (mkCoVarCoercion cv) `mkTransCoercion` coe 
                              Derived {} -> newDerivedId (EqPred xi rhs_ty)
                   ; can_cts <- mkCanonical fl cv'
                   ; return $ SomeTopInt can_cts Stop }
           _ 
             -> panicTcS $ text "TcSMonad.matchFam returned multiple instances!"
       }


-- Any other work item does not react with any top-level equations
doTopReact _workItem = return NoTopInt 
\end{code}


Note [FunDep and implicit parameter reactions] 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Currently, our story of interacting two dictionaries (or a dictionary
and top-level instances) for functional dependencies, and implicit
paramters, is that we simply produce new wanted equalities.  So for example

        class D a b | a -> b where ... 
    Inert: 
        d1 :g D Int Bool
    WorkItem: 
        d2 :w D Int alpha

    We generate the extra work item
        cv :w alpha ~ Bool
    where 'cv' is currently unused.  However, this new item reacts with d2,
    discharging it in favour of a new constraint d2' thus:
        d2' :w D Int Bool
	d2 := d2' |> D Int cv
    Now d2' can be discharged from d1

We could be more aggressive and try to *immediately* solve the dictionary 
using those extra equalities. With the same inert set and work item we
might dischard d2 directly:

        cv :w alpha ~ Bool
        d2 := d1 |> D Int cv

But in general it's a bit painful to figure out the necessary coercion,
so we just take the first approach. Here is a better example. Consider:
    class C a b c | a -> b 
And: 
     [Given]  d1 : C T Int Char 
     [Wanted] d2 : C T beta Int 
In this case, it's *not even possible* to solve the wanted immediately. 
So we should simply output the functional dependency and add this guy
[but NOT its superclasses] back in the worklist. Even worse: 
     [Given] d1 : C T Int beta 
     [Wanted] d2: C T beta Int 
Then it is solvable, but its very hard to detect this on the spot. 

It's exactly the same with implicit parameters, except that the
"aggressive" approach would be much easier to implement.

Note [When improvement happens]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We fire an improvement rule when

  * Two constraints match (modulo the fundep)
      e.g. C t1 t2, C t1 t3    where C a b | a->b
    The two match because the first arg is identical

  * At least one is not Given.  If they are both given, we don't fire
    the reaction because we have no way of constructing evidence for a
    new equality nor does it seem right to create a new wanted goal
    (because the goal will most likely contain untouchables, which
    can't be solved anyway)!
   
Note that we *do* fire the improvement if one is Given and one is Derived.
The latter can be a superclass of a wanted goal. Example (tcfail138)
    class L a b | a -> b
    class (G a, L a b) => C a b

    instance C a b' => G (Maybe a)
    instance C a b  => C (Maybe a) a
    instance L (Maybe a) a

When solving the superclasses of the (C (Maybe a) a) instance, we get
  Given:  C a b  ... and hance by superclasses, (G a, L a b)
  Wanted: G (Maybe a)
Use the instance decl to get
  Wanted: C a b'
The (C a b') is inert, so we generate its Derived superclasses (L a b'),
and now we need improvement between that derived superclass an the Given (L a b)

Note [Overriding implicit parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   f :: (?x::a) -> Bool -> a
  
   g v = let ?x::Int = 3 
         in (f v, let ?x::Bool = True in f v)

This should probably be well typed, with
   g :: Bool -> (Int, Bool)

So the inner binding for ?x::Bool *overrides* the outer one.
Hence a work-item Given overrides an inert-item Given.

Note [Given constraint that matches an instance declaration]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What should we do when we discover that one (or more) top-level 
instances match a given (or solved) class constraint? We have 
two possibilities:

  1. Reject the program. The reason is that there may not be a unique
     best strategy for the solver. Example, from the OutsideIn(X) paper:
       instance P x => Q [x] 
       instance (x ~ y) => R [x] y 
     
       wob :: forall a b. (Q [b], R b a) => a -> Int 

       g :: forall a. Q [a] => [a] -> Int 
       g x = wob x 

       will generate the impliation constraint: 
            Q [a] => (Q [beta], R beta [a]) 
       If we react (Q [beta]) with its top-level axiom, we end up with a 
       (P beta), which we have no way of discharging. On the other hand, 
       if we react R beta [a] with the top-level we get  (beta ~ a), which 
       is solvable and can help us rewrite (Q [beta]) to (Q [a]) which is 
       now solvable by the given Q [a]. 
 
     However, this option is restrictive, for instance [Example 3] from 
     Note [Recursive dictionaries] will fail to work. 

  2. Ignore the problem, hoping that the situations where there exist indeed
     such multiple strategies are rare: Indeed the cause of the previous 
     problem is that (R [x] y) yields the new work (x ~ y) which can be 
     *spontaneously* solved, not using the givens. 

We are choosing option 2 below but we might consider having a flag as well.


Note [New Wanted Superclass Work] 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Even in the case of wanted constraints, we may add some superclasses 
as new given work. The reason is: 

        To allow FD-like improvement for type families. Assume that 
        we have a class 
             class C a b | a -> b 
        and we have to solve the implication constraint: 
             C a b => C a beta 
        Then, FD improvement can help us to produce a new wanted (beta ~ b) 

        We want to have the same effect with the type family encoding of 
        functional dependencies. Namely, consider: 
             class (F a ~ b) => C a b 
        Now suppose that we have: 
               given: C a b 
               wanted: C a beta 
        By interacting the given we will get given (F a ~ b) which is not 
        enough by itself to make us discharge (C a beta). However, we 
        may create a new derived equality from the super-class of the
        wanted constraint (C a beta), namely derived (F a ~ beta). 
        Now we may interact this with given (F a ~ b) to get: 
                  derived :  beta ~ b 
        But 'beta' is a touchable unification variable, and hence OK to 
        unify it with 'b', replacing the derived evidence with the identity. 

        This requires trySpontaneousSolve to solve *derived*
        equalities that have a touchable in their RHS, *in addition*
        to solving wanted equalities.

We also need to somehow use the superclasses to quantify over a minimal, 
constraint see note [Minimize by Superclasses] in TcSimplify.


Finally, here is another example where this is useful. 

Example 1:
----------
   class (F a ~ b) => C a b 
And we are given the wanteds:
      w1 : C a b 
      w2 : C a c 
      w3 : b ~ c 
We surely do *not* want to quantify over (b ~ c), since if someone provides
dictionaries for (C a b) and (C a c), these dictionaries can provide a proof 
of (b ~ c), hence no extra evidence is necessary. Here is what will happen: 

     Step 1: We will get new *given* superclass work, 
             provisionally to our solving of w1 and w2
             
               g1: F a ~ b, g2 : F a ~ c, 
               w1 : C a b, w2 : C a c, w3 : b ~ c

             The evidence for g1 and g2 is a superclass evidence term: 

               g1 := sc w1, g2 := sc w2

     Step 2: The givens will solve the wanted w3, so that 
               w3 := sym (sc w1) ; sc w2 
                  
     Step 3: Now, one may naively assume that then w2 can be solve from w1
             after rewriting with the (now solved equality) (b ~ c). 
             
             But this rewriting is ruled out by the isGoodRectDict! 

Conclusion, we will (correctly) end up with the unsolved goals 
    (C a b, C a c)   

NB: The desugarer needs be more clever to deal with equalities 
    that participate in recursive dictionary bindings. 

\begin{code}
data LookupInstResult
  = NoInstance
  | GenInst [WantedEvVar] EvTerm 

matchClassInst :: Class -> [Type] -> WantedLoc -> TcS LookupInstResult
matchClassInst clas tys loc
   = do { let pred = mkClassPred clas tys 
        ; mb_result <- matchClass clas tys
        ; case mb_result of
            MatchInstNo   -> return NoInstance
            MatchInstMany -> return NoInstance -- defer any reactions of a multitude until 
                                               -- we learn more about the reagent 
            MatchInstSingle (dfun_id, mb_inst_tys) -> 
              do { checkWellStagedDFun pred dfun_id loc

 	-- It's possible that not all the tyvars are in
	-- the substitution, tenv. For example:
	--	instance C X a => D X where ...
	-- (presumably there's a functional dependency in class C)
	-- Hence mb_inst_tys :: Either TyVar TcType 

                 ; tys <- instDFunTypes mb_inst_tys 
                 ; let (theta, _) = tcSplitPhiTy (applyTys (idType dfun_id) tys)
                 ; if null theta then
                       return (GenInst [] (EvDFunApp dfun_id tys []))
                   else do
                     { ev_vars <- instDFunConstraints theta
                     ; let wevs = [EvVarX w loc | w <- ev_vars]
                     ; return $ GenInst wevs (EvDFunApp dfun_id tys ev_vars) }
                 }
        }
\end{code}
