\begin{code}
module TcInteract ( 
     solveInteract, AtomicInert, 
     InertSet, emptyInert, updInertSet, extractUnsolved, solveOne 
  ) where  

#include "HsVersions.h"


import BasicTypes 
import TcCanonical
import VarSet
import Type
import TypeRep 

import Id 
import VarEnv
import Var

import TcType
import HsBinds 

import InstEnv 
import Class 
import TyCon 
import Name

import FunDeps

import Control.Monad ( when ) 

import Coercion
import Outputable

import TcRnTypes 
import TcErrors
import TcSMonad 
import Bag
import qualified Data.Map as Map 
import Maybes 

import Control.Monad( zipWithM, unless )
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
 
Note that 6 and 7 are /not/ enforced by canonicalization but rather by 
insertion in the inert list, ie by TcInteract. 

During the process of solving, the inert set will contain some
previously given constraints, some wanted constraints, and some given
constraints which have arisen from solving wanted constraints. For
now we do not distinguish between given and solved constraints.

Note that we must switch wanted inert items to given when going under an
implication constraint (when in top-level inference mode).

Note [InertSet FlattenSkolemEqClass] 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The inert_fsks field of the inert set contains an "inverse map" of all the 
flatten skolem equalities in the inert set. For instance, if inert_cts looks
like this: 
 
    fsk1 ~ fsk2 
    fsk3 ~ fsk2 
    fsk4 ~ fsk5 

Then, the inert_fsks fields holds the following map: 
    fsk2 |-> { fsk1, fsk3 } 
    fsk5 |-> { fsk4 } 
Along with the necessary coercions to convert fsk1 and fsk3 back to fsk2 
and fsk4 back to fsk5. Hence, the invariants of the inert_fsks field are: 
  
   (a) All TcTyVars in the domain and range of inert_fsks are flatten skolems
   (b) All TcTyVars in the domain of inert_fsk occur naked as rhs in some 
       equalities of inert_cts 
   (c) For every mapping  fsk1 |-> { (fsk2,co), ... } it must be: 
         co : fsk2 ~ fsk1 

The role of the inert_fsks is to make it easy to maintain the equivalence
class of each flatten skolem, which is much needed to correctly do spontaneous
solving. See Note [Loopy Spontaneous Solving] 
\begin{code}

-- See Note [InertSet invariants]
data InertSet 
  = IS { inert_eqs  :: Bag.Bag CanonicalCt   -- Equalities only **CTyEqCan** 
       , inert_cts  :: Bag.Bag CanonicalCt   -- Other constraints 
       , inert_fds  :: FDImprovements        -- List of pairwise improvements that have kicked in already
                                             -- and reside either in the worklist or in the inerts 
       , inert_fsks :: Map.Map TcTyVar [(TcTyVar,Coercion)] }
       -- See Note [InertSet FlattenSkolemEqClass] 

type FDImprovement  = (PredType,PredType) 
type FDImprovements = [(PredType,PredType)] 

instance Outputable InertSet where
  ppr is = vcat [ vcat (map ppr (Bag.bagToList $ inert_eqs is))
                , vcat (map ppr (Bag.bagToList $ inert_cts is))
                , vcat (map (\(v,rest) -> ppr v <+> text "|->" <+> hsep (map (ppr.fst) rest)) 
                       (Map.toList $ inert_fsks is)
                       )
                ]
                       
emptyInert :: InertSet
emptyInert = IS { inert_eqs = Bag.emptyBag
                , inert_cts = Bag.emptyBag, inert_fsks = Map.empty, inert_fds = [] }

updInertSet :: InertSet -> AtomicInert -> InertSet 
-- Introduces an element in the inert set for the first time 
updInertSet (IS { inert_eqs = eqs, inert_cts = cts, inert_fsks = fsks, inert_fds = fdis })  
            item@(CTyEqCan { cc_id    = cv
                           , cc_tyvar = tv1 
                           , cc_rhs   = xi })
  | Just tv2 <- tcGetTyVar_maybe xi,
    FlatSkol {} <- tcTyVarDetails tv1, 
    FlatSkol {} <- tcTyVarDetails tv2 
  = let eqs'  = eqs `Bag.snocBag` item 
        fsks' = Map.insertWith (++) tv2 [(tv1, mkCoVarCoercion cv)] fsks
        -- See Note [InertSet FlattenSkolemEqClass] 
    in IS { inert_eqs = eqs', inert_cts = cts, inert_fsks = fsks', inert_fds = fdis }
updInertSet (IS { inert_eqs = eqs, inert_cts = cts
                , inert_fsks = fsks, inert_fds = fdis }) item 
  | isTyEqCCan item 
  = let eqs' = eqs `Bag.snocBag` item 
    in IS { inert_eqs = eqs', inert_cts = cts, inert_fsks = fsks, inert_fds = fdis } 
  | otherwise 
  = let cts' = cts `Bag.snocBag` item
    in IS { inert_eqs = eqs, inert_cts = cts', inert_fsks = fsks, inert_fds = fdis } 

updInertSetFDImprs :: InertSet -> Maybe FDImprovement -> InertSet 
updInertSetFDImprs is (Just fdi) = is { inert_fds = fdi : inert_fds is } 
updInertSetFDImprs is Nothing    = is 

foldISEqCtsM :: Monad m => (a -> AtomicInert -> m a) -> a -> InertSet -> m a 
-- Fold over the equalities of the inerts
foldISEqCtsM k z IS { inert_eqs = eqs } 
  = Bag.foldlBagM k z eqs 

foldISOtherCtsM :: Monad m => (a -> AtomicInert -> m a) -> a -> InertSet -> m a 
-- Fold over other constraints in the inerts 
foldISOtherCtsM k z IS { inert_cts = cts } 
  = Bag.foldlBagM k z cts 

extractUnsolved :: InertSet -> (InertSet, CanonicalCts)
extractUnsolved is@(IS {inert_eqs = eqs, inert_cts = cts, inert_fds = fdis }) 
  = let is_init  = is { inert_eqs = emptyCCan 
                      , inert_cts = solved_cts, inert_fsks = Map.empty, inert_fds = fdis }
        is_final = Bag.foldlBag updInertSet is_init solved_eqs -- Add equalities carefully
    in (is_final, unsolved) 
  where (unsolved_cts, solved_cts) = Bag.partitionBag isWantedCt cts
        (unsolved_eqs, solved_eqs) = Bag.partitionBag isWantedCt eqs
        unsolved                   = unsolved_cts `unionBags` unsolved_eqs


getFskEqClass :: InertSet -> TcTyVar -> [(TcTyVar,Coercion)] 
-- Precondition: tv is a FlatSkol. See Note [InertSet FlattenSkolemEqClass] 
getFskEqClass (IS { inert_cts = cts, inert_fsks = fsks }) tv 
  = case lkpTyEqCanByLhs of
      Nothing  -> fromMaybe [] (Map.lookup tv fsks)  
      Just ceq -> 
        case tcGetTyVar_maybe (cc_rhs ceq) of 
          Just tv_rhs | FlatSkol {} <- tcTyVarDetails tv_rhs
            -> let ceq_co = mkSymCoercion $ mkCoVarCoercion (cc_id ceq)
                   mk_co (v,c) = (v, mkTransCoercion c ceq_co)
               in (tv_rhs, ceq_co): map mk_co (fromMaybe [] $ Map.lookup tv fsks) 
          _ -> []
  where lkpTyEqCanByLhs = Bag.foldlBag lkp Nothing cts 
        lkp :: Maybe CanonicalCt -> CanonicalCt -> Maybe CanonicalCt 
        lkp Nothing ct@(CTyEqCan {cc_tyvar = tv'}) | tv' == tv = Just ct 
        lkp other _ct = other 

haveBeenImproved :: FDImprovements -> PredType -> PredType -> Bool 
haveBeenImproved [] _ _ = False 
haveBeenImproved ((pty1,pty2):fdimprs) pty1' pty2' 
 | tcEqPred pty1 pty1' && tcEqPred pty2 pty2' 
 = True 
 | tcEqPred pty1 pty2' && tcEqPred pty2 pty1'
 = True 
 | otherwise 
 = haveBeenImproved fdimprs pty1' pty2' 

getFDImprovements :: InertSet -> FDImprovements 
-- Return a list of the improvements that have kicked in so far 
getFDImprovements = inert_fds 


isWantedCt :: CanonicalCt -> Bool 
isWantedCt ct = isWanted (cc_flavor ct)

{- TODO: Later ...
data Inert = IS { class_inerts :: FiniteMap Class Atomics
     	          ip_inerts    :: FiniteMap Class Atomics
     	          tyfun_inerts :: FiniteMap TyCon Atomics
		  tyvar_inerts :: FiniteMap TyVar Atomics
                }

Later should we also separate out givens and wanteds?
-}

\end{code}

Note [Touchables and givens]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Touchable variables will never show up in givens which are inputs to
the solver.  However, touchables may show up in givens generated by the flattener.  
For example,

  axioms:
    G Int ~ Char
    F Char ~ Int

  wanted:
    F (G alpha) ~w Int
  
canonicalises to

  G alpha ~g b
  F b ~w Int

which can be put in the inert set.  Suppose we also have a wanted

  alpha ~w Int

We cannot rewrite the given G alpha ~g b using the wanted alpha ~w
Int.  Instead, after reacting alpha ~w Int with the whole inert set,
we observe that we can solve it by unifying alpha with Int, so we mark
it as solved and put it back in the *work list*. [We also immediately unify
alpha := Int, without telling anyone, see trySpontaneousSolve function, to 
avoid doing this in the end.]

Later, because it is solved (given, in effect), we can use it to rewrite 
G alpha ~g b to G Int ~g b, which gets put back in the work list. Eventually, 
we will dispatch the remaining wanted constraints using the top-level axioms.

Finally, note that after reacting a wanted equality with the entire inert set
we may end up with something like

  b ~w alpha

which we should flip around to generate the solved constraint alpha ~s b.

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

-- A mixture of Given, Wanted, and Derived constraints. 
-- We split between equalities and the rest to process equalities first. 
type WorkList = CanonicalCts
type SWorkList = WorkList        -- A worklist of solved 

unionWorkLists :: WorkList -> WorkList -> WorkList 
unionWorkLists = andCCan

isEmptyWorkList :: WorkList -> Bool 
isEmptyWorkList = isEmptyCCan 

emptyWorkList :: WorkList
emptyWorkList = emptyCCan

workListFromCCan :: CanonicalCt -> WorkList 
workListFromCCan = singleCCan

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

type SimplifierStage = WorkItem -> InertSet -> TcS StageResult 

-- Combine a sequence of simplifier 'stages' to create a pipeline 
runSolverPipeline :: [(String, SimplifierStage)]
                  -> InertSet -> WorkItem 
                  -> TcS (InertSet, WorkList)
-- Precondition: non-empty list of stages 
runSolverPipeline pipeline inerts workItem
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
      = do { itr <- stage work_item inerts 
           ; traceTcS ("Stage result (" ++ name ++ ")") (ppr itr)
           ; let itr' = itr { sr_new_work = accum_work `unionWorkLists` sr_new_work itr }
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
solveInteract :: InertSet -> CanonicalCts -> TcS InertSet
solveInteract inert ws 
  = do { dyn_flags <- getDynFlags
       ; solveInteractWithDepth (ctxtStkDepth dyn_flags,0,[]) inert ws
       }
solveOne :: InertSet -> WorkItem -> TcS InertSet 
solveOne inerts workItem 
  = do { dyn_flags <- getDynFlags
       ; solveOneWithDepth (ctxtStkDepth dyn_flags,0,[]) inerts workItem
       }

-----------------
solveInteractWithDepth :: (Int, Int, [WorkItem])
                       -> InertSet -> WorkList -> TcS InertSet
solveInteractWithDepth ctxt@(max_depth,n,stack) inert ws 
  | isEmptyWorkList ws
  = return inert

  | n > max_depth 
  = solverDepthErrorTcS n stack

  | otherwise 
  = do { traceTcS "solveInteractWithDepth" $ 
              vcat [ text "Current depth =" <+> ppr n
                   , text "Max depth =" <+> ppr max_depth ]

	      -- Solve equalities first
       ; let (eqs, non_eqs) = Bag.partitionBag isTyEqCCan ws
       ; is_from_eqs <- Bag.foldlBagM (solveOneWithDepth ctxt) inert eqs
       ; Bag.foldlBagM (solveOneWithDepth ctxt) is_from_eqs non_eqs }

------------------
-- Fully interact the given work item with an inert set, and return a
-- new inert set which has assimilated the new information.
solveOneWithDepth :: (Int, Int, [WorkItem])
                  -> InertSet -> WorkItem -> TcS InertSet
solveOneWithDepth (max_depth, n, stack) inert work
  = do { traceTcS0 (indent ++ "Solving {") (ppr work)
       ; (new_inert, new_work) <- runSolverPipeline thePipeline inert work
         
       ; traceTcS0 (indent ++ "Subgoals:") (ppr new_work)

	 -- Recursively solve the new work generated 
         -- from workItem, with a greater depth
       ; res_inert <- solveInteractWithDepth (max_depth, n+1, work:stack)
                                new_inert new_work 

       ; traceTcS0 (indent ++ "Done }") (ppr work) 
       ; return res_inert }
  where
    indent = replicate (2*n) ' '

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

Case 1: In Spontaneous Solving 

     The OrientFlag is used to preserve the original orientation of a
     spontaneously solved equality (insofar the canonical constraints
     invariants allow it). This way we hope to be more efficient since
     when reaching the spontaneous solve stage, a particular
     constraint has already been inert-ified wrt to the preexisting
     inerts.

     Example: 
     Inerts:   [w1] : D alpha 
               [w2] : C beta 
               [w3] : F alpha ~ Int 
               [w4] : H beta  ~ Int 
     Untouchables = [beta] 
     Then a wanted (beta ~ alpha) comes along. 

        1) While interacting with the inerts it is going to kick w2,w4
           out of the inerts
        2) Then, it will spontaneoulsy be solved by (alpha := beta)
        3) Now (and here is the tricky part), to add him back as
           solved (alpha ~ beta) is no good because, in the next
           iteration, it will kick out w1,w3 as well so we will end up
           with *all* the inert equalities back in the worklist!

     So, we instead solve (alpha := beta), but we preserve the
     original orientation, so that we get a given (beta ~ alpha),
     which will result in no more inerts getting kicked out of the
     inert set in the next iteration.

Case 2: In Rewriting Equalities (function rewriteEqLHS) 

    When rewriting two equalities with the same LHS:
          (a)  (tv ~ xi1) 
          (b)  (tv ~ xi2) 
    We have a choice of producing work (xi1 ~ xi2) (up-to the
    canonicalization invariants) However, to prevent the inert items
    from getting kicked out of the inerts first, we prefer to
    canonicalize (xi1 ~ xi2) if (b) comes from the inert set, or (xi2
    ~ xi1) if (a) comes from the inert set.
    
    This choice is implemented using the WhichComesFromInert flag. 

Case 3: Functional Dependencies and IP improvement work
    TODO. Optimisation not yet implemented there. 

\begin{code}
spontaneousSolveStage :: SimplifierStage 
spontaneousSolveStage workItem inerts 
  = do { mSolve <- trySpontaneousSolve workItem inerts
       ; case mSolve of 
           Nothing -> -- no spontaneous solution for him, keep going
               return $ SR { sr_new_work   = emptyWorkList
                           , sr_inerts     = inerts
                           , sr_stop       = ContinueWith workItem }

           Just workList' -> -- He has been solved; workList' are all givens
               return $ SR { sr_new_work = workList'
                           , sr_inerts   = inerts 
                           , sr_stop     = Stop }
       }


data OrientFlag = OrientThisWay 
                | OrientOtherWay -- See Note [Efficient Orientation]

-- @trySpontaneousSolve wi@ solves equalities where one side is a
-- touchable unification variable. Returns:
--   * Nothing if we were not able to solve it
--   * Just wi' if we solved it, wi' (now a "given") should be put in the work list.
--     	    See Note [Touchables and givens] 
-- NB: just passing the inerts through for the skolem equivalence classes
trySpontaneousSolve :: WorkItem -> InertSet -> TcS (Maybe SWorkList)
trySpontaneousSolve workItem@(CTyEqCan { cc_id = cv, cc_flavor = gw, cc_tyvar = tv1, cc_rhs = xi }) inerts 
  | isGiven gw
  = return Nothing
  | Just tv2 <- tcGetTyVar_maybe xi
  = do { tch1 <- isTouchableMetaTyVar tv1
       ; tch2 <- isTouchableMetaTyVar tv2
       ; case (tch1, tch2) of
           (True,  True)  -> trySpontaneousEqTwoWay inerts cv gw tv1 tv2
           (True,  False) -> trySpontaneousEqOneWay OrientThisWay  inerts cv gw tv1 xi
           (False, True)  -> trySpontaneousEqOneWay OrientOtherWay inerts cv gw tv2 (mkTyVarTy tv1)
	   _ -> return Nothing }
  | otherwise
  = do { tch1 <- isTouchableMetaTyVar tv1
       ; if tch1 then trySpontaneousEqOneWay OrientThisWay inerts cv gw tv1 xi
                 else do { traceTcS "Untouchable LHS, can't spontaneously solve workitem:" (ppr workItem) 
                         ; return Nothing }
       }

  -- No need for 
  --      trySpontaneousSolve (CFunEqCan ...) = ...
  -- See Note [No touchables as FunEq RHS] in TcSMonad
trySpontaneousSolve _ _ = return Nothing 

----------------
trySpontaneousEqOneWay :: OrientFlag 
                       -> InertSet -> CoVar -> CtFlavor -> TcTyVar -> Xi 
                       -> TcS (Maybe SWorkList)
-- NB: The OrientFlag is there to help us recover the orientation of the original 
-- constraint which is important for enforcing the canonical constraints invariants. 
-- Also, tv is a MetaTyVar, not untouchable
trySpontaneousEqOneWay or_flag inerts cv gw tv xi	
  | not (isSigTyVar tv) || isTyVarTy xi 
  = do { kxi <- zonkTcTypeTcS xi >>= return . typeKind  -- Must look through the TcTyBinds
                                                        -- hence kxi and not typeKind xi
                                                        -- See Note [Kind Errors]
       ; if kxi `isSubKind` tyVarKind tv then
             solveWithIdentity or_flag inerts cv gw tv xi
         else if tyVarKind tv `isSubKind` kxi then 
             return Nothing -- kinds are compatible but we can't solveWithIdentity this way
                            -- This case covers the  a_touchable :: * ~ b_untouchable :: ?? 
                            -- which has to be deferred or floated out for someone else to solve 
                            -- it in a scope where 'b' is no longer untouchable. 
         else kindErrorTcS gw (mkTyVarTy tv) xi -- See Note [Kind errors]
       }
  | otherwise -- Still can't solve, sig tyvar and non-variable rhs
  = return Nothing 

----------------
trySpontaneousEqTwoWay :: InertSet -> CoVar -> CtFlavor -> TcTyVar -> TcTyVar
                       -> TcS (Maybe SWorkList)
-- Both tyvars are *touchable* MetaTyvars so there is only a chance for kind error here
trySpontaneousEqTwoWay inerts cv gw tv1 tv2
  | k1 `isSubKind` k2
  , nicer_to_update_tv2 = solveWithIdentity OrientOtherWay inerts cv gw tv2 (mkTyVarTy tv1)
  | k2 `isSubKind` k1 
  = solveWithIdentity OrientThisWay inerts cv gw tv1 (mkTyVarTy tv2) 
  | otherwise -- None is a subkind of the other, but they are both touchable! 
  = kindErrorTcS gw (mkTyVarTy tv1) (mkTyVarTy tv2) -- See Note [Kind errors]
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
simply returns @Nothing@ then that wanted constraint is going to propagate all the way and 
get quantified over in inference mode. That's bad because we do know at this point that the 
constraint is insoluble. Instead, we call 'kindErrorTcS' here, which immediately fails. 

The same applies in canonicalization code in case of kind errors in the givens. 

However, when we canonicalize givens we only check for compatibility (@compatKind@). 
If there were a kind error in the givens, this means some form of inconsistency or dead code. 

When we spontaneously solve wanteds we may have to look through the bindings, hence we 
call zonkTcTypeTcS above. The reason is that maybe xi is @alpha@ where alpha :: ? and 
a previous spontaneous solving has set (alpha := f) with (f :: *). The reason that xi is 
still alpha and not f is becasue the solved constraint may be oriented as (f ~ alpha) instead
of (alpha ~ f). Then we should be using @xi@s "real" kind, which is * and not ?, when we try
to detect whether spontaneous solving is possible. 


Note [Spontaneous solving and kind compatibility] 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that our canonical constraints insist that only *given* equalities (tv ~ xi) 
or (F xis ~ rhs) require the LHS and the RHS to have exactly the same kinds. 

  - We have to require this because: 
        Given equalities can be freely used to rewrite inside 
        other types or constraints.
  - We do not have to do the same for wanteds because:
        First, wanted equations (tv ~ xi) where tv is a touchable
        unification variable may have kinds that do not agree (the
        kind of xi must be a sub kind of the kind of tv).  Second, any
        potential kind mismatch will result in the constraint not
        being soluble, which will be reported anyway. This is the
        reason that @trySpontaneousOneWay@ and @trySpontaneousTwoWay@
        will perform a kind compatibility check, and only then will
        they proceed to @solveWithIdentity@.

Caveat: 
  - Givens from higher-rank, such as: 
          type family T b :: * -> * -> * 
          type instance T Bool = (->) 

          f :: forall a. ((T a ~ (->)) => ...) -> a -> ... 
          flop = f (...) True 
     Whereas we would be able to apply the type instance, we would not be able to 
     use the given (T Bool ~ (->)) in the body of 'flop' 

Note [Loopy Spontaneous Solving] 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Example 1: (The problem of loopy spontaneous solving) 
****************************************************************************
Consider the original wanted: 
   wanted :  Maybe (E alpha) ~ alpha 
where E is a type family, such that E (T x) = x. After canonicalization, 
as a result of flattening, we will get: 
   given  : E alpha ~ fsk 
   wanted : alpha ~ Maybe fsk
where (fsk := E alpha, on the side). Now, if we spontaneously *solve* 
(alpha := Maybe fsk) we are in trouble! Instead, we should refrain from solving 
it and keep it as wanted.  In inference mode we'll end up quantifying over
   (alpha ~ Maybe (E alpha))
Hence, 'solveWithIdentity' performs a small occurs check before
actually solving. But this occurs check *must look through* flatten skolems.

However, it may be the case that the flatten skolem in hand is equal to some other 
flatten skolem whith *does not* mention our unification variable. Here's a typical example:

Example 2: (The need of keeping track of flatten skolem equivalence classes) 
****************************************************************************
Original wanteds: 
   g: F alpha ~ F beta 
   w: alpha ~ F alpha 
After canonicalization: 
   g: F beta ~ f1 
   g: F alpha ~ f1 
   w: alpha ~ f2 
   g: F alpha ~ f2 
After some reactions: 
   g: f1 ~ f2 
   g: F beta ~ f1 
   w: alpha ~ f2 
   g: F alpha ~ f2 
At this point, we will try to spontaneously solve (alpha ~ f2) which remains as yet unsolved.
We will look inside f2, which immediately mentions (F alpha), so it's not good to unify! However
by looking at the equivalence class of the flatten skolems, we can see that it is fine to 
unify (alpha ~ f1) which solves our goals! 

Example 3: (The need of looking through TyBinds for already spontaneously solved variables)
*******************************************************************************************
A similar problem happens because of other spontaneous solving. Suppose we have the 
following wanteds, arriving in this exact order:
  (first)  w: beta ~ alpha 
  (second) w: alpha ~ fsk 
  (third)  g: F beta ~ fsk
Then, we first spontaneously solve the first constraint, making (beta := alpha), and having
(beta ~ alpha) as given. *Then* we encounter the second wanted (alpha ~ fsk). "fsk" does not 
obviously mention alpha, so naively we can also spontaneously solve (alpha := fsk). But 
that is wrong since fsk mentions beta, which has already secretly been unified to alpha! 

To avoid this problem, the same occurs check must unveil rewritings that can happen because 
of spontaneously having solved other constraints. 

Example 4: (Orientation of (tv ~ xi) equalities) 
************************************************
We orient equalities (tv ~ xi) so that flatten skolems appear on the left, if possible. Here
is an example of why this is needed: 

  [Wanted] w1: alpha ~ fsk 
  [Given]  g1: F alpha ~ fsk 
  [Given]  g2: b ~ fsk 
  Flatten skolem equivalence class = [] 

Assume that g2 is *not* oriented properly, as shown above. Then we would like to spontaneously
solve w1 but we can't set alpha := fsk, since fsk hides the type F alpha. However, by using 
the equation g2 it would be possible to solve w1 by setting  alpha := b. In other words, it is
not enough to look at a flatten skolem equivalence class to try to find alternatives to unify
with. We may have to go to other variables. 

By orienting the equalities so that flatten skolems are in the LHS we are eliminating them
as much as possible from the RHS of other wanted equalities, and hence it suffices to look 
in their flatten skolem equivalence classes. 

This situation appears in the IndTypesPerf test case, inside indexed-types/.

Note [Avoid double unifications] 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The spontaneous solver has to return a given which mentions the unified unification
variable *on the left* of the equality. Here is what happens if not: 
  Original wanted:  (a ~ alpha),  (alpha ~ Int) 
We spontaneously solve the first wanted, without changing the order! 
      given : a ~ alpha      [having unified alpha := a] 
Now the second wanted comes along, but he cannot rewrite the given, so we simply continue.
At the end we spontaneously solve that guy, *reunifying*  [alpha := Int] 

We avoid this problem by orienting the given so that the unification
variable is on the left.  [Note that alternatively we could attempt to
enforce this at canonicalization]

See also Note [No touchables as FunEq RHS] in TcSMonad; avoiding
double unifications is the main reason we disallow touchable
unification variables as RHS of type family equations: F xis ~ alpha.

\begin{code}
----------------
solveWithIdentity :: OrientFlag 
                  -> InertSet 
                  -> CoVar -> CtFlavor -> TcTyVar -> Xi 
                  -> TcS (Maybe SWorkList)
-- Solve with the identity coercion 
-- Precondition: kind(xi) is a sub-kind of kind(tv)
-- Precondition: CtFlavor is Wanted or Derived
-- See [New Wanted Superclass Work] to see why solveWithIdentity 
--     must work for Derived as well as Wanted
solveWithIdentity or_flag inerts cv gw tv xi 
  = do { tybnds <- getTcSTyBindsMap
       ; case occurCheck tybnds inerts tv xi of 
           Nothing              -> return Nothing 
           Just (xi_unflat,coi) -> solve_with xi_unflat coi }
  where
    solve_with xi_unflat coi  -- coi : xi_unflat ~ xi  
      = do { traceTcS "Sneaky unification:" $ 
                       vcat [text "Coercion variable:  " <+> ppr gw, 
                             text "Coercion:           " <+> pprEq (mkTyVarTy tv) xi,
                             text "Left  Kind is     : " <+> ppr (typeKind (mkTyVarTy tv)),
                             text "Right Kind is     : " <+> ppr (typeKind xi)
                  ]
--           ; setWantedTyBind tv xi_unflat        -- Set tv := xi_unflat
--           ; cv_given <- newGivOrDerCoVar (mkTyVarTy tv) xi_unflat xi_unflat
           ; let flav = mkGivenFlavor gw UnkSkol 
           ; (cts, co) <- case coi of 
               ACo co  -> do { cv_given <- newGivOrDerCoVar (mkTyVarTy tv)  xi_unflat xi_unflat
                             ; setWantedTyBind tv xi_unflat
                             ; can_eqs <- case or_flag of 
                                            OrientThisWay  -> canEq flav cv_given (mkTyVarTy tv) xi_unflat
                                            OrientOtherWay -> canEq flav cv_given xi_unflat (mkTyVarTy tv) 
                             ; return (can_eqs, co) }
               IdCo co -> do { cv_given <- newGivOrDerCoVar (mkTyVarTy tv) xi xi 
                             ; setWantedTyBind tv xi
                             ; can_eqs <- case or_flag of 
                                            OrientThisWay  -> canEq flav cv_given (mkTyVarTy tv) xi
                                            OrientOtherWay -> canEq flav cv_given xi (mkTyVarTy tv)
                             ; return (can_eqs, co) }
           ; case gw of 
               Wanted  {} -> setWantedCoBind  cv co
               Derived {} -> setDerivedCoBind cv co 
               _          -> pprPanic "Can't spontaneously solve *given*" empty 
	              -- See Note [Avoid double unifications] 
           ; return $ Just cts }

occurCheck :: VarEnv (TcTyVar, TcType) -> InertSet
           -> TcTyVar -> TcType -> Maybe (TcType,CoercionI) 
-- Traverse @ty@ to make sure that @tv@ does not appear under some flatten skolem. 
-- If it appears under some flatten skolem look in that flatten skolem equivalence class 
-- (see Note [InertSet FlattenSkolemEqClass], [Loopy Spontaneous Solving]) to see if you 
-- can find a different flatten skolem to use, that is, one that does not mention @tv@.
-- 
-- Postcondition: Just (ty', coi) = occurCheck binds inerts tv ty 
--       coi :: ty' ~ ty 
-- NB: The returned type ty' may not be flat!

occurCheck ty_binds inerts the_tv the_ty
  = ok emptyVarSet the_ty 
  where 
    -- If (fsk `elem` bad) then tv occurs in any rendering
    -- of the type under the expansion of fsk
    ok bad this_ty@(TyConApp tc tys) 
      | Just tys_cois <- allMaybes (map (ok bad) tys) 
      , (tys',cois') <- unzip tys_cois
      = Just (TyConApp tc tys', mkTyConAppCoI tc cois') 
      | isSynTyCon tc, Just ty_expanded <- tcView this_ty
      = ok bad ty_expanded   -- See Note [Type synonyms and the occur check] in TcUnify
    ok bad (PredTy sty) 
      | Just (sty',coi) <- ok_pred bad sty 
      = Just (PredTy sty', coi) 
    ok bad (FunTy arg res) 
      | Just (arg', coiarg) <- ok bad arg, Just (res', coires) <- ok bad res
      = Just (FunTy arg' res', mkFunTyCoI coiarg coires) 
    ok bad (AppTy fun arg) 
      | Just (fun', coifun) <- ok bad fun, Just (arg', coiarg) <- ok bad arg 
      = Just (AppTy fun' arg', mkAppTyCoI coifun coiarg) 
    ok bad (ForAllTy tv1 ty1) 
    -- WARNING: What if it is a (t1 ~ t2) => t3? It's not handled properly at the moment. 
      | Just (ty1', coi) <- ok bad ty1 
      = Just (ForAllTy tv1 ty1', mkForAllTyCoI tv1 coi) 

    -- Variable cases 
    ok bad this_ty@(TyVarTy tv) 
      | tv == the_tv           		        = Nothing             -- Occurs check error
      | not (isTcTyVar tv) 		        = Just (this_ty, IdCo this_ty) -- Bound var
      | FlatSkol zty <- tcTyVarDetails tv       = ok_fsk bad tv zty
      | Just (_,ty) <- lookupVarEnv ty_binds tv = ok bad ty 
      | otherwise                               = Just (this_ty, IdCo this_ty)

    -- Check if there exists a ty bind already, as a result of sneaky unification. 
    -- Fall through
    ok _bad _ty = Nothing 

    -----------
    ok_pred bad (ClassP cn tys)
      | Just tys_cois <- allMaybes $ map (ok bad) tys 
      = let (tys', cois') = unzip tys_cois 
        in Just (ClassP cn tys', mkClassPPredCoI cn cois')
    ok_pred bad (IParam nm ty)   
      | Just (ty',co') <- ok bad ty 
      = Just (IParam nm ty', mkIParamPredCoI nm co') 
    ok_pred bad (EqPred ty1 ty2) 
      | Just (ty1',coi1) <- ok bad ty1, Just (ty2',coi2) <- ok bad ty2
      = Just (EqPred ty1' ty2', mkEqPredCoI coi1 coi2) 
    ok_pred _ _ = Nothing 

    -----------
    ok_fsk bad fsk zty
      | fsk `elemVarSet` bad 
            -- We are already trying to find a rendering of fsk, 
	    -- and to do that it seems we need a rendering, so fail
      = Nothing
      | otherwise 
      = firstJusts (ok new_bad zty : map (go_under_fsk new_bad) fsk_equivs)
      where
        fsk_equivs = getFskEqClass inerts fsk 
        new_bad    = bad `extendVarSetList` (fsk : map fst fsk_equivs)

    -----------
    go_under_fsk bad_tvs (fsk,co)
      | FlatSkol zty <- tcTyVarDetails fsk
      = case ok bad_tvs zty of
           Nothing        -> Nothing
           Just (ty,coi') -> Just (ty, mkTransCoI coi' (ACo co)) 
      | otherwise = pprPanic "go_down_equiv" (ppr fsk)
\end{code}


*********************************************************************************
*                                                                               * 
                       The interact-with-inert Stage
*                                                                               *
*********************************************************************************

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

        , ir_improvement  :: Maybe FDImprovement -- In case improvement kicked in
        }

-- What to do with the inert reactant.
data InertAction = KeepInert | DropInert
  deriving Eq

mkIRContinue :: Monad m => WorkItem -> InertAction -> WorkList -> m InteractResult
mkIRContinue wi keep newWork = return $ IR (ContinueWith wi) keep newWork Nothing 

mkIRStop :: Monad m => InertAction -> WorkList -> m InteractResult
mkIRStop keep newWork = return $ IR Stop keep newWork Nothing

mkIRStop_RecordImprovement :: Monad m => InertAction -> WorkList -> FDImprovement -> m InteractResult 
mkIRStop_RecordImprovement keep newWork fdimpr = return $ IR Stop keep newWork (Just fdimpr) 


dischargeWorkItem :: Monad m => m InteractResult
dischargeWorkItem = mkIRStop KeepInert emptyWorkList

noInteraction :: Monad m => WorkItem -> m InteractResult
noInteraction workItem = mkIRContinue workItem KeepInert emptyWorkList

data WhichComesFromInert = LeftComesFromInert | RightComesFromInert 
     -- See Note [Efficient Orientation, Case 2] 


---------------------------------------------------
-- Interact a single WorkItem with the equalities of an inert set as far as possible, i.e. until we 
-- get a Stop result from an individual reaction (i.e. when the WorkItem is consumed), or until we've 
-- interact the WorkItem with the entire equalities of the InertSet

interactWithInertEqsStage :: SimplifierStage 
interactWithInertEqsStage workItem inert
  = foldISEqCtsM interactNext initITR inert 
  where initITR = SR { sr_inerts   = IS { inert_eqs  = emptyCCan -- We will fold over the equalities
                                        , inert_fsks = Map.empty -- which will generate those two again
                                        , inert_cts  = inert_cts inert
                                        , inert_fds  = inert_fds inert
                                        }
                     , sr_new_work = emptyWorkList
                     , sr_stop     = ContinueWith workItem }


---------------------------------------------------
-- Interact a single WorkItem with *non-equality* constraints in the inert set. 
-- Precondition: equality interactions must have already happened, hence we have 
-- to pick up some information from the incoming inert, before folding over the 
-- "Other" constraints it contains! 
interactWithInertsStage :: SimplifierStage
interactWithInertsStage workItem inert
  = foldISOtherCtsM interactNext initITR inert
  where 
    initITR = SR { -- Pick up: (1) equations, (2) FD improvements, (3) FlatSkol equiv. classes
                   sr_inerts   = IS { inert_eqs  = inert_eqs inert 
                                    , inert_cts  = emptyCCan      
                                    , inert_fds  = inert_fds inert 
                                    , inert_fsks = inert_fsks inert }
                 , sr_new_work = emptyWorkList
                 , sr_stop     = ContinueWith workItem }

interactNext :: StageResult -> AtomicInert -> TcS StageResult 
interactNext it inert  
  | ContinueWith workItem <- sr_stop it
  = do { let inerts      = sr_inerts it 
             fdimprs_old = getFDImprovements inerts 

       ; ir <- interactWithInert fdimprs_old inert workItem 

       -- New inerts depend on whether we KeepInert or not and must 
       -- be updated with FD improvement information from the interaction result (ir) 
       ; let inerts_new = updInertSetFDImprs upd_inert (ir_improvement ir) 
             upd_inert  = if ir_inert_action ir == KeepInert 
                          then inerts `updInertSet` inert else inerts

       ; return $ SR { sr_inerts   = inerts_new
                     , sr_new_work = sr_new_work it `unionWorkLists` ir_new_work ir
                     , sr_stop     = ir_stop ir } }
  | otherwise 
  = return $ it { sr_inerts = (sr_inerts it) `updInertSet` inert }

-- Do a single interaction of two constraints.
interactWithInert :: FDImprovements -> AtomicInert -> WorkItem -> TcS InteractResult
interactWithInert fdimprs inert workitem 
  =  do { ctxt <- getTcSContext
        ; let is_allowed  = allowedInteraction (simplEqsOnly ctxt) inert workitem 
              inert_ev    = cc_id inert 
              work_ev     = cc_id workitem 

        -- Never interact a wanted and a derived where the derived's evidence 
        -- mentions the wanted evidence in an unguarded way. 
        -- See Note [Superclasses and recursive dictionaries] 
        -- and Note [New Wanted Superclass Work] 
        -- We don't have to do this for givens, as we fully know the evidence for them. 
        ; rec_ev_ok <- 
            case (cc_flavor inert, cc_flavor workitem) of 
              (Wanted loc, Derived {}) -> isGoodRecEv work_ev  (WantedEvVar inert_ev loc)
              (Derived {}, Wanted loc) -> isGoodRecEv inert_ev (WantedEvVar work_ev loc)
              _                        -> return True 

        ; if is_allowed && rec_ev_ok then 
              doInteractWithInert fdimprs inert workitem 
          else 
              noInteraction workitem 
        }

allowedInteraction :: Bool -> AtomicInert -> WorkItem -> Bool 
-- Allowed interactions 
allowedInteraction eqs_only (CDictCan {}) (CDictCan {}) = not eqs_only
allowedInteraction eqs_only (CIPCan {})   (CIPCan {})   = not eqs_only
allowedInteraction _ _ _ = True 

--------------------------------------------
doInteractWithInert :: FDImprovements -> CanonicalCt -> CanonicalCt -> TcS InteractResult
-- Identical class constraints.

doInteractWithInert fdimprs
           (CDictCan { cc_id = d1, cc_flavor = fl1, cc_class = cls1, cc_tyargs = tys1 }) 
  workItem@(CDictCan { cc_id = d2, cc_flavor = fl2, cc_class = cls2, cc_tyargs = tys2 })
  | cls1 == cls2 && (and $ zipWith tcEqType tys1 tys2)
  = solveOneFromTheOther (d1,fl1) workItem 

  | cls1 == cls2 && (not (isGiven fl1 && isGiven fl2))
  = 	 -- See Note [When improvement happens]
    do { let pty1 = ClassP cls1 tys1 
             pty2 = ClassP cls2 tys2 
             work_item_pred_loc = (pty2, ppr d2)
             inert_pred_loc     = (pty1, ppr d1)
	     loc                = combineCtLoc fl1 fl2
             eqn_pred_locs = improveFromAnother work_item_pred_loc inert_pred_loc         

       ; wevvars <- mkWantedFunDepEqns loc eqn_pred_locs 
       ; fd_work <- canWanteds wevvars 
              	 -- See Note [Generating extra equalities]
       ; traceTcS "Checking if improvements existed." (ppr fdimprs) 
       ; if isEmptyWorkList fd_work || haveBeenImproved fdimprs pty1 pty2 then
             -- Must keep going
             mkIRContinue workItem KeepInert fd_work 
         else do { traceTcS "Recording improvement and throwing item back in worklist." (ppr (pty1,pty2))
                 ; mkIRStop_RecordImprovement KeepInert 
                      (fd_work `unionWorkLists` workListFromCCan workItem) (pty1,pty2)
                 }
         -- See Note [FunDep Reactions] 
       }

-- Class constraint and given equality: use the equality to rewrite
-- the class constraint. 
doInteractWithInert _fdimprs
                    (CTyEqCan { cc_id = cv, cc_flavor = ifl, cc_tyvar = tv, cc_rhs = xi }) 
                    (CDictCan { cc_id = dv, cc_flavor = wfl, cc_class = cl, cc_tyargs = xis }) 
  | ifl `canRewrite` wfl 
  , tv `elemVarSet` tyVarsOfTypes xis
  = if isDerivedSC wfl then 
        mkIRStop KeepInert $ emptyWorkList -- See Note [Adding Derived Superclasses]
    else do { rewritten_dict <- rewriteDict (cv,tv,xi) (dv,wfl,cl,xis)
            -- Continue with rewritten Dictionary because we can only be in the 
            -- interactWithEqsStage, so the dictionary is inert. 
            ; mkIRContinue rewritten_dict KeepInert emptyWorkList }
    
doInteractWithInert _fdimprs 
                    (CDictCan { cc_id = dv, cc_flavor = ifl, cc_class = cl, cc_tyargs = xis }) 
           workItem@(CTyEqCan { cc_id = cv, cc_flavor = wfl, cc_tyvar = tv, cc_rhs = xi })
  | wfl `canRewrite` ifl
  , tv `elemVarSet` tyVarsOfTypes xis
  = if isDerivedSC ifl then
        mkIRContinue workItem DropInert emptyWorkList -- No need to do any rewriting, 
                                                      -- see Note [Adding Derived Superclasses]
    else do { rewritten_dict <- rewriteDict (cv,tv,xi) (dv,ifl,cl,xis) 
            ; mkIRContinue workItem DropInert (workListFromCCan rewritten_dict) }

-- Class constraint and given equality: use the equality to rewrite
-- the class constraint.
doInteractWithInert _fdimprs 
                    (CTyEqCan { cc_id = cv, cc_flavor = ifl, cc_tyvar = tv, cc_rhs = xi }) 
                    (CIPCan { cc_id = ipid, cc_flavor = wfl, cc_ip_nm = nm, cc_ip_ty = ty }) 
  | ifl `canRewrite` wfl
  , tv `elemVarSet` tyVarsOfType ty 
  = do { rewritten_ip <- rewriteIP (cv,tv,xi) (ipid,wfl,nm,ty) 
       ; mkIRContinue rewritten_ip KeepInert emptyWorkList } 

doInteractWithInert _fdimprs 
                    (CIPCan { cc_id = ipid, cc_flavor = ifl, cc_ip_nm = nm, cc_ip_ty = ty }) 
           workItem@(CTyEqCan { cc_id = cv, cc_flavor = wfl, cc_tyvar = tv, cc_rhs = xi })
  | wfl `canRewrite` ifl
  , tv `elemVarSet` tyVarsOfType ty
  = do { rewritten_ip <- rewriteIP (cv,tv,xi) (ipid,ifl,nm,ty) 
       ; mkIRContinue workItem DropInert (workListFromCCan rewritten_ip) }

-- Two implicit parameter constraints.  If the names are the same,
-- but their types are not, we generate a wanted type equality 
-- that equates the type (this is "improvement").  
-- However, we don't actually need the coercion evidence,
-- so we just generate a fresh coercion variable that isn't used anywhere.
doInteractWithInert _fdimprs 
                    (CIPCan { cc_id = id1, cc_flavor = ifl, cc_ip_nm = nm1, cc_ip_ty = ty1 }) 
           workItem@(CIPCan { cc_flavor = wfl, cc_ip_nm = nm2, cc_ip_ty = ty2 })
  | nm1 == nm2 && isGiven wfl && isGiven ifl
  = 	-- See Note [Overriding implicit parameters]
        -- Dump the inert item, override totally with the new one
	-- Do not require type equality
    mkIRContinue workItem DropInert emptyWorkList

  | nm1 == nm2 && ty1 `tcEqType` ty2 
  = solveOneFromTheOther (id1,ifl) workItem 

  | nm1 == nm2
  =  	-- See Note [When improvement happens]
    do { co_var <- newWantedCoVar ty1 ty2 
       ; let flav = Wanted (combineCtLoc ifl wfl) 
       ; cans <- mkCanonical flav co_var 
       ; mkIRContinue workItem KeepInert cans }


-- Inert: equality, work item: function equality

-- Never rewrite a given with a wanted equality, and a type function
-- equality can never rewrite an equality.  Note also that if we have
-- F x1 ~ x2 and a ~ x3, and a occurs in x2, we don't rewrite it.  We
-- can wait until F x1 ~ x2 matches another F x1 ~ x4, and only then
-- we will ``expose'' x2 and x4 to rewriting.

-- Otherwise, we can try rewriting the type function equality with the equality.
doInteractWithInert _fdimprs
                    (CTyEqCan { cc_id = cv1, cc_flavor = ifl, cc_tyvar = tv, cc_rhs = xi1 }) 
                    (CFunEqCan { cc_id = cv2, cc_flavor = wfl, cc_fun = tc
                               , cc_tyargs = args, cc_rhs = xi2 })
  | ifl `canRewrite` wfl 
  , tv `elemVarSet` tyVarsOfTypes args
  = do { rewritten_funeq <- rewriteFunEq (cv1,tv,xi1) (cv2,wfl,tc,args,xi2) 
       ; mkIRStop KeepInert (workListFromCCan rewritten_funeq) } 
         -- must Stop here, because we may no longer be inert after the rewritting.

-- Inert: function equality, work item: equality
doInteractWithInert _fdimprs
                    (CFunEqCan {cc_id = cv1, cc_flavor = ifl, cc_fun = tc
                              , cc_tyargs = args, cc_rhs = xi1 }) 
           workItem@(CTyEqCan { cc_id = cv2, cc_flavor = wfl, cc_tyvar = tv, cc_rhs = xi2 })
  | wfl `canRewrite` ifl
  , tv `elemVarSet` tyVarsOfTypes args
  = do { rewritten_funeq <- rewriteFunEq (cv2,tv,xi2) (cv1,ifl,tc,args,xi1) 
       ; mkIRContinue workItem DropInert (workListFromCCan rewritten_funeq) } 

doInteractWithInert _fdimprs
                    (CFunEqCan { cc_id = cv1, cc_flavor = fl1, cc_fun = tc1
                               , cc_tyargs = args1, cc_rhs = xi1 }) 
           workItem@(CFunEqCan { cc_id = cv2, cc_flavor = fl2, cc_fun = tc2
                               , cc_tyargs = args2, cc_rhs = xi2 })
  | fl1 `canSolve` fl2 && lhss_match
  = do { cans <- rewriteEqLHS LeftComesFromInert  (mkCoVarCoercion cv1,xi1) (cv2,fl2,xi2) 
       ; mkIRStop KeepInert cans } 
  | fl2 `canSolve` fl1 && lhss_match
  = do { cans <- rewriteEqLHS RightComesFromInert (mkCoVarCoercion cv2,xi2) (cv1,fl1,xi1) 
       ; mkIRContinue workItem DropInert cans }
  where
    lhss_match = tc1 == tc2 && and (zipWith tcEqType args1 args2) 

doInteractWithInert _fdimprs 
           inert@(CTyEqCan { cc_id = cv1, cc_flavor = fl1, cc_tyvar = tv1, cc_rhs = xi1 }) 
           workItem@(CTyEqCan { cc_id = cv2, cc_flavor = fl2, cc_tyvar = tv2, cc_rhs = xi2 })
-- Check for matching LHS 
  | fl1 `canSolve` fl2 && tv1 == tv2 
  = do { cans <- rewriteEqLHS LeftComesFromInert (mkCoVarCoercion cv1,xi1) (cv2,fl2,xi2) 
       ; mkIRStop KeepInert cans } 

  | fl2 `canSolve` fl1 && tv1 == tv2 
  = do { cans <- rewriteEqLHS RightComesFromInert (mkCoVarCoercion cv2,xi2) (cv1,fl1,xi1) 
       ; mkIRContinue workItem DropInert cans } 

-- Check for rewriting RHS 
  | fl1 `canRewrite` fl2 && tv1 `elemVarSet` tyVarsOfType xi2 
  = do { rewritten_eq <- rewriteEqRHS (cv1,tv1,xi1) (cv2,fl2,tv2,xi2) 
       ; mkIRStop KeepInert rewritten_eq }
  | fl2 `canRewrite` fl1 && tv2 `elemVarSet` tyVarsOfType xi1
  = do { rewritten_eq <- rewriteEqRHS (cv2,tv2,xi2) (cv1,fl1,tv1,xi1) 
       ; mkIRContinue workItem DropInert rewritten_eq } 

-- Finally, if workitem is a Flatten Equivalence Class constraint and the 
-- inert is a wanted constraint, even when the workitem cannot rewrite the 
-- inert, drop the inert out because you may have to reconsider solving the 
-- inert *using* the equivalence class you created. See note [Loopy Spontaneous Solving]
-- and [InertSet FlattenSkolemEqClass] 

  | not $ isGiven fl1,                  -- The inert is wanted or derived
    isMetaTyVar tv1,                    -- and has a unification variable lhs
    FlatSkol {} <- tcTyVarDetails tv2,  -- And workitem is a flatten skolem equality
    Just tv2'   <- tcGetTyVar_maybe xi2, FlatSkol {} <- tcTyVarDetails tv2' 
  = mkIRContinue workItem DropInert (workListFromCCan inert)   


-- Fall-through case for all other situations
doInteractWithInert _fdimprs _ workItem = noInteraction workItem

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
           _given_or_derived -> setDictBind dv' (EvCast dv dict_co) 
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
           _given_or_derived -> setIPBind ipid' (EvCast ipid ip_co) 
       ; return (CIPCan { cc_id = ipid'
                        , cc_flavor = gw
                        , cc_ip_nm = nm
                        , cc_ip_ty = ty' }) }
   
rewriteFunEq :: (CoVar,TcTyVar,Xi) -> (CoVar,CtFlavor,TyCon, [Xi], Xi) -> TcS CanonicalCt
rewriteFunEq (cv1,tv,xi1) (cv2,gw, tc,args,xi2) 
  = do { let arg_cos = substTysWith [tv] [mkCoVarCoercion cv1] args 
             args'   = substTysWith [tv] [xi1] args 
             fun_co  = mkTyConCoercion tc arg_cos 
       ; cv2' <- case gw of 
                   Wanted {} -> do { cv2' <- newWantedCoVar (mkTyConApp tc args') xi2 
                                   ; setWantedCoBind cv2 $ 
                                     mkTransCoercion fun_co (mkCoVarCoercion cv2') 
                                   ; return cv2' } 
                   _giv_or_der -> newGivOrDerCoVar (mkTyConApp tc args') xi2 $
                                  mkTransCoercion (mkSymCoercion fun_co) (mkCoVarCoercion cv2) 
       ; return (CFunEqCan { cc_id = cv2'
                           , cc_flavor = gw
                           , cc_tyargs = args'
                           , cc_fun = tc 
                           , cc_rhs = xi2 }) }


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
  = do { when (isWanted gw) (setWantedCoBind cv2 (mkSymCoercion co2')) 
       ; return emptyCCan } 
  | otherwise 
  = do { cv2' <- 
           case gw of 
             Wanted {} 
                 -> do { cv2' <- newWantedCoVar (mkTyVarTy tv2) xi2' 
                       ; setWantedCoBind cv2 $ 
                         mkCoVarCoercion cv2' `mkTransCoercion` mkSymCoercion co2'
                       ; return cv2' } 
             _giv_or_der 
                 -> newGivOrDerCoVar (mkTyVarTy tv2) xi2' $ 
                    mkCoVarCoercion cv2 `mkTransCoercion` co2'

       ; xi2'' <- canOccursCheck gw tv2 xi2' -- we know xi2' is *not* tv2 
       ; canEq gw cv2' (mkTyVarTy tv2) xi2''
       }
  where 
    xi2' = substTyWith [tv1] [xi1] xi2 
    co2' = substTyWith [tv1] [mkCoVarCoercion cv1] xi2  -- xi2 ~ xi2[xi1/tv1]


rewriteEqLHS :: WhichComesFromInert -> (Coercion,Xi) -> (CoVar,CtFlavor,Xi) -> TcS WorkList
-- Used to ineract two equalities of the following form: 
-- First Equality:   co1: (XXX ~ xi1)  
-- Second Equality:  cv2: (XXX ~ xi2) 
-- Where the cv1 `canSolve` cv2 equality 
-- We have an option of creating new work (xi1 ~ xi2) OR (xi2 ~ xi1), 
--    See Note [Efficient Orientation] for that 
rewriteEqLHS which (co1,xi1) (cv2,gw,xi2) 
  = do { cv2' <- case (isWanted gw, which) of 
                   (True,LeftComesFromInert) ->
                       do { cv2' <- newWantedCoVar xi2 xi1 
                          ; setWantedCoBind cv2 $ 
                            co1 `mkTransCoercion` mkSymCoercion (mkCoVarCoercion cv2')
                          ; return cv2' } 
                   (True,RightComesFromInert) -> 
                       do { cv2' <- newWantedCoVar xi1 xi2 
                          ; setWantedCoBind cv2 $ 
                            co1 `mkTransCoercion` mkCoVarCoercion cv2'
                          ; return cv2' } 
                   (False,LeftComesFromInert) ->
                       newGivOrDerCoVar xi2 xi1 $ 
                       mkSymCoercion (mkCoVarCoercion cv2) `mkTransCoercion` co1 
                   (False,RightComesFromInert) -> 
                        newGivOrDerCoVar xi1 xi2 $ 
                        mkSymCoercion co1 `mkTransCoercion` mkCoVarCoercion cv2
       ; mkCanonical gw cv2'
       }
                                           
solveOneFromTheOther :: (EvVar, CtFlavor) -> CanonicalCt -> TcS InteractResult 
-- First argument inert, second argument workitem. They both represent 
-- wanted/given/derived evidence for the *same* predicate so we try here to 
-- discharge one directly from the other. 
--
-- Precondition: value evidence only (implicit parameters, classes) 
--               not coercion
solveOneFromTheOther (iid,ifl) workItem 
      -- Both derived needs a special case. You might think that we do not need
      -- two evidence terms for the same claim. But, since the evidence is partial, 
      -- either evidence may do in some cases; see TcSMonad.isGoodRecEv.
      -- See also Example 3 in Note [Superclasses and recursive dictionaries] 
  | isDerived ifl && isDerived wfl 
  = noInteraction workItem 

  | ifl `canSolve` wfl
  = do { unless (isGiven wfl) $ setEvBind wid (EvId iid) 
           -- Overwrite the binding, if one exists
	   -- For Givens, which are lambda-bound, nothing to overwrite,
       ; dischargeWorkItem }

  | otherwise  -- wfl `canSolve` ifl 
  = do { unless (isGiven ifl) $ setEvBind iid (EvId wid)
       ; mkIRContinue workItem DropInert emptyWorkList }

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
this item as solved (in effect, given) into our inert set and with that add 
its superclass constraints (as given) in our worklist. 

But now we have added partially solved constraints to the worklist which may 
interact with other wanteds. Consider the example: 

Example 1: 

    class Eq b => Foo a b        --- 0-th selector
    instance Eq a => Foo [a] a   --- fooDFun

and wanted (Foo [t] t). We are first going to see that the instance matches 
and create an inert set that includes the solved (Foo [t] t) and its 
superclasses. 
       d1 :_g Foo [t] t                 d1 := EvDFunApp fooDFun d3 
       d2 :_g Eq t                      d2 := EvSuperClass d1 0 
Our work list is going to contain a new *wanted* goal
       d3 :_w Eq t 
It is wrong to react the wanted (Eq t) with the given (Eq t) because that would 
construct loopy evidence. Hence the check isGoodRecEv in doInteractWithInert. 

OK, so we have ruled out bad behaviour, but how do we ge recursive dictionaries, 
at all? Consider

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
  = NoTopInt 	           -- No top-level interaction
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
topReactionsStage workItem inerts 
  = do { tir <- tryTopReact workItem 
       ; case tir of 
           NoTopInt -> 
               return $ SR { sr_inerts   = inerts 
                           , sr_new_work = emptyWorkList 
                           , sr_stop     = ContinueWith workItem } 
           SomeTopInt tir_new_work tir_new_inert -> 
               return $ SR { sr_inerts   = inerts 
                           , sr_new_work = tir_new_work
                           , sr_stop     = tir_new_inert
                           }
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
-- The work item does not react with the inert set, 
-- so try interaction with top-level instances

-- Given dictionary; just add superclasses
-- See Note [Given constraint that matches an instance declaration]
doTopReact workItem@(CDictCan { cc_id = dv, cc_flavor = Given loc
                              , cc_class = cls, cc_tyargs = xis })
  = do { sc_work <- newGivenSCWork dv loc cls xis 
       ; return $ SomeTopInt sc_work (ContinueWith workItem) }

-- Derived dictionary
-- Do not add any further derived superclasses; their 
-- full transitive closure has already been added. 
-- But do look for functional dependencies
doTopReact workItem@(CDictCan { cc_id = dv, cc_flavor = Derived loc _
                              , cc_class = cls, cc_tyargs = xis })
  = do { fd_work <- findClassFunDeps dv cls xis loc
       ; if isEmptyWorkList fd_work then 
              return NoTopInt
         else return $ SomeTopInt { tir_new_work = fd_work
                                  , tir_new_inert = ContinueWith workItem } }

doTopReact workItem@(CDictCan { cc_id = dv, cc_flavor = Wanted loc
                              , cc_class = cls, cc_tyargs = xis }) 
  = do { -- See Note [MATCHING-SYNONYMS]
       ; lkp_inst_res <- matchClassInst cls xis loc
       ; case lkp_inst_res of 
           NoInstance -> 
             do { traceTcS "doTopReact/ no class instance for" (ppr dv) 
                ; fd_work <- findClassFunDeps dv cls xis loc
                ; if isEmptyWorkList fd_work then 
                      do { sc_work <- newDerivedSCWork dv loc cls xis
                                 -- See Note [Adding Derived Superclasses] 
				 -- NB: workItem is inert, but it isn't solved
				 -- keep it as inert, although it's not solved 
				 -- because we have now reacted all its 
 				 -- top-level fundep-induced equalities!
                         ; return $ SomeTopInt 
                              { tir_new_work = fd_work `unionWorkLists` sc_work
                              , tir_new_inert = ContinueWith workItem } }

                  else -- More fundep work produced, don't do any superclass stuff, 
                       -- just thow him back in the worklist, which will prioritize 
                       -- the solution of fd equalities
                       return $ SomeTopInt 
                              { tir_new_work = fd_work `unionWorkLists` 
                                               workListFromCCan workItem
                              , tir_new_inert = Stop } }

           GenInst wtvs ev_term ->  -- Solved 
	   	   -- No need to do fundeps stuff here; the instance 
		   -- matches already so we won't get any more info
		   -- from functional dependencies
               do { traceTcS "doTopReact/ found class instance for" (ppr dv) 
                  ; setDictBind dv ev_term 
                  ; inst_work <- canWanteds wtvs
                  ; if null wtvs
                    -- Solved in one step and no new wanted work produced. 
                    -- i.e we directly matched a top-level instance
		    -- No point in caching this in 'inert', nor in adding superclasses
                    then return $ SomeTopInt { tir_new_work  = emptyWorkList 
                                             , tir_new_inert = Stop }

                    -- Solved and new wanted work produced, you may cache the 
		    -- (tentatively solved) dictionary as Derived and its superclasses
                    else do { let solved = makeSolvedByInst workItem
                            ; sc_work <- newDerivedSCWork dv loc cls xis
                                         -- See Note [Adding Derived Superclasses]
                            ; return $ SomeTopInt 
                                  { tir_new_work  = inst_work `unionWorkLists` sc_work 
                                  , tir_new_inert = ContinueWith solved } }
       }          }

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
                              Wanted {} -> do { cv' <- newWantedCoVar rhs_ty xi
                                              ; setWantedCoBind cv $ 
                                                    coe `mkTransCoercion`
                                                      mkCoVarCoercion cv'
                                              ; return cv' }
                              _ -> newGivOrDerCoVar xi rhs_ty $ 
                                   mkSymCoercion (mkCoVarCoercion cv) `mkTransCoercion` coe 

                   ; can_cts <- mkCanonical fl cv'
                   ; return $ SomeTopInt can_cts Stop }
           _ 
             -> panicTcS $ text "TcSMonad.matchFam returned multiple instances!"
       }


-- Any other work item does not react with any top-level equations
doTopReact _workItem = return NoTopInt 

----------------------
findClassFunDeps :: EvVar -> Class -> [Xi] -> WantedLoc -> TcS WorkList
-- Look for a fundep reaction beween the wanted item 
-- and a top-level instance declaration
findClassFunDeps dv cls xis loc
 = do { instEnvs <- getInstEnvs
      ; let eqn_pred_locs = improveFromInstEnv (classInstances instEnvs)
                                               (ClassP cls xis, ppr dv)
      ; wevvars <- mkWantedFunDepEqns loc eqn_pred_locs 
	     	      -- NB: fundeps generate some wanted equalities, but 
	   	      --     we don't use their evidence for anything
      ; canWanteds wevvars }
\end{code}

Note [Adding Derived Superclasses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
Generally speaking, we want to be able to add derived superclasses of
unsolved wanteds, and wanteds that have been partially being solved
via an instance. This is important to be able to simplify the inferred
constraints more (and to allow for recursive dictionaries, less
importantly). Example:

Inferred wanted constraint is (Eq a, Ord a), but we'd only like to
quantify over Ord a, hence we would like to be able to add the
superclass of Ord a as Derived and use it to solve the wanted Eq a.

Hence we will add Derived superclasses in the following two cases: 
    (1) When we meet an unsolved wanted in top-level reactions
    (2) When we partially solve a wanted in top-level reactions using an instance decl.

At that point, we have two options:
    (1) Add transitively add *ALL* of the superclasses of the Derived
    (2) Add only the immediate ones, but whenever we meet a Derived in
        the future, add its own superclasses as Derived.

Option (2) is terrible, because deriveds may be rewritten or kicked
out of the inert set, which will result in slightly rewritten
superclasses being reintroduced in the worklist and the inert set. Eg:

    class C a => B a 
    instance Foo a => B [a] 

Original constraints: 
[Wanted] d : B [a] 
[Given] co : a ~ Int 

We apply the instance to the wanted and put it and its superclasses as
as Deriveds in the inerts:

[Derived] d : B [a] 
[Derived] (sel d) : C [a]

The work is now: 
[Given] co  : a ~ Int 
[Wanted] d' : Foo a 

Now, suppose that we interact the Derived with the Given equality, and
kick him out of the inert, the next time around a superclass C [Int]
will be produced -- but we already *have* C [a] in the inerts which
will anyway get rewritten to C [Int].

So we choose (1), and *never* introduce any more superclass work from
Deriveds.  This enables yet another optimisation: If we ever meet an
equality that can rewrite a Derived, if that Derived is a superclass
derived (like C [a] above), i.e. not a partially solved one (like B
[a]) above, we may simply completely *discard* that Derived. The
reason is because somewhere in the inert lies the original wanted, or
partially solved constraint that gave rise to that superclass, and
that constraint *will* be kicked out, and *will* result in the
rewritten superclass to be added in the inerts later on, anyway.



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
Even in the case of wanted constraints, we add all of its superclasses as 
new given work. There are several reasons for this: 
     a) to minimise error messages; 
        eg suppose we have wanted (Eq a, Ord a)
	     then we report only (Ord a) unsoluble

     b) to make the smallest number of constraints when *inferring* a type
        (same Eq/Ord example)

     c) for recursive dictionaries we *must* add the superclasses
        so that we can use them when solving a sub-problem

     d) To allow FD-like improvement for type families. Assume that 
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

Here is another example where this is useful. 

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

newGivenSCWork :: EvVar -> GivenLoc -> Class -> [Xi] -> TcS WorkList
newGivenSCWork ev loc cls xis
  | NoScSkol <- ctLocOrigin loc  -- Very important!
  = return emptyWorkList
  | otherwise
  = newImmSCWorkFromFlavored ev (Given loc) cls xis >>= return 

newDerivedSCWork :: EvVar -> WantedLoc -> Class -> [Xi] -> TcS WorkList 
newDerivedSCWork ev loc cls xis 
  =  do { ims <- newImmSCWorkFromFlavored ev flavor cls xis 
        ; rec_sc_work ims  }
  where 
    rec_sc_work :: CanonicalCts -> TcS CanonicalCts 
    rec_sc_work cts 
      = do { bg <- mapBagM (\c -> do { ims <- imm_sc_work c 
                                     ; recs_ims <- rec_sc_work ims 
                                     ; return $ consBag c recs_ims }) cts 
           ; return $ concatBag bg } 
    imm_sc_work (CDictCan { cc_id = dv, cc_flavor = fl, cc_class = cls, cc_tyargs = xis })
       = newImmSCWorkFromFlavored dv fl cls xis 
    imm_sc_work _ct = return emptyCCan 

    flavor = Derived loc DerSC 

newImmSCWorkFromFlavored :: EvVar -> CtFlavor -> Class -> [Xi] -> TcS WorkList
-- Returns immediate superclasses 
newImmSCWorkFromFlavored ev flavor cls xis 
  = do { let (tyvars, sc_theta, _, _) = classBigSig cls 
             sc_theta1 = substTheta (zipTopTvSubst tyvars xis) sc_theta
       ; sc_vars <- zipWithM inst_one sc_theta1 [0..]
       ; mkCanonicals flavor sc_vars }
  where
    inst_one pred n = newGivOrDerEvVar pred (EvSuperClass ev n)


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
                     ; let wevs = [WantedEvVar w loc | w <- ev_vars]
                     ; return $ GenInst wevs (EvDFunApp dfun_id tys ev_vars) }
                 }
        }
\end{code}
