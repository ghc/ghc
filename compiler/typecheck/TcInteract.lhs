\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module TcInteract ( 
     solveInteractGiven,  -- Solves [EvVar],GivenLoc
     solveInteractCts,    -- Solves [Cts]
  ) where  

#include "HsVersions.h"


import BasicTypes ()
import TcCanonical
import VarSet
import Type
import Unify
import FamInstEnv
import Coercion( mkAxInstRHS )

import Var
import TcType
import PrelNames (singIClassName)

import Class
import TyCon
import Name

import FunDeps

import TcEvidence
import Outputable

import TcMType ( zonkTcPredType )

import TcRnTypes
import TcErrors
import TcSMonad
import Maybes( orElse )
import Bag

import Control.Monad ( foldM )

import VarEnv
import qualified Data.Traversable as Traversable

import Control.Monad( when, unless )
import Pair ()
import UniqFM
import FastString ( sLit ) 
import DynFlags
import Util
\end{code}
**********************************************************************
*                                                                    * 
*                      Main Interaction Solver                       *
*                                                                    *
**********************************************************************

Note [Basic Simplifier Plan] 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. Pick an element from the WorkList if there exists one with depth 
   less thanour context-stack depth. 

2. Run it down the 'stage' pipeline. Stages are: 
      - canonicalization
      - inert reactions
      - spontaneous reactions
      - top-level intreactions
   Each stage returns a StopOrContinue and may have sideffected 
   the inerts or worklist.
  
   The threading of the stages is as follows: 
      - If (Stop) is returned by a stage then we start again from Step 1. 
      - If (ContinueWith ct) is returned by a stage, we feed 'ct' on to 
        the next stage in the pipeline. 
4. If the element has survived (i.e. ContinueWith x) the last stage 
   then we add him in the inerts and jump back to Step 1.

If in Step 1 no such element exists, we have exceeded our context-stack 
depth and will simply fail.
\begin{code}

solveInteractCts :: [Ct] -> TcS (Bag Implication)
-- Returns a bag of residual implications that have arisen while solving
-- this particular worklist.
solveInteractCts cts 
  = do { traceTcS "solveInteractCtS" (vcat [ text "cts =" <+> ppr cts ]) 
       ; updWorkListTcS (appendWorkListCt cts) >> solveInteract 
       ; impls <- getTcSImplics
       ; updTcSImplics (const emptyBag) -- Nullify residual implications
       ; return impls }

solveInteractGiven :: GivenLoc -> [EvVar] -> TcS (Bag Implication)
-- In principle the givens can kick out some wanteds from the inert
-- resulting in solving some more wanted goals here which could emit
-- implications. That's why I return a bag of implications. Not sure
-- if this can happen in practice though.
solveInteractGiven gloc evs
  = solveInteractCts (map mk_noncan evs)
  where 
    mk_noncan ev = CNonCanonical { cc_ev = Given { ctev_gloc = gloc 
                                                 , ctev_evtm = EvId ev
                                                 , ctev_pred = evVarPred ev }
                                 , cc_depth = 0 }

-- The main solver loop implements Note [Basic Simplifier Plan]
---------------------------------------------------------------
solveInteract :: TcS ()
-- Returns the final InertSet in TcS, WorkList will be eventually empty.
solveInteract
  = {-# SCC "solveInteract" #-}
    do { dyn_flags <- getDynFlags
       ; let max_depth = ctxtStkDepth dyn_flags
             solve_loop
              = {-# SCC "solve_loop" #-}
                do { sel <- selectNextWorkItem max_depth
                   ; case sel of 
                      NoWorkRemaining     -- Done, successfuly (modulo frozen)
                        -> return ()
                      MaxDepthExceeded ct -- Failure, depth exceeded
                        -> wrapErrTcS $ solverDepthErrorTcS (cc_depth ct) [ct]
                      NextWorkItem ct     -- More work, loop around!
                        -> runSolverPipeline thePipeline ct >> solve_loop }
       ; solve_loop }

type WorkItem = Ct
type SimplifierStage = WorkItem -> TcS StopOrContinue

continueWith :: WorkItem -> TcS StopOrContinue
continueWith work_item = return (ContinueWith work_item) 

data SelectWorkItem 
       = NoWorkRemaining      -- No more work left (effectively we're done!)
       | MaxDepthExceeded Ct  -- More work left to do but this constraint has exceeded
                              -- the max subgoal depth and we must stop 
       | NextWorkItem Ct      -- More work left, here's the next item to look at 

selectNextWorkItem :: SubGoalDepth -- Max depth allowed
                   -> TcS SelectWorkItem
selectNextWorkItem max_depth
  = updWorkListTcS_return pick_next
  where 
    pick_next :: WorkList -> (SelectWorkItem, WorkList)
    pick_next wl = case selectWorkItem wl of
                     (Nothing,_) 
                         -> (NoWorkRemaining,wl)           -- No more work
                     (Just ct, new_wl) 
                         | cc_depth ct > max_depth         -- Depth exceeded
                         -> (MaxDepthExceeded ct,new_wl)
                     (Just ct, new_wl) 
                         -> (NextWorkItem ct, new_wl)      -- New workitem and worklist

runSolverPipeline :: [(String,SimplifierStage)] -- The pipeline 
                  -> WorkItem                   -- The work item 
                  -> TcS () 
-- Run this item down the pipeline, leaving behind new work and inerts
runSolverPipeline pipeline workItem 
  = do { initial_is <- getTcSInerts 
       ; traceTcS "Start solver pipeline {" $ 
                  vcat [ ptext (sLit "work item = ") <+> ppr workItem 
                       , ptext (sLit "inerts    = ") <+> ppr initial_is]

       ; final_res  <- run_pipeline pipeline (ContinueWith workItem)

       ; final_is <- getTcSInerts
       ; case final_res of 
           Stop            -> do { traceTcS "End solver pipeline (discharged) }" 
                                       (ptext (sLit "inerts    = ") <+> ppr final_is)
                                 ; return () }
           ContinueWith ct -> do { traceTcS "End solver pipeline (not discharged) }" $
                                       vcat [ ptext (sLit "final_item = ") <+> ppr ct
                                            , ptext (sLit "inerts     = ") <+> ppr final_is]
                                 ; updInertSetTcS ct }
       }
  where run_pipeline :: [(String,SimplifierStage)] -> StopOrContinue -> TcS StopOrContinue
        run_pipeline [] res = return res 
        run_pipeline _ Stop = return Stop 
        run_pipeline ((stg_name,stg):stgs) (ContinueWith ct)
          = do { traceTcS ("runStage " ++ stg_name ++ " {")
                          (text "workitem   = " <+> ppr ct) 
               ; res <- stg ct 
               ; traceTcS ("end stage " ++ stg_name ++ " }") empty
               ; run_pipeline stgs res 
               }
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
thePipeline :: [(String,SimplifierStage)]
thePipeline = [ ("lookup-in-inerts",        lookupInInertsStage)
              , ("canonicalization",        canonicalizationStage)
              , ("spontaneous solve",       spontaneousSolveStage)
              , ("interact with inerts",    interactWithInertsStage)
              , ("top-level reactions",     topReactionsStage) ]
\end{code}


\begin{code}

-- A quick lookup everywhere to see if we know about this constraint
--------------------------------------------------------------------
lookupInInertsStage :: SimplifierStage
lookupInInertsStage ct
  | Wanted { ctev_evar = ev_id, ctev_pred = pred } <- cc_ev ct
  = do { is <- getTcSInerts
       ; case lookupInInerts is pred of
           Just ctev
             |  not (isDerived ctev)
             -> do { setEvBind ev_id (ctEvTerm ctev)
                   ; return Stop }
           _ -> continueWith ct }
  | otherwise -- I could do something like that for givens 
              -- as well I suppose but it is not a big deal
  = continueWith ct


-- The canonicalization stage, see TcCanonical for details
----------------------------------------------------------
canonicalizationStage :: SimplifierStage
canonicalizationStage = TcCanonical.canonicalize 
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
    
Case 2: Functional Dependencies 
    Again, we should prefer, if possible, the inert variables on the RHS

\begin{code}
spontaneousSolveStage :: SimplifierStage 
spontaneousSolveStage workItem
  = do { mSolve <- trySpontaneousSolve workItem
       ; spont_solve mSolve } 
  where spont_solve SPCantSolve 
          | isCTyEqCan workItem                    -- Unsolved equality
          = do { kickOutRewritableInerts workItem  -- NB: will add workItem in inerts
               ; return Stop }
          | otherwise
          = continueWith workItem
        spont_solve (SPSolved workItem')           -- Post: workItem' must be equality
          = do { bumpStepCountTcS
               ; traceFireTcS (cc_depth workItem) $
                 ptext (sLit "Spontaneous:") <+> ppr workItem

                 -- NB: will add the item in the inerts
               ; kickOutRewritableInerts workItem'
               -- .. and Stop
               ; return Stop }

kickOutRewritableInerts :: Ct -> TcS () 
-- Pre:  ct is a CTyEqCan 
-- Post: The TcS monad is left with the thinner non-rewritable inerts; but which
--       contains the new constraint.
--       The rewritable end up in the worklist
kickOutRewritableInerts ct
  = {-# SCC "kickOutRewritableInerts" #-}
    do { traceTcS "kickOutRewritableInerts" $ text "workitem = " <+> ppr ct
       ; (wl,ieqs) <- {-# SCC "kick_out_rewritable" #-}
                      modifyInertTcS (kick_out_rewritable ct)
       ; traceTcS "Kicked out the following constraints" $ ppr wl
       ; is <- getTcSInerts 
       ; traceTcS "Remaining inerts are" $ ppr is

       -- Step 1: Rewrite as many of the inert_eqs on the spot!
       -- NB: if it is a given constraint just use the cached evidence
       -- to optimize e.g. mkRefl coercions from spontaneously solved cts.
       ; bnds <- getTcEvBindsMap
       ; let ct_coercion = getCtCoercion bnds ct 

       ; new_ieqs <- {-# SCC "rewriteInertEqsFromInertEq" #-}
                     rewriteInertEqsFromInertEq (cc_tyvar ct,
                                                 ct_coercion,cc_ev ct) ieqs
       ; let upd_eqs is = is { inert_cans = new_ics }
                        where ics     = inert_cans is
                              new_ics = ics { inert_eqs = new_ieqs }
       ; modifyInertTcS (\is -> ((), upd_eqs is)) 
         
       ; is <- getTcSInerts 
       ; traceTcS "Final inerts are" $ ppr is
       
         -- Step 2: Add the new guy in
       ; updInertSetTcS ct

       ; traceTcS "Kick out" (ppr ct $$ ppr wl)
       ; updWorkListTcS (unionWorkList wl) }

rewriteInertEqsFromInertEq :: (TcTyVar, TcCoercion, CtEvidence) -- A new substitution
                           -> TyVarEnv Ct                     -- All the inert equalities
                           -> TcS (TyVarEnv Ct)               -- The new inert equalities
rewriteInertEqsFromInertEq (subst_tv, _subst_co, subst_fl) ieqs
-- The goal: traverse the inert equalities and throw some of them back to the worklist
-- if you have to rewrite and recheck them for occurs check errors. 
-- To see which ones we must throw out see Note [Delicate equality kick-out]
 = do { mieqs <- Traversable.mapM do_one ieqs 
      ; traceTcS "Original inert equalities:" (ppr ieqs)
      ; let flatten_justs elem venv
              | Just act <- elem = extendVarEnv venv (cc_tyvar act) act
              | otherwise = venv                                     
            final_ieqs = foldVarEnv flatten_justs emptyVarEnv mieqs
      ; traceTcS "Remaining inert equalities:" (ppr final_ieqs)
      ; return final_ieqs }

 where do_one ct
         | subst_fl `canRewrite` fl && (subst_tv `elemVarSet` tyVarsOfCt ct) 
         = if fl `canRewrite` subst_fl then
               -- If also the inert can rewrite the subst then there is no danger of 
               -- occurs check errors sor keep it there. No need to rewrite the inert equality
               -- (as we did in the past) because of point (8) of 
               -- Note [Detailed InertCans Invariants] and 
             return (Just ct)
             -- used to be: rewrite_on_the_spot ct >>= ( return . Just )
           else -- We have to throw inert back to worklist for occurs checks 
             updWorkListTcS (extendWorkListEq ct) >> return Nothing
         | otherwise -- Just keep it there
         = return (Just ct)
         where 
           fl  = cc_ev ct

kick_out_rewritable :: Ct 
                    -> InertSet 
                    -> ((WorkList, TyVarEnv Ct),InertSet)
-- Post: returns ALL inert equalities, to be dealt with later
-- 
kick_out_rewritable ct is@(IS { inert_cans = 
                                   IC { inert_eqs    = eqmap
                                      , inert_eq_tvs = inscope
                                      , inert_dicts  = dictmap
                                      , inert_funeqs = funeqmap
                                      , inert_irreds = irreds }
                              , inert_frozen = frozen })
  = ((kicked_out,eqmap), remaining)
  where
    rest_out = fro_out `andCts` dicts_out `andCts` irs_out
    kicked_out = WorkList { wl_eqs    = []
                          , wl_funeqs = bagToList feqs_out
                          , wl_rest   = bagToList rest_out }
  
    remaining = is { inert_cans = IC { inert_eqs = emptyVarEnv
                                     , inert_eq_tvs = inscope 
                                       -- keep the same, safe and cheap
                                     , inert_dicts = dicts_in
                                     , inert_funeqs = feqs_in
                                     , inert_irreds = irs_in }
                   , inert_frozen = fro_in } 
                -- NB: Notice that don't rewrite 
                -- inert_solved_dicts, and inert_solved_funeqs
                -- optimistically. But when we lookup we have to take the 
                -- subsitution into account
    fl = cc_ev ct
    tv = cc_tyvar ct

    (feqs_out,  feqs_in)    = partCtFamHeadMap rewritable funeqmap
    (dicts_out, dicts_in)   = partitionCCanMap rewritable dictmap

    (irs_out,   irs_in)   = partitionBag rewritable irreds
    (fro_out,   fro_in)   = partitionBag rewritable frozen

    rewritable ct = (fl `canRewrite` cc_ev ct)  &&
                    (tv `elemVarSet` tyVarsOfCt ct) 
                    -- NB: tyVarsOfCt will return the type 
                    --     variables /and the kind variables/ that are 
                    --     directly visible in the type. Hence we will
                    --     have exposed all the rewriting we care about
                    --     to make the most precise kinds visible for 
                    --     matching classes etc. No need to kick out 
                    --     constraints that mention type variables whose
                    --     kinds could contain this variable!

\end{code}

Note [Delicate equality kick-out]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

Delicate:
When kicking out rewritable constraints, it would be safe to simply
kick out all rewritable equalities, but instead we only kick out those
that, when rewritten, may result in occur-check errors. Example:

          WorkItem =   [G] a ~ b
          Inerts   = { [W] b ~ [a] }
Now at this point the work item cannot be further rewritten by the
inert (due to the weaker inert flavor). Instead the workitem can 
rewrite the inert leading to potential occur check errors. So we must
kick the inert out. On the other hand, if the inert flavor was as 
powerful or more powerful than the workitem flavor, the work-item could 
not have reached this stage (because it would have already been 
rewritten by the inert).

The coclusion is: we kick out the 'dangerous' equalities that may
require recanonicalization (occurs checks) and the rest we keep 
there in the inerts without further checks.

In the past we used to rewrite-on-the-spot those equalities that we keep in,
but this is no longer necessary see Note [Non-idempotent inert substitution].

\begin{code}
data SPSolveResult = SPCantSolve
                   | SPSolved WorkItem 

-- SPCantSolve means that we can't do the unification because e.g. the variable is untouchable
-- SPSolved workItem' gives us a new *given* to go on 

-- @trySpontaneousSolve wi@ solves equalities where one side is a
-- touchable unification variable.
--     	    See Note [Touchables and givens] 
trySpontaneousSolve :: WorkItem -> TcS SPSolveResult
trySpontaneousSolve workItem@(CTyEqCan { cc_ev = gw
                                       , cc_tyvar = tv1, cc_rhs = xi, cc_depth = d })
  | isGiven gw
  = return SPCantSolve
  | Just tv2 <- tcGetTyVar_maybe xi
  = do { tch1 <- isTouchableMetaTyVar tv1
       ; tch2 <- isTouchableMetaTyVar tv2
       ; case (tch1, tch2) of
           (True,  True)  -> trySpontaneousEqTwoWay d gw tv1 tv2
           (True,  False) -> trySpontaneousEqOneWay d gw tv1 xi
           (False, True)  -> trySpontaneousEqOneWay d gw tv2 (mkTyVarTy tv1)
	   _ -> return SPCantSolve }
  | otherwise
  = do { tch1 <- isTouchableMetaTyVar tv1
       ; if tch1 then trySpontaneousEqOneWay d gw tv1 xi
                 else do { traceTcS "Untouchable LHS, can't spontaneously solve workitem:" $
                           ppr workItem 
                         ; return SPCantSolve }
       }

  -- No need for 
  --      trySpontaneousSolve (CFunEqCan ...) = ...
  -- See Note [No touchables as FunEq RHS] in TcSMonad
trySpontaneousSolve _ = return SPCantSolve

----------------
trySpontaneousEqOneWay :: SubGoalDepth 
                       -> CtEvidence -> TcTyVar -> Xi -> TcS SPSolveResult
-- tv is a MetaTyVar, not untouchable
trySpontaneousEqOneWay d gw tv xi
  | not (isSigTyVar tv) || isTyVarTy xi
  = solveWithIdentity d gw tv xi
  | otherwise -- Still can't solve, sig tyvar and non-variable rhs
  = return SPCantSolve

----------------
trySpontaneousEqTwoWay :: SubGoalDepth 
                       -> CtEvidence -> TcTyVar -> TcTyVar -> TcS SPSolveResult
-- Both tyvars are *touchable* MetaTyvars so there is only a chance for kind error here

trySpontaneousEqTwoWay d gw tv1 tv2
  = do { let k1_sub_k2 = k1 `tcIsSubKind` k2
       ; if k1_sub_k2 && nicer_to_update_tv2
         then solveWithIdentity d gw tv2 (mkTyVarTy tv1)
         else solveWithIdentity d gw tv1 (mkTyVarTy tv2) }
  where
    k1 = tyVarKind tv1
    k2 = tyVarKind tv2
    nicer_to_update_tv2 = isSigTyVar tv1 || isSystemName (Var.varName tv2)
\end{code}

Note [Kind errors] 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the wanted problem: 
      alpha ~ (# Int, Int #) 
where alpha :: ArgKind and (# Int, Int #) :: (#). We can't spontaneously solve this constraint, 
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

solveWithIdentity :: SubGoalDepth 
                  -> CtEvidence -> TcTyVar -> Xi -> TcS SPSolveResult
-- Solve with the identity coercion 
-- Precondition: kind(xi) is a sub-kind of kind(tv)
-- Precondition: CtEvidence is Wanted or Derived
-- See [New Wanted Superclass Work] to see why solveWithIdentity 
--     must work for Derived as well as Wanted
-- Returns: workItem where 
--        workItem = the new Given constraint
--
-- NB: No need for an occurs check here, because solveWithIdentity always 
--     arises from a CTyEqCan, a *canonical* constraint.  Its invariants
--     say that in (a ~ xi), the type variable a does not appear in xi.
--     See TcRnTypes.Ct invariants.
solveWithIdentity d wd tv xi 
  = do { let tv_ty = mkTyVarTy tv
       ; traceTcS "Sneaky unification:" $ 
                       vcat [text "Constraint:" <+> ppr wd,
                             text "Coercion:" <+> pprEq tv_ty xi,
                             text "Left Kind is:" <+> ppr (typeKind tv_ty),
                             text "Right Kind is:" <+> ppr (typeKind xi) ]

       ; let xi' = defaultKind xi      
               -- We only instantiate kind unification variables
               -- with simple kinds like *, not OpenKind or ArgKind
               -- cf TcUnify.uUnboundKVar

       ; setWantedTyBind tv xi'
       ; let refl_evtm = EvCoercion (mkTcReflCo xi')
             refl_pred = mkTcEqPred tv_ty xi'

       ; when (isWanted wd) $ 
              setEvBind (ctev_evar wd) refl_evtm

       ; let given_fl = Given { ctev_gloc = mkGivenLoc (ctev_wloc wd) UnkSkol
                              , ctev_pred = refl_pred
                              , ctev_evtm = refl_evtm }
             
       ; return $ 
         SPSolved (CTyEqCan { cc_ev = given_fl
                            , cc_tyvar  = tv, cc_rhs = xi', cc_depth = d }) }
\end{code}


*********************************************************************************
*                                                                               * 
                       The interact-with-inert Stage
*                                                                               *
*********************************************************************************

Note [

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
-- Interaction result of  WorkItem <~> Ct

data InteractResult 
    = IRWorkItemConsumed { ir_fire :: String } 
    | IRInertConsumed    { ir_fire :: String } 
    | IRKeepGoing        { ir_fire :: String }

irWorkItemConsumed :: String -> TcS InteractResult
irWorkItemConsumed str = return (IRWorkItemConsumed str) 

irInertConsumed :: String -> TcS InteractResult
irInertConsumed str = return (IRInertConsumed str) 

irKeepGoing :: String -> TcS InteractResult 
irKeepGoing str = return (IRKeepGoing str) 
-- You can't discard neither workitem or inert, but you must keep 
-- going. It's possible that new work is waiting in the TcS worklist. 


interactWithInertsStage :: WorkItem -> TcS StopOrContinue 
-- Precondition: if the workitem is a CTyEqCan then it will not be able to 
-- react with anything at this stage. 
interactWithInertsStage wi 
  = do { traceTcS "interactWithInerts" $ text "workitem = " <+> ppr wi
       ; rels <- extractRelevantInerts wi 
       ; traceTcS "relevant inerts are:" $ ppr rels
       ; foldlBagM interact_next (ContinueWith wi) rels }

  where interact_next Stop atomic_inert 
          = updInertSetTcS atomic_inert >> return Stop
        interact_next (ContinueWith wi) atomic_inert 
          = do { ir <- doInteractWithInert atomic_inert wi
               ; let mk_msg rule keep_doc 
                       = vcat [ text rule <+> keep_doc
                              , ptext (sLit "InertItem =") <+> ppr atomic_inert
                              , ptext (sLit "WorkItem  =") <+> ppr wi ]
               ; case ir of 
                   IRWorkItemConsumed { ir_fire = rule } 
                       -> do { bumpStepCountTcS
                             ; traceFireTcS (cc_depth wi) 
                                            (mk_msg rule (text "WorkItemConsumed"))
                             ; updInertSetTcS atomic_inert
                             ; return Stop } 
                   IRInertConsumed { ir_fire = rule }
                       -> do { bumpStepCountTcS
                             ; traceFireTcS (cc_depth atomic_inert) 
                                            (mk_msg rule (text "InertItemConsumed"))
                             ; return (ContinueWith wi) }
                   IRKeepGoing {} -- Should we do a bumpStepCountTcS? No for now.
                       -> do { updInertSetTcS atomic_inert
                             ; return (ContinueWith wi) }
               }

\end{code}

\begin{code}
--------------------------------------------

doInteractWithInert :: Ct -> Ct -> TcS InteractResult
-- Identical class constraints.
doInteractWithInert
  inertItem@(CDictCan { cc_ev = fl1, cc_class = cls1, cc_tyargs = tys1 })
   workItem@(CDictCan { cc_ev = fl2, cc_class = cls2, cc_tyargs = tys2 })

  | cls1 == cls2  
  = do { let pty1 = mkClassPred cls1 tys1
             pty2 = mkClassPred cls2 tys2
             inert_pred_loc     = (pty1, pprFlavorArising fl1)
             work_item_pred_loc = (pty2, pprFlavorArising fl2)

       ; traceTcS "doInteractWithInert" (vcat [ text "inertItem = " <+> ppr inertItem
                                              , text "workItem  = " <+> ppr workItem ])
 
       ; let fd_eqns = improveFromAnother inert_pred_loc work_item_pred_loc
       ; any_fundeps <- rewriteWithFunDeps fd_eqns tys2 fl2
                -- We don't really rewrite tys2, see below _rewritten_tys2, so that's ok
                -- NB: We do create FDs for given to report insoluble equations that arise
                -- from pairs of Givens, and also because of floating when we approximate
                -- implications. The relevant test is: typecheck/should_fail/FDsFromGivens.hs
                -- Also see Note [When improvement happens]
                -- 
       
       ; case any_fundeps of
           -- No Functional Dependencies 
           Nothing             
               | eqTypes tys1 tys2 -> solveOneFromTheOther "Cls/Cls" fl1 workItem
               | otherwise         -> irKeepGoing "NOP"

           -- Actual Functional Dependencies
           Just (_rewritten_tys2, fd_work)
              -- Standard thing: create derived fds and keep on going. Importantly we don't
               -- throw workitem back in the worklist because this can cause loops. See #5236.
               -> do { emitFDWorkAsDerived fd_work (cc_depth workItem)
                     ; irKeepGoing "Cls/Cls (new fundeps)" } -- Just keep going without droping the inert 
       }
 
-- Two pieces of irreducible evidence: if their types are *exactly identical* 
-- we can rewrite them. We can never improve using this: 
-- if we want ty1 :: Constraint and have ty2 :: Constraint it clearly does not 
-- mean that (ty1 ~ ty2)
doInteractWithInert (CIrredEvCan { cc_ev = ifl, cc_ty = ty1 })
           workItem@(CIrredEvCan { cc_ty = ty2 })
  | ty1 `eqType` ty2
  = solveOneFromTheOther "Irred/Irred" ifl workItem

doInteractWithInert ii@(CFunEqCan { cc_ev = fl1, cc_fun = tc1
                                  , cc_tyargs = args1, cc_rhs = xi1, cc_depth = d1 }) 
                    wi@(CFunEqCan { cc_ev = fl2, cc_fun = tc2
                                  , cc_tyargs = args2, cc_rhs = xi2, cc_depth = d2 })
  | fl1 `canSolve` fl2 && lhss_match
  = do { traceTcS "interact with inerts: FunEq/FunEq" $ 
         vcat [ text "workItem =" <+> ppr wi
              , text "inertItem=" <+> ppr ii ]

       ; let xev = XEvTerm xcomp xdecomp
             -- xcomp : [(xi2 ~ xi1)] -> (F args ~ xi2) 
             xcomp [x] = EvCoercion (co1 `mkTcTransCo` mk_sym_co x)
             xcomp _   = panic "No more goals!"
             -- xdecomp : (F args ~ xi2) -> [(xi2 ~ xi1)]                 
             xdecomp x = [EvCoercion (mk_sym_co x `mkTcTransCo` co1)]

       ; ctevs <- xCtFlavor fl2 [mkTcEqPred xi2 xi1] xev
                         -- See Note [Cache-caused loops]
                         -- Why not (mkTcEqPred xi1 xi2)? See Note [Efficient orientation]
       ; add_to_work d2 ctevs 
       ; irWorkItemConsumed "FunEq/FunEq" }

  | fl2 `canSolve` fl1 && lhss_match
  = do { traceTcS "interact with inerts: FunEq/FunEq" $ 
         vcat [ text "workItem =" <+> ppr wi
              , text "inertItem=" <+> ppr ii ]

       ; let xev = XEvTerm xcomp xdecomp
              -- xcomp : [(xi2 ~ xi1)] -> [(F args ~ xi1)]
             xcomp [x] = EvCoercion (co2 `mkTcTransCo` evTermCoercion x)
             xcomp _ = panic "No more goals!"
             -- xdecomp : (F args ~ xi1) -> [(xi2 ~ xi1)]
             xdecomp x = [EvCoercion (mkTcSymCo co2 `mkTcTransCo` evTermCoercion x)]

       ; ctevs <- xCtFlavor fl1 [mkTcEqPred xi2 xi1] xev 
                          -- See Note [Cache-caused loops]
                          -- Why not (mkTcEqPred xi1 xi2)? See Note [Efficient orientation]

       ; add_to_work d1 ctevs 
       ; irInertConsumed "FunEq/FunEq"}
  where
    add_to_work d [ctev] = updWorkListTcS $ extendWorkListEq $
                           CNonCanonical {cc_ev = ctev, cc_depth = d}
    add_to_work _ _ = return ()

    lhss_match = tc1 == tc2 && eqTypes args1 args2 
    co1 = evTermCoercion $ ctEvTerm fl1
    co2 = evTermCoercion $ ctEvTerm fl2
    mk_sym_co x = mkTcSymCo (evTermCoercion x)
    
doInteractWithInert _ _ = irKeepGoing "NOP"

\end{code}


Note [Shadowing of Implicit Parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider the following example:

f :: (?x :: Char) => Char
f = let ?x = 'a' in ?x

The "let ?x = ..." generates an implication constraint of the form:

?x :: Char => ?x :: Char

Furthermore, the signature for `f` also generates an implication
constraint, so we end up with the following nested implication:

?x :: Char => (?x :: Char => ?x :: Char)

Note that the wanted (?x :: Char) constraint may be solved in
two incompatible ways:  either by using the parameter from the
signature, or by using the local definition.  Our intention is
that the local definition should "shadow" the parameter of the
signature, and we implement this as follows: when we add a new
given implicit parameter to the inert set, it replaces any existing
givens for the same implicit parameter.

This works for the normal cases but it has an odd side effect
in some pathological programs like this:

-- This is accepted, the second parameter shadows
f1 :: (?x :: Int, ?x :: Char) => Char
f1 = ?x

-- This is rejected, the second parameter shadows
f2 :: (?x :: Int, ?x :: Char) => Int
f2 = ?x

Both of these are actually wrong:  when we try to use either one,
we'll get two incompatible wnated constraints (?x :: Int, ?x :: Char),
which would lead to an error.

I can think of two ways to fix this:

  1. Simply disallow multiple constratits for the same implicit
    parameter---this is never useful, and it can be detected completely
    syntactically.

  2. Move the shadowing machinery to the location where we nest
     implications, and add some code here that will produce an
     error if we get multiple givens for the same implicit parameter.









Note [Cache-caused loops]
~~~~~~~~~~~~~~~~~~~~~~~~~
It is very dangerous to cache a rewritten wanted family equation as 'solved' in our 
solved cache (which is the default behaviour or xCtFlavor), because the interaction 
may not be contributing towards a solution. Here is an example:

Initial inert set:
  [W] g1 : F a ~ beta1
Work item:
  [W] g2 : F a ~ beta2
The work item will react with the inert yielding the _same_ inert set plus:
    i)   Will set g2 := g1 `cast` g3   
    ii)  Will add to our solved cache that [S] g2 : F a ~ beta2
    iii) Will emit [W] g3 : beta1 ~ beta2 
Now, the g3 work item will be spontaneously solved to [G] g3 : beta1 ~ beta2
and then it will react the item in the inert ([W] g1 : F a ~ beta1). So it 
will set 
      g1 := g ; sym g3 
and what is g? Well it would ideally be a new goal of type (F a ~ beta2) but
remember that we have this in our solved cache, and it is ... g2! In short we 
created the evidence loop:

        g2 := g1 ; g3 
        g3 := refl
        g1 := g2 ; sym g3 

To avoid this situation we do not cache as solved any workitems (or inert) 
which did not really made a 'step' towards proving some goal. Solved's are 
just an optimization so we don't lose anything in terms of completeness of 
solving.

\begin{code}

solveOneFromTheOther :: String    -- Info 
                     -> CtEvidence  -- Inert 
                     -> Ct        -- WorkItem 
                     -> TcS InteractResult
-- Preconditions: 
-- 1) inert and work item represent evidence for the /same/ predicate
-- 2) ip/class/irred evidence (no coercions) only
solveOneFromTheOther info ifl workItem
  | isDerived wfl
  = irWorkItemConsumed ("Solved[DW] " ++ info)

  | isDerived ifl -- The inert item is Derived, we can just throw it away, 
    	      	  -- The workItem is inert wrt earlier inert-set items, 
		  -- so it's safe to continue on from this point
  = irInertConsumed ("Solved[DI] " ++ info)
  
  | otherwise
  = ASSERT( ifl `canSolve` wfl )
      -- Because of Note [The Solver Invariant], plus Derived dealt with
    do { case wfl of
           Wanted { ctev_evar = ev_id } -> setEvBind ev_id (ctEvTerm ifl)
           _                            -> return ()
           -- Overwrite the binding, if one exists
	   -- If both are Given, we already have evidence; no need to duplicate
       ; irWorkItemConsumed ("Solved " ++ info) }
  where 
     wfl = cc_ev workItem
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


%************************************************************************
%*                                                                      *
%*          Functional dependencies, instantiation of equations
%*                                                                      *
%************************************************************************

When we spot an equality arising from a functional dependency,
we now use that equality (a "wanted") to rewrite the work-item
constraint right away.  This avoids two dangers

 Danger 1: If we send the original constraint on down the pipeline
           it may react with an instance declaration, and in delicate
	   situations (when a Given overlaps with an instance) that
	   may produce new insoluble goals: see Trac #4952

 Danger 2: If we don't rewrite the constraint, it may re-react
           with the same thing later, and produce the same equality
           again --> termination worries.

To achieve this required some refactoring of FunDeps.lhs (nicer
now!).  

\begin{code}
rewriteWithFunDeps :: [Equation]
                   -> [Xi] 
                   -> CtEvidence
                   -> TcS (Maybe ([Xi], [CtEvidence])) 
                                           -- Not quite a WantedEvVar unfortunately
                                           -- Because our intention could be to make 
                                           -- it derived at the end of the day
-- NB: The flavor of the returned EvVars will be decided by the caller
-- Post: returns no trivial equalities (identities) and all EvVars returned are fresh
rewriteWithFunDeps eqn_pred_locs xis fl
 = do { fd_ev_poss <- mapM (instFunDepEqn wloc) eqn_pred_locs
      ; let fd_ev_pos :: [(Int,CtEvidence)]
            fd_ev_pos = concat fd_ev_poss
            rewritten_xis = rewriteDictParams fd_ev_pos xis
      ; if null fd_ev_pos then return Nothing
        else return (Just (rewritten_xis, map snd fd_ev_pos)) }
 where wloc | Given { ctev_gloc = gl } <- fl
            = setCtLocOrigin gl FunDepOrigin
            | otherwise
            = ctev_wloc fl

instFunDepEqn :: WantedLoc -> Equation -> TcS [(Int,CtEvidence)]
-- Post: Returns the position index as well as the corresponding FunDep equality
instFunDepEqn wl (FDEqn { fd_qtvs = tvs, fd_eqs = eqs
                        , fd_pred1 = d1, fd_pred2 = d2 })
  = do { (subst, _) <- instFlexiTcS tvs  -- Takes account of kind substitution
       ; foldM (do_one subst) [] eqs }
  where 
    do_one subst ievs (FDEq { fd_pos = i, fd_ty_left = ty1, fd_ty_right = ty2 })
       = let sty1 = Type.substTy subst ty1 
             sty2 = Type.substTy subst ty2 
         in if eqType sty1 sty2 then return ievs -- Return no trivial equalities
            else do { mb_eqv <- newDerived (push_ctx wl) (mkTcEqPred sty1 sty2)
                    ; case mb_eqv of
                         Just ctev -> return $ (i,ctev):ievs
                         Nothing   -> return ievs }
                           -- We are eventually going to emit FD work back in the work list so 
                           -- it is important that we only return the /freshly created/ and not 
                           -- some existing equality!
    push_ctx :: WantedLoc -> WantedLoc 
    push_ctx loc = pushErrCtxt FunDepOrigin (False, mkEqnMsg d1 d2) loc


mkEqnMsg :: (TcPredType, SDoc) 
         -> (TcPredType, SDoc) -> TidyEnv -> TcM (TidyEnv, SDoc)
mkEqnMsg (pred1,from1) (pred2,from2) tidy_env
  = do  { zpred1 <- zonkTcPredType pred1
        ; zpred2 <- zonkTcPredType pred2
	; let { tpred1 = tidyType tidy_env zpred1
              ; tpred2 = tidyType tidy_env zpred2 }
	; let msg = vcat [ptext (sLit "When using functional dependencies to combine"),
			  nest 2 (sep [ppr tpred1 <> comma, nest 2 from1]), 
			  nest 2 (sep [ppr tpred2 <> comma, nest 2 from2])]
	; return (tidy_env, msg) }

rewriteDictParams :: [(Int,CtEvidence)] -- A set of coercions : (pos, ty' ~ ty)
                  -> [Type]             -- A sequence of types: tys
                  -> [Type]                   
rewriteDictParams param_eqs tys
  = zipWith do_one tys [0..]
  where
    do_one :: Type -> Int -> Type
    do_one ty n = case lookup n param_eqs of
                    Just wev -> get_fst_ty wev
                    Nothing  -> ty

    get_fst_ty ctev
      | Just (ty1, _) <- getEqPredTys_maybe (ctEvPred ctev)
      = ty1
      | otherwise 
      = panic "rewriteDictParams: non equality fundep!?"

        
emitFDWorkAsDerived :: [CtEvidence]   -- All Derived
                    -> SubGoalDepth -> TcS () 
emitFDWorkAsDerived evlocs d 
  = updWorkListTcS $ appendWorkListEqs (map mk_fd_ct evlocs)
  where 
    mk_fd_ct der_ev = CNonCanonical { cc_ev = der_ev, cc_depth = d }
\end{code}




*********************************************************************************
*                                                                               * 
                       The top-reaction Stage
*                                                                               *
*********************************************************************************

\begin{code}

topReactionsStage :: SimplifierStage 
topReactionsStage workItem 
 = tryTopReact workItem 
   

tryTopReact :: WorkItem -> TcS StopOrContinue
tryTopReact wi 
 = do { inerts <- getTcSInerts
      ; tir <- doTopReact inerts wi
      ; case tir of 
          NoTopInt -> return (ContinueWith wi)
          SomeTopInt rule what_next 
                   -> do { bumpStepCountTcS 
                         ; traceFireTcS (cc_depth wi) $
                           vcat [ ptext (sLit "Top react:") <+> text rule
                                , text "WorkItem =" <+> ppr wi ]
                         ; return what_next } }

data TopInteractResult 
 = NoTopInt
 | SomeTopInt { tir_rule :: String, tir_new_item :: StopOrContinue }


doTopReact :: InertSet -> WorkItem -> TcS TopInteractResult
-- The work item does not react with the inert set, so try interaction with top-level 
-- instances. Note:
--
--   (a) The place to add superclasses in not here in doTopReact stage. 
--       Instead superclasses are added in the worklist as part of the
--       canonicalization process. See Note [Adding superclasses].
--
--   (b) See Note [Given constraint that matches an instance declaration] 
--       for some design decisions for given dictionaries. 

doTopReact inerts workItem
  = do { traceTcS "doTopReact" (ppr workItem)
       ; case workItem of
      	   CDictCan { cc_ev = fl, cc_class = cls, cc_tyargs = xis
      	            , cc_depth = d }
      	      -> doTopReactDict inerts workItem fl cls xis d

      	   CFunEqCan { cc_ev = fl, cc_fun = tc, cc_tyargs = args
      	             , cc_rhs = xi, cc_depth = d }
      	      -> doTopReactFunEq fl tc args xi d

      	   _  -> -- Any other work item does not react with any top-level equations
      	         return NoTopInt  }

--------------------
doTopReactDict :: InertSet -> WorkItem -> CtEvidence -> Class -> [Xi]
               -> SubGoalDepth -> TcS TopInteractResult
doTopReactDict inerts workItem fl cls xis depth
  = do { instEnvs <- getInstEnvs 
       ; let fd_eqns = improveFromInstEnv instEnvs 
                           (mkClassPred cls xis, arising_sdoc)
             
       ; m <- rewriteWithFunDeps fd_eqns xis fl
       ; case m of
           Just (_xis',fd_work) ->
               do { emitFDWorkAsDerived fd_work depth
                  ; return SomeTopInt { tir_rule = "Dict/Top (fundeps)"
                                      , tir_new_item = ContinueWith workItem } }
           Nothing 
             | isWanted fl 
             -> do { lkup_inst_res  <- matchClassInst inerts cls xis (getWantedLoc fl)
                   ; case lkup_inst_res of
                       GenInst wtvs ev_term -> do { addSolvedDict fl 
                                                  ; doSolveFromInstance wtvs ev_term }
                       NoInstance -> return NoTopInt }
             | otherwise
             -> return NoTopInt }
   where 
     arising_sdoc
       | isGiven fl = pprArisingAt $ getGivenLoc fl
       | otherwise  = pprArisingAt $ getWantedLoc fl
     
     dict_id = ctEvId fl
     
     doSolveFromInstance :: [CtEvidence] -> EvTerm -> TcS TopInteractResult
      -- Precondition: evidence term matches the predicate workItem
     doSolveFromInstance evs ev_term 
        | null evs
        = do { traceTcS "doTopReact/found nullary instance for" $
               ppr dict_id
             ; setEvBind dict_id ev_term
             ; return $ 
               SomeTopInt { tir_rule = "Dict/Top (solved, no new work)" 
                          , tir_new_item = Stop } }
        | otherwise 
        = do { traceTcS "doTopReact/found non-nullary instance for" $ 
               ppr dict_id
             ; setEvBind dict_id ev_term
             ; let mk_new_wanted ev
                       = CNonCanonical { cc_ev   = ev
                                       , cc_depth = depth + 1 }
             ; updWorkListTcS (appendWorkListCt (map mk_new_wanted evs))
             ; return $
               SomeTopInt { tir_rule     = "Dict/Top (solved, more work)"
                          , tir_new_item = Stop } }

--------------------
doTopReactFunEq :: CtEvidence -> TyCon -> [Xi] -> Xi
                -> SubGoalDepth -> TcS TopInteractResult
doTopReactFunEq fl tc args xi d
  = ASSERT (isSynFamilyTyCon tc) -- No associated data families have 
                                 -- reached that far 

    -- First look in the cache of solved funeqs
    do { fun_eq_cache <- getTcSInerts >>= (return . inert_solved_funeqs)
       ; case lookupFamHead fun_eq_cache (mkTyConApp tc args) of {
            Just ctev -> ASSERT( not (isDerived ctev) )
                         ASSERT( isEqPred (ctEvPred ctev) )
                         succeed_with (evTermCoercion (ctEvTerm ctev)) 
                                      (snd (getEqPredTys (ctEvPred ctev))) ;
            Nothing -> 

    -- No cached solved, so look up in top-level instances
    do { match_res <- matchFam tc args   -- See Note [MATCHING-SYNONYMS]
       ; case match_res of {
           Nothing -> return NoTopInt ;
           Just (famInst, rep_tys) -> 

    -- Found a top-level instance
    do {    -- Add it to the solved goals
         unless (isDerived fl) $
         do { addSolvedFunEq fl }

       ; let coe_ax = famInstAxiom famInst 
       ; succeed_with (mkTcAxInstCo coe_ax rep_tys)
                      (mkAxInstRHS coe_ax rep_tys) } } } } }
  where
    succeed_with :: TcCoercion -> TcType -> TcS TopInteractResult
    succeed_with coe rhs_ty 
      = do { ctevs <- xCtFlavor fl [mkTcEqPred rhs_ty xi] xev
           ; traceTcS ("doTopReactFunEq ") (ppr ctevs)
           ; case ctevs of
               [ctev] -> updWorkListTcS $ extendWorkListEq $
                         CNonCanonical { cc_ev = ctev
                                       , cc_depth  = d+1 }
               ctevs -> -- No subgoal (because it's cached)
                        ASSERT( null ctevs) return () 
           ; return $ SomeTopInt { tir_rule = "Fun/Top"
                                 , tir_new_item = Stop } }
      where
        xdecomp x = [EvCoercion (mkTcSymCo coe `mkTcTransCo` evTermCoercion x)]
        xcomp [x] = EvCoercion (coe `mkTcTransCo` evTermCoercion x)
        xcomp _   = panic "No more goals!"
        xev = XEvTerm xcomp xdecomp
\end{code}


Note [FunDep and implicit parameter reactions] 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Currently, our story of interacting two dictionaries (or a dictionary
and top-level instances) for functional dependencies, and implicit
paramters, is that we simply produce new Derived equalities.  So for example

        class D a b | a -> b where ... 
    Inert: 
        d1 :g D Int Bool
    WorkItem: 
        d2 :w D Int alpha

    We generate the extra work item
        cv :d alpha ~ Bool
    where 'cv' is currently unused.  However, this new item can perhaps be 
    spontaneously solved to become given and react with d2,
    discharging it in favour of a new constraint d2' thus:
        d2' :w D Int Bool
	d2 := d2' |> D Int cv
    Now d2' can be discharged from d1

We could be more aggressive and try to *immediately* solve the dictionary 
using those extra equalities, but that requires those equalities to carry 
evidence and derived do not carry evidence.

If that were the case with the same inert set and work item we might dischard 
d2 directly:

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
   
Note that we *do* fire the improvement if one is Given and one is Derived (e.g. a 
superclass of a Wanted goal) or if both are Given.

Example (tcfail138)
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

Test typecheck/should_fail/FDsFromGivens also shows why it's a good idea to 
emit Derived FDs for givens as well. 

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
     Note [Recursive instances and superclases] will fail to work. 

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
  | GenInst [CtEvidence] EvTerm 

matchClassInst :: InertSet -> Class -> [Type] -> WantedLoc -> TcS LookupInstResult

matchClassInst _ clas [ _, ty ] _
  | className clas == singIClassName
  , Just n <- isNumLitTy ty = return $ GenInst [] $ EvLit $ EvNum n

  | className clas == singIClassName
  , Just s <- isStrLitTy ty = return $ GenInst [] $ EvLit $ EvStr s


matchClassInst inerts clas tys loc
   = do { dflags <- getDynFlags
        ; let pred = mkClassPred clas tys 
              incoherent_ok = xopt Opt_IncoherentInstances  dflags
        ; mb_result <- matchClass clas tys
        ; untch <- getUntouchables
        ; traceTcS "matchClassInst" $ vcat [ text "pred =" <+> ppr pred
                                           , text "inerts=" <+> ppr inerts
                                           , text "untouchables=" <+> ppr untch ]
        ; case mb_result of
            MatchInstNo   -> return NoInstance
            MatchInstMany -> return NoInstance -- defer any reactions of a multitude until
                                               -- we learn more about the reagent 
            MatchInstSingle (_,_)
              | not incoherent_ok && given_overlap untch 
              -> -- see Note [Instance and Given overlap]
                 do { traceTcS "Delaying instance application" $ 
                       vcat [ text "Workitem=" <+> pprType (mkClassPred clas tys)
                            , text "Relevant given dictionaries=" <+> ppr givens_for_this_clas ]
                     ; return NoInstance
                     }

            MatchInstSingle (dfun_id, mb_inst_tys) ->
              do { checkWellStagedDFun pred dfun_id loc

                       -- mb_inst_tys :: Maybe TcType 
                       -- See Note [DFunInstType: instantiating types] in InstEnv

                 ; (tys, dfun_phi) <- instDFunType dfun_id mb_inst_tys
                 ; let (theta, _) = tcSplitPhiTy dfun_phi
                 ; if null theta then
                       return (GenInst [] (EvDFunApp dfun_id tys []))
                   else do
                     { evc_vars <- instDFunConstraints loc theta
                     ; let new_ev_vars = freshGoals evc_vars
                           -- new_ev_vars are only the real new variables that can be emitted 
                           dfun_app = EvDFunApp dfun_id tys (getEvTerms evc_vars)
                     ; return $ GenInst new_ev_vars dfun_app } }
        }
   where 
     givens_for_this_clas :: Cts
     givens_for_this_clas 
         = lookupUFM (cts_given (inert_dicts $ inert_cans inerts)) clas 
             `orElse` emptyCts

     given_overlap :: TcsUntouchables -> Bool
     given_overlap untch = anyBag (matchable untch) givens_for_this_clas

     matchable untch (CDictCan { cc_class = clas_g, cc_tyargs = sys
                               , cc_ev = fl })
       | isGiven fl
       = ASSERT( clas_g == clas )
         case tcUnifyTys (\tv -> if isTouchableMetaTyVar_InRange untch tv && 
                                    tv `elemVarSet` tyVarsOfTypes tys
                                 then BindMe else Skolem) tys sys of
       -- We can't learn anything more about any variable at this point, so the only
       -- cause of overlap can be by an instantiation of a touchable unification
       -- variable. Hence we only bind touchable unification variables. In addition,
       -- we use tcUnifyTys instead of tcMatchTys to rule out cyclic substitutions.
            Nothing -> False
            Just _  -> True
       | otherwise = False -- No overlap with a solved, already been taken care of 
                           -- by the overlap check with the instance environment.
     matchable _tys ct = pprPanic "Expecting dictionary!" (ppr ct)
\end{code}

Note [Instance and Given overlap]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Assume that we have an inert set that looks as follows:
       [Given] D [Int]
And an instance declaration: 
       instance C a => D [a]
A new wanted comes along of the form: 
       [Wanted] D [alpha]

One possibility is to apply the instance declaration which will leave us 
with an unsolvable goal (C alpha). However, later on a new constraint may 
arise (for instance due to a functional dependency between two later dictionaries), 
that will add the equality (alpha ~ Int), in which case our ([Wanted] D [alpha]) 
will be transformed to [Wanted] D [Int], which could have been discharged by the given. 

The solution is that in matchClassInst and eventually in topReact, we get back with 
a matching instance, only when there is no Given in the inerts which is unifiable to
this particular dictionary.

The end effect is that, much as we do for overlapping instances, we delay choosing a 
class instance if there is a possibility of another instance OR a given to match our 
constraint later on. This fixes bugs #4981 and #5002.

This is arguably not easy to appear in practice due to our aggressive prioritization 
of equality solving over other constraints, but it is possible. I've added a test case 
in typecheck/should-compile/GivenOverlapping.hs

We ignore the overlap problem if -XIncoherentInstances is in force: see
Trac #6002 for a worked-out example where this makes a difference.

Moreover notice that our goals here are different than the goals of the top-level 
overlapping checks. There we are interested in validating the following principle:
 
    If we inline a function f at a site where the same global instance environment
    is available as the instance environment at the definition site of f then we 
    should get the same behaviour. 

But for the Given Overlap check our goal is just related to completeness of 
constraint solving. 
