\begin{code}
module TcInteract (
     solveInteractGiven,  -- Solves [EvVar],GivenLoc
     solveInteract,       -- Solves Cts
  ) where

#include "HsVersions.h"

import BasicTypes ()
import TcCanonical
import VarSet
import Type
import Unify
import InstEnv( lookupInstEnv, instanceDFunId )
import CoAxiom(sfInteractTop, sfInteractInert)

import Var
import TcType
import PrelNames (knownNatClassName, knownSymbolClassName, ipClassNameKey )
import TysWiredIn ( coercibleClass )
import Id( idType, mkSysLocalM )
import Class
import TyCon
import DataCon
import Name
import RdrName ( GlobalRdrEnv, lookupGRE_Name, mkRdrQual, is_as,
                 is_decl, Provenance(Imported), gre_prov )
import FunDeps
import FamInstEnv ( FamInstEnvs, instNewTyConTF_maybe )

import TcEvidence
import Outputable

import TcMType ( zonkTcPredType )

import TcRnTypes
import TcErrors
import TcSMonad
import Bag

import Control.Monad ( foldM )
import Data.Maybe ( catMaybes )
import Data.List( partition )

import VarEnv

import Control.Monad( when, unless, forM )
import Pair (Pair(..))
import Unique( hasKey )
import FastString ( sLit, fsLit )
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
solveInteractGiven :: CtLoc -> [TcTyVar] -> [EvVar] -> TcS ()
-- In principle the givens can kick out some wanteds from the inert
-- resulting in solving some more wanted goals here which could emit
-- implications. That's why I return a bag of implications. Not sure
-- if this can happen in practice though.
solveInteractGiven loc fsks givens
  = do { implics <- solveInteract (fsk_bag `unionBags` given_bag)
       ; ASSERT( isEmptyBag implics )
         return () }  -- We do not decompose *given* polymorphic equalities
                      --    (forall a. t1 ~ forall a. t2)
                      -- What would the evidence look like?!
                      -- See Note [Do not decompose given polytype equalities]
                      -- in TcCanonical
  where
    given_bag = listToBag [ mkNonCanonical $ CtGiven { ctev_evtm = EvId ev_id
                                                     , ctev_pred = evVarPred ev_id
                                                     , ctev_loc = loc }
                          | ev_id <- givens ]

    fsk_bag = listToBag [ mkNonCanonical $ CtGiven { ctev_evtm = EvCoercion (mkTcReflCo Nominal tv_ty)
                                                   , ctev_pred = pred
                                                   , ctev_loc = loc }
                        | tv <- fsks
                        , let FlatSkol fam_ty = tcTyVarDetails tv
                              tv_ty = mkTyVarTy tv
                              pred  = mkTcEqPred fam_ty tv_ty
                        ]

-- The main solver loop implements Note [Basic Simplifier Plan]
---------------------------------------------------------------
solveInteract :: Cts -> TcS (Bag Implication)
-- Returns the final InertSet in TcS
-- Has no effect on work-list or residual-iplications
solveInteract cts
  = {-# SCC "solveInteract" #-}
    withWorkList cts $
    do { dyn_flags <- getDynFlags
       ; solve_loop (maxSubGoalDepth dyn_flags) }
  where
    solve_loop max_depth
      = {-# SCC "solve_loop" #-}
        do { sel <- selectNextWorkItem max_depth
           ; case sel of
              NoWorkRemaining     -- Done, successfuly (modulo frozen)
                -> return ()
              MaxDepthExceeded cnt ct -- Failure, depth exceeded
                -> wrapErrTcS $ solverDepthErrorTcS cnt (ctEvidence ct)
              NextWorkItem ct     -- More work, loop around!
                -> do { runSolverPipeline thePipeline ct; solve_loop max_depth } }

type WorkItem = Ct
type SimplifierStage = WorkItem -> TcS StopOrContinue

data SelectWorkItem
       = NoWorkRemaining      -- No more work left (effectively we're done!)
       | MaxDepthExceeded SubGoalCounter Ct
                              -- More work left to do but this constraint has exceeded
                              -- the maximum depth for one of the subgoal counters and we
                              -- must stop
       | NextWorkItem Ct      -- More work left, here's the next item to look at

selectNextWorkItem :: SubGoalDepth -- Max depth allowed
                   -> TcS SelectWorkItem
selectNextWorkItem max_depth
  = updWorkListTcS_return pick_next
  where
    pick_next :: WorkList -> (SelectWorkItem, WorkList)
    pick_next wl
      = case selectWorkItem wl of
          (Nothing,_)
              -> (NoWorkRemaining,wl)           -- No more work
          (Just ct, new_wl)
              | Just cnt <- subGoalDepthExceeded max_depth (ctLocDepth (ctLoc ct)) -- Depth exceeded
              -> (MaxDepthExceeded cnt ct,new_wl)
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

       ; bumpStepCountTcS    -- One step for each constraint processed
       ; final_res  <- run_pipeline pipeline (ContinueWith workItem)

       ; final_is <- getTcSInerts
       ; case final_res of
           Stop            -> do { traceTcS "End solver pipeline (discharged) }"
                                       (ptext (sLit "inerts    = ") <+> ppr final_is)
                                 ; return () }
           ContinueWith ct -> do { traceFireTcS ct (ptext (sLit "Kept as inert"))
                                 ; traceTcS "End solver pipeline (not discharged) }" $
                                       vcat [ ptext (sLit "final_item = ") <+> ppr ct
                                            , pprTvBndrs (varSetElems $ tyVarsOfCt ct)
                                            , ptext (sLit "inerts     = ") <+> ppr final_is]
                                 ; insertInertItemTcS ct }
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
thePipeline = [ ("canonicalization",        TcCanonical.canonicalize)
              , ("interact with inerts",    interactWithInertsStage)
              , ("top-level reactions",     topReactionsStage) ]
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
-- Interaction result of  WorkItem <~> Ct

type StopNowFlag = Bool    -- True <=> stop after this interaction

interactWithInertsStage :: WorkItem -> TcS StopOrContinue
-- Precondition: if the workitem is a CTyEqCan then it will not be able to
-- react with anything at this stage.

interactWithInertsStage wi
  = do { inerts <- getTcSInerts
       ; let ics = inert_cans inerts
       ; (mb_ics', stop) <- case wi of
             CTyEqCan    {} -> interactTyVarEq ics wi
             CFunEqCan   {} -> interactFunEq   ics wi
             CIrredEvCan {} -> interactIrred   ics wi
             CDictCan    {} -> interactDict    ics wi
             _ -> pprPanic "interactWithInerts" (ppr wi)
                -- CHoleCan are put straight into inert_frozen, so never get here
                -- CNonCanonical have been canonicalised
       ; case mb_ics' of
           Just ics' -> setTcSInerts (inerts { inert_cans = ics' })
           Nothing          -> return ()
       ; case stop of
            True  -> return Stop
            False -> return (ContinueWith wi) }
\end{code}

\begin{code}
data InteractResult = IRKeep | IRReplace | IRDelete
instance Outputable InteractResult where
  ppr IRKeep    = ptext (sLit "keep")
  ppr IRReplace = ptext (sLit "replace")
  ppr IRDelete  = ptext (sLit "delete")

solveOneFromTheOther :: CtEvidence  -- Inert
                     -> CtEvidence  -- WorkItem
                     -> TcS (InteractResult, StopNowFlag)
-- Preconditions:
-- 1) inert and work item represent evidence for the /same/ predicate
-- 2) ip/class/irred evidence (no coercions) only
solveOneFromTheOther ev_i ev_w
  | isDerived ev_w
  = return (IRKeep, True)

  | isDerived ev_i -- The inert item is Derived, we can just throw it away,
                   -- The ev_w is inert wrt earlier inert-set items,
                   -- so it's safe to continue on from this point
  = return (IRDelete, False)

  | CtWanted { ctev_evar = ev_id } <- ev_w
  = do { setEvBind ev_id (ctEvTerm ev_i)
       ; return (IRKeep, True) }

  | CtWanted { ctev_evar = ev_id } <- ev_i
  = do { setEvBind ev_id (ctEvTerm ev_w)
       ; return (IRReplace, True) }

  | otherwise      -- If both are Given, we already have evidence; no need to duplicate
                   -- But the work item *overrides* the inert item (hence IRReplace)
                   -- See Note [Shadowing of Implicit Parameters]
  = return (IRReplace, True)
\end{code}

*********************************************************************************
*                                                                               *
                   interactIrred
*                                                                               *
*********************************************************************************

\begin{code}
-- Two pieces of irreducible evidence: if their types are *exactly identical*
-- we can rewrite them. We can never improve using this:
-- if we want ty1 :: Constraint and have ty2 :: Constraint it clearly does not
-- mean that (ty1 ~ ty2)
interactIrred :: InertCans -> Ct -> TcS (Maybe InertCans, StopNowFlag)

interactIrred inerts workItem@(CIrredEvCan { cc_ev = ev_w })
  | let pred = ctEvPred ev_w
        (matching_irreds, others) = partitionBag (\ct -> ctPred ct `tcEqType` pred)
                                                 (inert_irreds inerts)
  , (ct_i : rest) <- bagToList matching_irreds
  , let ctev_i = ctEvidence ct_i
  = ASSERT( null rest )
    do { (inert_effect, stop_now) <- solveOneFromTheOther ctev_i ev_w
       ; let inerts' = case inert_effect of
                          IRKeep    -> Nothing
                          IRDelete  -> Just (inerts { inert_irreds = others })
                          IRReplace -> Just (inerts { inert_irreds = extendCts others workItem })
       ; when stop_now $ traceFireTcS workItem $
         ptext (sLit "Irred equal") <+> parens (ppr inert_effect)
       ; return (inerts', stop_now) }

  | otherwise
  = return (Nothing, False)

interactIrred _ wi = pprPanic "interactIrred" (ppr wi)
\end{code}

*********************************************************************************
*                                                                               *
                   interactDict
*                                                                               *
*********************************************************************************

\begin{code}
interactDict :: InertCans -> Ct -> TcS (Maybe InertCans, StopNowFlag)
interactDict inerts workItem@(CDictCan { cc_ev = ev_w, cc_class = cls, cc_tyargs = tys })
  | let dicts = inert_dicts inerts
  , Just ct_i <- findDict (inert_dicts inerts) cls tys
  , let ctev_i = ctEvidence ct_i
  = do { (inert_effect, stop_now) <- solveOneFromTheOther ctev_i ev_w
       ; let inerts' = case inert_effect of
                          IRKeep    -> Nothing
                          IRDelete  -> Just (inerts { inert_dicts = delDict dicts cls tys })
                          IRReplace -> Just (inerts { inert_dicts = addDict dicts cls tys workItem })
       ; when stop_now $ traceFireTcS workItem $
         ptext (sLit "Dict equal") <+> parens (ppr inert_effect)
       ; return (inerts', stop_now) }

  | cls `hasKey` ipClassNameKey
  , isGiven ev_w
  = interactGivenIP inerts workItem

  | otherwise
  = do { mapBagM_ (addFunDepWork workItem) (findDictsByClass (inert_dicts inerts) cls)
               -- Standard thing: create derived fds and keep on going. Importantly we don't
               -- throw workitem back in the worklist because this can cause loops (see #5236)
       ; return (Nothing, False) }

interactDict _ wi = pprPanic "interactDict" (ppr wi)

interactGivenIP :: InertCans -> Ct -> TcS (Maybe InertCans, StopNowFlag)
-- Work item is Given (?x:ty)
-- See Note [Shadowing of Implicit Parameters]
interactGivenIP inerts workItem@(CDictCan { cc_class = cls, cc_tyargs = tys@(ip_str:_) })
  = do { traceFireTcS workItem $ ptext (sLit "Given IP")
       ; return (Just (inerts { inert_dicts = addDict filtered_dicts cls tys workItem }), True) }
  where
    dicts           = inert_dicts inerts
    ip_dicts        = findDictsByClass dicts cls
    other_ip_dicts  = filterBag (not . is_this_ip) ip_dicts
    filtered_dicts  = addDictsByClass dicts cls other_ip_dicts

    -- Pick out any Given constraints for the same implicit parameter
    is_this_ip (CDictCan { cc_ev = ev, cc_tyargs = ip_str':_ })
       = isGiven ev && ip_str `tcEqType` ip_str'
    is_this_ip _ = False

interactGivenIP _ wi = pprPanic "interactGivenIP" (ppr wi)

addFunDepWork :: Ct -> Ct -> TcS ()
addFunDepWork work_ct inert_ct
  = do { let work_loc           = ctLoc work_ct
             inert_loc          = ctLoc inert_ct
             inert_pred_loc     = (ctPred inert_ct, pprArisingAt inert_loc)
             work_item_pred_loc = (ctPred work_ct,  pprArisingAt work_loc)

       ; let fd_eqns = improveFromAnother inert_pred_loc work_item_pred_loc
       ; fd_work <- rewriteWithFunDeps fd_eqns work_loc
                -- We don't really rewrite tys2, see below _rewritten_tys2, so that's ok
                -- NB: We do create FDs for given to report insoluble equations that arise
                -- from pairs of Givens, and also because of floating when we approximate
                -- implications. The relevant test is: typecheck/should_fail/FDsFromGivens.hs
                -- Also see Note [When improvement happens]

       ; traceTcS "addFuNDepWork"
                  (vcat [ text "inertItem =" <+> ppr inert_ct
                        , text "workItem  =" <+> ppr work_ct
                        , text "fundeps =" <+> ppr fd_work ])

       ; case fd_work of
           [] -> return ()
           _  -> updWorkListTcS (extendWorkListEqs fd_work)    }
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
*given* implicit parameter to the inert set, it replaces any existing
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


*********************************************************************************
*                                                                               *
                   interactFunEq
*                                                                               *
*********************************************************************************

\begin{code}
interactFunEq :: InertCans -> Ct -> TcS (Maybe InertCans, StopNowFlag)
interactFunEq inerts workItem@(CFunEqCan { cc_ev = ev, cc_fun = tc
                                         , cc_tyargs = args, cc_rhs = rhs })
  | (CFunEqCan { cc_ev = ev_i, cc_rhs = rhs_i } : _) <- matching_inerts
  , ev_i `canRewrite` ev
  = do { traceTcS "interact with inerts: FunEq/FunEq" $
         vcat [ text "workItem =" <+> ppr workItem
              , text "inertItem=" <+> ppr ev_i ]
       ; solveFunEq ev_i rhs_i ev rhs
       ; return (Nothing, True) }

  | (ev_i : _) <- [ ev_i | CFunEqCan { cc_ev = ev_i, cc_rhs = rhs_i } <- matching_inerts
                         , rhs_i `tcEqType` rhs    -- Duplicates
                         , ev_i `canRewriteOrSame` ev ]
  = do { when (isWanted ev) (setEvBind (ctev_evar ev) (ctEvTerm ev_i))
       ; return (Nothing, True) }

  | eq_is@(eq_i : _) <- matching_inerts
  , ev `canRewrite` ctEvidence eq_i   -- This is unusual
  = do { let solve (CFunEqCan { cc_ev = ev_i, cc_rhs = rhs_i })
                      = solveFunEq ev rhs ev_i rhs_i
             solve ct = pprPanic "interactFunEq" (ppr ct)
       ; mapM_ solve eq_is
       ; return (Just (inerts { inert_funeqs = replaceFunEqs funeqs tc args workItem }), True) }

  | (CFunEqCan { cc_rhs = rhs_i } : _) <- matching_inerts
  = do { mb <- newDerived loc (mkTcEqPred rhs_i rhs)
       ; case mb of
           Just x  -> updWorkListTcS (extendWorkListEq (mkNonCanonical x))
           Nothing -> return ()
       ; return (Nothing, False) }

   | Just ops <- isBuiltInSynFamTyCon_maybe tc
   = do { let is = findFunEqsByTyCon funeqs tc
        ; traceTcS "builtInCandidates: " $ ppr is
        ; let interact = sfInteractInert ops args rhs
        ; impMbs <- sequence
                 [ do mb <- newDerived (ctev_loc iev) (mkTcEqPred lhs_ty rhs_ty)
                      case mb of
                        Just x -> return $ Just $ mkNonCanonical x
                        Nothing -> return Nothing
                 | CFunEqCan { cc_tyargs = iargs
                             , cc_rhs = ixi
                             , cc_ev = iev } <- is
                 , Pair lhs_ty rhs_ty <- interact iargs ixi
                 ]
        ; let imps = catMaybes impMbs
        ; unless (null imps) $ updWorkListTcS (extendWorkListEqs imps)
        ; return (Nothing, False) }

  | otherwise
  = return (Nothing, False)
  where
    funeqs = inert_funeqs inerts
    matching_inerts = findFunEqs funeqs tc args
    loc = ctev_loc ev

interactFunEq _ wi = pprPanic "interactFunEq" (ppr wi)


solveFunEq :: CtEvidence    -- From this  :: F tys ~ xi1
           -> Type
           -> CtEvidence    -- Solve this :: F tys ~ xi2
           -> Type
           -> TcS ()
solveFunEq from_this xi1 solve_this xi2
  = do { ctevs <- xCtFlavor solve_this [mkTcEqPred xi2 xi1] xev
             -- No caching!  See Note [Cache-caused loops]
             -- Why not (mkTcEqPred xi1 xi2)? See Note [Efficient orientation]

       ; emitWorkNC ctevs }
  where
    from_this_co = evTermCoercion $ ctEvTerm from_this

    xev = XEvTerm xcomp xdecomp

    -- xcomp : [(xi2 ~ xi1)] -> (F tys ~ xi2)
    xcomp [x] = EvCoercion (from_this_co `mkTcTransCo` mk_sym_co x)
    xcomp _   = panic "No more goals!"

    -- xdecomp : (F tys ~ xi2) -> [(xi2 ~ xi1)]
    xdecomp x = [EvCoercion (mk_sym_co x `mkTcTransCo` from_this_co)]

    mk_sym_co x = mkTcSymCo (evTermCoercion x)
\end{code}

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


Note [Efficient Orientation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we are interacting two FunEqCans with the same LHS:
          (inert)  ci :: (F ty ~ xi_i)
          (work)   cw :: (F ty ~ xi_w)
We prefer to keep the inert (else we pass the work item on down
the pipeline, which is a bit silly).  If we keep the inert, we
will (a) discharge 'cw'
     (b) produce a new equality work-item (xi_w ~ xi_i)
Notice the orientation (xi_w ~ xi_i) NOT (xi_i ~ xi_w):
    new_work :: xi_w ~ xi_i
    cw := ci ; sym new_work
Why?  Consider the simplest case when xi1 is a type variable.  If
we generate xi1~xi2, porcessing that constraint will kick out 'ci'.
If we generate xi2~xi1, there is less chance of that happening.
Of course it can and should still happen if xi1=a, xi1=Int, say.
But we want to avoid it happening needlessly.

Similarly, if we *can't* keep the inert item (because inert is Wanted,
and work is Given, say), we prefer to orient the new equality (xi_i ~
xi_w).

Note [Carefully solve the right CFunEqCan]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ---- OLD COMMENT, NOW NOT NEEDED
   ---- becuase we now allow multiple
   ---- wanted FunEqs with the same head
Consider the constraints
  c1 :: F Int ~ a      -- Arising from an application line 5
  c2 :: F Int ~ Bool   -- Arising from an application line 10
Suppose that 'a' is a unification variable, arising only from
flattening.  So there is no error on line 5; it's just a flattening
variable.  But there is (or might be) an error on line 10.

Two ways to combine them, leaving either (Plan A)
  c1 :: F Int ~ a      -- Arising from an application line 5
  c3 :: a ~ Bool       -- Arising from an application line 10
or (Plan B)
  c2 :: F Int ~ Bool   -- Arising from an application line 10
  c4 :: a ~ Bool       -- Arising from an application line 5

Plan A will unify c3, leaving c1 :: F Int ~ Bool as an error
on the *totally innocent* line 5.  An example is test SimpleFail16
where the expected/actual message comes out backwards if we use
the wrong plan.

The second is the right thing to do.  Hence the isMetaTyVarTy
test when solving pairwise CFunEqCan.


*********************************************************************************
*                                                                               *
                   interactTyVarEq
*                                                                               *
*********************************************************************************

\begin{code}
interactTyVarEq :: InertCans -> Ct -> TcS (Maybe InertCans, StopNowFlag)
-- CTyEqCans are always consumed, returning Stop
interactTyVarEq inerts workItem@(CTyEqCan { cc_tyvar = tv, cc_rhs = rhs , cc_ev = ev })
  | (ev_i : _) <- [ ev_i | CTyEqCan { cc_ev = ev_i, cc_rhs = rhs_i }
                             <- findTyEqs (inert_eqs inerts) tv
                         , ev_i `canRewriteOrSame` ev
                         , rhs_i `tcEqType` rhs ]
  =  -- Inert:     a ~ b
     -- Work item: a ~ b
    do { when (isWanted ev) (setEvBind (ctev_evar ev) (ctEvTerm ev_i))
       ; traceFireTcS workItem (ptext (sLit "Solved from inert"))
       ; return (Nothing, True) }

  | Just tv_rhs <- getTyVar_maybe rhs
  , (ev_i : _) <- [ ev_i | CTyEqCan { cc_ev = ev_i, cc_rhs = rhs_i }
                             <- findTyEqs (inert_eqs inerts) tv_rhs
                         , ev_i `canRewriteOrSame` ev
                         , rhs_i `tcEqType` mkTyVarTy tv ]
  =  -- Inert:     a ~ b
     -- Work item: b ~ a
    do { when (isWanted ev) (setEvBind (ctev_evar ev)
                                (EvCoercion (mkTcSymCo (evTermCoercion (ctEvTerm ev_i)))))
       ; traceFireTcS workItem (ptext (sLit "Solved from inert (r)"))
       ; return (Nothing, True) }

  | otherwise
  = do { mb_solved <- trySpontaneousSolve ev tv rhs
       ; case mb_solved of
           SPCantSolve   -- Includes givens
              -> do { untch <- getUntouchables
                    ; traceTcS "Can't solve tyvar equality"
                          (vcat [ text "LHS:" <+> ppr tv <+> dcolon <+> ppr (tyVarKind tv)
                                , text "RHS:" <+> ppr rhs <+> dcolon <+> ppr (typeKind rhs)
                                , text "Untouchables =" <+> ppr untch ])
                    ; (n_kicked, inerts') <- kickOutRewritable ev tv inerts
                    ; traceFireTcS workItem $
                      ptext (sLit "Kept as inert") <+> ppr_kicked n_kicked
                    ; return (Just (addInertCan inerts' workItem), True) }


           SPSolved new_tv
              -- Post: tv ~ xi is now in TyBinds, no need to put in inerts as well
              -- see Note [Spontaneously solved in TyBinds]
              -> do { (n_kicked, inerts') <- kickOutRewritable givenFlavour new_tv inerts
                    ; traceFireTcS workItem $
                      ptext (sLit "Spontaneously solved") <+> ppr_kicked n_kicked
                    ; return (Just inerts', True) } }

interactTyVarEq _ wi = pprPanic "interactTyVarEq" (ppr wi)

givenFlavour :: CtEvidence
-- Used just to pass to kickOutRewritable
givenFlavour = CtGiven { ctev_pred = panic "givenFlavour:ev"
                       , ctev_evtm = panic "givenFlavour:tm"
                       , ctev_loc  = panic "givenFlavour:loc" }

ppr_kicked :: Int -> SDoc
ppr_kicked 0 = empty
ppr_kicked n = parens (int n <+> ptext (sLit "kicked out"))
\end{code}

Note [Spontaneously solved in TyBinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we encounter a constraint ([W] alpha ~ tau) which can be spontaneously solved,
we record the equality on the TyBinds of the TcSMonad. In the past, we used to also
add a /given/ version of the constraint ([G] alpha ~ tau) to the inert
canonicals -- and potentially kick out other equalities that mention alpha.

Then, the flattener only had to look in the inert equalities during flattening of a
type (TcCanonical.flattenTyVar).

However it is a bit silly to record these equalities /both/ in the inerts AND the
TyBinds, so we have now eliminated spontaneously solved equalities from the inerts,
and only record them in the TyBinds of the TcS monad. The flattener is now consulting
these binds /and/ the inerts for potentially unsolved or other given equalities.

\begin{code}
kickOutRewritable :: CtEvidence   -- Flavour of the equality that is
                                  -- being added to the inert set
                  -> TcTyVar      -- The new equality is tv ~ ty
                  -> InertCans
                  -> TcS (Int, InertCans)
kickOutRewritable new_ev new_tv
                  (IC { inert_eqs = tv_eqs
                      , inert_dicts  = dictmap
                      , inert_funeqs = funeqmap
                      , inert_irreds = irreds
                      , inert_insols = insols })
  = do { traceTcS "kickOutRewritable" $
            vcat [ text "tv = " <+> ppr new_tv
                 , ptext (sLit "Kicked out =") <+> ppr kicked_out]
       ; updWorkListTcS (appendWorkList kicked_out)
       ; return (workListSize kicked_out, inert_cans_in) }
  where
                -- NB: Notice that don't rewrite
                -- inert_solved_dicts, and inert_solved_funeqs
                -- optimistically. But when we lookup we have to
                -- take the subsitution into account
    inert_cans_in = IC { inert_eqs = tv_eqs_in
                       , inert_dicts = dicts_in
                       , inert_funeqs = feqs_in
                       , inert_irreds = irs_in
                       , inert_insols = insols_in }

    kicked_out = WorkList { wl_eqs    = tv_eqs_out
                          , wl_funeqs = foldrBag insertDeque emptyDeque feqs_out
                          , wl_rest   = bagToList (dicts_out `andCts` irs_out
                                                   `andCts` insols_out) }

    (tv_eqs_out,  tv_eqs_in) = foldVarEnv kick_out_eqs ([], emptyVarEnv) tv_eqs
    (feqs_out,   feqs_in)    = partitionFunEqs  kick_out_ct    funeqmap
    (dicts_out,  dicts_in)   = partitionDicts   kick_out_ct    dictmap
    (irs_out,    irs_in)     = partitionBag     kick_out_irred irreds
    (insols_out, insols_in)  = partitionBag     kick_out_ct    insols
      -- Kick out even insolubles; see Note [Kick out insolubles]

    kick_out_ct :: Ct -> Bool
    kick_out_ct ct =  new_ev `canRewrite` ctEvidence ct
                   && new_tv `elemVarSet` tyVarsOfCt ct
         -- See Note [Kicking out inert constraints]

    kick_out_irred :: Ct -> Bool
    kick_out_irred ct =  new_ev `canRewrite` ctEvidence ct
                      && new_tv `elemVarSet` closeOverKinds (tyVarsOfCt ct)
          -- See Note [Kicking out Irreds]

    kick_out_eqs :: EqualCtList -> ([Ct], TyVarEnv EqualCtList) 
                 -> ([Ct], TyVarEnv EqualCtList)
    kick_out_eqs eqs (acc_out, acc_in)
      = (eqs_out ++ acc_out, case eqs_in of
                               []      -> acc_in
                               (eq1:_) -> extendVarEnv acc_in (cc_tyvar eq1) eqs_in)
      where
        (eqs_out, eqs_in) = partition kick_out_eq eqs


    kick_out_eq :: Ct -> Bool
    kick_out_eq (CTyEqCan { cc_tyvar = tv, cc_rhs = rhs, cc_ev = ev })
      =  (new_ev `canRewrite` ev)  -- See Note [Delicate equality kick-out]
      && (new_tv `elemVarSet` kind_vars ||    -- (1)
          (not (ev `canRewrite` new_ev) &&    -- (2)
           new_tv `elemVarSet` (extendVarSet (tyVarsOfType rhs) tv)))
      where
        kind_vars = tyVarsOfType (tyVarKind tv) `unionVarSet`
                    tyVarsOfType (typeKind rhs)

    kick_out_eq other_ct = pprPanic "kick_out_eq" (ppr other_ct)
\end{code}

Note [Kicking out inert constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given a new (a -> ty) inert, we want to kick out an existing inert
constraint if
  a) the new constraint can rewrite the inert one
  b) 'a' is free in the inert constraint (so that it *will*)
     rewrite it if we kick it out.

For (b) we use tyVarsOfCt, which returns the type variables /and
the kind variables/ that are directly visible in the type. Hence we
will have exposed all the rewriting we care about to make the most
precise kinds visible for matching classes etc. No need to kick out
constraints that mention type variables whose kinds contain this
variable!  (Except see Note [Kicking out Irreds].)

Note [Kicking out Irreds]
~~~~~~~~~~~~~~~~~~~~~~~~~
There is an awkward special case for Irreds.  When we have a
kind-mis-matched equality constraint (a:k1) ~ (ty:k2), we turn it into
an Irred (see Note [Equalities with incompatible kinds] in
TcCanonical). So in this case the free kind variables of k1 and k2
are not visible.  More precisely, the type looks like
   (~) k1 (a:k1) (ty:k2)
because (~) has kind forall k. k -> k -> Constraint.  So the constraint
itself is ill-kinded.  We can "see" k1 but not k2.  That's why we use
closeOverKinds to make sure we see k2.

This is not pretty. Maybe (~) should have kind 
   (~) :: forall k1 k1. k1 -> k2 -> Constraint


Note [Kick out insolubles]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have an insoluble alpha ~ [alpha], which is insoluble
because an occurs check.  And then we unify alpha := [Int].
Then we really want to rewrite the insouluble to [Int] ~ [[Int]].
Now it can be decomposed.  Otherwise we end up with a "Can't match
[Int] ~ [[Int]]" which is true, but a bit confusing because the
outer type constructors match.

Note [Delicate equality kick-out]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When adding an equality (a ~ xi), we kick out an inert type-variable
equality (b ~ phi) in two cases

(1) If the new tyvar appears in the kind vars of the LHS or RHS of
    the inert.  Example:
    Work item: [G] k ~ *
    Inert:     [W] (a:k) ~ ty
               [W] (b:*) ~ c :: k
    We must kick out those blocked inerts so that we rewrite them
    and can subsequently unify.

(2) If the new tyvar appears in the RHS of the inert
    AND the inert cannot rewrite the work item
          Work item:  [G] a ~ b
          Inert:      [W] b ~ [a]
    Now at this point the work item cannot be further rewritten by the
    inert (due to the weaker inert flavor). But we can't add the work item
    as-is because the inert set would then have a cyclic substitution,
    when rewriting a wanted type mentioning 'a'. So we must kick the inert out.

    We have to do this only if the inert *cannot* rewrite the work item;
    it it can, then the work item will have been fully rewritten by the
    inert set during canonicalisation.  So for example:
         Work item: [W] a ~ Int
         Inert:     [W] b ~ [a]
    No need to kick out the inert, beause the inert substitution is not
    necessarily idemopotent.  See Note [Non-idempotent inert substitution].

See also Note [Detailed InertCans Invariants]

\begin{code}
data SPSolveResult = SPCantSolve
                   | SPSolved TcTyVar
                     -- We solved this /unification/ variable to some type using reflexivity

-- SPCantSolve means that we can't do the unification because e.g. the variable is untouchable
-- SPSolved workItem' gives us a new *given* to go on

-- @trySpontaneousSolve wi@ solves equalities where one side is a
-- touchable unification variable.
trySpontaneousSolve :: CtEvidence -> TcTyVar -> Xi -> TcS SPSolveResult
trySpontaneousSolve gw tv1 xi
  | isGiven gw   -- See Note [Touchables and givens]
  = return SPCantSolve

  | Just tv2 <- tcGetTyVar_maybe xi
  = do { tch1 <- isTouchableMetaTyVarTcS tv1
       ; tch2 <- isTouchableMetaTyVarTcS tv2
       ; case (tch1, tch2) of
           (True,  True)  -> trySpontaneousEqTwoWay gw tv1 tv2
           (True,  False) -> trySpontaneousEqOneWay gw tv1 xi
           (False, True)  -> trySpontaneousEqOneWay gw tv2 (mkTyVarTy tv1)
           _              -> return SPCantSolve }
  | otherwise
  = do { tch1 <- isTouchableMetaTyVarTcS tv1
       ; if tch1 then trySpontaneousEqOneWay gw tv1 xi
                 else return SPCantSolve }

----------------
trySpontaneousEqOneWay :: CtEvidence -> TcTyVar -> Xi -> TcS SPSolveResult
-- tv is a MetaTyVar, not untouchable
trySpontaneousEqOneWay gw tv xi
  | not (isSigTyVar tv) || isTyVarTy xi
  , typeKind xi `tcIsSubKind` tyVarKind tv
  = solveWithIdentity gw tv xi
  | otherwise -- Still can't solve, sig tyvar and non-variable rhs
  = return SPCantSolve

----------------
trySpontaneousEqTwoWay :: CtEvidence -> TcTyVar -> TcTyVar -> TcS SPSolveResult
-- Both tyvars are *touchable* MetaTyvars so there is only a chance for kind error here

trySpontaneousEqTwoWay gw tv1 tv2
  | k1 `tcIsSubKind` k2 && nicer_to_update_tv2
  = solveWithIdentity gw tv2 (mkTyVarTy tv1)
  | k2 `tcIsSubKind` k1
  = solveWithIdentity gw tv1 (mkTyVarTy tv2)
  | otherwise
  = return SPCantSolve
  where
    k1 = tyVarKind tv1
    k2 = tyVarKind tv2
    nicer_to_update_tv2 = isSigTyVar tv1 || isSystemName (Var.varName tv2)
\end{code}

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
solveWithIdentity :: CtEvidence -> TcTyVar -> Xi -> TcS SPSolveResult
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
solveWithIdentity wd tv xi
  = do { let tv_ty = mkTyVarTy tv
       ; traceTcS "Sneaky unification:" $
                       vcat [text "Unifies:" <+> ppr tv <+> ptext (sLit ":=") <+> ppr xi,
                             text "Coercion:" <+> pprEq tv_ty xi,
                             text "Left Kind is:" <+> ppr (typeKind tv_ty),
                             text "Right Kind is:" <+> ppr (typeKind xi) ]

       ; let xi' = defaultKind xi
               -- We only instantiate kind unification variables
               -- with simple kinds like *, not OpenKind or ArgKind
               -- cf TcUnify.uUnboundKVar

       ; setWantedTyBind tv xi'
       ; let refl_evtm = EvCoercion (mkTcReflCo Nominal xi')

       ; when (isWanted wd) $
              setEvBind (ctev_evar wd) refl_evtm

       ; return (SPSolved tv) }
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
        where   d1 = dfEqD d2

*BUT* we have an inert set which gives us (no superclasses):
        d1 :_g Eq (D [])
By the instance declaration of Eq we can show the 'd2' goal if
        d3 :_w Eq (D [])
        where   d2 = dfEqList d3
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
 topReactionsStage, given case ]
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
because it lost legitimate superclass sharing, and it still didn't do the job:
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
rewriteWithFunDeps :: [Equation] -> CtLoc -> TcS [Ct]
-- NB: The returned constraints are all Derived
-- Post: returns no trivial equalities (identities) and all EvVars returned are fresh
rewriteWithFunDeps eqn_pred_locs loc
 = do { fd_cts <- mapM (instFunDepEqn loc) eqn_pred_locs
      ; return (concat fd_cts) }

instFunDepEqn :: CtLoc -> Equation -> TcS [Ct]
-- Post: Returns the position index as well as the corresponding FunDep equality
instFunDepEqn loc (FDEqn { fd_qtvs = tvs, fd_eqs = eqs
                         , fd_pred1 = d1, fd_pred2 = d2 })
  = do { (subst, _) <- instFlexiTcS tvs  -- Takes account of kind substitution
       ; foldM (do_one subst) [] eqs }
  where
    der_loc = pushErrCtxt FunDepOrigin (False, mkEqnMsg d1 d2) loc

    do_one subst ievs (FDEq { fd_ty_left = ty1, fd_ty_right = ty2 })
       | tcEqType sty1 sty2
       = return ievs -- Return no trivial equalities
       | otherwise
       = do { mb_eqv <- newDerived der_loc (mkTcEqPred sty1 sty2)
            ; case mb_eqv of
                 Just ev -> return (mkNonCanonical (ev {ctev_loc = der_loc}) : ievs)
                 Nothing -> return ievs }
                   -- We are eventually going to emit FD work back in the work list so
                   -- it is important that we only return the /freshly created/ and not
                   -- some existing equality!
       where
         sty1 = Type.substTy subst ty1
         sty2 = Type.substTy subst ty2

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
\end{code}




*********************************************************************************
*                                                                               *
                       The top-reaction Stage
*                                                                               *
*********************************************************************************

\begin{code}
topReactionsStage :: WorkItem -> TcS StopOrContinue
topReactionsStage wi
 = do { inerts <- getTcSInerts
      ; tir <- doTopReact inerts wi
      ; case tir of
          NoTopInt -> return (ContinueWith wi)
          SomeTopInt rule what_next
                   -> do { traceFireTcS wi $
                           ptext (sLit "Top react:") <+> text rule
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
           CDictCan { cc_ev = fl, cc_class = cls, cc_tyargs = xis }
              -> doTopReactDict inerts fl cls xis

           CFunEqCan { cc_ev = fl, cc_fun = tc, cc_tyargs = args , cc_rhs = xi }
              -> doTopReactFunEq workItem fl tc args xi

           _  -> -- Any other work item does not react with any top-level equations
                 return NoTopInt  }

--------------------
doTopReactDict :: InertSet -> CtEvidence -> Class -> [Xi] -> TcS TopInteractResult
-- Try to use type-class instance declarations to simplify the constraint
doTopReactDict inerts fl cls xis
  | not (isWanted fl)   -- Never use instances for Given or Derived constraints
  = try_fundeps_and_return

  | Just ev <- lookupSolvedDict inerts pred   -- Cached
  , ctEvCheckDepth (ctLocDepth (ctev_loc fl)) ev
  = do { setEvBind dict_id (ctEvTerm ev);
       ; return $ SomeTopInt { tir_rule = "Dict/Top (cached)"
                             , tir_new_item = Stop } }

  | otherwise  -- Not cached
   = do { lkup_inst_res <- matchClassInst inerts cls xis loc
         ; case lkup_inst_res of
               GenInst wtvs ev_term -> do { addSolvedDict fl
                                          ; solve_from_instance wtvs ev_term }
               NoInstance -> try_fundeps_and_return }
   where
     arising_sdoc = pprArisingAt loc
     dict_id = ctEvId fl
     pred = mkClassPred cls xis
     loc = ctev_loc fl

     solve_from_instance :: [CtEvidence] -> EvTerm -> TcS TopInteractResult
      -- Precondition: evidence term matches the predicate workItem
     solve_from_instance evs ev_term
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
                       = mkNonCanonical (ev {ctev_loc = bumpCtLocDepth CountConstraints loc })
             ; updWorkListTcS (extendWorkListCts (map mk_new_wanted evs))
             ; return $
               SomeTopInt { tir_rule     = "Dict/Top (solved, more work)"
                          , tir_new_item = Stop } }

     -- We didn't solve it; so try functional dependencies with
     -- the instance environment, and return
     -- NB: even if there *are* some functional dependencies against the
     -- instance environment, there might be a unique match, and if
     -- so we make sure we get on and solve it first. See Note [Weird fundeps]
     try_fundeps_and_return
       = do { instEnvs <- getInstEnvs
            ; let fd_eqns = improveFromInstEnv instEnvs (pred, arising_sdoc)
            ; fd_work <- rewriteWithFunDeps fd_eqns loc
            ; unless (null fd_work) (updWorkListTcS (extendWorkListEqs fd_work))
            ; return NoTopInt }

--------------------
doTopReactFunEq :: Ct -> CtEvidence -> TyCon -> [Xi] -> Xi -> TcS TopInteractResult
doTopReactFunEq _ct fl fun_tc args xi
  = ASSERT(isSynFamilyTyCon fun_tc) -- No associated data families have
                                     -- reached this far
    -- Look in the cache of solved funeqs
    do { fun_eq_cache <- getTcSInerts >>= (return . inert_solved_funeqs)
       ; case lookupFamHead fun_eq_cache fam_ty of {
           Just (ctev, rhs_ty)
             | ctev `canRewriteOrSame` fl  -- See Note [Cached solved FunEqs]
             -> ASSERT( not (isDerived ctev) )
                succeed_with "Fun/Cache" (evTermCoercion (ctEvTerm ctev)) rhs_ty ;
           _other ->

    -- Look up in top-level instances, or built-in axiom
    do { match_res <- matchFam fun_tc args   -- See Note [MATCHING-SYNONYMS]
       ; case match_res of {
           Nothing -> do { try_improvement; return NoTopInt } ;
           Just (co, ty) ->

    -- Found a top-level instance
    do {    -- Add it to the solved goals
         unless (isDerived fl) (addSolvedFunEq fam_ty fl xi)

       ; succeed_with "Fun/Top" co ty } } } } }
  where
    fam_ty = mkTyConApp fun_tc args
    loc = ctev_loc fl

    try_improvement
      | Just ops <- isBuiltInSynFamTyCon_maybe fun_tc
      = do { let eqns = sfInteractTop ops args xi
           ; impsMb <- mapM (\(Pair x y) -> newDerived loc (mkTcEqPred x y)) eqns
           ; let work = map mkNonCanonical (catMaybes impsMb)
           ; unless (null work) (updWorkListTcS (extendWorkListEqs work)) }
      | otherwise
      = return ()

    succeed_with :: String -> TcCoercion -> TcType -> TcS TopInteractResult
    succeed_with str co rhs_ty    -- co :: fun_tc args ~ rhs_ty
      = do { ctevs <- xCtFlavor fl [mkTcEqPred rhs_ty xi] xev
           ; traceTcS ("doTopReactFunEq " ++ str) (ppr ctevs)
           ; case ctevs of
               [ctev] -> updWorkListTcS $ extendWorkListEq $
                         mkNonCanonical (ctev { ctev_loc = bumpCtLocDepth CountTyFunApps loc })
               ctevs -> -- No subgoal (because it's cached)
                        ASSERT( null ctevs) return ()
           ; return $ SomeTopInt { tir_rule = str
                                 , tir_new_item = Stop } }
      where
        xdecomp x = [EvCoercion (mkTcSymCo co `mkTcTransCo` evTermCoercion x)]
        xcomp [x] = EvCoercion (co `mkTcTransCo` evTermCoercion x)
        xcomp _   = panic "No more goals!"
        xev = XEvTerm xcomp xdecomp
\end{code}

Note [Cached solved FunEqs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
When trying to solve, say (FunExpensive big-type ~ ty), it's important
to see if we have reduced (FunExpensive big-type) before, lest we
simply repeat it.  Hence the lookup in inert_solved_funeqs.  Moreover
we must use `canRewriteOrSame` becuase both uses might (say) be Wanteds,
and we *still* want to save the re-computation.

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

Note [Weird fundeps]
~~~~~~~~~~~~~~~~~~~~
Consider   class Het a b | a -> b where
              het :: m (f c) -> a -> m b

           class GHet (a :: * -> *) (b :: * -> *) | a -> b
           instance            GHet (K a) (K [a])
           instance Het a b => GHet (K a) (K b)

The two instances don't actually conflict on their fundeps,
although it's pretty strange.  So they are both accepted. Now
try   [W] GHet (K Int) (K Bool)
This triggers fudeps from both instance decls; but it also
matches a *unique* instance decl, and we should go ahead and
pick that one right now.  Otherwise, if we don't, it ends up
unsolved in the inert set and is reported as an error.

Trac #7875 is a case in point.

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

instance Outputable LookupInstResult where
  ppr NoInstance = text "NoInstance"
  ppr (GenInst ev t) = text "GenInst" <+> ppr ev <+> ppr t


matchClassInst :: InertSet -> Class -> [Type] -> CtLoc -> TcS LookupInstResult

matchClassInst _ clas [ ty ] _
  | className clas == knownNatClassName
  , Just n <- isNumLitTy ty = makeDict (EvNum n)

  | className clas == knownSymbolClassName
  , Just s <- isStrLitTy ty = makeDict (EvStr s)

  where
  {- This adds a coercion that will convert the literal into a dictionary
     of the appropriate type.  See Note [KnownNat & KnownSymbol and EvLit]
     in TcEvidence.  The coercion happens in 2 steps:

     Integer -> SNat n     -- representation of literal to singleton
     SNat n  -> KnownNat n -- singleton to dictionary

     The process is mirrored for Symbols:
     String    -> SSymbol n
     SSymbol n -> KnownSymbol n
  -}
  makeDict evLit =
    case unwrapNewTyCon_maybe (classTyCon clas) of
      Just (_,_, axDict)
        | [ meth ]   <- classMethods clas
        , Just tcRep <- tyConAppTyCon_maybe -- SNat
                      $ funResultTy         -- SNat n
                      $ dropForAlls         -- KnownNat n => SNat n
                      $ idType meth         -- forall n. KnownNat n => SNat n
        , Just (_,_,axRep) <- unwrapNewTyCon_maybe tcRep
        -> return $
           let co1 = mkTcSymCo $ mkTcUnbranchedAxInstCo Representational axRep  [ty]
               co2 = mkTcSymCo $ mkTcUnbranchedAxInstCo Representational axDict [ty]
           in GenInst [] $ mkEvCast (EvLit evLit) (mkTcTransCo co1 co2)

      _ -> panicTcS (text "Unexpected evidence for" <+> ppr (className clas)
                     $$ vcat (map (ppr . idType) (classMethods clas)))

matchClassInst _ clas [ _k, ty1, ty2 ] loc
  | clas == coercibleClass =  do
      traceTcS "matchClassInst for" $ ppr clas <+> ppr ty1 <+> ppr ty2 <+> text "at depth" <+> ppr (ctLocDepth loc)
      rdr_env <- getGlobalRdrEnvTcS
      famenv <- getFamInstEnvs
      safeMode <- safeLanguageOn `fmap` getDynFlags
      ev <- getCoercibleInst safeMode famenv rdr_env loc ty1 ty2
      traceTcS "matchClassInst returned" $ ppr ev
      return ev

matchClassInst inerts clas tys loc
   = do { dflags <- getDynFlags
        ; untch <- getUntouchables
        ; traceTcS "matchClassInst" $ vcat [ text "pred =" <+> ppr pred
                                           , text "inerts=" <+> ppr inerts
                                           , text "untouchables=" <+> ppr untch ]
        ; instEnvs <- getInstEnvs
        ; case lookupInstEnv instEnvs clas tys of
            ([], _, _)               -- Nothing matches
                -> do { traceTcS "matchClass not matching" $
                        vcat [ text "dict" <+> ppr pred ]
                      ; return NoInstance }

            ([(ispec, inst_tys)], [], _) -- A single match
                | not (xopt Opt_IncoherentInstances dflags)
                , given_overlap untch
                -> -- See Note [Instance and Given overlap]
                   do { traceTcS "Delaying instance application" $
                          vcat [ text "Workitem=" <+> pprType (mkClassPred clas tys)
                               , text "Relevant given dictionaries=" <+> ppr givens_for_this_clas ]
                      ; return NoInstance  }

                | otherwise
                -> do   { let dfun_id = instanceDFunId ispec
                        ; traceTcS "matchClass success" $
                          vcat [text "dict" <+> ppr pred,
                                text "witness" <+> ppr dfun_id
                                               <+> ppr (idType dfun_id) ]
                                  -- Record that this dfun is needed
                        ; match_one dfun_id inst_tys }

            (matches, _, _)    -- More than one matches
                               -- Defer any reactions of a multitude
                               -- until we learn more about the reagent
                -> do   { traceTcS "matchClass multiple matches, deferring choice" $
                          vcat [text "dict" <+> ppr pred,
                                text "matches" <+> ppr matches]
                        ; return NoInstance } }
   where
     pred = mkClassPred clas tys

     match_one :: DFunId -> [Maybe TcType] -> TcS LookupInstResult
                  -- See Note [DFunInstType: instantiating types] in InstEnv
     match_one dfun_id mb_inst_tys
       = do { checkWellStagedDFun pred dfun_id loc
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

     givens_for_this_clas :: Cts
     givens_for_this_clas
         = filterBag isGivenCt (findDictsByClass (inert_dicts $ inert_cans inerts) clas)

     given_overlap :: Untouchables -> Bool
     given_overlap untch = anyBag (matchable untch) givens_for_this_clas

     matchable untch (CDictCan { cc_class = clas_g, cc_tyargs = sys
                               , cc_ev = fl })
       | isGiven fl
       = ASSERT( clas_g == clas )
         case tcUnifyTys (\tv -> if isTouchableMetaTyVar untch tv &&
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

-- See Note [Coercible Instances]
-- Changes to this logic should likely be reflected in coercible_msg in TcErrors.
getCoercibleInst :: Bool -> FamInstEnvs -> GlobalRdrEnv -> CtLoc -> TcType -> TcType -> TcS LookupInstResult
getCoercibleInst safeMode famenv rdr_env loc ty1 ty2
  | ty1 `tcEqType` ty2
  = do return $ GenInst []
              $ EvCoercion (TcRefl Representational ty1)
  | Just (tc1,tyArgs1) <- splitTyConApp_maybe ty1,
    Just (tc2,tyArgs2) <- splitTyConApp_maybe ty2,
    tc1 == tc2,
    nominalArgsAgree tc1 tyArgs1 tyArgs2,
    not safeMode || all (dataConsInScope rdr_env) (tyConsOfTyCon tc1)
  = do -- Mark all used data constructors as used
       when safeMode $ mapM_ (markDataConsAsUsed rdr_env) (tyConsOfTyCon tc1)
       -- We want evidence for all type arguments of role R
       arg_stuff <- forM (zip3 (tyConRoles tc1) tyArgs1 tyArgs2) $ \(r,ta1,ta2) ->
         case r of Nominal -> do
                        return
                          ( Nothing
                          , Nothing
                          , mkTcReflCo Nominal ta1 {- == ta2, due to nominalArgsAgree -}
                          )
                   Representational -> do
                        ct_ev <- requestCoercible loc ta1 ta2
                        local_var <- mkSysLocalM (fsLit "coev") $ mkCoerciblePred ta1 ta2
                        return
                          ( freshGoal ct_ev
                          , Just (EvBind local_var (getEvTerm ct_ev))
                          , mkTcCoVarCo local_var
                          )
                   Phantom -> do
                        return
                          ( Nothing
                          , Nothing
                          , TcPhantomCo ta1 ta2)
       let (arg_new, arg_binds, arg_cos) = unzip3 arg_stuff
           binds = EvBinds (listToBag (catMaybes arg_binds))
           tcCo = TcLetCo binds (mkTcTyConAppCo Representational tc1 arg_cos)
       return $ GenInst (catMaybes arg_new) (EvCoercion tcCo)

  | Just (tc,tyArgs) <- splitTyConApp_maybe ty1,
    Just (concTy, ntCo) <- instNewTyConTF_maybe famenv tc tyArgs,
    dataConsInScope rdr_env tc -- Do noot look at all tyConsOfTyCon
  = do markDataConsAsUsed rdr_env tc
       ct_ev <- requestCoercible loc concTy ty2
       local_var <- mkSysLocalM (fsLit "coev") $ mkCoerciblePred concTy ty2
       let binds = EvBinds (unitBag (EvBind local_var (getEvTerm ct_ev)))
           tcCo = TcLetCo binds $
                          coercionToTcCoercion ntCo `mkTcTransCo` mkTcCoVarCo local_var
       return $ GenInst (freshGoals [ct_ev]) (EvCoercion tcCo)

  | Just (tc,tyArgs) <- splitTyConApp_maybe ty2,
    Just (concTy, ntCo) <- instNewTyConTF_maybe famenv tc tyArgs,
    dataConsInScope rdr_env tc -- Do noot look at all tyConsOfTyCon
  = do markDataConsAsUsed rdr_env tc
       ct_ev <- requestCoercible loc ty1 concTy
       local_var <- mkSysLocalM (fsLit "coev") $ mkCoerciblePred ty1 concTy
       let binds = EvBinds (unitBag (EvBind local_var (getEvTerm ct_ev)))
           tcCo = TcLetCo binds $
                          mkTcCoVarCo local_var `mkTcTransCo` mkTcSymCo (coercionToTcCoercion ntCo)
       return $ GenInst (freshGoals [ct_ev]) (EvCoercion tcCo)

  | otherwise
  = return NoInstance

nominalArgsAgree :: TyCon -> [Type] -> [Type] -> Bool
nominalArgsAgree tc tys1 tys2 = all ok $ zip3 (tyConRoles tc) tys1 tys2
  where ok (r,t1,t2) = r /= Nominal || t1 `tcEqType` t2

dataConsInScope :: GlobalRdrEnv -> TyCon -> Bool
dataConsInScope rdr_env tc = not hidden_data_cons
  where
    data_con_names = map dataConName (tyConDataCons tc)
    hidden_data_cons = not (isWiredInName (tyConName tc)) &&
                       (isAbstractTyCon tc || any not_in_scope data_con_names)
    not_in_scope dc  = null (lookupGRE_Name rdr_env dc)

markDataConsAsUsed :: GlobalRdrEnv -> TyCon -> TcS ()
markDataConsAsUsed rdr_env tc = addUsedRdrNamesTcS
  [ mkRdrQual (is_as (is_decl imp_spec)) occ
  | dc <- tyConDataCons tc
  , let dc_name = dataConName dc
        occ  = nameOccName dc_name
        gres = lookupGRE_Name rdr_env dc_name
  , not (null gres)
  , Imported (imp_spec:_) <- [gre_prov (head gres)] ]

requestCoercible :: CtLoc -> TcType -> TcType -> TcS MaybeNew
requestCoercible loc ty1 ty2 =
    ASSERT2( typeKind ty1 `tcEqKind` typeKind ty2, ppr ty1 <+> ppr ty2)
    newWantedEvVarNonrec loc' (mkCoerciblePred ty1 ty2)
  where loc' = bumpCtLocDepth CountConstraints loc

\end{code}

Note [Coercible Instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The class Coercible is special: There are no regular instances, and the user
cannot even define them (it is listed as an `abstractClass` in TcValidity).
Instead, the type checker will create instances and their evidence out of thin
air, in getCoercibleInst. The following “instances” are present:

 1. instance Coercible a a
    for any type a at any kind k.

 2. instance (Coercible t1_r t1'_r, Coercible t2_r t2_r',...) =>
       Coercible (C t1_r  t2_r  ... t1_p  t2_p  ... t1_n t2_n ...)
                 (C t1_r' t2_r' ... t1_p' t2_p' ... t1_n t2_n ...)
    for a type constructor C where
     * the nominal type arguments are not changed,
     * the phantom type arguments may change arbitrarily
     * the representational type arguments are again Coercible

    The type constructor can be used undersaturated; then the Coercible
    instance is at a higher kind. This does not cause problems.

    Furthermore in Safe Haskell code, we check that
     * the data constructors of C are in scope and
     * the data constructors of all type constructors used in the definition of
     * C are in scope.
       This is required as otherwise the previous check can be circumvented by
       just adding a local data type around C.

 3. instance Coercible r b => Coercible (NT t1 t2 ...) b
    instance Coercible a r => Coercible a (NT t1 t2 ...)
    for a newtype constructor NT (nor data family instance that resolves to a
    newtype) where
     * r is the concrete type of NT, instantiated with the arguments t1 t2 ...
     * the data constructors of NT are in scope.

    Again, the newtype TyCon can appear undersaturated, but only if it has
    enough arguments to apply the newtype coercion (which is eta-reduced). Examples:
      newtype NT a = NT (Either a Int)
      Coercible (NT Int) (Either Int Int) -- ok
      newtype NT2 a b = NT2 (b -> a)
      newtype NT3 a b = NT3 (b -> a)
      Coercible (NT2 Int) (NT3 Int) -- cannot be derived

  4. instance (forall a. Coercible t1 t2) => Coercible (forall a. t1) (forall a. t2)
     (which would be illegal to write like that in the source code, but we have
     it nevertheless).

The type checker generates evidence in the form of EvCoercion, but the
TcCoercion therein has role Representational,  which are turned into Core
coercions by dsEvTerm in DsBinds.

The evidence for the first three instance is generated here by
getCoercibleInst, the forth instance is implemented in the canonicalization
stage using deferTcSForAllEq.

When the constraint cannot be solved, it is treated as any other unsolved
constraint, i.e. it can turn up in an inferred type signature, or reported to
the user as a regular "Cannot derive instance ..." error. In the latter case,
coercible_msg in TcErrors gives additional explanations of why GHC could not
find a Coercible instance, so it duplicates some of the logic from
getCoercibleInst (in negated form).


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
