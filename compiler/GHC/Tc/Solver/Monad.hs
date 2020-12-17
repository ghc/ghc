{-# LANGUAGE CPP, DeriveFunctor, TypeFamilies, ScopedTypeVariables, TypeApplications,
             DerivingStrategies, GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates -Wno-incomplete-uni-patterns #-}

-- | Type definitions for the constraint solver
module GHC.Tc.Solver.Monad (

    -- The work list
    WorkList(..), isEmptyWorkList, emptyWorkList,
    extendWorkListNonEq, extendWorkListCt,
    extendWorkListCts, extendWorkListEq,
    appendWorkList,
    selectNextWorkItem,
    workListSize,
    getWorkList, updWorkListTcS, pushLevelNoWorkList,

    -- The TcS monad
    TcS, runTcS, runTcSDeriveds, runTcSWithEvBinds, runTcSInerts,
    failTcS, warnTcS, addErrTcS, wrapTcS,
    runTcSEqualities,
    nestTcS, nestImplicTcS, setEvBindsTcS,
    emitImplicationTcS, emitTvImplicationTcS,

    runTcPluginTcS, addUsedGRE, addUsedGREs, keepAlive,
    matchGlobalInst, TcM.ClsInstResult(..),

    QCInst(..),

    -- Tracing etc
    panicTcS, traceTcS,
    traceFireTcS, bumpStepCountTcS, csTraceTcS,
    wrapErrTcS, wrapWarnTcS,
    resetUnificationFlag, setUnificationFlag,

    -- Evidence creation and transformation
    MaybeNew(..), freshGoals, isFresh, getEvExpr,

    newTcEvBinds, newNoTcEvBinds,
    newWantedEq, newWantedEq_SI, emitNewWantedEq,
    newWanted, newWanted_SI, newWantedEvVar,
    newWantedNC, newWantedEvVarNC,
    newDerivedNC,
    newBoundEvVarId,
    unifyTyVar, reportUnifications,
    setEvBind, setWantedEq,
    setWantedEvTerm, setEvBindIfWanted,
    newEvVar, newGivenEvVar, newGivenEvVars,
    emitNewDeriveds, emitNewDerivedEq,
    checkReductionDepth,
    getSolvedDicts, setSolvedDicts,

    getInstEnvs, getFamInstEnvs,                -- Getting the environments
    getTopEnv, getGblEnv, getLclEnv,
    getTcEvBindsVar, getTcLevel,
    getTcEvTyCoVars, getTcEvBindsMap, setTcEvBindsMap,
    tcLookupClass, tcLookupId,

    -- Inerts
    InertSet(..), InertCans(..), emptyInert,
    updInertTcS, updInertCans, updInertDicts, updInertIrreds,
    getHasGivenEqs, setInertCans,
    getInertEqs, getInertCans, getInertGivens,
    getInertInsols, getInnermostGivenEqLevel,
    getTcSInerts, setTcSInerts,
    matchableGivens, prohibitedSuperClassSolve, mightMatchLater,
    getUnsolvedInerts,
    removeInertCts, getPendingGivenScs,
    addInertCan, insertFunEq, addInertForAll,
    emitWorkNC, emitWork,
    isImprovable,

    -- The Model
    kickOutAfterUnification,

    -- Inert Safe Haskell safe-overlap failures
    addInertSafehask, insertSafeOverlapFailureTcS, updInertSafehask,
    getSafeOverlapFailures,

    -- Inert CDictCans
    DictMap, emptyDictMap, lookupInertDict, findDictsByClass, addDict,
    addDictsByClass, delDict, foldDicts, filterDicts, findDict,

    -- Inert CEqCans
    EqualCtList(..), findTyEqs, foldTyEqs,
    findEq,

    -- Inert solved dictionaries
    addSolvedDict, lookupSolvedDict,

    -- Irreds
    foldIrreds,

    -- The family application cache
    lookupFamAppInert, lookupFamAppCache, extendFamAppCache,
    pprKicked,

    -- Inert function equalities
    findFunEq, findFunEqsByTyCon,

    instDFunType,                              -- Instantiation

    -- MetaTyVars
    newFlexiTcSTy, instFlexi, instFlexiX,
    cloneMetaTyVar,
    tcInstSkolTyVarsX,

    TcLevel,
    isFilledMetaTyVar_maybe, isFilledMetaTyVar,
    zonkTyCoVarsAndFV, zonkTcType, zonkTcTypes, zonkTcTyVar, zonkCo,
    zonkTyCoVarsAndFVList,
    zonkSimples, zonkWC,
    zonkTyCoVarKind,

    -- References
    newTcRef, readTcRef, writeTcRef, updTcRef,

    -- Misc
    getDefaultInfo, getDynFlags, getGlobalRdrEnvTcS,
    matchFam, matchFamTcM,
    checkWellStagedDFun,
    pprEq,                                   -- Smaller utils, re-exported from TcM
                                             -- TODO (DV): these are only really used in the
                                             -- instance matcher in GHC.Tc.Solver. I am wondering
                                             -- if the whole instance matcher simply belongs
                                             -- here

    breakTyVarCycle, rewriterView
) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Driver.Env

import qualified GHC.Tc.Utils.Instantiate as TcM
import GHC.Core.InstEnv
import GHC.Tc.Instance.Family as FamInst
import GHC.Core.FamInstEnv

import qualified GHC.Tc.Utils.Monad    as TcM
import qualified GHC.Tc.Utils.TcMType  as TcM
import qualified GHC.Tc.Instance.Class as TcM( matchGlobalInst, ClsInstResult(..) )
import qualified GHC.Tc.Utils.Env      as TcM
       ( checkWellStaged, tcGetDefaultTys, tcLookupClass, tcLookupId, topIdLvl )
import GHC.Tc.Instance.Class( InstanceWhat(..), safeOverlap, instanceReturnsDictCon )
import GHC.Tc.Utils.TcType
import GHC.Driver.Session
import GHC.Core.Type
import qualified GHC.Core.TyCo.Rep as Rep  -- this needs to be used only very locally
import GHC.Core.Coercion
import GHC.Core.Unify

import GHC.Utils.Error
import GHC.Tc.Types.Evidence
import GHC.Core.Class
import GHC.Core.TyCon
import GHC.Tc.Errors   ( solverDepthErrorTcS )

import GHC.Types.Name
import GHC.Types.TyThing
import GHC.Unit.Module ( HasModule, getModule )
import GHC.Types.Name.Reader ( GlobalRdrEnv, GlobalRdrElt )
import qualified GHC.Rename.Env as TcM
import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Data.Bag as Bag
import GHC.Types.Unique.Supply
import GHC.Utils.Misc
import GHC.Tc.Types
import GHC.Tc.Types.Origin
import GHC.Tc.Types.Constraint
import GHC.Core.Predicate

import GHC.Types.Unique.Set
import GHC.Core.TyCon.Env
import GHC.Data.Maybe

import GHC.Core.Map.Type
import GHC.Data.TrieMap

import Control.Monad
import GHC.Utils.Monad
import Data.IORef
import Data.List ( partition, mapAccumL )
import Data.List.NonEmpty ( NonEmpty(..), cons, toList, nonEmpty )
import qualified Data.List.NonEmpty as NE
import Control.Arrow ( first )

#if defined(DEBUG)
import GHC.Data.Graph.Directed
#endif

{-
************************************************************************
*                                                                      *
*                            Worklists                                *
*  Canonical and non-canonical constraints that the simplifier has to  *
*  work on. Including their simplification depths.                     *
*                                                                      *
*                                                                      *
************************************************************************

Note [WorkList priorities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
A WorkList contains canonical and non-canonical items (of all flavours).
Notice that each Ct now has a simplification depth. We may
consider using this depth for prioritization as well in the future.

As a simple form of priority queue, our worklist separates out

* equalities (wl_eqs); see Note [Prioritise equalities]
* all the rest (wl_rest)

Note [Prioritise equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's very important to process equalities /first/:

* (Efficiency)  The general reason to do so is that if we process a
  class constraint first, we may end up putting it into the inert set
  and then kicking it out later.  That's extra work compared to just
  doing the equality first.

* (Avoiding fundep iteration) As #14723 showed, it's possible to
  get non-termination if we
      - Emit the Derived fundep equalities for a class constraint,
        generating some fresh unification variables.
      - That leads to some unification
      - Which kicks out the class constraint
      - Which isn't solved (because there are still some more Derived
        equalities in the work-list), but generates yet more fundeps
  Solution: prioritise derived equalities over class constraints

* (Class equalities) We need to prioritise equalities even if they
  are hidden inside a class constraint;
  see Note [Prioritise class equalities]

* (Kick-out) We want to apply this priority scheme to kicked-out
  constraints too (see the call to extendWorkListCt in kick_out_rewritable
  E.g. a CIrredCan can be a hetero-kinded (t1 ~ t2), which may become
  homo-kinded when kicked out, and hence we want to prioritise it.

* (Derived equalities) Originally we tried to postpone processing
  Derived equalities, in the hope that we might never need to deal
  with them at all; but in fact we must process Derived equalities
  eagerly, partly for the (Efficiency) reason, and more importantly
  for (Avoiding fundep iteration).

Note [Prioritise class equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We prioritise equalities in the solver (see selectWorkItem). But class
constraints like (a ~ b) and (a ~~ b) are actually equalities too;
see Note [The equality types story] in GHC.Builtin.Types.Prim.

Failing to prioritise these is inefficient (more kick-outs etc).
But, worse, it can prevent us spotting a "recursive knot" among
Wanted constraints.  See comment:10 of #12734 for a worked-out
example.

So we arrange to put these particular class constraints in the wl_eqs.

  NB: since we do not currently apply the substitution to the
  inert_solved_dicts, the knot-tying still seems a bit fragile.
  But this makes it better.

-}

-- See Note [WorkList priorities]
data WorkList
  = WL { wl_eqs     :: [Ct]  -- CEqCan, CDictCan, CIrredCan
                             -- Given, Wanted, and Derived
                       -- Contains both equality constraints and their
                       -- class-level variants (a~b) and (a~~b);
                       -- See Note [Prioritise equalities]
                       -- See Note [Prioritise class equalities]

       , wl_rest    :: [Ct]

       , wl_implics :: Bag Implication  -- See Note [Residual implications]
    }

appendWorkList :: WorkList -> WorkList -> WorkList
appendWorkList
    (WL { wl_eqs = eqs1, wl_rest = rest1
        , wl_implics = implics1 })
    (WL { wl_eqs = eqs2, wl_rest = rest2
        , wl_implics = implics2 })
   = WL { wl_eqs     = eqs1     ++ eqs2
        , wl_rest    = rest1    ++ rest2
        , wl_implics = implics1 `unionBags`   implics2 }

workListSize :: WorkList -> Int
workListSize (WL { wl_eqs = eqs, wl_rest = rest })
  = length eqs + length rest

extendWorkListEq :: Ct -> WorkList -> WorkList
extendWorkListEq ct wl = wl { wl_eqs = ct : wl_eqs wl }

extendWorkListNonEq :: Ct -> WorkList -> WorkList
-- Extension by non equality
extendWorkListNonEq ct wl = wl { wl_rest = ct : wl_rest wl }

extendWorkListDeriveds :: [CtEvidence] -> WorkList -> WorkList
extendWorkListDeriveds evs wl
  = extendWorkListCts (map mkNonCanonical evs) wl

extendWorkListImplic :: Implication -> WorkList -> WorkList
extendWorkListImplic implic wl = wl { wl_implics = implic `consBag` wl_implics wl }

extendWorkListCt :: Ct -> WorkList -> WorkList
-- Agnostic
extendWorkListCt ct wl
 = case classifyPredType (ctPred ct) of
     EqPred {}
       -> extendWorkListEq ct wl

     ClassPred cls _  -- See Note [Prioritise class equalities]
       |  isEqPredClass cls
       -> extendWorkListEq ct wl

     _ -> extendWorkListNonEq ct wl

extendWorkListCts :: [Ct] -> WorkList -> WorkList
-- Agnostic
extendWorkListCts cts wl = foldr extendWorkListCt wl cts

isEmptyWorkList :: WorkList -> Bool
isEmptyWorkList (WL { wl_eqs = eqs, wl_rest = rest, wl_implics = implics })
  = null eqs && null rest && isEmptyBag implics

emptyWorkList :: WorkList
emptyWorkList = WL { wl_eqs  = [], wl_rest = [], wl_implics = emptyBag }

selectWorkItem :: WorkList -> Maybe (Ct, WorkList)
-- See Note [Prioritise equalities]
selectWorkItem wl@(WL { wl_eqs = eqs, wl_rest = rest })
  | ct:cts <- eqs  = Just (ct, wl { wl_eqs    = cts })
  | ct:cts <- rest = Just (ct, wl { wl_rest   = cts })
  | otherwise      = Nothing

getWorkList :: TcS WorkList
getWorkList = do { wl_var <- getTcSWorkListRef
                 ; wrapTcS (TcM.readTcRef wl_var) }

selectNextWorkItem :: TcS (Maybe Ct)
-- Pick which work item to do next
-- See Note [Prioritise equalities]
selectNextWorkItem
  = do { wl_var <- getTcSWorkListRef
       ; wl <- readTcRef wl_var
       ; case selectWorkItem wl of {
           Nothing -> return Nothing ;
           Just (ct, new_wl) ->
    do { -- checkReductionDepth (ctLoc ct) (ctPred ct)
         -- This is done by GHC.Tc.Solver.Interact.chooseInstance
       ; writeTcRef wl_var new_wl
       ; return (Just ct) } } }

-- Pretty printing
instance Outputable WorkList where
  ppr (WL { wl_eqs = eqs, wl_rest = rest, wl_implics = implics })
   = text "WL" <+> (braces $
     vcat [ ppUnless (null eqs) $
            text "Eqs =" <+> vcat (map ppr eqs)
          , ppUnless (null rest) $
            text "Non-eqs =" <+> vcat (map ppr rest)
          , ppUnless (isEmptyBag implics) $
            ifPprDebug (text "Implics =" <+> vcat (map ppr (bagToList implics)))
                       (text "(Implics omitted)")
          ])


{- *********************************************************************
*                                                                      *
                InertSet: the inert set
*                                                                      *
*                                                                      *
********************************************************************* -}

data InertSet
  = IS { inert_cans :: InertCans
              -- Canonical Given, Wanted, Derived
              -- Sometimes called "the inert set"

       , inert_cycle_breakers :: [(TcTyVar, TcType)]
              -- a list of CycleBreakerTv / original family applications
              -- used to undo the cycle-breaking needed to handle
              -- Note [Type variable cycles in Givens] in GHC.Tc.Solver.Canonical

       , inert_famapp_cache :: FunEqMap (TcCoercion, TcType)
              -- Just a hash-cons cache for use when reducing family applications
              -- only
              --
              -- If    F tys :-> (co, rhs, flav),
              -- then  co :: rhs ~N F tys
              -- all evidence is from instances or Givens; no coercion holes here
              -- (We have no way of "kicking out" from the cache, so putting
              --  wanteds here means we can end up solving a Wanted with itself. Bad)

       , inert_solved_dicts   :: DictMap CtEvidence
              -- All Wanteds, of form ev :: C t1 .. tn
              -- See Note [Solved dictionaries]
              -- and Note [Do not add superclasses of solved dictionaries]
       }

instance Outputable InertSet where
  ppr (IS { inert_cans = ics
          , inert_solved_dicts = solved_dicts })
      = vcat [ ppr ics
             , ppUnless (null dicts) $
               text "Solved dicts =" <+> vcat (map ppr dicts) ]
         where
           dicts = bagToList (dictsToBag solved_dicts)

emptyInertCans :: InertCans
emptyInertCans
  = IC { inert_eqs          = emptyDVarEnv
       , inert_given_eq_lvl = topTcLevel
       , inert_given_eqs    = False
       , inert_dicts        = emptyDicts
       , inert_safehask     = emptyDicts
       , inert_funeqs       = emptyFunEqs
       , inert_insts        = []
       , inert_irreds       = emptyCts }

emptyInert :: InertSet
emptyInert
  = IS { inert_cans           = emptyInertCans
       , inert_cycle_breakers = []
       , inert_famapp_cache   = emptyFunEqs
       , inert_solved_dicts   = emptyDictMap }


{- Note [Solved dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we apply a top-level instance declaration, we add the "solved"
dictionary to the inert_solved_dicts.  In general, we use it to avoid
creating a new EvVar when we have a new goal that we have solved in
the past.

But in particular, we can use it to create *recursive* dictionaries.
The simplest, degenerate case is
    instance C [a] => C [a] where ...
If we have
    [W] d1 :: C [x]
then we can apply the instance to get
    d1 = $dfCList d
    [W] d2 :: C [x]
Now 'd1' goes in inert_solved_dicts, and we can solve d2 directly from d1.
    d1 = $dfCList d
    d2 = d1

See Note [Example of recursive dictionaries]

VERY IMPORTANT INVARIANT:

 (Solved Dictionary Invariant)
    Every member of the inert_solved_dicts is the result
    of applying an instance declaration that "takes a step"

    An instance "takes a step" if it has the form
        dfunDList d1 d2 = MkD (...) (...) (...)
    That is, the dfun is lazy in its arguments, and guarantees to
    immediately return a dictionary constructor.  NB: all dictionary
    data constructors are lazy in their arguments.

    This property is crucial to ensure that all dictionaries are
    non-bottom, which in turn ensures that the whole "recursive
    dictionary" idea works at all, even if we get something like
        rec { d = dfunDList d dx }
    See Note [Recursive superclasses] in GHC.Tc.TyCl.Instance.

 Reason:
   - All instances, except two exceptions listed below, "take a step"
     in the above sense

   - Exception 1: local quantified constraints have no such guarantee;
     indeed, adding a "solved dictionary" when appling a quantified
     constraint led to the ability to define unsafeCoerce
     in #17267.

   - Exception 2: the magic built-in instance for (~) has no
     such guarantee.  It behaves as if we had
         class    (a ~# b) => (a ~ b) where {}
         instance (a ~# b) => (a ~ b) where {}
     The "dfun" for the instance is strict in the coercion.
     Anyway there's no point in recording a "solved dict" for
     (t1 ~ t2); it's not going to allow a recursive dictionary
     to be constructed.  Ditto (~~) and Coercible.

THEREFORE we only add a "solved dictionary"
  - when applying an instance declaration
  - subject to Exceptions 1 and 2 above

In implementation terms
  - GHC.Tc.Solver.Monad.addSolvedDict adds a new solved dictionary,
    conditional on the kind of instance

  - It is only called when applying an instance decl,
    in GHC.Tc.Solver.Interact.doTopReactDict

  - ClsInst.InstanceWhat says what kind of instance was
    used to solve the constraint.  In particular
      * LocalInstance identifies quantified constraints
      * BuiltinEqInstance identifies the strange built-in
        instances for equality.

  - ClsInst.instanceReturnsDictCon says which kind of
    instance guarantees to return a dictionary constructor

Other notes about solved dictionaries

* See also Note [Do not add superclasses of solved dictionaries]

* The inert_solved_dicts field is not rewritten by equalities,
  so it may get out of date.

* The inert_solved_dicts are all Wanteds, never givens

* We only cache dictionaries from top-level instances, not from
  local quantified constraints.  Reason: if we cached the latter
  we'd need to purge the cache when bringing new quantified
  constraints into scope, because quantified constraints "shadow"
  top-level instances.

Note [Do not add superclasses of solved dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Every member of inert_solved_dicts is the result of applying a
dictionary function, NOT of applying superclass selection to anything.
Consider

        class Ord a => C a where
        instance Ord [a] => C [a] where ...

Suppose we are trying to solve
  [G] d1 : Ord a
  [W] d2 : C [a]

Then we'll use the instance decl to give

  [G] d1 : Ord a     Solved: d2 : C [a] = $dfCList d3
  [W] d3 : Ord [a]

We must not add d4 : Ord [a] to the 'solved' set (by taking the
superclass of d2), otherwise we'll use it to solve d3, without ever
using d1, which would be a catastrophe.

Solution: when extending the solved dictionaries, do not add superclasses.
That's why each element of the inert_solved_dicts is the result of applying
a dictionary function.

Note [Example of recursive dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--- Example 1

    data D r = ZeroD | SuccD (r (D r));

    instance (Eq (r (D r))) => Eq (D r) where
        ZeroD     == ZeroD     = True
        (SuccD a) == (SuccD b) = a == b
        _         == _         = False;

    equalDC :: D [] -> D [] -> Bool;
    equalDC = (==);

We need to prove (Eq (D [])). Here's how we go:

   [W] d1 : Eq (D [])
By instance decl of Eq (D r):
   [W] d2 : Eq [D []]      where   d1 = dfEqD d2
By instance decl of Eq [a]:
   [W] d3 : Eq (D [])      where   d2 = dfEqList d3
                                   d1 = dfEqD d2
Now this wanted can interact with our "solved" d1 to get:
    d3 = d1

-- Example 2:
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
    [G] d0 : Foo t
    [W] d1 : Data Maybe [t]   -- Desired superclass

We may add the given in the inert set, along with its superclasses
  Inert:
    [G] d0 : Foo t
    [G] d01 : Data Maybe t   -- Superclass of d0
  WorkList
    [W] d1 : Data Maybe [t]

Solve d1 using instance dfunData2; d1 := dfunData2 d2 d3
  Inert:
    [G] d0 : Foo t
    [G] d01 : Data Maybe t   -- Superclass of d0
  Solved:
        d1 : Data Maybe [t]
  WorkList:
    [W] d2 : Sat (Maybe [t])
    [W] d3 : Data Maybe t

Now, we may simplify d2 using dfunSat; d2 := dfunSat d4
  Inert:
    [G] d0 : Foo t
    [G] d01 : Data Maybe t   -- Superclass of d0
  Solved:
        d1 : Data Maybe [t]
        d2 : Sat (Maybe [t])
  WorkList:
    [W] d3 : Data Maybe t
    [W] d4 : Foo [t]

Now, we can just solve d3 from d01; d3 := d01
  Inert
    [G] d0 : Foo t
    [G] d01 : Data Maybe t   -- Superclass of d0
  Solved:
        d1 : Data Maybe [t]
        d2 : Sat (Maybe [t])
  WorkList
    [W] d4 : Foo [t]

Now, solve d4 using dfunFoo2;  d4 := dfunFoo2 d5
  Inert
    [G] d0  : Foo t
    [G] d01 : Data Maybe t   -- Superclass of d0
  Solved:
        d1 : Data Maybe [t]
        d2 : Sat (Maybe [t])
        d4 : Foo [t]
  WorkList:
    [W] d5 : Foo t

Now, d5 can be solved! d5 := d0

Result
   d1 := dfunData2 d2 d3
   d2 := dfunSat d4
   d3 := d01
   d4 := dfunFoo2 d5
   d5 := d0
-}

{- *********************************************************************
*                                                                      *
                InertCans: the canonical inerts
*                                                                      *
*                                                                      *
********************************************************************* -}

data InertCans   -- See Note [Detailed InertCans Invariants] for more
  = IC { inert_eqs :: InertEqs
              -- See Note [inert_eqs: the inert equalities]
              -- All CEqCans with a TyVarLHS; index is the LHS tyvar
              -- Domain = skolems and untouchables; a touchable would be unified

       , inert_funeqs :: FunEqMap EqualCtList
              -- All CEqCans with a TyFamLHS; index is the whole family head type.
              -- LHS is fully rewritten (modulo eqCanRewrite constraints)
              --     wrt inert_eqs
              -- Can include all flavours, [G], [W], [WD], [D]

       , inert_dicts :: DictMap Ct
              -- Dictionaries only
              -- All fully rewritten (modulo flavour constraints)
              --     wrt inert_eqs

       , inert_insts :: [QCInst]

       , inert_safehask :: DictMap Ct
              -- Failed dictionary resolution due to Safe Haskell overlapping
              -- instances restriction. We keep this separate from inert_dicts
              -- as it doesn't cause compilation failure, just safe inference
              -- failure.
              --
              -- ^ See Note [Safe Haskell Overlapping Instances Implementation]
              -- in "GHC.Tc.Solver"

       , inert_irreds :: Cts
              -- Irreducible predicates that cannot be made canonical,
              --     and which don't interact with others (e.g.  (c a))
              -- and insoluble predicates (e.g.  Int ~ Bool, or a ~ [a])

       , inert_given_eq_lvl :: TcLevel
              -- The TcLevel of the innermost implication that has a Given
              -- equality of the sort that make a unification variable untouchable
              -- (see Note [Unification preconditions] in GHC.Tc.Utils.Unify).
              -- See Note [Tracking Given equalities] below

       , inert_given_eqs :: Bool
              -- True <=> The inert Givens *at this level* (tcl_tclvl)
              --          could includes at least one equality /other than/ a
              --          let-bound skolem equality.
              -- Reason: report these givens when reporting a failed equality
              -- See Note [Tracking Given equalities]
       }

type InertEqs    = DTyVarEnv EqualCtList

newtype EqualCtList = EqualCtList (NonEmpty Ct)
  deriving newtype Outputable
  -- See Note [EqualCtList invariants]

unitEqualCtList :: Ct -> EqualCtList
unitEqualCtList ct = EqualCtList (ct :| [])

addToEqualCtList :: Ct -> EqualCtList -> EqualCtList
-- NB: This function maintains the "derived-before-wanted" invariant of EqualCtList,
-- but not the others. See Note [EqualCtList invariants]
addToEqualCtList ct (EqualCtList old_eqs)
  | isWantedCt ct
  , eq1 :| eqs <- old_eqs
  = EqualCtList (eq1 :| ct : eqs)
  | otherwise
  = EqualCtList (ct `cons` old_eqs)

filterEqualCtList :: (Ct -> Bool) -> EqualCtList -> Maybe EqualCtList
filterEqualCtList pred (EqualCtList cts)
  = fmap EqualCtList (nonEmpty $ NE.filter pred cts)

equalCtListToList :: EqualCtList -> [Ct]
equalCtListToList (EqualCtList cts) = toList cts

listToEqualCtList :: [Ct] -> Maybe EqualCtList
-- NB: This does not maintain invariants other than having the EqualCtList be
-- non-empty
listToEqualCtList cts = EqualCtList <$> nonEmpty cts

{- Note [Tracking Given equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For reasons described in (UNTOUCHABLE) in GHC.Tc.Utils.Unify
Note [Unification preconditions], we can't unify
   alpha[2] ~ Int
under a level-4 implication if there are any Given equalities
bound by the implications at level 3 of 4.  To that end, the
InertCans tracks

  inert_given_eq_lvl :: TcLevel
     -- The TcLevel of the innermost implication that has a Given
     -- equality of the sort that make a unification variable untouchable
     -- (see Note [Unification preconditions] in GHC.Tc.Utils.Unify).

We update inert_given_eq_lvl whenever we add a Given to the
inert set, in updateGivenEqs.

Then a unification variable alpha[n] is untouchable iff
    n < inert_given_eq_lvl
that is, if the unification variable was born outside an
enclosing Given equality.

Exactly which constraints should trigger (UNTOUCHABLE), and hence
should update inert_given_eq_lvl?

* We do /not/ need to worry about let-bound skolems, such ast
     forall[2] a. a ~ [b] => blah
  See Note [Let-bound skolems]

* Consider an implication
      forall[2]. beta[1] => alpha[1] ~ Int
  where beta is a unification variable that has already been unified
  to () in an outer scope.  Then alpha[1] is perfectly touchable and
  we can unify alpha := Int. So when deciding whether the givens contain
  an equality, we should canonicalise first, rather than just looking at
  the /original/ givens (#8644).

 * However, we must take account of *potential* equalities. Consider the
   same example again, but this time we have /not/ yet unified beta:
      forall[2] beta[1] => ...blah...

   Because beta might turn into an equality, updateGivenEqs conservatively
   treats it as a potential equality, and updates inert_give_eq_lvl

 * What about something like forall[2] a b. a ~ F b => [W] alpha[1] ~ X y z?

   That Given cannot affect the Wanted, because the Given is entirely
   *local*: it mentions only skolems bound in the very same
   implication. Such equalities need not make alpha untouchable. (Test
   case typecheck/should_compile/LocalGivenEqs has a real-life
   motivating example, with some detailed commentary.)
   Hence the 'mentionsOuterVar' test in updateGivenEqs.

   However, solely to support better error messages
   (see Note [HasGivenEqs] in GHC.Tc.Types.Constraint) we also track
   these "local" equalities in the boolean inert_given_eqs field.
   This field is used only to set the ic_given_eqs field to LocalGivenEqs;
   see the function getHasGivenEqs.

   Here is a simpler case that triggers this behaviour:

     data T where
       MkT :: F a ~ G b => a -> b -> T

     f (MkT _ _) = True

   Because of this behaviour around local equality givens, we can infer the
   type of f. This is typecheck/should_compile/LocalGivenEqs2.

 * We need not look at the equality relation involved (nominal vs
   representational), because representational equalities can still
   imply nominal ones. For example, if (G a ~R G b) and G's argument's
   role is nominal, then we can deduce a ~N b.

Note [Let-bound skolems]
~~~~~~~~~~~~~~~~~~~~~~~~
If   * the inert set contains a canonical Given CEqCan (a ~ ty)
and  * 'a' is a skolem bound in this very implication,

then:
a) The Given is pretty much a let-binding, like
      f :: (a ~ b->c) => a -> a
   Here the equality constraint is like saying
      let a = b->c in ...
   It is not adding any new, local equality  information,
   and hence can be ignored by has_given_eqs

b) 'a' will have been completely substituted out in the inert set,
   so we can safely discard it.

For an example, see #9211.

See also GHC.Tc.Utils.Unify Note [Deeper level on the left] for how we ensure
that the right variable is on the left of the equality when both are
tyvars.

You might wonder whether the skolem really needs to be bound "in the
very same implication" as the equuality constraint.
Consider this (c.f. #15009):

  data S a where
    MkS :: (a ~ Int) => S a

  g :: forall a. S a -> a -> blah
  g x y = let h = \z. ( z :: Int
                      , case x of
                           MkS -> [y,z])
          in ...

From the type signature for `g`, we get `y::a` .  Then when we
encounter the `\z`, we'll assign `z :: alpha[1]`, say.  Next, from the
body of the lambda we'll get

  [W] alpha[1] ~ Int                             -- From z::Int
  [W] forall[2]. (a ~ Int) => [W] alpha[1] ~ a   -- From [y,z]

Now, unify alpha := a.  Now we are stuck with an unsolved alpha~Int!
So we must treat alpha as untouchable under the forall[2] implication.

Note [Detailed InertCans Invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The InertCans represents a collection of constraints with the following properties:

  * All canonical

  * No two dictionaries with the same head
  * No two CIrreds with the same type

  * Family equations inert wrt top-level family axioms

  * Dictionaries have no matching top-level instance

  * Given family or dictionary constraints don't mention touchable
    unification variables

  * Non-CEqCan constraints are fully rewritten with respect
    to the CEqCan equalities (modulo eqCanRewrite of course;
    eg a wanted cannot rewrite a given)

  * CEqCan equalities: see Note [inert_eqs: the inert equalities]
    Also see documentation in Constraint.Ct for a list of invariants

Note [EqualCtList invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    * All are equalities
    * All these equalities have the same LHS
    * The list is never empty
    * No element of the list can rewrite any other
    * Derived before Wanted

From the fourth invariant it follows that the list is
   - A single [G], or
   - Zero or one [D] or [WD], followed by any number of [W]

The Wanteds can't rewrite anything which is why we put them last

Note [inert_eqs: the inert equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Definition [Can-rewrite relation]
A "can-rewrite" relation between flavours, written f1 >= f2, is a
binary relation with the following properties

  (R1) >= is transitive
  (R2) If f1 >= f, and f2 >= f,
       then either f1 >= f2 or f2 >= f1

Lemma.  If f1 >= f then f1 >= f1
Proof.  By property (R2), with f1=f2

Definition [Generalised substitution]
A "generalised substitution" S is a set of triples (t0 -f-> t), where
  t0 is a type variable or an exactly-saturated type family application
    (that is, t0 is a CanEqLHS)
  t is a type
  f is a flavour
such that
  (WF1) if (t0 -f1-> t1) in S
           (t0' -f2-> t2) in S
        then either not (f1 >= f2) or t0 does not appear within t0'
  (WF2) if (t0 -f-> t) is in S, then t /= t0

Definition [Applying a generalised substitution]
If S is a generalised substitution
   S(f,t0) = t,  if (t0 -fs-> t) in S, and fs >= f
           = apply S to components of t0, otherwise
See also Note [Flavours with roles].

Theorem: S(f,t0) is well defined as a function.
Proof: Suppose (t0 -f1-> t1) and (t0 -f2-> t2) are both in S,
               and  f1 >= f and f2 >= f
       Then by (R2) f1 >= f2 or f2 >= f1, which contradicts (WF1)

Notation: repeated application.
  S^0(f,t)     = t
  S^(n+1)(f,t) = S(f, S^n(t))

Definition: inert generalised substitution
A generalised substitution S is "inert" iff

  (IG1) there is an n such that
        for every f,t, S^n(f,t) = S^(n+1)(f,t)

By (IG1) we define S*(f,t) to be the result of exahaustively
applying S(f,_) to t.

----------------------------------------------------------------
Our main invariant:
   the inert CEqCans should be an inert generalised substitution
----------------------------------------------------------------

Note that inertness is not the same as idempotence.  To apply S to a
type, you may have to apply it recursively.  But inertness does
guarantee that this recursive use will terminate.

Note [Extending the inert equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Main Theorem [Stability under extension]
   Suppose we have a "work item"
       t0 -fw-> t
   and an inert generalised substitution S,
   THEN the extended substitution T = S+(t0 -fw-> t)
        is an inert generalised substitution
   PROVIDED
      (T1) S(fw,t0) = t0     -- LHS of work-item is a fixpoint of S(fw,_)
      (T2) S(fw,t)  = t      -- RHS of work-item is a fixpoint of S(fw,_)
      (T3) t0 not in t       -- No occurs check in the work item

      AND, for every (t0' -fs-> s) in S:
           (K0) not (fw >= fs)
                Reason: suppose we kick out (a -fs-> s),
                        and add (t0 -fw-> t) to the inert set.
                        The latter can't rewrite the former,
                        so the kick-out achieved nothing

           OR { (K1) t0 is not rewritable in t0'. That is, t0 does not occur
                     in t0' (except perhaps in a cast or coercion).
                     Reason: if fw >= fs, WF1 says we can't have both
                             t0 -fw-> t  and  F t0 -fs-> s

                AND (K2): guarantees inertness of the new substitution
                    {  (K2a) not (fs >= fs)
                    OR (K2b) fs >= fw
                    OR (K2d) t0 not in s }

                AND (K3) See Note [K3: completeness of solving]
                    { (K3a) If the role of fs is nominal: s /= t0
                      (K3b) If the role of fs is representational:
                            s is not of form (t0 t1 .. tn) } }


Conditions (T1-T3) are established by the canonicaliser
Conditions (K1-K3) are established by GHC.Tc.Solver.Monad.kickOutRewritable

The idea is that
* (T1-2) are guaranteed by exhaustively rewriting the work-item
  with S(fw,_).

* T3 is guaranteed by a simple occurs-check on the work item.
  This is done during canonicalisation, in canEqCanLHSFinish; invariant
  (TyEq:OC) of CEqCan.

* (K1-3) are the "kick-out" criteria.  (As stated, they are really the
  "keep" criteria.) If the current inert S contains a triple that does
  not satisfy (K1-3), then we remove it from S by "kicking it out",
  and re-processing it.

* Note that kicking out is a Bad Thing, because it means we have to
  re-process a constraint.  The less we kick out, the better.
  TODO: Make sure that kicking out really *is* a Bad Thing. We've assumed
  this but haven't done the empirical study to check.

* Assume we have  G>=G, G>=W and that's all.  Then, when performing
  a unification we add a new given  a -G-> ty.  But doing so does NOT require
  us to kick out an inert wanted that mentions a, because of (K2a).  This
  is a common case, hence good not to kick out.

* Lemma (L2): if not (fw >= fw), then K0 holds and we kick out nothing
  Proof: using Definition [Can-rewrite relation], fw can't rewrite anything
         and so K0 holds.  Intuitively, since fw can't rewrite anything,
         adding it cannot cause any loops
  This is a common case, because Wanteds cannot rewrite Wanteds.
  It's used to avoid even looking for constraint to kick out.

* Lemma (L1): The conditions of the Main Theorem imply that there is no
              (t0 -fs-> t) in S, s.t.  (fs >= fw).
  Proof. Suppose the contrary (fs >= fw).  Then because of (T1),
  S(fw,t0)=t0.  But since fs>=fw, S(fw,t0) = s, hence s=t0.  But now we
  have (t0 -fs-> t0) in S, which contradicts (WF2).

* The extended substitution satisfies (WF1) and (WF2)
  - (K1) plus (L1) guarantee that the extended substitution satisfies (WF1).
  - (T3) guarantees (WF2).

* (K2) is about inertness.  Intuitively, any infinite chain T^0(f,t),
  T^1(f,t), T^2(f,T).... must pass through the new work item infinitely
  often, since the substitution without the work item is inert; and must
  pass through at least one of the triples in S infinitely often.

  - (K2a): if not(fs>=fs) then there is no f that fs can rewrite (fs>=f),
    and hence this triple never plays a role in application S(f,a).
    It is always safe to extend S with such a triple.

    (NB: we could strengten K1) in this way too, but see K3.

  - (K2b): If this holds then, by (T2), b is not in t.  So applying the
    work item does not generate any new opportunities for applying S

  - (K2c): If this holds, we can't pass through this triple infinitely
    often, because if we did then fs>=f, fw>=f, hence by (R2)
      * either fw>=fs, contradicting K2c
      * or fs>=fw; so by the argument in K2b we can't have a loop

  - (K2d): if a not in s, we hae no further opportunity to apply the
    work item, similar to (K2b)

  NB: Dimitrios has a PDF that does this in more detail

Key lemma to make it watertight.
  Under the conditions of the Main Theorem,
  forall f st fw >= f, a is not in S^k(f,t), for any k

Also, consider roles more carefully. See Note [Flavours with roles]

Note [K3: completeness of solving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(K3) is not necessary for the extended substitution
to be inert.  In fact K1 could be made stronger by saying
   ... then (not (fw >= fs) or not (fs >= fs))
But it's not enough for S to be inert; we also want completeness.
That is, we want to be able to solve all soluble wanted equalities.
Suppose we have

   work-item   b -G-> a
   inert-item  a -W-> b

Assuming (G >= W) but not (W >= W), this fulfills all the conditions,
so we could extend the inerts, thus:

   inert-items   b -G-> a
                 a -W-> b

But if we kicked-out the inert item, we'd get

   work-item     a -W-> b
   inert-item    b -G-> a

Then rewrite the work-item gives us (a -W-> a), which is soluble via Refl.
So we add one more clause to the kick-out criteria

Another way to understand (K3) is that we treat an inert item
        a -f-> b
in the same way as
        b -f-> a
So if we kick out one, we should kick out the other.  The orientation
is somewhat accidental.

When considering roles, we also need the second clause (K3b). Consider

  work-item    c -G/N-> a
  inert-item   a -W/R-> b c

The work-item doesn't get rewritten by the inert, because (>=) doesn't hold.
But we don't kick out the inert item because not (W/R >= W/R).  So we just
add the work item. But then, consider if we hit the following:

  work-item    b -G/N-> Id
  inert-items  a -W/R-> b c
               c -G/N-> a
where
  newtype Id x = Id x

For similar reasons, if we only had (K3a), we wouldn't kick the
representational inert out. And then, we'd miss solving the inert, which
now reduced to reflexivity.

The solution here is to kick out representational inerts whenever the
tyvar of a work item is "exposed", where exposed means being at the
head of the top-level application chain (a t1 .. tn).  See
is_can_eq_lhs_head. This is encoded in (K3b).

Beware: if we make this test succeed too often, we kick out too much,
and the solver might loop.  Consider (#14363)
  work item:   [G] a ~R f b
  inert item:  [G] b ~R f a
In GHC 8.2 the completeness tests more aggressive, and kicked out
the inert item; but no rewriting happened and there was an infinite
loop.  All we need is to have the tyvar at the head.

Note [Flavours with roles]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The system described in Note [inert_eqs: the inert equalities]
discusses an abstract
set of flavours. In GHC, flavours have two components: the flavour proper,
taken from {Wanted, Derived, Given} and the equality relation (often called
role), taken from {NomEq, ReprEq}.
When substituting w.r.t. the inert set,
as described in Note [inert_eqs: the inert equalities],
we must be careful to respect all components of a flavour.
For example, if we have

  inert set: a -G/R-> Int
             b -G/R-> Bool

  type role T nominal representational

and we wish to compute S(W/R, T a b), the correct answer is T a Bool, NOT
T Int Bool. The reason is that T's first parameter has a nominal role, and
thus rewriting a to Int in T a b is wrong. Indeed, this non-congruence of
substitution means that the proof in Note [The inert equalities] may need
to be revisited, but we don't think that the end conclusion is wrong.
-}

instance Outputable InertCans where
  ppr (IC { inert_eqs = eqs
          , inert_funeqs = funeqs, inert_dicts = dicts
          , inert_safehask = safehask, inert_irreds = irreds
          , inert_given_eq_lvl = ge_lvl
          , inert_given_eqs = given_eqs
          , inert_insts = insts })

    = braces $ vcat
      [ ppUnless (isEmptyDVarEnv eqs) $
        text "Equalities:"
          <+> pprCts (foldDVarEnv folder emptyCts eqs)
      , ppUnless (isEmptyTcAppMap funeqs) $
        text "Type-function equalities =" <+> pprCts (foldFunEqs folder funeqs emptyCts)
      , ppUnless (isEmptyTcAppMap dicts) $
        text "Dictionaries =" <+> pprCts (dictsToBag dicts)
      , ppUnless (isEmptyTcAppMap safehask) $
        text "Safe Haskell unsafe overlap =" <+> pprCts (dictsToBag safehask)
      , ppUnless (isEmptyCts irreds) $
        text "Irreds =" <+> pprCts irreds
      , ppUnless (null insts) $
        text "Given instances =" <+> vcat (map ppr insts)
      , text "Innermost given equalities =" <+> ppr ge_lvl
      , text "Given eqs at this level =" <+> ppr given_eqs
      ]
    where
      folder (EqualCtList eqs) rest = nonEmptyToBag eqs `andCts` rest

{- *********************************************************************
*                                                                      *
             Shadow constraints and improvement
*                                                                      *
************************************************************************

Note [The improvement story and derived shadows]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Because Wanteds cannot rewrite Wanteds (see Note [Wanteds do not
rewrite Wanteds] in GHC.Tc.Types.Constraint), we may miss some opportunities for
solving.  Here's a classic example (indexed-types/should_fail/T4093a)

    Ambiguity check for f: (Foo e ~ Maybe e) => Foo e

    We get [G] Foo e ~ Maybe e    (CEqCan)
           [W] Foo ee ~ Foo e     (CEqCan)       -- ee is a unification variable
           [W] Foo ee ~ Maybe ee  (CEqCan)

    The first Wanted gets rewritten to

           [W] Foo ee ~ Maybe e

    But now we appear to be stuck, since we don't rewrite Wanteds with
    Wanteds.  This is silly because we can see that ee := e is the
    only solution.

The basic plan is
  * generate Derived constraints that shadow Wanted constraints
  * allow Derived to rewrite Derived
  * in order to cause some unifications to take place
  * that in turn solve the original Wanteds

The ONLY reason for all these Derived equalities is to tell us how to
unify a variable: that is, what Mark Jones calls "improvement".

The same idea is sometimes also called "saturation"; find all the
equalities that must hold in any solution.

Or, equivalently, you can think of the derived shadows as implementing
the "model": a non-idempotent but no-occurs-check substitution,
reflecting *all* *Nominal* equalities (a ~N ty) that are not
immediately soluble by unification.

More specifically, here's how it works (Oct 16):

* Wanted constraints are born as [WD]; this behaves like a
  [W] and a [D] paired together.

* When we are about to add a [WD] to the inert set, if it can
  be rewritten by a [D] a ~ ty, then we split it into [W] and [D],
  putting the latter into the work list (see maybeEmitShadow).

In the example above, we get to the point where we are stuck:
    [WD] Foo ee ~ Foo e
    [WD] Foo ee ~ Maybe ee

But now when [WD] Foo ee ~ Maybe ee is about to be added, we'll
split it into [W] and [D], since the inert [WD] Foo ee ~ Foo e
can rewrite it.  Then:
    work item: [D] Foo ee ~ Maybe ee
    inert:     [W] Foo ee ~ Maybe ee
               [WD] Foo ee ~ Maybe e

See Note [Splitting WD constraints].  Now the work item is rewritten
by the [WD] and we soon get ee := e.

Additional notes:

  * The derived shadow equalities live in inert_eqs, along with
    the Givens and Wanteds; see Note [EqualCtList invariants].

  * We make Derived shadows only for Wanteds, not Givens.  So we
    have only [G], not [GD] and [G] plus splitting.  See
    Note [Add derived shadows only for Wanteds]

  * We also get Derived equalities from functional dependencies
    and type-function injectivity; see calls to unifyDerived.

  * It's worth having [WD] rather than just [W] and [D] because
    * efficiency: silly to process the same thing twice
    * inert_dicts is a finite map keyed by
      the type; it's inconvenient for it to map to TWO constraints

Another example requiring Deriveds is in
Note [Put touchable variables on the left] in GHC.Tc.Solver.Canonical.

Note [Splitting WD constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We are about to add a [WD] constraint to the inert set; and we
know that the inert set has fully rewritten it.  Should we split
it into [W] and [D], and put the [D] in the work list for further
work?

* CDictCan (C tys):
  Yes if the inert set could rewrite tys to make the class constraint,
  or type family, fire.  That is, yes if the inert_eqs intersects
  with the free vars of tys.  For this test we use
  (anyRewritableTyVar True) which ignores casts and coercions in tys,
  because rewriting the casts or coercions won't make the thing fire
  more often.

* CEqCan (lhs ~ ty): Yes if the inert set could rewrite 'lhs' or 'ty'.
  We need to check both 'lhs' and 'ty' against the inert set:
    - Inert set contains  [D] a ~ ty2
      Then we want to put [D] a ~ ty in the worklist, so we'll
      get [D] ty ~ ty2 with consequent good things

    - Inert set contains [D] b ~ a, where b is in ty.
      We can't just add [WD] a ~ ty[b] to the inert set, because
      that breaks the inert-set invariants.  If we tried to
      canonicalise another [D] constraint mentioning 'a', we'd
      get an infinite loop

  Moreover we must use (anyRewritableTyVar False) for the RHS,
  because even tyvars in the casts and coercions could give
  an infinite loop if we don't expose it

* CIrredCan: Yes if the inert set can rewrite the constraint.
  We used to think splitting irreds was unnecessary, but
  see Note [Splitting Irred WD constraints]

* Others: nothing is gained by splitting.

Note [Splitting Irred WD constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Splitting Irred constraints can make a difference. Here is the
scenario:

  a[sk] :: F v     -- F is a type family
  beta :: alpha

  work item: [WD] a ~ beta

This is heterogeneous, so we emit a kind equality and make the work item an
inert Irred.

  work item: [D] F v ~ alpha
  inert: [WD] (a |> co) ~ beta (CIrredCan)

Can't make progress on the work item. Add to inert set. This kicks out the
old inert, because a [D] can rewrite a [WD].

  work item: [WD] (a |> co) ~ beta
  inert: [D] F v ~ alpha (CEqCan)

Can't make progress on this work item either (although GHC tries by
decomposing the cast and rewriting... but that doesn't make a difference),
which is still hetero. Emit a new kind equality and add to inert set. But,
critically, we split the Irred.

  work list:
   [D] F v ~ alpha (CEqCan)
   [D] (a |> co) ~ beta (CIrred) -- this one was split off
  inert:
   [W] (a |> co) ~ beta
   [D] F v ~ alpha

We quickly solve the first work item, as it's the same as an inert.

  work item: [D] (a |> co) ~ beta
  inert:
   [W] (a |> co) ~ beta
   [D] F v ~ alpha

We decompose the cast, yielding

  [D] a ~ beta

We then rewrite the kinds. The lhs kind is F v, which flattens to alpha.

  co' :: F v ~ alpha
  [D] (a |> co') ~ beta

Now this equality is homo-kinded. So we swizzle it around to

  [D] beta ~ (a |> co')

and set beta := a |> co', and go home happy.

If we don't split the Irreds, we loop. This is all dangerously subtle.

This is triggered by test case typecheck/should_compile/SplitWD.

Note [Add derived shadows only for Wanteds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We only add shadows for Wanted constraints. That is, we have
[WD] but not [GD]; and maybeEmitShaodw looks only at [WD]
constraints.

It does just possibly make sense ot add a derived shadow for a
Given. If we created a Derived shadow of a Given, it could be
rewritten by other Deriveds, and that could, conceivably, lead to a
useful unification.

But (a) I have been unable to come up with an example of this
        happening
    (b) see #12660 for how adding the derived shadows
        of a Given led to an infinite loop.
    (c) It's unlikely that rewriting derived Givens will lead
        to a unification because Givens don't mention touchable
        unification variables

For (b) there may be other ways to solve the loop, but simply
reraining from adding derived shadows of Givens is particularly
simple.  And it's more efficient too!

Still, here's one possible reason for adding derived shadows
for Givens.  Consider
           work-item [G] a ~ [b], inerts has [D] b ~ a.
If we added the derived shadow (into the work list)
         [D] a ~ [b]
When we process it, we'll rewrite to a ~ [a] and get an
occurs check.  Without it we'll miss the occurs check (reporting
inaccessible code); but that's probably OK.

Note [Keep CDictCan shadows as CDictCan]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
  class C a => D a b
and [G] D a b, [G] C a in the inert set.  Now we insert
[D] b ~ c.  We want to kick out a derived shadow for [D] D a b,
so we can rewrite it with the new constraint, and perhaps get
instance reduction or other consequences.

BUT we do not want to kick out a *non-canonical* (D a b). If we
did, we would do this:
  - rewrite it to [D] D a c, with pend_sc = True
  - use expandSuperClasses to add C a
  - go round again, which solves C a from the givens
This loop goes on for ever and triggers the simpl_loop limit.

Solution: kick out the CDictCan which will have pend_sc = False,
because we've already added its superclasses.  So we won't re-add
them.  If we forget the pend_sc flag, our cunning scheme for avoiding
generating superclasses repeatedly will fail.

See #11379 for a case of this.

Note [Do not do improvement for WOnly]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do improvement between two constraints (e.g. for injectivity
or functional dependencies) only if both are "improvable". And
we improve a constraint wrt the top-level instances only if
it is improvable.

Improvable:     [G] [WD] [D}
Not improvable: [W]

Reasons:

* It's less work: fewer pairs to compare

* Every [W] has a shadow [D] so nothing is lost

* Consider [WD] C Int b,  where 'b' is a skolem, and
    class C a b | a -> b
    instance C Int Bool
  We'll do a fundep on it and emit [D] b ~ Bool
  That will kick out constraint [WD] C Int b
  Then we'll split it to [W] C Int b (keep in inert)
                     and [D] C Int b (in work list)
  When processing the latter we'll rewrite it to
        [D] C Int Bool
  At that point it would be /stupid/ to interact it
  with the inert [W] C Int b in the inert set; after all,
  it's the very constraint from which the [D] C Int Bool
  was split!  We can avoid this by not doing improvement
  on [W] constraints. This came up in #12860.
-}

maybeEmitShadow :: InertCans -> Ct -> TcS Ct
-- See Note [The improvement story and derived shadows]
maybeEmitShadow ics ct
  | let ev = ctEvidence ct
  , CtWanted { ctev_pred = pred, ctev_loc = loc
             , ctev_nosh = WDeriv } <- ev
  , shouldSplitWD (inert_eqs ics) (inert_funeqs ics) ct
  = do { traceTcS "Emit derived shadow" (ppr ct)
       ; let derived_ev = CtDerived { ctev_pred = pred
                                    , ctev_loc  = loc }
             shadow_ct = ct { cc_ev = derived_ev }
               -- Te shadow constraint keeps the canonical shape.
               -- This just saves work, but is sometimes important;
               -- see Note [Keep CDictCan shadows as CDictCan]
       ; emitWork [shadow_ct]

       ; let ev' = ev { ctev_nosh = WOnly }
             ct' = ct { cc_ev = ev' }
                 -- Record that it now has a shadow
                 -- This is /the/ place we set the flag to WOnly
       ; return ct' }

  | otherwise
  = return ct

shouldSplitWD :: InertEqs -> FunEqMap EqualCtList -> Ct -> Bool
-- Precondition: 'ct' is [WD], and is inert
-- True <=> we should split ct ito [W] and [D] because
--          the inert_eqs can make progress on the [D]
-- See Note [Splitting WD constraints]

shouldSplitWD inert_eqs fun_eqs (CDictCan { cc_tyargs = tys })
  = should_split_match_args inert_eqs fun_eqs tys
    -- NB True: ignore coercions
    -- See Note [Splitting WD constraints]

shouldSplitWD inert_eqs fun_eqs (CEqCan { cc_lhs = TyVarLHS tv, cc_rhs = ty
                                        , cc_eq_rel = eq_rel })
  =  tv `elemDVarEnv` inert_eqs
  || anyRewritableCanEqLHS eq_rel (canRewriteTv inert_eqs) (canRewriteTyFam fun_eqs) ty
  -- NB False: do not ignore casts and coercions
  -- See Note [Splitting WD constraints]

shouldSplitWD inert_eqs fun_eqs (CEqCan { cc_ev = ev, cc_eq_rel = eq_rel })
  = anyRewritableCanEqLHS eq_rel (canRewriteTv inert_eqs) (canRewriteTyFam fun_eqs)
                          (ctEvPred ev)

shouldSplitWD inert_eqs fun_eqs (CIrredCan { cc_ev = ev })
  = anyRewritableCanEqLHS (ctEvEqRel ev) (canRewriteTv inert_eqs)
                          (canRewriteTyFam fun_eqs) (ctEvPred ev)

shouldSplitWD _ _ _ = False   -- No point in splitting otherwise

should_split_match_args :: InertEqs -> FunEqMap EqualCtList -> [TcType] -> Bool
-- True if the inert_eqs can rewrite anything in the argument types
should_split_match_args inert_eqs fun_eqs tys
  = any (anyRewritableCanEqLHS NomEq (canRewriteTv inert_eqs) (canRewriteTyFam fun_eqs)) tys
    -- See Note [Splitting WD constraints]

canRewriteTv :: InertEqs -> EqRel -> TyVar -> Bool
canRewriteTv inert_eqs eq_rel tv
  | Just (EqualCtList (ct :| _)) <- lookupDVarEnv inert_eqs tv
  , CEqCan { cc_eq_rel = eq_rel1 } <- ct
  = eq_rel1 `eqCanRewrite` eq_rel
  | otherwise
  = False

canRewriteTyFam :: FunEqMap EqualCtList -> EqRel -> TyCon -> [Type] -> Bool
canRewriteTyFam fun_eqs eq_rel tf args
  | Just (EqualCtList (ct :| _)) <- findFunEq fun_eqs tf args
  , CEqCan { cc_eq_rel = eq_rel1 } <- ct
  = eq_rel1 `eqCanRewrite` eq_rel
  | otherwise
  = False

isImprovable :: CtEvidence -> Bool
-- See Note [Do not do improvement for WOnly]
isImprovable (CtWanted { ctev_nosh = WOnly }) = False
isImprovable _                                = True


{- *********************************************************************
*                                                                      *
                   Inert equalities
*                                                                      *
********************************************************************* -}

addTyEq :: InertEqs -> TcTyVar -> Ct -> InertEqs
addTyEq old_eqs tv ct
  = extendDVarEnv_C add_eq old_eqs tv (unitEqualCtList ct)
  where
    add_eq old_eqs _ = addToEqualCtList ct old_eqs

addCanFunEq :: FunEqMap EqualCtList -> TyCon -> [TcType] -> Ct
            -> FunEqMap EqualCtList
addCanFunEq old_eqs fun_tc fun_args ct
  = alterTcApp old_eqs fun_tc fun_args upd
  where
    upd (Just old_equal_ct_list) = Just $ addToEqualCtList ct old_equal_ct_list
    upd Nothing                  = Just $ unitEqualCtList ct

foldTyEqs :: (Ct -> b -> b) -> InertEqs -> b -> b
foldTyEqs k eqs z
  = foldDVarEnv (\(EqualCtList cts) z -> foldr k z cts) z eqs

findTyEqs :: InertCans -> TyVar -> [Ct]
findTyEqs icans tv = maybe [] id (fmap @Maybe equalCtListToList $
                                  lookupDVarEnv (inert_eqs icans) tv)

delEq :: InertCans -> CanEqLHS -> TcType -> InertCans
delEq ic lhs rhs = case lhs of
    TyVarLHS tv
      -> ic { inert_eqs = alterDVarEnv upd (inert_eqs ic) tv }
    TyFamLHS tf args
      -> ic { inert_funeqs = alterTcApp (inert_funeqs ic) tf args upd }
  where
    isThisOne :: Ct -> Bool
    isThisOne (CEqCan { cc_rhs = t1 }) = tcEqTypeNoKindCheck rhs t1
    isThisOne other = pprPanic "delEq" (ppr lhs $$ ppr ic $$ ppr other)

    upd :: Maybe EqualCtList -> Maybe EqualCtList
    upd (Just eq_ct_list) = filterEqualCtList (not . isThisOne) eq_ct_list
    upd Nothing           = Nothing

findEq :: InertCans -> CanEqLHS -> [Ct]
findEq icans (TyVarLHS tv) = findTyEqs icans tv
findEq icans (TyFamLHS fun_tc fun_args)
  = maybe [] id (fmap @Maybe equalCtListToList $
                 findFunEq (inert_funeqs icans) fun_tc fun_args)

{- *********************************************************************
*                                                                      *
                   Inert instances: inert_insts
*                                                                      *
********************************************************************* -}

addInertForAll :: QCInst -> TcS ()
-- Add a local Given instance, typically arising from a type signature
addInertForAll new_qci
  = do { ics  <- getInertCans
       ; ics1 <- add_qci ics

       -- Update given equalities. C.f updateGivenEqs
       ; tclvl <- getTcLevel
       ; let pred         = qci_pred new_qci
             not_equality = isClassPred pred && not (isEqPred pred)
                  -- True <=> definitely not an equality
                  -- A qci_pred like (f a) might be an equality

             ics2 | not_equality = ics1
                  | otherwise    = ics1 { inert_given_eq_lvl = tclvl
                                        , inert_given_eqs    = True }

       ; setInertCans ics2 }
  where
    add_qci :: InertCans -> TcS InertCans
    -- See Note [Do not add duplicate quantified instances]
    add_qci ics@(IC { inert_insts = qcis })
      | any same_qci qcis
      = do { traceTcS "skipping duplicate quantified instance" (ppr new_qci)
           ; return ics }

      | otherwise
      = do { traceTcS "adding new inert quantified instance" (ppr new_qci)
           ; return (ics { inert_insts = new_qci : qcis }) }

    same_qci old_qci = tcEqType (ctEvPred (qci_ev old_qci))
                                (ctEvPred (qci_ev new_qci))

{- Note [Do not add duplicate quantified instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (#15244):

  f :: (C g, D g) => ....
  class S g => C g where ...
  class S g => D g where ...
  class (forall a. Eq a => Eq (g a)) => S g where ...

Then in f's RHS there are two identical quantified constraints
available, one via the superclasses of C and one via the superclasses
of D.  The two are identical, and it seems wrong to reject the program
because of that. But without doing duplicate-elimination we will have
two matching QCInsts when we try to solve constraints arising from f's
RHS.

The simplest thing is simply to eliminate duplicates, which we do here.
-}

{- *********************************************************************
*                                                                      *
                  Adding an inert
*                                                                      *
************************************************************************

Note [Adding an equality to the InertCans]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When adding an equality to the inerts:

* Split [WD] into [W] and [D] if the inerts can rewrite the latter;
  done by maybeEmitShadow.

* Kick out any constraints that can be rewritten by the thing
  we are adding.  Done by kickOutRewritable.

* Note that unifying a:=ty, is like adding [G] a~ty; just use
  kickOutRewritable with Nominal, Given.  See kickOutAfterUnification.
-}

addInertCan :: Ct -> TcS ()
-- Precondition: item /is/ canonical
-- See Note [Adding an equality to the InertCans]
addInertCan ct
  = do { traceTcS "addInertCan {" $
         text "Trying to insert new inert item:" <+> ppr ct

       ; ics <- getInertCans
       ; ct  <- maybeEmitShadow ics ct
       ; ics <- maybeKickOut ics ct
       ; tclvl <- getTcLevel
       ; setInertCans (add_item tclvl ics ct)

       ; traceTcS "addInertCan }" $ empty }

maybeKickOut :: InertCans -> Ct -> TcS InertCans
-- For a CEqCan, kick out any inert that can be rewritten by the CEqCan
maybeKickOut ics ct
  | CEqCan { cc_lhs = lhs, cc_ev = ev, cc_eq_rel = eq_rel } <- ct
  = do { (_, ics') <- kickOutRewritable (ctEvFlavour ev, eq_rel) lhs ics
       ; return ics' }
  | otherwise
  = return ics

add_item :: TcLevel -> InertCans -> Ct -> InertCans
add_item tc_lvl
         ics@(IC { inert_funeqs = funeqs, inert_eqs = eqs })
         item@(CEqCan { cc_lhs = lhs })
  = updateGivenEqs tc_lvl item $
    case lhs of
       TyFamLHS tc tys -> ics { inert_funeqs = addCanFunEq funeqs tc tys item }
       TyVarLHS tv     -> ics { inert_eqs    = addTyEq eqs tv item }

add_item tc_lvl ics@(IC { inert_irreds = irreds }) item@(CIrredCan {})
  = updateGivenEqs tc_lvl item $   -- An Irred might turn out to be an
                                 -- equality, so we play safe
    ics { inert_irreds = irreds `Bag.snocBag` item }

add_item _ ics item@(CDictCan { cc_class = cls, cc_tyargs = tys })
  = ics { inert_dicts = addDict (inert_dicts ics) cls tys item }

add_item _ _ item
  = pprPanic "upd_inert set: can't happen! Inserting " $
    ppr item   -- Can't be CNonCanonical because they only land in inert_irreds

updateGivenEqs :: TcLevel -> Ct -> InertCans -> InertCans
-- Set the inert_given_eq_level to the current level (tclvl)
-- if the constraint is a given equality that should prevent
-- filling in an outer unification variable.
-- See See Note [Tracking Given equalities]
updateGivenEqs tclvl ct inerts@(IC { inert_given_eq_lvl = ge_lvl })
  | not (isGivenCt ct) = inerts
  | not_equality ct    = inerts -- See Note [Let-bound skolems]
  | otherwise          = inerts { inert_given_eq_lvl = ge_lvl'
                                , inert_given_eqs    = True }
  where
    ge_lvl' | mentionsOuterVar tclvl (ctEvidence ct)
              -- Includes things like (c a), which *might* be an equality
            = tclvl
            | otherwise
            = ge_lvl

    not_equality :: Ct -> Bool
    -- True <=> definitely not an equality of any kind
    --          except for a let-bound skolem, which doesn't count
    --          See Note [Let-bound skolems]
    -- NB: no need to spot the boxed CDictCan (a ~ b) because its
    --     superclass (a ~# b) will be a CEqCan
    not_equality (CEqCan { cc_lhs = TyVarLHS tv }) = not (isOuterTyVar tclvl tv)
    not_equality (CDictCan {})                     = True
    not_equality _                                 = False

-----------------------------------------
kickOutRewritable  :: CtFlavourRole  -- Flavour/role of the equality that
                                      -- is being added to the inert set
                   -> CanEqLHS        -- The new equality is lhs ~ ty
                   -> InertCans
                   -> TcS (Int, InertCans)
kickOutRewritable new_fr new_lhs ics
  = do { let (kicked_out, ics') = kick_out_rewritable new_fr new_lhs ics
             n_kicked = workListSize kicked_out

       ; unless (n_kicked == 0) $
         do { updWorkListTcS (appendWorkList kicked_out)

              -- The famapp-cache contains Given evidence from the inert set.
              -- If we're kicking out Givens, we need to remove this evidence
              -- from the cache, too.
            ; let kicked_given_ev_vars =
                    [ ev_var | ct <- wl_eqs kicked_out
                             , CtGiven { ctev_evar = ev_var } <- [ctEvidence ct] ]
            ; when (new_fr `eqCanRewriteFR` (Given, NomEq) &&
                   -- if this isn't true, no use looking through the constraints
                    not (null kicked_given_ev_vars)) $
              do { traceTcS "Given(s) have been kicked out; drop from famapp-cache"
                            (ppr kicked_given_ev_vars)
                 ; dropFromFamAppCache (mkVarSet kicked_given_ev_vars) }

            ; csTraceTcS $
              hang (text "Kick out, lhs =" <+> ppr new_lhs)
                 2 (vcat [ text "n-kicked =" <+> int n_kicked
                         , text "kicked_out =" <+> ppr kicked_out
                         , text "Residual inerts =" <+> ppr ics' ]) }

       ; return (n_kicked, ics') }

kick_out_rewritable :: CtFlavourRole  -- Flavour/role of the equality that
                                      -- is being added to the inert set
                    -> CanEqLHS       -- The new equality is lhs ~ ty
                    -> InertCans
                    -> (WorkList, InertCans)
-- See Note [kickOutRewritable]
kick_out_rewritable new_fr new_lhs
                    ics@(IC { inert_eqs      = tv_eqs
                            , inert_dicts    = dictmap
                            , inert_funeqs   = funeqmap
                            , inert_irreds   = irreds
                            , inert_insts    = old_insts })
  | not (new_fr `eqMayRewriteFR` new_fr)
  = (emptyWorkList, ics)
        -- If new_fr can't rewrite itself, it can't rewrite
        -- anything else, so no need to kick out anything.
        -- (This is a common case: wanteds can't rewrite wanteds)
        -- Lemma (L2) in Note [Extending the inert equalities]

  | otherwise
  = (kicked_out, inert_cans_in)
  where
    -- inert_safehask stays unchanged; is that right?
    inert_cans_in = ics { inert_eqs      = tv_eqs_in
                        , inert_dicts    = dicts_in
                        , inert_funeqs   = feqs_in
                        , inert_irreds   = irs_in
                        , inert_insts    = insts_in }

    kicked_out :: WorkList
    -- NB: use extendWorkList to ensure that kicked-out equalities get priority
    -- See Note [Prioritise equalities] (Kick-out).
    -- The irreds may include non-canonical (hetero-kinded) equality
    -- constraints, which perhaps may have become soluble after new_lhs
    -- is substituted; ditto the dictionaries, which may include (a~b)
    -- or (a~~b) constraints.
    kicked_out = foldr extendWorkListCt
                          (emptyWorkList { wl_eqs = tv_eqs_out ++ feqs_out })
                          ((dicts_out `andCts` irs_out)
                            `extendCtsList` insts_out)

    (tv_eqs_out, tv_eqs_in) = foldDVarEnv (kick_out_eqs extend_tv_eqs)
                                          ([], emptyDVarEnv) tv_eqs
    (feqs_out,   feqs_in)   = foldFunEqs  (kick_out_eqs extend_fun_eqs)
                                          funeqmap ([], emptyFunEqs)
    (dicts_out,  dicts_in)  = partitionDicts   kick_out_ct dictmap
    (irs_out,    irs_in)    = partitionBag     kick_out_ct irreds
      -- Kick out even insolubles: See Note [Rewrite insolubles]
      -- Of course we must kick out irreducibles like (c a), in case
      -- we can rewrite 'c' to something more useful

    -- Kick-out for inert instances
    -- See Note [Quantified constraints] in GHC.Tc.Solver.Canonical
    insts_out :: [Ct]
    insts_in  :: [QCInst]
    (insts_out, insts_in)
       | fr_may_rewrite (Given, NomEq)  -- All the insts are Givens
       = partitionWith kick_out_qci old_insts
       | otherwise
       = ([], old_insts)
    kick_out_qci qci
      | let ev = qci_ev qci
      , fr_can_rewrite_ty NomEq (ctEvPred (qci_ev qci))
      = Left (mkNonCanonical ev)
      | otherwise
      = Right qci

    (_, new_role) = new_fr

    fr_tv_can_rewrite_ty :: TyVar -> EqRel -> Type -> Bool
    fr_tv_can_rewrite_ty new_tv role ty
      = anyRewritableTyVar True role can_rewrite ty
                  -- True: ignore casts and coercions
      where
        can_rewrite :: EqRel -> TyVar -> Bool
        can_rewrite old_role tv = new_role `eqCanRewrite` old_role && tv == new_tv

    fr_tf_can_rewrite_ty :: TyCon -> [TcType] -> EqRel -> Type -> Bool
    fr_tf_can_rewrite_ty new_tf new_tf_args role ty
      = anyRewritableTyFamApp role can_rewrite ty
      where
        can_rewrite :: EqRel -> TyCon -> [TcType] -> Bool
        can_rewrite old_role old_tf old_tf_args
          = new_role `eqCanRewrite` old_role &&
            tcEqTyConApps new_tf new_tf_args old_tf old_tf_args
              -- it's possible for old_tf_args to have too many. This is fine;
              -- we'll only check what we need to.

    {-# INLINE fr_can_rewrite_ty #-}   -- perform the check here only once
    fr_can_rewrite_ty :: EqRel -> Type -> Bool
    fr_can_rewrite_ty = case new_lhs of
      TyVarLHS new_tv             -> fr_tv_can_rewrite_ty new_tv
      TyFamLHS new_tf new_tf_args -> fr_tf_can_rewrite_ty new_tf new_tf_args

    fr_may_rewrite :: CtFlavourRole -> Bool
    fr_may_rewrite fs = new_fr `eqMayRewriteFR` fs
        -- Can the new item rewrite the inert item?

    {-# INLINE kick_out_ct #-}   -- perform case on new_lhs here only once
    kick_out_ct :: Ct -> Bool
    -- Kick it out if the new CEqCan can rewrite the inert one
    -- See Note [kickOutRewritable]
    kick_out_ct = case new_lhs of
      TyVarLHS new_tv -> \ct -> let fs@(_,role) = ctFlavourRole ct in
                                fr_may_rewrite fs
                             && fr_tv_can_rewrite_ty new_tv role (ctPred ct)
      TyFamLHS new_tf new_tf_args
        -> \ct -> let fs@(_, role) = ctFlavourRole ct in
                  fr_may_rewrite fs
               && fr_tf_can_rewrite_ty new_tf new_tf_args role (ctPred ct)

    extend_tv_eqs :: InertEqs -> CanEqLHS -> EqualCtList -> InertEqs
    extend_tv_eqs eqs (TyVarLHS tv) cts = extendDVarEnv eqs tv cts
    extend_tv_eqs eqs other _cts = pprPanic "extend_tv_eqs" (ppr eqs $$ ppr other)

    extend_fun_eqs :: FunEqMap EqualCtList -> CanEqLHS -> EqualCtList
                   -> FunEqMap EqualCtList
    extend_fun_eqs eqs (TyFamLHS fam_tc fam_args) cts
      = insertTcApp eqs fam_tc fam_args cts
    extend_fun_eqs eqs other _cts = pprPanic "extend_fun_eqs" (ppr eqs $$ ppr other)

    kick_out_eqs :: (container -> CanEqLHS -> EqualCtList -> container)
                 -> EqualCtList -> ([Ct], container)
                 -> ([Ct], container)
    kick_out_eqs extend eqs (acc_out, acc_in)
      = (eqs_out `chkAppend` acc_out, case listToEqualCtList eqs_in of
            Nothing -> acc_in
            Just eqs_in_ecl@(EqualCtList (eq1 :| _))
                    -> extend acc_in (cc_lhs eq1) eqs_in_ecl)
      where
        (eqs_out, eqs_in) = partition kick_out_eq (equalCtListToList eqs)

    -- Implements criteria K1-K3 in Note [Extending the inert equalities]
    kick_out_eq (CEqCan { cc_lhs = lhs, cc_rhs = rhs_ty
                        , cc_ev = ev, cc_eq_rel = eq_rel })
      | not (fr_may_rewrite fs)
      = False  -- Keep it in the inert set if the new thing can't rewrite it

      -- Below here (fr_may_rewrite fs) is True
      | fr_can_rewrite_ty eq_rel (canEqLHSType lhs) = True   -- (K1)
         -- The above check redundantly checks the role & flavour,
         -- but it's very convenient

      | kick_out_for_inertness    = True
      | kick_out_for_completeness = True
      | otherwise                 = False

      where
        fs = (ctEvFlavour ev, eq_rel)
        kick_out_for_inertness
          =        (fs `eqMayRewriteFR` fs)       -- (K2a)
            && not (fs `eqMayRewriteFR` new_fr)   -- (K2b)
            && fr_can_rewrite_ty eq_rel rhs_ty    -- (K2d)
            -- (K2c) is guaranteed by the first guard of keep_eq

        kick_out_for_completeness  -- (K3) and Note [K3: completeness of solving]
          = case eq_rel of
              NomEq  -> rhs_ty `eqType` canEqLHSType new_lhs -- (K3a)
              ReprEq -> is_can_eq_lhs_head new_lhs rhs_ty    -- (K3b)

    kick_out_eq ct = pprPanic "keep_eq" (ppr ct)

    is_can_eq_lhs_head (TyVarLHS tv) = go
      where
        go (Rep.TyVarTy tv')   = tv == tv'
        go (Rep.AppTy fun _)   = go fun
        go (Rep.CastTy ty _)   = go ty
        go (Rep.TyConApp {})   = False
        go (Rep.LitTy {})      = False
        go (Rep.ForAllTy {})   = False
        go (Rep.FunTy {})      = False
        go (Rep.CoercionTy {}) = False
    is_can_eq_lhs_head (TyFamLHS fun_tc fun_args) = go
      where
        go (Rep.TyVarTy {})       = False
        go (Rep.AppTy {})         = False  -- no TyConApp to the left of an AppTy
        go (Rep.CastTy ty _)      = go ty
        go (Rep.TyConApp tc args) = tcEqTyConApps fun_tc fun_args tc args
        go (Rep.LitTy {})         = False
        go (Rep.ForAllTy {})      = False
        go (Rep.FunTy {})         = False
        go (Rep.CoercionTy {})    = False

kickOutAfterUnification :: TcTyVar -> TcS Int
kickOutAfterUnification new_tv
  = do { ics <- getInertCans
       ; (n_kicked, ics2) <- kickOutRewritable (Given,NomEq)
                                                 (TyVarLHS new_tv) ics
                     -- Given because the tv := xi is given; NomEq because
                     -- only nominal equalities are solved by unification

       ; setInertCans ics2
       ; return n_kicked }

-- See Wrinkle (2) in Note [Equalities with incompatible kinds] in GHC.Tc.Solver.Canonical
kickOutAfterFillingCoercionHole :: CoercionHole -> Coercion -> TcS ()
kickOutAfterFillingCoercionHole hole filled_co
  = do { ics <- getInertCans
       ; let (kicked_out, ics') = kick_out ics
             n_kicked           = workListSize kicked_out

       ; unless (n_kicked == 0) $
         do { updWorkListTcS (appendWorkList kicked_out)
            ; csTraceTcS $
              hang (text "Kick out, hole =" <+> ppr hole)
                 2 (vcat [ text "n-kicked =" <+> int n_kicked
                         , text "kicked_out =" <+> ppr kicked_out
                         , text "Residual inerts =" <+> ppr ics' ]) }

       ; setInertCans ics' }
  where
    holes_of_co = coercionHolesOfCo filled_co

    kick_out :: InertCans -> (WorkList, InertCans)
    kick_out ics@(IC { inert_irreds = irreds })
      = let (to_kick, to_keep) = partitionBagWith kick_ct irreds

            kicked_out = extendWorkListCts (bagToList to_kick) emptyWorkList
            ics'       = ics { inert_irreds = to_keep }
        in
        (kicked_out, ics')

    kick_ct :: Ct -> Either Ct Ct
         -- Left: kick out; Right: keep. But even if we keep, we may need
         -- to update the set of blocking holes
    kick_ct ct@(CIrredCan { cc_status = BlockedCIS holes })
      | hole `elementOfUniqSet` holes
      = let new_holes = holes `delOneFromUniqSet` hole
                              `unionUniqSets` holes_of_co
            updated_ct = ct { cc_status = BlockedCIS new_holes }
        in
        if isEmptyUniqSet new_holes
        then Left updated_ct
        else Right updated_ct
    kick_ct other = Right other

{- Note [kickOutRewritable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also Note [inert_eqs: the inert equalities].

When we add a new inert equality (lhs ~N ty) to the inert set,
we must kick out any inert items that could be rewritten by the
new equality, to maintain the inert-set invariants.

  - We want to kick out an existing inert constraint if
    a) the new constraint can rewrite the inert one
    b) 'lhs' is free in the inert constraint (so that it *will*)
       rewrite it if we kick it out.

    For (b) we use anyRewritableCanLHS, which examines the types /and
    kinds/ that are directly visible in the type. Hence
    we will have exposed all the rewriting we care about to make the
    most precise kinds visible for matching classes etc. No need to
    kick out constraints that mention type variables whose kinds
    contain this LHS!

  - A Derived equality can kick out [D] constraints in inert_eqs,
    inert_dicts, inert_irreds etc.

  - We don't kick out constraints from inert_solved_dicts, and
    inert_solved_funeqs optimistically. But when we lookup we have to
    take the substitution into account

NB: we could in principle avoid kick-out:
  a) When unifying a meta-tyvar from an outer level, because
     then the entire implication will be iterated; see
     Note [The Unification Level Flag]

  b) For Givens, after a unification.  By (GivenInv) in GHC.Tc.Utils.TcType
     Note [TcLevel invariants], a Given can't include a meta-tyvar from
     its own level, so it falls under (a).  Of course, we must still
     kick out Givens when adding a new non-unificaiton Given.

But kicking out more vigorously may lead to earlier unification and fewer
iterations, so we don't take advantage of these possibilities.

Note [Rewrite insolubles]
~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have an insoluble alpha ~ [alpha], which is insoluble
because an occurs check.  And then we unify alpha := [Int].  Then we
really want to rewrite the insoluble to [Int] ~ [[Int]].  Now it can
be decomposed.  Otherwise we end up with a "Can't match [Int] ~
[[Int]]" which is true, but a bit confusing because the outer type
constructors match.

Hence:
 * In the main simplifier loops in GHC.Tc.Solver (solveWanteds,
   simpl_loop), we feed the insolubles in solveSimpleWanteds,
   so that they get rewritten (albeit not solved).

 * We kick insolubles out of the inert set, if they can be
   rewritten (see GHC.Tc.Solver.Monad.kick_out_rewritable)

 * We rewrite those insolubles in GHC.Tc.Solver.Canonical.
   See Note [Make sure that insolubles are fully rewritten]
-}



--------------
addInertSafehask :: InertCans -> Ct -> InertCans
addInertSafehask ics item@(CDictCan { cc_class = cls, cc_tyargs = tys })
  = ics { inert_safehask = addDict (inert_dicts ics) cls tys item }

addInertSafehask _ item
  = pprPanic "addInertSafehask: can't happen! Inserting " $ ppr item

insertSafeOverlapFailureTcS :: InstanceWhat -> Ct -> TcS ()
-- See Note [Safe Haskell Overlapping Instances Implementation] in GHC.Tc.Solver
insertSafeOverlapFailureTcS what item
  | safeOverlap what = return ()
  | otherwise        = updInertCans (\ics -> addInertSafehask ics item)

getSafeOverlapFailures :: TcS Cts
-- See Note [Safe Haskell Overlapping Instances Implementation] in GHC.Tc.Solver
getSafeOverlapFailures
 = do { IC { inert_safehask = safehask } <- getInertCans
      ; return $ foldDicts consCts safehask emptyCts }

--------------
addSolvedDict :: InstanceWhat -> CtEvidence -> Class -> [Type] -> TcS ()
-- Conditionally add a new item in the solved set of the monad
-- See Note [Solved dictionaries]
addSolvedDict what item cls tys
  | isWanted item
  , instanceReturnsDictCon what
  = do { traceTcS "updSolvedSetTcs:" $ ppr item
       ; updInertTcS $ \ ics ->
             ics { inert_solved_dicts = addDict (inert_solved_dicts ics) cls tys item } }
  | otherwise
  = return ()

getSolvedDicts :: TcS (DictMap CtEvidence)
getSolvedDicts = do { ics <- getTcSInerts; return (inert_solved_dicts ics) }

setSolvedDicts :: DictMap CtEvidence -> TcS ()
setSolvedDicts solved_dicts
  = updInertTcS $ \ ics ->
    ics { inert_solved_dicts = solved_dicts }


{- *********************************************************************
*                                                                      *
                  Other inert-set operations
*                                                                      *
********************************************************************* -}

updInertTcS :: (InertSet -> InertSet) -> TcS ()
-- Modify the inert set with the supplied function
updInertTcS upd_fn
  = do { is_var <- getTcSInertsRef
       ; wrapTcS (do { curr_inert <- TcM.readTcRef is_var
                     ; TcM.writeTcRef is_var (upd_fn curr_inert) }) }

getInertCans :: TcS InertCans
getInertCans = do { inerts <- getTcSInerts; return (inert_cans inerts) }

setInertCans :: InertCans -> TcS ()
setInertCans ics = updInertTcS $ \ inerts -> inerts { inert_cans = ics }

updRetInertCans :: (InertCans -> (a, InertCans)) -> TcS a
-- Modify the inert set with the supplied function
updRetInertCans upd_fn
  = do { is_var <- getTcSInertsRef
       ; wrapTcS (do { inerts <- TcM.readTcRef is_var
                     ; let (res, cans') = upd_fn (inert_cans inerts)
                     ; TcM.writeTcRef is_var (inerts { inert_cans = cans' })
                     ; return res }) }

updInertCans :: (InertCans -> InertCans) -> TcS ()
-- Modify the inert set with the supplied function
updInertCans upd_fn
  = updInertTcS $ \ inerts -> inerts { inert_cans = upd_fn (inert_cans inerts) }

updInertDicts :: (DictMap Ct -> DictMap Ct) -> TcS ()
-- Modify the inert set with the supplied function
updInertDicts upd_fn
  = updInertCans $ \ ics -> ics { inert_dicts = upd_fn (inert_dicts ics) }

updInertSafehask :: (DictMap Ct -> DictMap Ct) -> TcS ()
-- Modify the inert set with the supplied function
updInertSafehask upd_fn
  = updInertCans $ \ ics -> ics { inert_safehask = upd_fn (inert_safehask ics) }

updInertIrreds :: (Cts -> Cts) -> TcS ()
-- Modify the inert set with the supplied function
updInertIrreds upd_fn
  = updInertCans $ \ ics -> ics { inert_irreds = upd_fn (inert_irreds ics) }

getInertEqs :: TcS (DTyVarEnv EqualCtList)
getInertEqs = do { inert <- getInertCans; return (inert_eqs inert) }

getInnermostGivenEqLevel :: TcS TcLevel
getInnermostGivenEqLevel = do { inert <- getInertCans
                              ; return (inert_given_eq_lvl inert) }

getInertInsols :: TcS Cts
-- Returns insoluble equality constraints
-- specifically including Givens
getInertInsols = do { inert <- getInertCans
                    ; return (filterBag insolubleEqCt (inert_irreds inert)) }

getInertGivens :: TcS [Ct]
-- Returns the Given constraints in the inert set
getInertGivens
  = do { inerts <- getInertCans
       ; let all_cts = foldDicts (:) (inert_dicts inerts)
                     $ foldFunEqs (\ecl out -> equalCtListToList ecl ++ out)
                                  (inert_funeqs inerts)
                     $ concatMap equalCtListToList (dVarEnvElts (inert_eqs inerts))
       ; return (filter isGivenCt all_cts) }

getPendingGivenScs :: TcS [Ct]
-- Find all inert Given dictionaries, or quantified constraints,
--     whose cc_pend_sc flag is True
--     and that belong to the current level
-- Set their cc_pend_sc flag to False in the inert set, and return that Ct
getPendingGivenScs = do { lvl <- getTcLevel
                        ; updRetInertCans (get_sc_pending lvl) }

get_sc_pending :: TcLevel -> InertCans -> ([Ct], InertCans)
get_sc_pending this_lvl ic@(IC { inert_dicts = dicts, inert_insts = insts })
  = ASSERT2( all isGivenCt sc_pending, ppr sc_pending )
       -- When getPendingScDics is called,
       -- there are never any Wanteds in the inert set
    (sc_pending, ic { inert_dicts = dicts', inert_insts = insts' })
  where
    sc_pending = sc_pend_insts ++ sc_pend_dicts

    sc_pend_dicts = foldDicts get_pending dicts []
    dicts' = foldr add dicts sc_pend_dicts

    (sc_pend_insts, insts') = mapAccumL get_pending_inst [] insts

    get_pending :: Ct -> [Ct] -> [Ct]  -- Get dicts with cc_pend_sc = True
                                       -- but flipping the flag
    get_pending dict dicts
        | Just dict' <- isPendingScDict dict
        , belongs_to_this_level (ctEvidence dict)
        = dict' : dicts
        | otherwise
        = dicts

    add :: Ct -> DictMap Ct -> DictMap Ct
    add ct@(CDictCan { cc_class = cls, cc_tyargs = tys }) dicts
        = addDict dicts cls tys ct
    add ct _ = pprPanic "getPendingScDicts" (ppr ct)

    get_pending_inst :: [Ct] -> QCInst -> ([Ct], QCInst)
    get_pending_inst cts qci@(QCI { qci_ev = ev })
       | Just qci' <- isPendingScInst qci
       , belongs_to_this_level ev
       = (CQuantCan qci' : cts, qci')
       | otherwise
       = (cts, qci)

    belongs_to_this_level ev = ctLocLevel (ctEvLoc ev) == this_lvl
    -- We only want Givens from this level; see (3a) in
    -- Note [The superclass story] in GHC.Tc.Solver.Canonical

getUnsolvedInerts :: TcS ( Bag Implication
                         , Cts )   -- All simple constraints
-- Return all the unsolved [Wanted] or [Derived] constraints
--
-- Post-condition: the returned simple constraints are all fully zonked
--                     (because they come from the inert set)
--                 the unsolved implics may not be
getUnsolvedInerts
 = do { IC { inert_eqs    = tv_eqs
           , inert_funeqs = fun_eqs
           , inert_irreds = irreds
           , inert_dicts  = idicts
           } <- getInertCans

      ; let unsolved_tv_eqs  = foldTyEqs add_if_unsolved tv_eqs emptyCts
            unsolved_fun_eqs = foldFunEqs add_if_unsolveds fun_eqs emptyCts
            unsolved_irreds  = Bag.filterBag is_unsolved irreds
            unsolved_dicts   = foldDicts add_if_unsolved idicts emptyCts
            unsolved_others  = unsolved_irreds `unionBags` unsolved_dicts

      ; implics <- getWorkListImplics

      ; traceTcS "getUnsolvedInerts" $
        vcat [ text " tv eqs =" <+> ppr unsolved_tv_eqs
             , text "fun eqs =" <+> ppr unsolved_fun_eqs
             , text "others =" <+> ppr unsolved_others
             , text "implics =" <+> ppr implics ]

      ; return ( implics, unsolved_tv_eqs `unionBags`
                          unsolved_fun_eqs `unionBags`
                          unsolved_others) }
  where
    add_if_unsolved :: Ct -> Cts -> Cts
    add_if_unsolved ct cts | is_unsolved ct = ct `consCts` cts
                           | otherwise      = cts

    add_if_unsolveds :: EqualCtList -> Cts -> Cts
    add_if_unsolveds new_cts old_cts = foldr add_if_unsolved old_cts
                                             (equalCtListToList new_cts)

    is_unsolved ct = not (isGivenCt ct)   -- Wanted or Derived

getHasGivenEqs :: TcLevel           -- TcLevel of this implication
               -> TcS ( HasGivenEqs -- are there Given equalities?
                      , Cts )       -- Insoluble equalities arising from givens
-- See Note [Tracking Given equalities]
getHasGivenEqs tclvl
  = do { inerts@(IC { inert_irreds       = irreds
                    , inert_given_eqs    = given_eqs
                    , inert_given_eq_lvl = ge_lvl })
              <- getInertCans
       ; let insols = filterBag insolubleEqCt irreds
                      -- Specifically includes ones that originated in some
                      -- outer context but were refined to an insoluble by
                      -- a local equality; so do /not/ add ct_given_here.

             -- See Note [HasGivenEqs] in GHC.Tc.Types.Constraint, and
             -- Note [Tracking Given equalities] in this module
             has_ge | ge_lvl == tclvl = MaybeGivenEqs
                    | given_eqs       = LocalGivenEqs
                    | otherwise       = NoGivenEqs

       ; traceTcS "getHasGivenEqs" $
         vcat [ text "given_eqs:" <+> ppr given_eqs
              , text "ge_lvl:" <+> ppr ge_lvl
              , text "ambient level:" <+> ppr tclvl
              , text "Inerts:" <+> ppr inerts
              , text "Insols:" <+> ppr insols]
       ; return (has_ge, insols) }

mentionsOuterVar :: TcLevel -> CtEvidence -> Bool
mentionsOuterVar tclvl ev
  = anyFreeVarsOfType (isOuterTyVar tclvl) $
    ctEvPred ev

isOuterTyVar :: TcLevel -> TyCoVar -> Bool
-- True of a type variable that comes from a
-- shallower level than the ambient level (tclvl)
isOuterTyVar tclvl tv
  | isTyVar tv = ASSERT2( not (isTouchableMetaTyVar tclvl tv), ppr tv <+> ppr tclvl  )
                 tclvl `strictlyDeeperThan` tcTyVarLevel tv
    -- ASSERT: we are dealing with Givens here, and invariant (GivenInv) from
    -- Note Note [TcLevel invariants] in GHC.Tc.Utils.TcType ensures that there can't
    -- be a touchable meta tyvar.   If this wasn't true, you might worry that,
    -- at level 3, a meta-tv alpha[3] gets unified with skolem b[2], and thereby
    -- becomes "outer" even though its level numbers says it isn't.
  | otherwise  = False  -- Coercion variables; doesn't much matter

-- | Returns Given constraints that might,
-- potentially, match the given pred. This is used when checking to see if a
-- Given might overlap with an instance. See Note [Instance and Given overlap]
-- in "GHC.Tc.Solver.Interact"
matchableGivens :: CtLoc -> PredType -> InertSet -> Cts
matchableGivens loc_w pred_w (IS { inert_cans = inert_cans })
  = filterBag matchable_given all_relevant_givens
  where
    -- just look in class constraints and irreds. matchableGivens does get called
    -- for ~R constraints, but we don't need to look through equalities, because
    -- canonical equalities are used for rewriting. We'll only get caught by
    -- non-canonical -- that is, irreducible -- equalities.
    all_relevant_givens :: Cts
    all_relevant_givens
      | Just (clas, _) <- getClassPredTys_maybe pred_w
      = findDictsByClass (inert_dicts inert_cans) clas
        `unionBags` inert_irreds inert_cans
      | otherwise
      = inert_irreds inert_cans

    matchable_given :: Ct -> Bool
    matchable_given ct
      | CtGiven { ctev_loc = loc_g, ctev_pred = pred_g } <- ctEvidence ct
      = mightMatchLater pred_g loc_g pred_w loc_w

      | otherwise
      = False

mightMatchLater :: TcPredType -> CtLoc -> TcPredType -> CtLoc -> Bool
-- See Note [What might match later?]
mightMatchLater given_pred given_loc wanted_pred wanted_loc
  | prohibitedSuperClassSolve given_loc wanted_loc
  = False

  | SurelyApart <- tcUnifyTysFG bind_meta_tv [flattened_given] [flattened_wanted]
  = False

  | otherwise
  = True   -- safe answer
  where
    in_scope  = mkInScopeSet $ tyCoVarsOfTypes [given_pred, wanted_pred]

    -- NB: flatten both at the same time, so that we can share mappings
    -- from type family applications to variables, and also to guarantee
    -- that the fresh variables are really fresh between the given and
    -- the wanted.
    ([flattened_given, flattened_wanted], var_mapping)
      = flattenTysX in_scope [given_pred, wanted_pred]

    bind_meta_tv :: TcTyVar -> BindFlag
    -- Any meta tyvar may be unified later, so we treat it as
    -- bindable when unifying with givens. That ensures that we
    -- conservatively assume that a meta tyvar might get unified with
    -- something that matches the 'given', until demonstrated
    -- otherwise.  More info in Note [Instance and Given overlap]
    -- in GHC.Tc.Solver.Interact
    bind_meta_tv tv | is_meta_tv tv = BindMe

                    | Just (_fam_tc, fam_args) <- lookupVarEnv var_mapping tv
                    , anyFreeVarsOfTypes is_meta_tv fam_args
                    = BindMe

                    | otherwise     = Skolem

     -- CycleBreakerTvs really stands for a type family application in
     -- a given; these won't contain touchable meta-variables
    is_meta_tv = isMetaTyVar <&&> not . isCycleBreakerTyVar

prohibitedSuperClassSolve :: CtLoc -> CtLoc -> Bool
-- See Note [Solving superclass constraints] in GHC.Tc.TyCl.Instance
prohibitedSuperClassSolve from_loc solve_loc
  | GivenOrigin (InstSC given_size) <- ctLocOrigin from_loc
  , ScOrigin wanted_size <- ctLocOrigin solve_loc
  = given_size >= wanted_size
  | otherwise
  = False

{- Note [Unsolved Derived equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In getUnsolvedInerts, we return a derived equality from the inert_eqs
because it is a candidate for floating out of this implication.  We
only float equalities with a meta-tyvar on the left, so we only pull
those out here.

Note [What might match later?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We must determine whether a Given might later match a Wanted. We
definitely need to account for the possibility that any metavariable
in the Wanted might be arbitrarily instantiated. We do *not* want
to allow skolems in the Given to be instantiated. But what about
type family applications? (Examples are below the explanation.)

To allow flexibility in how type family applications unify we use
the Core flattener. See
Note [Flattening type-family applications when matching instances] in GHC.Core.Unify.
The Core flattener replaces all type family applications with
fresh variables. The next question: should we allow these fresh
variables in the domain of a unifying substitution?

A type family application that mentions only skolems is settled: any
skolems would have been rewritten w.r.t. Givens by now. These type
family applications match only themselves. A type family application
that mentions metavariables, on the other hand, can match anything.
So, if the original type family application contains a metavariable,
we use BindMe to tell the unifier to allow it in the substitution.
On the other hand, a type family application with only skolems is
considered rigid.

Examples:
    [G] C a
    [W] C alpha
  This easily might match later.

    [G] C a
    [W] C (F alpha)
  This might match later, too, but we need to flatten the (F alpha)
  to a fresh variable so that the unifier can connect the two.

    [G] C (F alpha)
    [W] C a
  This also might match later. Again, we will need to flatten to
  find this out. (Surprised about a metavariable in a Given? That
  can easily happen -- See Note [GivenInv] in GHC.Tc.Utils.TcType.)

    [G] C (F a)
    [W] C a
  This won't match later. We're not going to get new Givens that
  can inform the F a, and so this is a no-go.

This treatment fixes #18910 and is tested in
typecheck/should_compile/InstanceGivenOverlap{,2}
-}

removeInertCts :: [Ct] -> InertCans -> InertCans
-- ^ Remove inert constraints from the 'InertCans', for use when a
-- typechecker plugin wishes to discard a given.
removeInertCts cts icans = foldl' removeInertCt icans cts

removeInertCt :: InertCans -> Ct -> InertCans
removeInertCt is ct =
  case ct of

    CDictCan  { cc_class = cl, cc_tyargs = tys } ->
      is { inert_dicts = delDict (inert_dicts is) cl tys }

    CEqCan    { cc_lhs  = lhs, cc_rhs = rhs } -> delEq is lhs rhs

    CQuantCan {}     -> panic "removeInertCt: CQuantCan"
    CIrredCan {}     -> panic "removeInertCt: CIrredEvCan"
    CNonCanonical {} -> panic "removeInertCt: CNonCanonical"

-- | Looks up a family application in the inerts; returned coercion
-- is oriented input ~ output
lookupFamAppInert :: TyCon -> [Type] -> TcS (Maybe (TcCoercion, TcType, CtFlavourRole))
lookupFamAppInert fam_tc tys
  = do { IS { inert_cans = IC { inert_funeqs = inert_funeqs } } <- getTcSInerts
       ; return (lookup_inerts inert_funeqs) }
  where
    lookup_inerts inert_funeqs
      | Just (EqualCtList (CEqCan { cc_ev = ctev, cc_rhs = rhs } :| _))
          <- findFunEq inert_funeqs fam_tc tys
      = Just (ctEvCoercion ctev, rhs, ctEvFlavourRole ctev)
      | otherwise = Nothing

lookupInInerts :: CtLoc -> TcPredType -> TcS (Maybe CtEvidence)
-- Is this exact predicate type cached in the solved or canonicals of the InertSet?
lookupInInerts loc pty
  | ClassPred cls tys <- classifyPredType pty
  = do { inerts <- getTcSInerts
       ; return (lookupSolvedDict inerts loc cls tys `mplus`
                 lookupInertDict (inert_cans inerts) loc cls tys) }
  | otherwise -- NB: No caching for equalities, IPs, holes, or errors
  = return Nothing

-- | Look up a dictionary inert.
lookupInertDict :: InertCans -> CtLoc -> Class -> [Type] -> Maybe CtEvidence
lookupInertDict (IC { inert_dicts = dicts }) loc cls tys
  = case findDict dicts loc cls tys of
      Just ct -> Just (ctEvidence ct)
      _       -> Nothing

-- | Look up a solved inert.
lookupSolvedDict :: InertSet -> CtLoc -> Class -> [Type] -> Maybe CtEvidence
-- Returns just if exactly this predicate type exists in the solved.
lookupSolvedDict (IS { inert_solved_dicts = solved }) loc cls tys
  = case findDict solved loc cls tys of
      Just ev -> Just ev
      _       -> Nothing

---------------------------
lookupFamAppCache :: TyCon -> [Type] -> TcS (Maybe (TcCoercion, TcType))
lookupFamAppCache fam_tc tys
  = do { IS { inert_famapp_cache = famapp_cache } <- getTcSInerts
       ; case findFunEq famapp_cache fam_tc tys of
           result@(Just (co, ty)) ->
             do { traceTcS "famapp_cache hit" (vcat [ ppr (mkTyConApp fam_tc tys)
                                                    , ppr ty
                                                    , ppr co ])
                ; return result }
           Nothing -> return Nothing }

extendFamAppCache :: TyCon -> [Type] -> (TcCoercion, TcType) -> TcS ()
-- NB: co :: rhs ~ F tys, to match expectations of rewriter
extendFamAppCache tc xi_args stuff@(_, ty)
  = do { dflags <- getDynFlags
       ; when (gopt Opt_FamAppCache dflags) $
    do { traceTcS "extendFamAppCache" (vcat [ ppr tc <+> ppr xi_args
                                            , ppr ty ])
            -- 'co' can be bottom, in the case of derived items
       ; updInertTcS $ \ is@(IS { inert_famapp_cache = fc }) ->
            is { inert_famapp_cache = insertFunEq fc tc xi_args stuff } } }

-- Remove entries from the cache whose evidence mentions variables in the
-- supplied set
dropFromFamAppCache :: VarSet -> TcS ()
dropFromFamAppCache varset
  = do { inerts@(IS { inert_famapp_cache = famapp_cache }) <- getTcSInerts
       ; let filtered = filterTcAppMap check famapp_cache
       ; setTcSInerts $ inerts { inert_famapp_cache = filtered } }
  where
    check :: (TcCoercion, TcType) -> Bool
    check (co, _) = not (anyFreeVarsOfCo (`elemVarSet` varset) co)

{- *********************************************************************
*                                                                      *
                   Irreds
*                                                                      *
********************************************************************* -}

foldIrreds :: (Ct -> b -> b) -> Cts -> b -> b
foldIrreds k irreds z = foldr k z irreds


{- *********************************************************************
*                                                                      *
                   TcAppMap
*                                                                      *
************************************************************************

Note [Use loose types in inert set]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Whenever we are looking up an inert dictionary (CDictCan) or function
equality (CEqCan), we use a TcAppMap, which uses the Unique of the
class/type family tycon and then a trie which maps the arguments. This
trie does *not* need to match the kinds of the arguments; this Note
explains why.

Consider the types ty0 = (T ty1 ty2 ty3 ty4) and ty0' = (T ty1' ty2' ty3' ty4'),
where ty4 and ty4' have different kinds. Let's further assume that both types
ty0 and ty0' are well-typed. Because the kind of T is closed, it must be that
one of the ty1..ty3 does not match ty1'..ty3' (and that the kind of the fourth
argument to T is dependent on whichever one changed). Since we are matching
all arguments, during the inert-set lookup, we know that ty1..ty3 do indeed
match ty1'..ty3'. Therefore, the kind of ty4 and ty4' must match, too --
without ever looking at it.

Accordingly, we use LooseTypeMap, which skips the kind check when looking
up a type. I (Richard E) believe this is just an optimization, and that
looking at kinds would be harmless.

-}

type TcAppMap a = DTyConEnv (ListMap LooseTypeMap a)
    -- Indexed by tycon then the arg types, using "loose" matching, where
    -- we don't require kind equality. This allows, for example, (a |> co)
    -- to match (a).
    -- See Note [Use loose types in inert set]
    -- Used for types and classes; hence UniqDFM
    -- See Note [foldTM determinism] in GHC.Data.TrieMap for why we use DTyConEnv here

isEmptyTcAppMap :: TcAppMap a -> Bool
isEmptyTcAppMap m = isEmptyDTyConEnv m

emptyTcAppMap :: TcAppMap a
emptyTcAppMap = emptyDTyConEnv

findTcApp :: TcAppMap a -> TyCon -> [Type] -> Maybe a
findTcApp m tc tys = do { tys_map <- lookupDTyConEnv m tc
                        ; lookupTM tys tys_map }

delTcApp :: TcAppMap a -> TyCon -> [Type] -> TcAppMap a
delTcApp m tc tys = adjustDTyConEnv (deleteTM tys) m tc

insertTcApp :: TcAppMap a -> TyCon -> [Type] -> a -> TcAppMap a
insertTcApp m tc tys ct = alterDTyConEnv alter_tm m tc
  where
    alter_tm mb_tm = Just (insertTM tys ct (mb_tm `orElse` emptyTM))

alterTcApp :: forall a. TcAppMap a -> TyCon -> [Type] -> (Maybe a -> Maybe a) -> TcAppMap a
alterTcApp m tc tys upd = alterDTyConEnv alter_tm m tc
  where
    alter_tm :: Maybe (ListMap LooseTypeMap a) -> Maybe (ListMap LooseTypeMap a)
    alter_tm m_elt = Just (alterTM tys upd (m_elt `orElse` emptyTM))

filterTcAppMap :: forall a. (a -> Bool) -> TcAppMap a -> TcAppMap a
filterTcAppMap f m = mapMaybeDTyConEnv one_tycon m
  where
    one_tycon :: ListMap LooseTypeMap a -> Maybe (ListMap LooseTypeMap a)
    one_tycon tm
      | isEmptyTM filtered_tm = Nothing
      | otherwise             = Just filtered_tm
      where
        filtered_tm = filterTM f tm

tcAppMapToBag :: TcAppMap a -> Bag a
tcAppMapToBag m = foldTcAppMap consBag m emptyBag

foldTcAppMap :: (a -> b -> b) -> TcAppMap a -> b -> b
foldTcAppMap k m z = foldDTyConEnv (foldTM k) z m


{- *********************************************************************
*                                                                      *
                   DictMap
*                                                                      *
********************************************************************* -}


{- Note [Tuples hiding implicit parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   f,g :: (?x::Int, C a) => a -> a
   f v = let ?x = 4 in g v

The call to 'g' gives rise to a Wanted constraint (?x::Int, C a).
We must /not/ solve this from the Given (?x::Int, C a), because of
the intervening binding for (?x::Int).  #14218.

We deal with this by arranging that we always fail when looking up a
tuple constraint that hides an implicit parameter. Not that this applies
  * both to the inert_dicts (lookupInertDict)
  * and to the solved_dicts (looukpSolvedDict)
An alternative would be not to extend these sets with such tuple
constraints, but it seemed more direct to deal with the lookup.

Note [Solving CallStack constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose f :: HasCallStack => blah.  Then

* Each call to 'f' gives rise to
    [W] s1 :: IP "callStack" CallStack    -- CtOrigin = OccurrenceOf f
  with a CtOrigin that says "OccurrenceOf f".
  Remember that HasCallStack is just shorthand for
    IP "callStack CallStack
  See Note [Overview of implicit CallStacks] in GHC.Tc.Types.Evidence

* We cannonicalise such constraints, in GHC.Tc.Solver.Canonical.canClassNC, by
  pushing the call-site info on the stack, and changing the CtOrigin
  to record that has been done.
   Bind:  s1 = pushCallStack <site-info> s2
   [W] s2 :: IP "callStack" CallStack   -- CtOrigin = IPOccOrigin

* Then, and only then, we can solve the constraint from an enclosing
  Given.

So we must be careful /not/ to solve 's1' from the Givens.  Again,
we ensure this by arranging that findDict always misses when looking
up souch constraints.
-}

type DictMap a = TcAppMap a

emptyDictMap :: DictMap a
emptyDictMap = emptyTcAppMap

findDict :: DictMap a -> CtLoc -> Class -> [Type] -> Maybe a
findDict m loc cls tys
  | hasIPSuperClasses cls tys -- See Note [Tuples hiding implicit parameters]
  = Nothing

  | Just {} <- isCallStackPred cls tys
  , OccurrenceOf {} <- ctLocOrigin loc
  = Nothing             -- See Note [Solving CallStack constraints]

  | otherwise
  = findTcApp m (classTyCon cls) tys

findDictsByClass :: DictMap a -> Class -> Bag a
findDictsByClass m cls
  | Just tm <- lookupDTyConEnv m (classTyCon cls) = foldTM consBag tm emptyBag
  | otherwise                                     = emptyBag

delDict :: DictMap a -> Class -> [Type] -> DictMap a
delDict m cls tys = delTcApp m (classTyCon cls) tys

addDict :: DictMap a -> Class -> [Type] -> a -> DictMap a
addDict m cls tys item = insertTcApp m (classTyCon cls) tys item

addDictsByClass :: DictMap Ct -> Class -> Bag Ct -> DictMap Ct
addDictsByClass m cls items
  = extendDTyConEnv m (classTyCon cls) (foldr add emptyTM items)
  where
    add ct@(CDictCan { cc_tyargs = tys }) tm = insertTM tys ct tm
    add ct _ = pprPanic "addDictsByClass" (ppr ct)

filterDicts :: (Ct -> Bool) -> DictMap Ct -> DictMap Ct
filterDicts f m = filterTcAppMap f m

partitionDicts :: (Ct -> Bool) -> DictMap Ct -> (Bag Ct, DictMap Ct)
partitionDicts f m = foldTcAppMap k m (emptyBag, emptyDicts)
  where
    k ct (yeses, noes) | f ct      = (ct `consBag` yeses, noes)
                       | otherwise = (yeses,              add ct noes)
    add ct@(CDictCan { cc_class = cls, cc_tyargs = tys }) m
      = addDict m cls tys ct
    add ct _ = pprPanic "partitionDicts" (ppr ct)

dictsToBag :: DictMap a -> Bag a
dictsToBag = tcAppMapToBag

foldDicts :: (a -> b -> b) -> DictMap a -> b -> b
foldDicts = foldTcAppMap

emptyDicts :: DictMap a
emptyDicts = emptyTcAppMap


{- *********************************************************************
*                                                                      *
                   FunEqMap
*                                                                      *
********************************************************************* -}

type FunEqMap a = TcAppMap a  -- A map whose key is a (TyCon, [Type]) pair

emptyFunEqs :: TcAppMap a
emptyFunEqs = emptyTcAppMap

findFunEq :: FunEqMap a -> TyCon -> [Type] -> Maybe a
findFunEq m tc tys = findTcApp m tc tys

findFunEqsByTyCon :: FunEqMap a -> TyCon -> [a]
-- Get inert function equation constraints that have the given tycon
-- in their head.  Not that the constraints remain in the inert set.
-- We use this to check for derived interactions with built-in type-function
-- constructors.
findFunEqsByTyCon m tc
  | Just tm <- lookupDTyConEnv m tc = foldTM (:) tm []
  | otherwise                       = []

foldFunEqs :: (a -> b -> b) -> FunEqMap a -> b -> b
foldFunEqs = foldTcAppMap

insertFunEq :: FunEqMap a -> TyCon -> [Type] -> a -> FunEqMap a
insertFunEq m tc tys val = insertTcApp m tc tys val

{-
************************************************************************
*                                                                      *
*              The TcS solver monad                                    *
*                                                                      *
************************************************************************

Note [The TcS monad]
~~~~~~~~~~~~~~~~~~~~
The TcS monad is a weak form of the main Tc monad

All you can do is
    * fail
    * allocate new variables
    * fill in evidence variables

Filling in a dictionary evidence variable means to create a binding
for it, so TcS carries a mutable location where the binding can be
added.  This is initialised from the innermost implication constraint.
-}

data TcSEnv
  = TcSEnv {
      tcs_ev_binds    :: EvBindsVar,

      tcs_unified     :: IORef Int,
         -- The number of unification variables we have filled
         -- The important thing is whether it is non-zero

      tcs_unif_lvl  :: IORef (Maybe TcLevel),
         -- The Unification Level Flag
         -- Outermost level at which we have unified a meta tyvar
         -- Starts at Nothing, then (Just i), then (Just j) where j<i
         -- See Note [The Unification Level Flag]

      tcs_count     :: IORef Int, -- Global step count

      tcs_inerts    :: IORef InertSet, -- Current inert set

      -- See Note [WorkList priorities] and
      tcs_worklist  :: IORef WorkList -- Current worklist
    }

---------------
newtype TcS a = TcS { unTcS :: TcSEnv -> TcM a } deriving (Functor)

instance Applicative TcS where
  pure x = TcS (\_ -> return x)
  (<*>) = ap

instance Monad TcS where
  m >>= k   = TcS (\ebs -> unTcS m ebs >>= \r -> unTcS (k r) ebs)

instance MonadFail TcS where
  fail err  = TcS (\_ -> fail err)

instance MonadUnique TcS where
   getUniqueSupplyM = wrapTcS getUniqueSupplyM

instance HasModule TcS where
   getModule = wrapTcS getModule

instance MonadThings TcS where
   lookupThing n = wrapTcS (lookupThing n)

-- Basic functionality
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wrapTcS :: TcM a -> TcS a
-- Do not export wrapTcS, because it promotes an arbitrary TcM to TcS,
-- and TcS is supposed to have limited functionality
wrapTcS = TcS . const -- a TcM action will not use the TcEvBinds

wrapErrTcS :: TcM a -> TcS a
-- The thing wrapped should just fail
-- There's no static check; it's up to the user
-- Having a variant for each error message is too painful
wrapErrTcS = wrapTcS

wrapWarnTcS :: TcM a -> TcS a
-- The thing wrapped should just add a warning, or no-op
-- There's no static check; it's up to the user
wrapWarnTcS = wrapTcS

failTcS, panicTcS  :: SDoc -> TcS a
warnTcS   :: WarningFlag -> SDoc -> TcS ()
addErrTcS :: SDoc -> TcS ()
failTcS      = wrapTcS . TcM.failWith
warnTcS flag = wrapTcS . TcM.addWarn (Reason flag)
addErrTcS    = wrapTcS . TcM.addErr
panicTcS doc = pprPanic "GHC.Tc.Solver.Canonical" doc

traceTcS :: String -> SDoc -> TcS ()
traceTcS herald doc = wrapTcS (TcM.traceTc herald doc)
{-# INLINE traceTcS #-}  -- see Note [INLINE conditional tracing utilities]

runTcPluginTcS :: TcPluginM a -> TcS a
runTcPluginTcS m = wrapTcS . runTcPluginM m =<< getTcEvBindsVar

instance HasDynFlags TcS where
    getDynFlags = wrapTcS getDynFlags

getGlobalRdrEnvTcS :: TcS GlobalRdrEnv
getGlobalRdrEnvTcS = wrapTcS TcM.getGlobalRdrEnv

bumpStepCountTcS :: TcS ()
bumpStepCountTcS = TcS $ \env -> do { let ref = tcs_count env
                                    ; n <- TcM.readTcRef ref
                                    ; TcM.writeTcRef ref (n+1) }

csTraceTcS :: SDoc -> TcS ()
csTraceTcS doc
  = wrapTcS $ csTraceTcM (return doc)
{-# INLINE csTraceTcS #-}  -- see Note [INLINE conditional tracing utilities]

traceFireTcS :: CtEvidence -> SDoc -> TcS ()
-- Dump a rule-firing trace
traceFireTcS ev doc
  = TcS $ \env -> csTraceTcM $
    do { n <- TcM.readTcRef (tcs_count env)
       ; tclvl <- TcM.getTcLevel
       ; return (hang (text "Step" <+> int n
                       <> brackets (text "l:" <> ppr tclvl <> comma <>
                                    text "d:" <> ppr (ctLocDepth (ctEvLoc ev)))
                       <+> doc <> colon)
                     4 (ppr ev)) }
{-# INLINE traceFireTcS #-}  -- see Note [INLINE conditional tracing utilities]

csTraceTcM :: TcM SDoc -> TcM ()
-- Constraint-solver tracing, -ddump-cs-trace
csTraceTcM mk_doc
  = do { dflags <- getDynFlags
       ; when (  dopt Opt_D_dump_cs_trace dflags
                  || dopt Opt_D_dump_tc_trace dflags )
              ( do { msg <- mk_doc
                   ; TcM.dumpTcRn False
                       (dumpOptionsFromFlag Opt_D_dump_cs_trace)
                       "" FormatText
                       msg }) }
{-# INLINE csTraceTcM #-}  -- see Note [INLINE conditional tracing utilities]

runTcS :: TcS a                -- What to run
       -> TcM (a, EvBindMap)
runTcS tcs
  = do { ev_binds_var <- TcM.newTcEvBinds
       ; res <- runTcSWithEvBinds ev_binds_var tcs
       ; ev_binds <- TcM.getTcEvBindsMap ev_binds_var
       ; return (res, ev_binds) }
-- | This variant of 'runTcS' will keep solving, even when only Deriveds
-- are left around. It also doesn't return any evidence, as callers won't
-- need it.
runTcSDeriveds :: TcS a -> TcM a
runTcSDeriveds tcs
  = do { ev_binds_var <- TcM.newTcEvBinds
       ; runTcSWithEvBinds ev_binds_var tcs }

-- | This can deal only with equality constraints.
runTcSEqualities :: TcS a -> TcM a
runTcSEqualities thing_inside
  = do { ev_binds_var <- TcM.newNoTcEvBinds
       ; runTcSWithEvBinds ev_binds_var thing_inside }

-- | A variant of 'runTcS' that takes and returns an 'InertSet' for
-- later resumption of the 'TcS' session.
runTcSInerts :: InertSet -> TcS a -> TcM (a, InertSet)
runTcSInerts inerts tcs = do
  ev_binds_var <- TcM.newTcEvBinds
  runTcSWithEvBinds' False ev_binds_var $ do
    setTcSInerts inerts
    a <- tcs
    new_inerts <- getTcSInerts
    return (a, new_inerts)

runTcSWithEvBinds :: EvBindsVar
                  -> TcS a
                  -> TcM a
runTcSWithEvBinds = runTcSWithEvBinds' True

runTcSWithEvBinds' :: Bool -- ^ Restore type variable cycles afterwards?
                           -- Don't if you want to reuse the InertSet.
                           -- See also Note [Type variable cycles in Givens]
                           -- in GHC.Tc.Solver.Canonical
                   -> EvBindsVar
                   -> TcS a
                   -> TcM a
runTcSWithEvBinds' restore_cycles ev_binds_var tcs
  = do { unified_var <- TcM.newTcRef 0
       ; step_count <- TcM.newTcRef 0
       ; inert_var <- TcM.newTcRef emptyInert
       ; wl_var <- TcM.newTcRef emptyWorkList
       ; unif_lvl_var <- TcM.newTcRef Nothing
       ; let env = TcSEnv { tcs_ev_binds      = ev_binds_var
                          , tcs_unified       = unified_var
                          , tcs_unif_lvl      = unif_lvl_var
                          , tcs_count         = step_count
                          , tcs_inerts        = inert_var
                          , tcs_worklist      = wl_var }

             -- Run the computation
       ; res <- unTcS tcs env

       ; count <- TcM.readTcRef step_count
       ; when (count > 0) $
         csTraceTcM $ return (text "Constraint solver steps =" <+> int count)

       ; when restore_cycles $
         do { inert_set <- TcM.readTcRef inert_var
            ; restoreTyVarCycles inert_set }

#if defined(DEBUG)
       ; ev_binds <- TcM.getTcEvBindsMap ev_binds_var
       ; checkForCyclicBinds ev_binds
#endif

       ; return res }

----------------------------
#if defined(DEBUG)
checkForCyclicBinds :: EvBindMap -> TcM ()
checkForCyclicBinds ev_binds_map
  | null cycles
  = return ()
  | null coercion_cycles
  = TcM.traceTc "Cycle in evidence binds" $ ppr cycles
  | otherwise
  = pprPanic "Cycle in coercion bindings" $ ppr coercion_cycles
  where
    ev_binds = evBindMapBinds ev_binds_map

    cycles :: [[EvBind]]
    cycles = [c | CyclicSCC c <- stronglyConnCompFromEdgedVerticesUniq edges]

    coercion_cycles = [c | c <- cycles, any is_co_bind c]
    is_co_bind (EvBind { eb_lhs = b }) = isEqPrimPred (varType b)

    edges :: [ Node EvVar EvBind ]
    edges = [ DigraphNode bind bndr (nonDetEltsUniqSet (evVarsOfTerm rhs))
            | bind@(EvBind { eb_lhs = bndr, eb_rhs = rhs}) <- bagToList ev_binds ]
            -- It's OK to use nonDetEltsUFM here as
            -- stronglyConnCompFromEdgedVertices is still deterministic even
            -- if the edges are in nondeterministic order as explained in
            -- Note [Deterministic SCC] in GHC.Data.Graph.Directed.
#endif

----------------------------
setEvBindsTcS :: EvBindsVar -> TcS a -> TcS a
setEvBindsTcS ref (TcS thing_inside)
 = TcS $ \ env -> thing_inside (env { tcs_ev_binds = ref })

nestImplicTcS :: EvBindsVar
              -> TcLevel -> TcS a
              -> TcS a
nestImplicTcS ref inner_tclvl (TcS thing_inside)
  = TcS $ \ TcSEnv { tcs_unified       = unified_var
                   , tcs_inerts        = old_inert_var
                   , tcs_count         = count
                   , tcs_unif_lvl      = unif_lvl
                   } ->
    do { inerts <- TcM.readTcRef old_inert_var
       ; let nest_inert = inerts { inert_cycle_breakers = []
                                 , inert_cans = (inert_cans inerts)
                                                   { inert_given_eqs = False } }
                 -- All other InertSet fields are inherited
       ; new_inert_var <- TcM.newTcRef nest_inert
       ; new_wl_var    <- TcM.newTcRef emptyWorkList
       ; let nest_env = TcSEnv { tcs_count         = count     -- Inherited
                               , tcs_unif_lvl      = unif_lvl  -- Inherited
                               , tcs_ev_binds      = ref
                               , tcs_unified       = unified_var
                               , tcs_inerts        = new_inert_var
                               , tcs_worklist      = new_wl_var }
       ; res <- TcM.setTcLevel inner_tclvl $
                thing_inside nest_env

       ; out_inert_set <- TcM.readTcRef new_inert_var
       ; restoreTyVarCycles out_inert_set

#if defined(DEBUG)
       -- Perform a check that the thing_inside did not cause cycles
       ; ev_binds <- TcM.getTcEvBindsMap ref
       ; checkForCyclicBinds ev_binds
#endif
       ; return res }

nestTcS ::  TcS a -> TcS a
-- Use the current untouchables, augmenting the current
-- evidence bindings, and solved dictionaries
-- But have no effect on the InertCans, or on the inert_famapp_cache
-- (we want to inherit the latter from processing the Givens)
nestTcS (TcS thing_inside)
  = TcS $ \ env@(TcSEnv { tcs_inerts = inerts_var }) ->
    do { inerts <- TcM.readTcRef inerts_var
       ; new_inert_var <- TcM.newTcRef inerts
       ; new_wl_var    <- TcM.newTcRef emptyWorkList
       ; let nest_env = env { tcs_inerts   = new_inert_var
                            , tcs_worklist = new_wl_var }

       ; res <- thing_inside nest_env

       ; new_inerts <- TcM.readTcRef new_inert_var

       -- we want to propagate the safe haskell failures
       ; let old_ic = inert_cans inerts
             new_ic = inert_cans new_inerts
             nxt_ic = old_ic { inert_safehask = inert_safehask new_ic }

       ; TcM.writeTcRef inerts_var  -- See Note [Propagate the solved dictionaries]
                        (inerts { inert_solved_dicts = inert_solved_dicts new_inerts
                                , inert_cans = nxt_ic })

       ; return res }

emitImplicationTcS :: TcLevel -> SkolemInfo
                   -> [TcTyVar]        -- Skolems
                   -> [EvVar]          -- Givens
                   -> Cts              -- Wanteds
                   -> TcS TcEvBinds
-- Add an implication to the TcS monad work-list
emitImplicationTcS new_tclvl skol_info skol_tvs givens wanteds
  = do { let wc = emptyWC { wc_simple = wanteds }
       ; imp <- wrapTcS $
                do { ev_binds_var <- TcM.newTcEvBinds
                   ; imp <- TcM.newImplication
                   ; return (imp { ic_tclvl  = new_tclvl
                                 , ic_skols  = skol_tvs
                                 , ic_given  = givens
                                 , ic_wanted = wc
                                 , ic_binds  = ev_binds_var
                                 , ic_info   = skol_info }) }

       ; emitImplication imp
       ; return (TcEvBinds (ic_binds imp)) }

emitTvImplicationTcS :: TcLevel -> SkolemInfo
                     -> [TcTyVar]        -- Skolems
                     -> Cts              -- Wanteds
                     -> TcS ()
-- Just like emitImplicationTcS but no givens and no bindings
emitTvImplicationTcS new_tclvl skol_info skol_tvs wanteds
  = do { let wc = emptyWC { wc_simple = wanteds }
       ; imp <- wrapTcS $
                do { ev_binds_var <- TcM.newNoTcEvBinds
                   ; imp <- TcM.newImplication
                   ; return (imp { ic_tclvl  = new_tclvl
                                 , ic_skols  = skol_tvs
                                 , ic_wanted = wc
                                 , ic_binds  = ev_binds_var
                                 , ic_info   = skol_info }) }

       ; emitImplication imp }


{- Note [Propagate the solved dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's really quite important that nestTcS does not discard the solved
dictionaries from the thing_inside.
Consider
   Eq [a]
   forall b. empty =>  Eq [a]
We solve the simple (Eq [a]), under nestTcS, and then turn our attention to
the implications.  It's definitely fine to use the solved dictionaries on
the inner implications, and it can make a significant performance difference
if you do so.
-}

-- Getters and setters of GHC.Tc.Utils.Env fields
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Getter of inerts and worklist
getTcSInertsRef :: TcS (IORef InertSet)
getTcSInertsRef = TcS (return . tcs_inerts)

getTcSWorkListRef :: TcS (IORef WorkList)
getTcSWorkListRef = TcS (return . tcs_worklist)

getTcSInerts :: TcS InertSet
getTcSInerts = getTcSInertsRef >>= readTcRef

setTcSInerts :: InertSet -> TcS ()
setTcSInerts ics = do { r <- getTcSInertsRef; writeTcRef r ics }

getWorkListImplics :: TcS (Bag Implication)
getWorkListImplics
  = do { wl_var <- getTcSWorkListRef
       ; wl_curr <- readTcRef wl_var
       ; return (wl_implics wl_curr) }

pushLevelNoWorkList :: SDoc -> TcS a -> TcS (TcLevel, a)
-- Push the level and run thing_inside
-- However, thing_inside should not generate any work items
#if defined(DEBUG)
pushLevelNoWorkList err_doc (TcS thing_inside)
  = TcS (\env -> TcM.pushTcLevelM $
                 thing_inside (env { tcs_worklist = wl_panic })
        )
  where
    wl_panic  = pprPanic "GHC.Tc.Solver.Monad.buildImplication" err_doc
                         -- This panic checks that the thing-inside
                         -- does not emit any work-list constraints
#else
pushLevelNoWorkList _ (TcS thing_inside)
  = TcS (\env -> TcM.pushTcLevelM (thing_inside env))  -- Don't check
#endif

updWorkListTcS :: (WorkList -> WorkList) -> TcS ()
updWorkListTcS f
  = do { wl_var <- getTcSWorkListRef
       ; updTcRef wl_var f }

emitWorkNC :: [CtEvidence] -> TcS ()
emitWorkNC evs
  | null evs
  = return ()
  | otherwise
  = emitWork (map mkNonCanonical evs)

emitWork :: [Ct] -> TcS ()
emitWork [] = return ()   -- avoid printing, among other work
emitWork cts
  = do { traceTcS "Emitting fresh work" (vcat (map ppr cts))
       ; updWorkListTcS (extendWorkListCts cts) }

emitImplication :: Implication -> TcS ()
emitImplication implic
  = updWorkListTcS (extendWorkListImplic implic)

newTcRef :: a -> TcS (TcRef a)
newTcRef x = wrapTcS (TcM.newTcRef x)

readTcRef :: TcRef a -> TcS a
readTcRef ref = wrapTcS (TcM.readTcRef ref)

writeTcRef :: TcRef a -> a -> TcS ()
writeTcRef ref val = wrapTcS (TcM.writeTcRef ref val)

updTcRef :: TcRef a -> (a->a) -> TcS ()
updTcRef ref upd_fn = wrapTcS (TcM.updTcRef ref upd_fn)

getTcEvBindsVar :: TcS EvBindsVar
getTcEvBindsVar = TcS (return . tcs_ev_binds)

getTcLevel :: TcS TcLevel
getTcLevel = wrapTcS TcM.getTcLevel

getTcEvTyCoVars :: EvBindsVar -> TcS TyCoVarSet
getTcEvTyCoVars ev_binds_var
  = wrapTcS $ TcM.getTcEvTyCoVars ev_binds_var

getTcEvBindsMap :: EvBindsVar -> TcS EvBindMap
getTcEvBindsMap ev_binds_var
  = wrapTcS $ TcM.getTcEvBindsMap ev_binds_var

setTcEvBindsMap :: EvBindsVar -> EvBindMap -> TcS ()
setTcEvBindsMap ev_binds_var binds
  = wrapTcS $ TcM.setTcEvBindsMap ev_binds_var binds

unifyTyVar :: TcTyVar -> TcType -> TcS ()
-- Unify a meta-tyvar with a type
-- We keep track of how many unifications have happened in tcs_unified,
--
-- We should never unify the same variable twice!
unifyTyVar tv ty
  = ASSERT2( isMetaTyVar tv, ppr tv )
    TcS $ \ env ->
    do { TcM.traceTc "unifyTyVar" (ppr tv <+> text ":=" <+> ppr ty)
       ; TcM.writeMetaTyVar tv ty
       ; TcM.updTcRef (tcs_unified env) (+1) }

reportUnifications :: TcS a -> TcS (Int, a)
reportUnifications (TcS thing_inside)
  = TcS $ \ env ->
    do { inner_unified <- TcM.newTcRef 0
       ; res <- thing_inside (env { tcs_unified = inner_unified })
       ; n_unifs <- TcM.readTcRef inner_unified
       ; TcM.updTcRef (tcs_unified env) (+ n_unifs)
       ; return (n_unifs, res) }

getDefaultInfo ::  TcS ([Type], (Bool, Bool))
getDefaultInfo = wrapTcS TcM.tcGetDefaultTys

-- Just get some environments needed for instance looking up and matching
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

getInstEnvs :: TcS InstEnvs
getInstEnvs = wrapTcS $ TcM.tcGetInstEnvs

getFamInstEnvs :: TcS (FamInstEnv, FamInstEnv)
getFamInstEnvs = wrapTcS $ FamInst.tcGetFamInstEnvs

getTopEnv :: TcS HscEnv
getTopEnv = wrapTcS $ TcM.getTopEnv

getGblEnv :: TcS TcGblEnv
getGblEnv = wrapTcS $ TcM.getGblEnv

getLclEnv :: TcS TcLclEnv
getLclEnv = wrapTcS $ TcM.getLclEnv

tcLookupClass :: Name -> TcS Class
tcLookupClass c = wrapTcS $ TcM.tcLookupClass c

tcLookupId :: Name -> TcS Id
tcLookupId n = wrapTcS $ TcM.tcLookupId n

-- Setting names as used (used in the deriving of Coercible evidence)
-- Too hackish to expose it to TcS? In that case somehow extract the used
-- constructors from the result of solveInteract
addUsedGREs :: [GlobalRdrElt] -> TcS ()
addUsedGREs gres = wrapTcS  $ TcM.addUsedGREs gres

addUsedGRE :: Bool -> GlobalRdrElt -> TcS ()
addUsedGRE warn_if_deprec gre = wrapTcS $ TcM.addUsedGRE warn_if_deprec gre

keepAlive :: Name -> TcS ()
keepAlive = wrapTcS . TcM.keepAlive

-- Various smaller utilities [TODO, maybe will be absorbed in the instance matcher]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

checkWellStagedDFun :: CtLoc -> InstanceWhat -> PredType -> TcS ()
-- Check that we do not try to use an instance before it is available.  E.g.
--    instance Eq T where ...
--    f x = $( ... (\(p::T) -> p == p)... )
-- Here we can't use the equality function from the instance in the splice

checkWellStagedDFun loc what pred
  | TopLevInstance { iw_dfun_id = dfun_id } <- what
  , let bind_lvl = TcM.topIdLvl dfun_id
  , bind_lvl > impLevel
  = wrapTcS $ TcM.setCtLocM loc $
    do { use_stage <- TcM.getStage
       ; TcM.checkWellStaged pp_thing bind_lvl (thLevel use_stage) }

  | otherwise
  = return ()    -- Fast path for common case
  where
    pp_thing = text "instance for" <+> quotes (ppr pred)

pprEq :: TcType -> TcType -> SDoc
pprEq ty1 ty2 = pprParendType ty1 <+> char '~' <+> pprParendType ty2

isFilledMetaTyVar_maybe :: TcTyVar -> TcS (Maybe Type)
isFilledMetaTyVar_maybe tv = wrapTcS (TcM.isFilledMetaTyVar_maybe tv)

isFilledMetaTyVar :: TcTyVar -> TcS Bool
isFilledMetaTyVar tv = wrapTcS (TcM.isFilledMetaTyVar tv)

zonkTyCoVarsAndFV :: TcTyCoVarSet -> TcS TcTyCoVarSet
zonkTyCoVarsAndFV tvs = wrapTcS (TcM.zonkTyCoVarsAndFV tvs)

zonkTyCoVarsAndFVList :: [TcTyCoVar] -> TcS [TcTyCoVar]
zonkTyCoVarsAndFVList tvs = wrapTcS (TcM.zonkTyCoVarsAndFVList tvs)

zonkCo :: Coercion -> TcS Coercion
zonkCo = wrapTcS . TcM.zonkCo

zonkTcType :: TcType -> TcS TcType
zonkTcType ty = wrapTcS (TcM.zonkTcType ty)

zonkTcTypes :: [TcType] -> TcS [TcType]
zonkTcTypes tys = wrapTcS (TcM.zonkTcTypes tys)

zonkTcTyVar :: TcTyVar -> TcS TcType
zonkTcTyVar tv = wrapTcS (TcM.zonkTcTyVar tv)

zonkSimples :: Cts -> TcS Cts
zonkSimples cts = wrapTcS (TcM.zonkSimples cts)

zonkWC :: WantedConstraints -> TcS WantedConstraints
zonkWC wc = wrapTcS (TcM.zonkWC wc)

zonkTyCoVarKind :: TcTyCoVar -> TcS TcTyCoVar
zonkTyCoVarKind tv = wrapTcS (TcM.zonkTyCoVarKind tv)

----------------------------
pprKicked :: Int -> SDoc
pprKicked 0 = empty
pprKicked n = parens (int n <+> text "kicked out")

{- *********************************************************************
*                                                                      *
*              The Unification Level Flag                              *
*                                                                      *
********************************************************************* -}

{- Note [The Unification Level Flag]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a deep tree of implication constraints
   forall[1] a.                              -- Outer-implic
      C alpha[1]                               -- Simple
      forall[2] c. ....(C alpha[1])....        -- Implic-1
      forall[2] b. ....(alpha[1] ~ Int)....    -- Implic-2

The (C alpha) is insoluble until we know alpha.  We solve alpha
by unifying alpha:=Int somewhere deep inside Implic-2. But then we
must try to solve the Outer-implic all over again. This time we can
solve (C alpha) both in Outer-implic, and nested inside Implic-1.

When should we iterate solving a level-n implication?
Answer: if any unification of a tyvar at level n takes place
        in the ic_implics of that implication.

* What if a unification takes place at level n-1? Then don't iterate
  level n, because we'll iterate level n-1, and that will in turn iterate
  level n.

* What if a unification takes place at level n, in the ic_simples of
  level n?  No need to track this, because the kick-out mechanism deals
  with it.  (We can't drop kick-out in favour of iteration, becuase kick-out
  works for skolem-equalities, not just unifications.)

So the monad-global Unification Level Flag, kept in tcs_unif_lvl keeps
track of
  - Whether any unifications at all have taken place (Nothing => no unifications)
  - If so, what is the outermost level that has seen a unification (Just lvl)

The iteration done in the simplify_loop/maybe_simplify_again loop in GHC.Tc.Solver.

It helpful not to iterate unless there is a chance of progress.  #8474 is
an example:

  * There's a deeply-nested chain of implication constraints.
       ?x:alpha => ?y1:beta1 => ... ?yn:betan => [W] ?x:Int

  * From the innermost one we get a [D] alpha[1] ~ Int,
    so we can unify.

  * It's better not to iterate the inner implications, but go all the
    way out to level 1 before iterating -- because iterating level 1
    will iterate the inner levels anyway.

(In the olden days when we "floated" thse Derived constraints, this was
much, much more important -- we got exponential behaviour, as each iteration
produced the same Derived constraint.)
-}


resetUnificationFlag :: TcS Bool
-- We are at ambient level i
-- If the unification flag = Just i, reset it to Nothing and return True
-- Otherwise leave it unchanged and return False
resetUnificationFlag
  = TcS $ \env ->
    do { let ref = tcs_unif_lvl env
       ; ambient_lvl <- TcM.getTcLevel
       ; mb_lvl <- TcM.readTcRef ref
       ; TcM.traceTc "resetUnificationFlag" $
         vcat [ text "ambient:" <+> ppr ambient_lvl
              , text "unif_lvl:" <+> ppr mb_lvl ]
       ; case mb_lvl of
           Nothing       -> return False
           Just unif_lvl | ambient_lvl `strictlyDeeperThan` unif_lvl
                         -> return False
                         | otherwise
                         -> do { TcM.writeTcRef ref Nothing
                               ; return True } }

setUnificationFlag :: TcLevel -> TcS ()
-- (setUnificationFlag i) sets the unification level to (Just i)
-- unless it already is (Just j) where j <= i
setUnificationFlag lvl
  = TcS $ \env ->
    do { let ref = tcs_unif_lvl env
       ; mb_lvl <- TcM.readTcRef ref
       ; case mb_lvl of
           Just unif_lvl | lvl `deeperThanOrSame` unif_lvl
                         -> return ()
           _ -> TcM.writeTcRef ref (Just lvl) }


{- *********************************************************************
*                                                                      *
*                Instantiation etc.
*                                                                      *
********************************************************************* -}

-- Instantiations
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

instDFunType :: DFunId -> [DFunInstType] -> TcS ([TcType], TcThetaType)
instDFunType dfun_id inst_tys
  = wrapTcS $ TcM.instDFunType dfun_id inst_tys

newFlexiTcSTy :: Kind -> TcS TcType
newFlexiTcSTy knd = wrapTcS (TcM.newFlexiTyVarTy knd)

cloneMetaTyVar :: TcTyVar -> TcS TcTyVar
cloneMetaTyVar tv = wrapTcS (TcM.cloneMetaTyVar tv)

instFlexi :: [TKVar] -> TcS TCvSubst
instFlexi = instFlexiX emptyTCvSubst

instFlexiX :: TCvSubst -> [TKVar] -> TcS TCvSubst
instFlexiX subst tvs
  = wrapTcS (foldlM instFlexiHelper subst tvs)

instFlexiHelper :: TCvSubst -> TKVar -> TcM TCvSubst
instFlexiHelper subst tv
  = do { uniq <- TcM.newUnique
       ; details <- TcM.newMetaDetails TauTv
       ; let name = setNameUnique (tyVarName tv) uniq
             kind = substTyUnchecked subst (tyVarKind tv)
             ty'  = mkTyVarTy (mkTcTyVar name kind details)
       ; TcM.traceTc "instFlexi" (ppr ty')
       ; return (extendTvSubst subst tv ty') }

matchGlobalInst :: DynFlags
                -> Bool      -- True <=> caller is the short-cut solver
                             -- See Note [Shortcut solving: overlap]
                -> Class -> [Type] -> TcS TcM.ClsInstResult
matchGlobalInst dflags short_cut cls tys
  = wrapTcS (TcM.matchGlobalInst dflags short_cut cls tys)

tcInstSkolTyVarsX :: TCvSubst -> [TyVar] -> TcS (TCvSubst, [TcTyVar])
tcInstSkolTyVarsX subst tvs = wrapTcS $ TcM.tcInstSkolTyVarsX subst tvs

-- Creating and setting evidence variables and CtFlavors
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data MaybeNew = Fresh CtEvidence | Cached EvExpr

isFresh :: MaybeNew -> Bool
isFresh (Fresh {})  = True
isFresh (Cached {}) = False

freshGoals :: [MaybeNew] -> [CtEvidence]
freshGoals mns = [ ctev | Fresh ctev <- mns ]

getEvExpr :: MaybeNew -> EvExpr
getEvExpr (Fresh ctev) = ctEvExpr ctev
getEvExpr (Cached evt) = evt

setEvBind :: EvBind -> TcS ()
setEvBind ev_bind
  = do { evb <- getTcEvBindsVar
       ; wrapTcS $ TcM.addTcEvBind evb ev_bind }

-- | Mark variables as used filling a coercion hole
useVars :: CoVarSet -> TcS ()
useVars co_vars
  = do { ev_binds_var <- getTcEvBindsVar
       ; let ref = ebv_tcvs ev_binds_var
       ; wrapTcS $
         do { tcvs <- TcM.readTcRef ref
            ; let tcvs' = tcvs `unionVarSet` co_vars
            ; TcM.writeTcRef ref tcvs' } }

-- | Equalities only
setWantedEq :: TcEvDest -> Coercion -> TcS ()
setWantedEq (HoleDest hole) co
  = do { useVars (coVarsOfCo co)
       ; fillCoercionHole hole co }
setWantedEq (EvVarDest ev) _ = pprPanic "setWantedEq" (ppr ev)

-- | Good for both equalities and non-equalities
setWantedEvTerm :: TcEvDest -> EvTerm -> TcS ()
setWantedEvTerm (HoleDest hole) tm
  | Just co <- evTermCoercion_maybe tm
  = do { useVars (coVarsOfCo co)
       ; fillCoercionHole hole co }
  | otherwise
  = -- See Note [Yukky eq_sel for a HoleDest]
    do { let co_var = coHoleCoVar hole
       ; setEvBind (mkWantedEvBind co_var tm)
       ; fillCoercionHole hole (mkTcCoVarCo co_var) }

setWantedEvTerm (EvVarDest ev_id) tm
  = setEvBind (mkWantedEvBind ev_id tm)

{- Note [Yukky eq_sel for a HoleDest]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
How can it be that a Wanted with HoleDest gets evidence that isn't
just a coercion? i.e. evTermCoercion_maybe returns Nothing.

Consider [G] forall a. blah => a ~ T
         [W] S ~# T

Then doTopReactEqPred carefully looks up the (boxed) constraint (S ~
T) in the quantified constraints, and wraps the (boxed) evidence it
gets back in an eq_sel to extract the unboxed (S ~# T).  We can't put
that term into a coercion, so we add a value binding
    h = eq_sel (...)
and the coercion variable h to fill the coercion hole.
We even re-use the CoHole's Id for this binding!

Yuk!
-}

fillCoercionHole :: CoercionHole -> Coercion -> TcS ()
fillCoercionHole hole co
  = do { wrapTcS $ TcM.fillCoercionHole hole co
       ; kickOutAfterFillingCoercionHole hole co }

setEvBindIfWanted :: CtEvidence -> EvTerm -> TcS ()
setEvBindIfWanted ev tm
  = case ev of
      CtWanted { ctev_dest = dest } -> setWantedEvTerm dest tm
      _                             -> return ()

newTcEvBinds :: TcS EvBindsVar
newTcEvBinds = wrapTcS TcM.newTcEvBinds

newNoTcEvBinds :: TcS EvBindsVar
newNoTcEvBinds = wrapTcS TcM.newNoTcEvBinds

newEvVar :: TcPredType -> TcS EvVar
newEvVar pred = wrapTcS (TcM.newEvVar pred)

newGivenEvVar :: CtLoc -> (TcPredType, EvTerm) -> TcS CtEvidence
-- Make a new variable of the given PredType,
-- immediately bind it to the given term
-- and return its CtEvidence
-- See Note [Bind new Givens immediately] in GHC.Tc.Types.Constraint
newGivenEvVar loc (pred, rhs)
  = do { new_ev <- newBoundEvVarId pred rhs
       ; return (CtGiven { ctev_pred = pred, ctev_evar = new_ev, ctev_loc = loc }) }

-- | Make a new 'Id' of the given type, bound (in the monad's EvBinds) to the
-- given term
newBoundEvVarId :: TcPredType -> EvTerm -> TcS EvVar
newBoundEvVarId pred rhs
  = do { new_ev <- newEvVar pred
       ; setEvBind (mkGivenEvBind new_ev rhs)
       ; return new_ev }

newGivenEvVars :: CtLoc -> [(TcPredType, EvTerm)] -> TcS [CtEvidence]
newGivenEvVars loc pts = mapM (newGivenEvVar loc) pts

emitNewWantedEq :: CtLoc -> Role -> TcType -> TcType -> TcS Coercion
-- | Emit a new Wanted equality into the work-list
emitNewWantedEq loc role ty1 ty2
  = do { (ev, co) <- newWantedEq loc role ty1 ty2
       ; updWorkListTcS (extendWorkListEq (mkNonCanonical ev))
       ; return co }

-- | Make a new equality CtEvidence
newWantedEq :: CtLoc -> Role -> TcType -> TcType
            -> TcS (CtEvidence, Coercion)
newWantedEq = newWantedEq_SI WDeriv

newWantedEq_SI :: ShadowInfo -> CtLoc -> Role
               -> TcType -> TcType
               -> TcS (CtEvidence, Coercion)
newWantedEq_SI si loc role ty1 ty2
  = do { hole <- wrapTcS $ TcM.newCoercionHole pty
       ; traceTcS "Emitting new coercion hole" (ppr hole <+> dcolon <+> ppr pty)
       ; return ( CtWanted { ctev_pred = pty, ctev_dest = HoleDest hole
                           , ctev_nosh = si
                           , ctev_loc = loc}
                , mkHoleCo hole ) }
  where
    pty = mkPrimEqPredRole role ty1 ty2

-- no equalities here. Use newWantedEq instead
newWantedEvVarNC :: CtLoc -> TcPredType -> TcS CtEvidence
newWantedEvVarNC = newWantedEvVarNC_SI WDeriv

newWantedEvVarNC_SI :: ShadowInfo -> CtLoc -> TcPredType -> TcS CtEvidence
-- Don't look up in the solved/inerts; we know it's not there
newWantedEvVarNC_SI si loc pty
  = do { new_ev <- newEvVar pty
       ; traceTcS "Emitting new wanted" (ppr new_ev <+> dcolon <+> ppr pty $$
                                         pprCtLoc loc)
       ; return (CtWanted { ctev_pred = pty, ctev_dest = EvVarDest new_ev
                          , ctev_nosh = si
                          , ctev_loc = loc })}

newWantedEvVar :: CtLoc -> TcPredType -> TcS MaybeNew
newWantedEvVar = newWantedEvVar_SI WDeriv

newWantedEvVar_SI :: ShadowInfo -> CtLoc -> TcPredType -> TcS MaybeNew
-- For anything except ClassPred, this is the same as newWantedEvVarNC
newWantedEvVar_SI si loc pty
  = do { mb_ct <- lookupInInerts loc pty
       ; case mb_ct of
            Just ctev
              | not (isDerived ctev)
              -> do { traceTcS "newWantedEvVar/cache hit" $ ppr ctev
                    ; return $ Cached (ctEvExpr ctev) }
            _ -> do { ctev <- newWantedEvVarNC_SI si loc pty
                    ; return (Fresh ctev) } }

newWanted :: CtLoc -> PredType -> TcS MaybeNew
-- Deals with both equalities and non equalities. Tries to look
-- up non-equalities in the cache
newWanted = newWanted_SI WDeriv

newWanted_SI :: ShadowInfo -> CtLoc -> PredType -> TcS MaybeNew
newWanted_SI si loc pty
  | Just (role, ty1, ty2) <- getEqPredTys_maybe pty
  = Fresh . fst <$> newWantedEq_SI si loc role ty1 ty2
  | otherwise
  = newWantedEvVar_SI si loc pty

-- deals with both equalities and non equalities. Doesn't do any cache lookups.
newWantedNC :: CtLoc -> PredType -> TcS CtEvidence
newWantedNC loc pty
  | Just (role, ty1, ty2) <- getEqPredTys_maybe pty
  = fst <$> newWantedEq loc role ty1 ty2
  | otherwise
  = newWantedEvVarNC loc pty

emitNewDeriveds :: CtLoc -> [TcPredType] -> TcS ()
emitNewDeriveds loc preds
  | null preds
  = return ()
  | otherwise
  = do { evs <- mapM (newDerivedNC loc) preds
       ; traceTcS "Emitting new deriveds" (ppr evs)
       ; updWorkListTcS (extendWorkListDeriveds evs) }

emitNewDerivedEq :: CtLoc -> Role -> TcType -> TcType -> TcS ()
-- Create new equality Derived and put it in the work list
-- There's no caching, no lookupInInerts
emitNewDerivedEq loc role ty1 ty2
  = do { ev <- newDerivedNC loc (mkPrimEqPredRole role ty1 ty2)
       ; traceTcS "Emitting new derived equality" (ppr ev $$ pprCtLoc loc)
       ; updWorkListTcS (extendWorkListEq (mkNonCanonical ev)) }
         -- Very important: put in the wl_eqs
         -- See Note [Prioritise equalities] (Avoiding fundep iteration)

newDerivedNC :: CtLoc -> TcPredType -> TcS CtEvidence
newDerivedNC loc pred
  = return $ CtDerived { ctev_pred = pred, ctev_loc = loc }

-- --------- Check done in GHC.Tc.Solver.Interact.selectNewWorkItem???? ---------
-- | Checks if the depth of the given location is too much. Fails if
-- it's too big, with an appropriate error message.
checkReductionDepth :: CtLoc -> TcType   -- ^ type being reduced
                    -> TcS ()
checkReductionDepth loc ty
  = do { dflags <- getDynFlags
       ; when (subGoalDepthExceeded dflags (ctLocDepth loc)) $
         wrapErrTcS $
         solverDepthErrorTcS loc ty }

matchFam :: TyCon -> [Type] -> TcS (Maybe (CoercionN, TcType))
-- Given (F tys) return (ty, co), where co :: ty ~N F tys
matchFam tycon args = fmap (fmap (first mkTcSymCo)) $ wrapTcS $ matchFamTcM tycon args

matchFamTcM :: TyCon -> [Type] -> TcM (Maybe (CoercionN, TcType))
-- Given (F tys) return (ty, co), where co :: F tys ~N ty
matchFamTcM tycon args
  = do { fam_envs <- FamInst.tcGetFamInstEnvs
       ; let match_fam_result
              = reduceTyFamApp_maybe fam_envs Nominal tycon args
       ; TcM.traceTc "matchFamTcM" $
         vcat [ text "Matching:" <+> ppr (mkTyConApp tycon args)
              , ppr_res match_fam_result ]
       ; return match_fam_result }
  where
    ppr_res Nothing        = text "Match failed"
    ppr_res (Just (co,ty)) = hang (text "Match succeeded:")
                                2 (vcat [ text "Rewrites to:" <+> ppr ty
                                        , text "Coercion:" <+> ppr co ])

{-
Note [Residual implications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The wl_implics in the WorkList are the residual implication
constraints that are generated while solving or canonicalising the
current worklist.  Specifically, when canonicalising
   (forall a. t1 ~ forall a. t2)
from which we get the implication
   (forall a. t1 ~ t2)
See GHC.Tc.Solver.Monad.deferTcSForAllEq
-}

{-
************************************************************************
*                                                                      *
              Breaking type variable cycles
*                                                                      *
************************************************************************
-}

-- | Replace all type family applications in the RHS with fresh variables,
-- emitting givens that relate the type family application to the variable.
-- See Note [Type variable cycles in Givens] in GHC.Tc.Solver.Canonical.
breakTyVarCycle :: CtLoc
                -> TcType      -- the RHS
                -> TcS TcType  -- new RHS that doesn't have any type families
-- This could be considerably more efficient. See Detail (5) of Note.
breakTyVarCycle loc = go
  where
    go ty | Just ty' <- rewriterView ty = go ty'
    go (Rep.TyConApp tc tys)
      | isTypeFamilyTyCon tc
      = do { let (fun_args, extra_args) = splitAt (tyConArity tc) tys
                 fun_app                = mkTyConApp tc fun_args
                 fun_app_kind           = tcTypeKind fun_app
           ; new_tv <- wrapTcS (TcM.newCycleBreakerTyVar fun_app_kind)
           ; let new_ty     = mkTyVarTy new_tv
                 given_pred = mkHeteroPrimEqPred fun_app_kind fun_app_kind
                                                 fun_app new_ty
                 given_term = evCoercion $ mkNomReflCo new_ty  -- See Detail (4) of Note
           ; new_given <- newGivenEvVar loc (given_pred, given_term)
           ; traceTcS "breakTyVarCycle replacing type family" (ppr new_given)
           ; emitWorkNC [new_given]
           ; updInertTcS $ \is ->
               is { inert_cycle_breakers = (new_tv, fun_app) :
                                           inert_cycle_breakers is }
           ; extra_args' <- mapM go extra_args
           ; return (mkAppTys new_ty extra_args') }
              -- Worried that this substitution will change kinds?
              -- See Detail (3) of Note

      | otherwise
      = mkTyConApp tc <$> mapM go tys

    go (Rep.AppTy ty1 ty2)       = mkAppTy <$> go ty1 <*> go ty2
    go (Rep.FunTy vis w arg res) = mkFunTy vis <$> go w <*> go arg <*> go res
    go (Rep.CastTy ty co)        = mkCastTy <$> go ty <*> pure co

    go ty@(Rep.TyVarTy {})    = return ty
    go ty@(Rep.LitTy {})      = return ty
    go ty@(Rep.ForAllTy {})   = return ty  -- See Detail (1) of Note
    go ty@(Rep.CoercionTy {}) = return ty  -- See Detail (2) of Note

-- | Fill in CycleBreakerTvs with the variables they stand for.
-- See Note [Type variable cycles in Givens] in GHC.Tc.Solver.Canonical.
restoreTyVarCycles :: InertSet -> TcM ()
restoreTyVarCycles is
  = forM_ (inert_cycle_breakers is) $ \ (cycle_breaker_tv, orig_ty) ->
    TcM.writeMetaTyVar cycle_breaker_tv orig_ty

-- Unwrap a type synonym only when either:
--   The type synonym is forgetful, or
--   the type synonym mentions a type family in its expansion
-- See Note [Rewriting synonyms] in GHC.Tc.Solver.Rewrite.
rewriterView :: TcType -> Maybe TcType
rewriterView ty@(Rep.TyConApp tc _)
  | isForgetfulSynTyCon tc || (isTypeSynonymTyCon tc && not (isFamFreeTyCon tc))
  = tcView ty
rewriterView _other = Nothing
