{-# LANGUAGE CPP, TypeFamilies #-}

-- Type definitions for the constraint solver
module TcSMonad (

    -- The work list
    WorkList(..), isEmptyWorkList, emptyWorkList,
    extendWorkListNonEq, extendWorkListCt,
    extendWorkListCts, extendWorkListEq, extendWorkListFunEq,
    appendWorkList, extendWorkListImplic,
    selectNextWorkItem,
    workListSize, workListWantedCount,
    getWorkList, updWorkListTcS,

    -- The TcS monad
    TcS, runTcS, runTcSDeriveds, runTcSWithEvBinds,
    failTcS, warnTcS, addErrTcS,
    runTcSEqualities,
    nestTcS, nestImplicTcS, setEvBindsTcS,
    checkConstraintsTcS, checkTvConstraintsTcS,

    runTcPluginTcS, addUsedGRE, addUsedGREs,
    matchGlobalInst, TcM.ClsInstResult(..),

    QCInst(..),

    -- Tracing etc
    panicTcS, traceTcS,
    traceFireTcS, bumpStepCountTcS, csTraceTcS,
    wrapErrTcS, wrapWarnTcS,

    -- Evidence creation and transformation
    MaybeNew(..), freshGoals, isFresh, getEvExpr,

    newTcEvBinds, newNoTcEvBinds,
    newWantedEq, emitNewWantedEq,
    newWanted, newWantedEvVar, newWantedNC, newWantedEvVarNC, newDerivedNC,
    newBoundEvVarId,
    unifyTyVar, unflattenFmv, reportUnifications,
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
    InertSet(..), InertCans(..),
    updInertTcS, updInertCans, updInertDicts, updInertIrreds,
    getNoGivenEqs, setInertCans,
    getInertEqs, getInertCans, getInertGivens,
    getInertInsols,
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

    -- Inert CTyEqCans
    EqualCtList, findTyEqs, foldTyEqs, isInInertEqs,
    lookupFlattenTyVar, lookupInertTyVar,

    -- Inert solved dictionaries
    addSolvedDict, lookupSolvedDict,

    -- Irreds
    foldIrreds,

    -- The flattening cache
    lookupFlatCache, extendFlatCache, newFlattenSkolem,            -- Flatten skolems
    dischargeFunEq, pprKicked,

    -- Inert CFunEqCans
    updInertFunEqs, findFunEq,
    findFunEqsByTyCon,

    instDFunType,                              -- Instantiation

    -- MetaTyVars
    newFlexiTcSTy, instFlexi, instFlexiX,
    cloneMetaTyVar, demoteUnfilledFmv,
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
    pprEq                                    -- Smaller utils, re-exported from TcM
                                             -- TODO (DV): these are only really used in the
                                             -- instance matcher in TcSimplify. I am wondering
                                             -- if the whole instance matcher simply belongs
                                             -- here
) where

#include "HsVersions.h"

import GhcPrelude

import HscTypes

import qualified Inst as TcM
import InstEnv
import FamInst
import FamInstEnv

import qualified TcRnMonad as TcM
import qualified TcMType as TcM
import qualified ClsInst as TcM( matchGlobalInst, ClsInstResult(..) )
import qualified TcEnv as TcM
       ( checkWellStaged, tcGetDefaultTys, tcLookupClass, tcLookupId, topIdLvl )
import PrelNames( heqTyConKey, eqTyConKey )
import ClsInst( InstanceWhat(..) )
import Kind
import TcType
import DynFlags
import Type
import Coercion
import Unify

import TcEvidence
import Class
import TyCon
import TcErrors   ( solverDepthErrorTcS )

import Name
import Module ( HasModule, getModule )
import RdrName ( GlobalRdrEnv, GlobalRdrElt )
import qualified RnEnv as TcM
import Var
import VarEnv
import VarSet
import Outputable
import Bag
import UniqSupply
import Util
import TcRnTypes

import Unique
import UniqFM
import UniqDFM
import Maybes

import CoreMap
import Control.Monad
import qualified Control.Monad.Fail as MonadFail
import MonadUtils
import Data.IORef
import Data.List ( partition, mapAccumL )

#if defined(DEBUG)
import Digraph
import UniqSet
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
A WorkList contains canonical and non-canonical items (of all flavors).
Notice that each Ct now has a simplification depth. We may
consider using this depth for prioritization as well in the future.

As a simple form of priority queue, our worklist separates out

* equalities (wl_eqs); see Note [Prioritise equalities]
* type-function equalities (wl_funeqs)
* all the rest (wl_rest)

Note [Prioritise equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's very important to process equalities /first/:

* (Efficiency)  The general reason to do so is that if we process a
  class constraint first, we may end up putting it into the inert set
  and then kicking it out later.  That's extra work compared to just
  doing the equality first.

* (Avoiding fundep iteration) As Trac #14723 showed, it's possible to
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
  homo-kinded when kicked out, and hence we want to priotitise it.

* (Derived equalities) Originally we tried to postpone processing
  Derived equalities, in the hope that we might never need to deal
  with them at all; but in fact we must process Derived equalities
  eagerly, partly for the (Efficiency) reason, and more importantly
  for (Avoiding fundep iteration).

Note [Prioritise class equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We prioritise equalities in the solver (see selectWorkItem). But class
constraints like (a ~ b) and (a ~~ b) are actually equalities too;
see Note [The equality types story] in TysPrim.

Failing to prioritise these is inefficient (more kick-outs etc).
But, worse, it can prevent us spotting a "recursive knot" among
Wanted constraints.  See comment:10 of Trac #12734 for a worked-out
example.

So we arrange to put these particular class constraints in the wl_eqs.

  NB: since we do not currently apply the substitution to the
  inert_solved_dicts, the knot-tying still seems a bit fragile.
  But this makes it better.
-}

-- See Note [WorkList priorities]
data WorkList
  = WL { wl_eqs     :: [Ct]  -- CTyEqCan, CDictCan, CIrredCan
                             -- Given, Wanted, and Derived
                       -- Contains both equality constraints and their
                       -- class-level variants (a~b) and (a~~b);
                       -- See Note [Prioritise equalities]
                       -- See Note [Prioritise class equalities]

       , wl_funeqs  :: [Ct]

       , wl_rest    :: [Ct]

       , wl_implics :: Bag Implication  -- See Note [Residual implications]
    }

appendWorkList :: WorkList -> WorkList -> WorkList
appendWorkList
    (WL { wl_eqs = eqs1, wl_funeqs = funeqs1, wl_rest = rest1
        , wl_implics = implics1 })
    (WL { wl_eqs = eqs2, wl_funeqs = funeqs2, wl_rest = rest2
        , wl_implics = implics2 })
   = WL { wl_eqs     = eqs1     ++ eqs2
        , wl_funeqs  = funeqs1  ++ funeqs2
        , wl_rest    = rest1    ++ rest2
        , wl_implics = implics1 `unionBags`   implics2 }

workListSize :: WorkList -> Int
workListSize (WL { wl_eqs = eqs, wl_funeqs = funeqs, wl_rest = rest })
  = length eqs + length funeqs + length rest

workListWantedCount :: WorkList -> Int
-- Count the things we need to solve
-- excluding the insolubles (c.f. inert_count)
workListWantedCount (WL { wl_eqs = eqs, wl_rest = rest })
  = count isWantedCt eqs + count is_wanted rest
  where
    is_wanted ct
     | CIrredCan { cc_ev = ev, cc_insol = insol } <- ct
     = not insol && isWanted ev
     | otherwise
     = isWantedCt ct

extendWorkListEq :: Ct -> WorkList -> WorkList
extendWorkListEq ct wl = wl { wl_eqs = ct : wl_eqs wl }

extendWorkListFunEq :: Ct -> WorkList -> WorkList
extendWorkListFunEq ct wl = wl { wl_funeqs = ct : wl_funeqs wl }

extendWorkListNonEq :: Ct -> WorkList -> WorkList
-- Extension by non equality
extendWorkListNonEq ct wl = wl { wl_rest = ct : wl_rest wl }

extendWorkListDeriveds :: [CtEvidence] -> WorkList -> WorkList
extendWorkListDeriveds evs wl
  = extendWorkListCts (map mkNonCanonical evs) wl

extendWorkListImplic :: Bag Implication -> WorkList -> WorkList
extendWorkListImplic implics wl = wl { wl_implics = implics `unionBags` wl_implics wl }

extendWorkListCt :: Ct -> WorkList -> WorkList
-- Agnostic
extendWorkListCt ct wl
 = case classifyPredType (ctPred ct) of
     EqPred NomEq ty1 _
       | Just tc <- tcTyConAppTyCon_maybe ty1
       , isTypeFamilyTyCon tc
       -> extendWorkListFunEq ct wl

     EqPred {}
       -> extendWorkListEq ct wl

     ClassPred cls _  -- See Note [Prioritise class equalities]
       |  cls `hasKey` heqTyConKey
       || cls `hasKey` eqTyConKey
       -> extendWorkListEq ct wl

     _ -> extendWorkListNonEq ct wl

extendWorkListCts :: [Ct] -> WorkList -> WorkList
-- Agnostic
extendWorkListCts cts wl = foldr extendWorkListCt wl cts

isEmptyWorkList :: WorkList -> Bool
isEmptyWorkList (WL { wl_eqs = eqs, wl_funeqs = funeqs
                    , wl_rest = rest, wl_implics = implics })
  = null eqs && null rest && null funeqs && isEmptyBag implics

emptyWorkList :: WorkList
emptyWorkList = WL { wl_eqs  = [], wl_rest = []
                   , wl_funeqs = [], wl_implics = emptyBag }

selectWorkItem :: WorkList -> Maybe (Ct, WorkList)
-- See Note [Prioritise equalities]
selectWorkItem wl@(WL { wl_eqs = eqs, wl_funeqs = feqs
                      , wl_rest = rest })
  | ct:cts <- eqs  = Just (ct, wl { wl_eqs    = cts })
  | ct:fes <- feqs = Just (ct, wl { wl_funeqs = fes })
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
         -- This is done by TcInteract.chooseInstance
       ; writeTcRef wl_var new_wl
       ; return (Just ct) } } }

-- Pretty printing
instance Outputable WorkList where
  ppr (WL { wl_eqs = eqs, wl_funeqs = feqs
          , wl_rest = rest, wl_implics = implics })
   = text "WL" <+> (braces $
     vcat [ ppUnless (null eqs) $
            text "Eqs =" <+> vcat (map ppr eqs)
          , ppUnless (null feqs) $
            text "Funeqs =" <+> vcat (map ppr feqs)
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

       , inert_fsks :: [(TcTyVar, TcType)]
              -- A list of (fsk, ty) pairs; we add one element when we flatten
              -- a function application in a Given constraint, creating
              -- a new fsk in newFlattenSkolem.  When leaving a nested scope,
              -- unflattenGivens unifies fsk := ty
              --
              -- We could also get this info from inert_funeqs, filtered by
              -- level, but it seems simpler and more direct to capture the
              -- fsk as we generate them.

       , inert_flat_cache :: ExactFunEqMap (TcCoercion, TcType, CtFlavour)
              -- See Note [Type family equations]
              -- If    F tys :-> (co, rhs, flav),
              -- then  co :: F tys ~ rhs
              --       flav is [G] or [WD]
              --
              -- Just a hash-cons cache for use when flattening only
              -- These include entirely un-processed goals, so don't use
              -- them to solve a top-level goal, else you may end up solving
              -- (w:F ty ~ a) by setting w:=w!  We just use the flat-cache
              -- when allocating a new flatten-skolem.
              -- Not necessarily inert wrt top-level equations (or inert_cans)

              -- NB: An ExactFunEqMap -- this doesn't match via loose types!

       , inert_solved_dicts   :: DictMap CtEvidence
              -- All Wanteds, of form ev :: C t1 .. tn
              -- See Note [Solved dictionaries]
              -- and Note [Do not add superclasses of solved dictionaries]
       }

instance Outputable InertSet where
  ppr (IS { inert_cans = ics
          , inert_fsks = ifsks
          , inert_solved_dicts = solved_dicts })
      = vcat [ ppr ics
             , text "Inert fsks =" <+> ppr ifsks
             , ppUnless (null dicts) $
               text "Solved dicts =" <+> vcat (map ppr dicts) ]
         where
           dicts = bagToList (dictsToBag solved_dicts)

emptyInertCans :: InertCans
emptyInertCans
  = IC { inert_count    = 0
       , inert_eqs      = emptyDVarEnv
       , inert_dicts    = emptyDicts
       , inert_safehask = emptyDicts
       , inert_funeqs   = emptyFunEqs
       , inert_insts    = []
       , inert_irreds   = emptyCts }

emptyInert :: InertSet
emptyInert
  = IS { inert_cans         = emptyInertCans
       , inert_fsks         = []
       , inert_flat_cache   = emptyExactFunEqs
       , inert_solved_dicts = emptyDictMap }


{- Note [Solved dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we apply a top-level instance declaration, we add the "solved"
dictionary to the inert_solved_dicts.  In general, we use it to avoid
creating a new EvVar when we have a new goal that we have solved in
the past.

But in particular, we can use it to create *recursive* dictionaries.
The simplest, degnerate case is
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
Other notes about solved dictionaries

* See also Note [Do not add superclasses of solved dictionaries]

* The inert_solved_dicts field is not rewritten by equalities,
  so it may get out of date.

* THe inert_solved_dicts are all Wanteds, never givens

* We only cache dictionaries from top-level instances, not from
  local quantified constraints.  Reason: if we cached the latter
  we'd need to purge the cache when bringing new quantified
  constraints into scope, because quantified constraints "shadow"
  top-level instances.

Note [Do not add superclasses of solved dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Every member of inert_solved_dicts is the result of applying a dictionary
function, NOT of applying superclass selection to anything.
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
              -- All CTyEqCans; index is the LHS tyvar
              -- Domain = skolems and untouchables; a touchable would be unified

       , inert_funeqs :: FunEqMap Ct
              -- All CFunEqCans; index is the whole family head type.
              -- All Nominal (that's an invarint of all CFunEqCans)
              -- LHS is fully rewritten (modulo eqCanRewrite constraints)
              --     wrt inert_eqs
              -- Can include all flavours, [G], [W], [WD], [D]
              -- See Note [Type family equations]

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
              -- in TcSimplify

       , inert_irreds :: Cts
              -- Irreducible predicates that cannot be made canonical,
              --     and which don't interact with others (e.g.  (c a))
              -- and insoluble predicates (e.g.  Int ~ Bool, or a ~ [a])

       , inert_count :: Int
              -- Number of Wanted goals in
              --     inert_eqs, inert_dicts, inert_safehask, inert_irreds
              -- Does not include insolubles
              -- When non-zero, keep trying to solve
       }

type InertEqs    = DTyVarEnv EqualCtList
type EqualCtList = [Ct]  -- See Note [EqualCtList invariants]

{- Note [Detailed InertCans Invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The InertCans represents a collection of constraints with the following properties:

  * All canonical

  * No two dictionaries with the same head
  * No two CIrreds with the same type

  * Family equations inert wrt top-level family axioms

  * Dictionaries have no matching top-level instance

  * Given family or dictionary constraints don't mention touchable
    unification variables

  * Non-CTyEqCan constraints are fully rewritten with respect
    to the CTyEqCan equalities (modulo canRewrite of course;
    eg a wanted cannot rewrite a given)

  * CTyEqCan equalities: see Note [Applying the inert substitution]
                         in TcFlatten

Note [EqualCtList invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    * All are equalities
    * All these equalities have the same LHS
    * The list is never empty
    * No element of the list can rewrite any other
    * Derived before Wanted

From the fourth invariant it follows that the list is
   - A single [G], or
   - Zero or one [D] or [WD], followd by any number of [W]

The Wanteds can't rewrite anything which is why we put them last

Note [Type family equations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Type-family equations, CFunEqCans, of form (ev : F tys ~ ty),
live in three places

  * The work-list, of course

  * The inert_funeqs are un-solved but fully processed, and in
    the InertCans. They can be [G], [W], [WD], or [D].

  * The inert_flat_cache.  This is used when flattening, to get maximal
    sharing. Everthing in the inert_flat_cache is [G] or [WD]

    It contains lots of things that are still in the work-list.
    E.g Suppose we have (w1: F (G a) ~ Int), and (w2: H (G a) ~ Int) in the
        work list.  Then we flatten w1, dumping (w3: G a ~ f1) in the work
        list.  Now if we flatten w2 before we get to w3, we still want to
        share that (G a).
    Because it contains work-list things, DO NOT use the flat cache to solve
    a top-level goal.  Eg in the above example we don't want to solve w3
    using w3 itself!

The CFunEqCan Ownership Invariant:

  * Each [G/W/WD] CFunEqCan has a distinct fsk or fmv
    It "owns" that fsk/fmv, in the sense that:
      - reducing a [W/WD] CFunEqCan fills in the fmv
      - unflattening a [W/WD] CFunEqCan fills in the fmv
      (in both cases unless an occurs-check would result)

  * In contrast a [D] CFunEqCan does not "own" its fmv:
      - reducing a [D] CFunEqCan does not fill in the fmv;
        it just generates an equality
      - unflattening ignores [D] CFunEqCans altogether


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
A "generalised substitution" S is a set of triples (a -f-> t), where
  a is a type variable
  t is a type
  f is a flavour
such that
  (WF1) if (a -f1-> t1) in S
           (a -f2-> t2) in S
        then neither (f1 >= f2) nor (f2 >= f1) hold
  (WF2) if (a -f-> t) is in S, then t /= a

Definition [Applying a generalised substitution]
If S is a generalised substitution
   S(f,a) = t,  if (a -fs-> t) in S, and fs >= f
          = a,  otherwise
Application extends naturally to types S(f,t), modulo roles.
See Note [Flavours with roles].

Theorem: S(f,a) is well defined as a function.
Proof: Suppose (a -f1-> t1) and (a -f2-> t2) are both in S,
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
   the inert CTyEqCans should be an inert generalised substitution
----------------------------------------------------------------

Note that inertness is not the same as idempotence.  To apply S to a
type, you may have to apply it recursive.  But inertness does
guarantee that this recursive use will terminate.

Note [Extending the inert equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Main Theorem [Stability under extension]
   Suppose we have a "work item"
       a -fw-> t
   and an inert generalised substitution S,
   THEN the extended substitution T = S+(a -fw-> t)
        is an inert generalised substitution
   PROVIDED
      (T1) S(fw,a) = a     -- LHS of work-item is a fixpoint of S(fw,_)
      (T2) S(fw,t) = t     -- RHS of work-item is a fixpoint of S(fw,_)
      (T3) a not in t      -- No occurs check in the work item

      AND, for every (b -fs-> s) in S:
           (K0) not (fw >= fs)
                Reason: suppose we kick out (a -fs-> s),
                        and add (a -fw-> t) to the inert set.
                        The latter can't rewrite the former,
                        so the kick-out achieved nothing

           OR { (K1) not (a = b)
                     Reason: if fw >= fs, WF1 says we can't have both
                             a -fw-> t  and  a -fs-> s

                AND (K2): guarantees inertness of the new substitution
                    {  (K2a) not (fs >= fs)
                    OR (K2b) fs >= fw
                    OR (K2d) a not in s }

                AND (K3) See Note [K3: completeness of solving]
                    { (K3a) If the role of fs is nominal: s /= a
                      (K3b) If the role of fs is representational:
                            s is not of form (a t1 .. tn) } }


Conditions (T1-T3) are established by the canonicaliser
Conditions (K1-K3) are established by TcSMonad.kickOutRewritable

The idea is that
* (T1-2) are guaranteed by exhaustively rewriting the work-item
  with S(fw,_).

* T3 is guaranteed by a simple occurs-check on the work item.
  This is done during canonicalisation, in canEqTyVar;
  (invariant: a CTyEqCan never has an occurs check).

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
              (a -fs-> t) in S, s.t.  (fs >= fw).
  Proof. Suppose the contrary (fs >= fw).  Then because of (T1),
  S(fw,a)=a.  But since fs>=fw, S(fw,a) = s, hence s=a.  But now we
  have (a -fs-> a) in S, which contradicts (WF2).

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
TcType.isTyVarHead. This is encoded in (K3b).

Beware: if we make this test succeed too often, we kick out too much,
and the solver might loop.  Consider (Trac #14363)
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
          , inert_insts = insts
          , inert_count = count })
    = braces $ vcat
      [ ppUnless (isEmptyDVarEnv eqs) $
        text "Equalities:"
          <+> pprCts (foldDVarEnv (\eqs rest -> listToBag eqs `andCts` rest) emptyCts eqs)
      , ppUnless (isEmptyTcAppMap funeqs) $
        text "Type-function equalities =" <+> pprCts (funEqsToBag funeqs)
      , ppUnless (isEmptyTcAppMap dicts) $
        text "Dictionaries =" <+> pprCts (dictsToBag dicts)
      , ppUnless (isEmptyTcAppMap safehask) $
        text "Safe Haskell unsafe overlap =" <+> pprCts (dictsToBag safehask)
      , ppUnless (isEmptyCts irreds) $
        text "Irreds =" <+> pprCts irreds
      , ppUnless (null insts) $
        text "Given instances =" <+> vcat (map ppr insts)
      , text "Unsolved goals =" <+> int count
      ]

{- *********************************************************************
*                                                                      *
             Shadow constraints and improvement
*                                                                      *
************************************************************************

Note [The improvement story and derived shadows]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Because Wanteds cannot rewrite Wanteds (see Note [Wanteds do not
rewrite Wanteds] in TcRnTypes), we may miss some opportunities for
solving.  Here's a classic example (indexed-types/should_fail/T4093a)

    Ambiguity check for f: (Foo e ~ Maybe e) => Foo e

    We get [G] Foo e ~ Maybe e
           [W] Foo e ~ Foo ee      -- ee is a unification variable
           [W] Foo ee ~ Maybe ee

    Flatten: [G] Foo e ~ fsk
             [G] fsk ~ Maybe e   -- (A)

             [W] Foo ee ~ fmv
             [W] fmv ~ fsk       -- (B) From Foo e ~ Foo ee
             [W] fmv ~ Maybe ee

    --> rewrite (B) with (A)
             [W] Foo ee ~ fmv
             [W] fmv ~ Maybe e
             [W] fmv ~ Maybe ee

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
    [WD] Foo ee ~ fmv
    [WD] fmv ~ Maybe e
    [WD] fmv ~ Maybe ee

But now when [WD] fmv ~ Maybe ee is about to be added, we'll
split it into [W] and [D], since the inert [WD] fmv ~ Maybe e
can rewrite it.  Then:
    work item: [D] fmv ~ Maybe ee
    inert:     [W] fmv ~ Maybe ee
               [WD] fmv ~ Maybe e   -- (C)
               [WD] Foo ee ~ fmv

See Note [Splitting WD constraints].  Now the work item is rewritten
by (C) and we soon get ee := e.

Additional notes:

  * The derived shadow equalities live in inert_eqs, along with
    the Givens and Wanteds; see Note [EqualCtList invariants].

  * We make Derived shadows only for Wanteds, not Givens.  So we
    have only [G], not [GD] and [G] plus splitting.  See
    Note [Add derived shadows only for Wanteds]

  * We also get Derived equalities from functional dependencies
    and type-function injectivity; see calls to unifyDerived.

  * This splitting business applies to CFunEqCans too; and then
    we do apply type-function reductions to the [D] CFunEqCan.
    See Note [Reduction for Derived CFunEqCans]

  * It's worth having [WD] rather than just [W] and [D] because
    * efficiency: silly to process the same thing twice
    * inert_funeqs, inert_dicts is a finite map keyed by
      the type; it's inconvenient for it to map to TWO constraints

Note [Splitting WD constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We are about to add a [WD] constraint to the inert set; and we
know that the inert set has fully rewritten it.  Should we split
it into [W] and [D], and put the [D] in the work list for further
work?

* CDictCan (C tys) or CFunEqCan (F tys ~ fsk):
  Yes if the inert set could rewrite tys to make the class constraint,
  or type family, fire.  That is, yes if the inert_eqs intersects
  with the free vars of tys.  For this test we use
  (anyRewritableTyVar True) which ignores casts and coercions in tys,
  because rewriting the casts or coercions won't make the thing fire
  more often.

* CTyEqCan (a ~ ty): Yes if the inert set could rewrite 'a' or 'ty'.
  We need to check both 'a' and 'ty' against the inert set:
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

This is heterogeneous, so we try flattening the kinds.

  co :: F v ~ fmv
  [WD] (a |> co) ~ beta

This is still hetero, so we emit a kind equality and make the work item an
inert Irred.

  work item: [D] fmv ~ alpha
  inert: [WD] (a |> co) ~ beta (CIrredCan)

Can't make progress on the work item. Add to inert set. This kicks out the
old inert, because a [D] can rewrite a [WD].

  work item: [WD] (a |> co) ~ beta
  inert: [D] fmv ~ alpha (CTyEqCan)

Can't make progress on this work item either (although GHC tries by
decomposing the cast and reflattening... but that doesn't make a difference),
which is still hetero. Emit a new kind equality and add to inert set. But,
critically, we split the Irred.

  work list:
   [D] fmv ~ alpha (CTyEqCan)
   [D] (a |> co) ~ beta (CIrred) -- this one was split off
  inert:
   [W] (a |> co) ~ beta
   [D] fmv ~ alpha

We quickly solve the first work item, as it's the same as an inert.

  work item: [D] (a |> co) ~ beta
  inert:
   [W] (a |> co) ~ beta
   [D] fmv ~ alpha

We decompose the cast, yielding

  [D] a ~ beta

We then flatten the kinds. The lhs kind is F v, which flattens to fmv which
then rewrites to alpha.

  co' :: F v ~ alpha
  [D] (a |> co') ~ beta

Now this equality is homo-kinded. So we swizzle it around to

  [D] beta ~ (a |> co')

and set beta := a |> co', and go home happy.

If we don't split the Irreds, we loop. This is all dangerously subtle.

This is triggered by test case typecheck/should_compile/SplitWD.

Note [Examples of how Derived shadows helps completeness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Trac #10009, a very nasty example:

    f :: (UnF (F b) ~ b) => F b -> ()

    g :: forall a. (UnF (F a) ~ a) => a -> ()
    g _ = f (undefined :: F a)

  For g we get [G] UnF (F a) ~ a
               [WD] UnF (F beta) ~ beta
               [WD] F a ~ F beta
  Flatten:
      [G] g1: F a ~ fsk1         fsk1 := F a
      [G] g2: UnF fsk1 ~ fsk2    fsk2 := UnF fsk1
      [G] g3: fsk2 ~ a

      [WD] w1: F beta ~ fmv1
      [WD] w2: UnF fmv1 ~ fmv2
      [WD] w3: fmv2 ~ beta
      [WD] w4: fmv1 ~ fsk1   -- From F a ~ F beta using flat-cache
                             -- and re-orient to put meta-var on left

Rewrite w2 with w4: [D] d1: UnF fsk1 ~ fmv2
React that with g2: [D] d2: fmv2 ~ fsk2
React that with w3: [D] beta ~ fsk2
            and g3: [D] beta ~ a -- Hooray beta := a
And that is enough to solve everything

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
    (b) see Trac #12660 for how adding the derived shadows
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

See Trac #11379 for a case of this.

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
  on [W] constraints. This came up in Trac #12860.
-}

maybeEmitShadow :: InertCans -> Ct -> TcS Ct
-- See Note [The improvement story and derived shadows]
maybeEmitShadow ics ct
  | let ev = ctEvidence ct
  , CtWanted { ctev_pred = pred, ctev_loc = loc
             , ctev_nosh = WDeriv } <- ev
  , shouldSplitWD (inert_eqs ics) ct
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

shouldSplitWD :: InertEqs -> Ct -> Bool
-- Precondition: 'ct' is [WD], and is inert
-- True <=> we should split ct ito [W] and [D] because
--          the inert_eqs can make progress on the [D]
-- See Note [Splitting WD constraints]

shouldSplitWD inert_eqs (CFunEqCan { cc_tyargs = tys })
  = should_split_match_args inert_eqs tys
    -- We don't need to split if the tv is the RHS fsk

shouldSplitWD inert_eqs (CDictCan { cc_tyargs = tys })
  = should_split_match_args inert_eqs tys
    -- NB True: ignore coercions
    -- See Note [Splitting WD constraints]

shouldSplitWD inert_eqs (CTyEqCan { cc_tyvar = tv, cc_rhs = ty
                                  , cc_eq_rel = eq_rel })
  =  tv `elemDVarEnv` inert_eqs
  || anyRewritableTyVar False eq_rel (canRewriteTv inert_eqs) ty
  -- NB False: do not ignore casts and coercions
  -- See Note [Splitting WD constraints]

shouldSplitWD inert_eqs (CIrredCan { cc_ev = ev })
  = anyRewritableTyVar False (ctEvEqRel ev) (canRewriteTv inert_eqs) (ctEvPred ev)

shouldSplitWD _ _ = False   -- No point in splitting otherwise

should_split_match_args :: InertEqs -> [TcType] -> Bool
-- True if the inert_eqs can rewrite anything in the argument
-- types, ignoring casts and coercions
should_split_match_args inert_eqs tys
  = any (anyRewritableTyVar True NomEq (canRewriteTv inert_eqs)) tys
    -- NB True: ignore casts coercions
    -- See Note [Splitting WD constraints]

canRewriteTv :: InertEqs -> EqRel -> TyVar -> Bool
canRewriteTv inert_eqs eq_rel tv
  | Just (ct : _) <- lookupDVarEnv inert_eqs tv
  , CTyEqCan { cc_eq_rel = eq_rel1 } <- ct
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
  = extendDVarEnv_C add_eq old_eqs tv [ct]
  where
    add_eq old_eqs _
      | isWantedCt ct
      , (eq1 : eqs) <- old_eqs
      = eq1 : ct : eqs
      | otherwise
      = ct : old_eqs

foldTyEqs :: (Ct -> b -> b) -> InertEqs -> b -> b
foldTyEqs k eqs z
  = foldDVarEnv (\cts z -> foldr k z cts) z eqs

findTyEqs :: InertCans -> TyVar -> EqualCtList
findTyEqs icans tv = lookupDVarEnv (inert_eqs icans) tv `orElse` []

delTyEq :: InertEqs -> TcTyVar -> TcType -> InertEqs
delTyEq m tv t = modifyDVarEnv (filter (not . isThisOne)) m tv
  where isThisOne (CTyEqCan { cc_rhs = t1 }) = eqType t t1
        isThisOne _                          = False

lookupInertTyVar :: InertEqs -> TcTyVar -> Maybe TcType
lookupInertTyVar ieqs tv
  = case lookupDVarEnv ieqs tv of
      Just (CTyEqCan { cc_rhs = rhs, cc_eq_rel = NomEq } : _ ) -> Just rhs
      _                                                        -> Nothing

lookupFlattenTyVar :: InertEqs -> TcTyVar -> TcType
-- See Note [lookupFlattenTyVar]
lookupFlattenTyVar ieqs ftv
  = lookupInertTyVar ieqs ftv `orElse` mkTyVarTy ftv

{- Note [lookupFlattenTyVar]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have an injective function F and
  inert_funeqs:   F t1 ~ fsk1
                  F t2 ~ fsk2
  inert_eqs:      fsk1 ~ fsk2

We never rewrite the RHS (cc_fsk) of a CFunEqCan.  But we /do/ want to
get the [D] t1 ~ t2 from the injectiveness of F.  So we look up the
cc_fsk of CFunEqCans in the inert_eqs when trying to find derived
equalities arising from injectivity.
-}


{- *********************************************************************
*                                                                      *
                   Inert instances: inert_insts
*                                                                      *
********************************************************************* -}

addInertForAll :: QCInst -> TcS ()
-- Add a local Given instance, typically arising from a type signature
addInertForAll new_qci
  = updInertCans $ \ics ->
    ics { inert_insts = add_qci (inert_insts ics) }
  where
    add_qci :: [QCInst] -> [QCInst]
    -- See Note [Do not add duplicate quantified instances]
    add_qci qcis | any same_qci qcis = qcis
                 | otherwise         = new_qci : qcis

    same_qci old_qci = tcEqType (ctEvPred (qci_ev old_qci))
                                (ctEvPred (qci_ev new_qci))

{- Note [Do not add duplicate quantified instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (Trac #15244):

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

The simplest thing is simply to eliminate duplicattes, which we do here.
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

Note [Kicking out CFunEqCan for fundeps]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider:
   New:    [D] fmv1 ~ fmv2
   Inert:  [W] F alpha ~ fmv1
           [W] F beta  ~ fmv2

where F is injective. The new (derived) equality certainly can't
rewrite the inerts. But we *must* kick out the first one, to get:

   New:   [W] F alpha ~ fmv1
   Inert: [W] F beta ~ fmv2
          [D] fmv1 ~ fmv2

and now improvement will discover [D] alpha ~ beta. This is important;
eg in Trac #9587.

So in kickOutRewritable we look at all the tyvars of the
CFunEqCan, including the fsk.
-}

addInertCan :: Ct -> TcS ()  -- Constraints *other than* equalities
-- Precondition: item /is/ canonical
-- See Note [Adding an equality to the InertCans]
addInertCan ct
  = do { traceTcS "insertInertCan {" $
         text "Trying to insert new inert item:" <+> ppr ct

       ; ics <- getInertCans
       ; ct  <- maybeEmitShadow ics ct
       ; ics <- maybeKickOut ics ct
       ; setInertCans (add_item ics ct)

       ; traceTcS "addInertCan }" $ empty }

maybeKickOut :: InertCans -> Ct -> TcS InertCans
-- For a CTyEqCan, kick out any inert that can be rewritten by the CTyEqCan
maybeKickOut ics ct
  | CTyEqCan { cc_tyvar = tv, cc_ev = ev, cc_eq_rel = eq_rel } <- ct
  = do { (_, ics') <- kickOutRewritable (ctEvFlavour ev, eq_rel) tv ics
       ; return ics' }
  | otherwise
  = return ics

add_item :: InertCans -> Ct -> InertCans
add_item ics item@(CFunEqCan { cc_fun = tc, cc_tyargs = tys })
  = ics { inert_funeqs = insertFunEq (inert_funeqs ics) tc tys item }

add_item ics item@(CTyEqCan { cc_tyvar = tv, cc_ev = ev })
  = ics { inert_eqs   = addTyEq (inert_eqs ics) tv item
        , inert_count = bumpUnsolvedCount ev (inert_count ics) }

add_item ics@(IC { inert_irreds = irreds, inert_count = count })
         item@(CIrredCan { cc_ev = ev, cc_insol = insoluble })
  = ics { inert_irreds = irreds `Bag.snocBag` item
        , inert_count  = if insoluble
                         then count  -- inert_count does not include insolubles
                         else bumpUnsolvedCount ev count }

add_item ics item@(CDictCan { cc_ev = ev, cc_class = cls, cc_tyargs = tys })
  = ics { inert_dicts = addDict (inert_dicts ics) cls tys item
        , inert_count = bumpUnsolvedCount ev (inert_count ics) }

add_item _ item
  = pprPanic "upd_inert set: can't happen! Inserting " $
    ppr item   -- Can't be CNonCanonical, CHoleCan,
               -- because they only land in inert_irreds

bumpUnsolvedCount :: CtEvidence -> Int -> Int
bumpUnsolvedCount ev n | isWanted ev = n+1
                       | otherwise   = n


-----------------------------------------
kickOutRewritable  :: CtFlavourRole  -- Flavour/role of the equality that
                                      -- is being added to the inert set
                    -> TcTyVar        -- The new equality is tv ~ ty
                    -> InertCans
                    -> TcS (Int, InertCans)
kickOutRewritable new_fr new_tv ics
  = do { let (kicked_out, ics') = kick_out_rewritable new_fr new_tv ics
             n_kicked = workListSize kicked_out

       ; unless (n_kicked == 0) $
         do { updWorkListTcS (appendWorkList kicked_out)
            ; csTraceTcS $
              hang (text "Kick out, tv =" <+> ppr new_tv)
                 2 (vcat [ text "n-kicked =" <+> int n_kicked
                         , text "kicked_out =" <+> ppr kicked_out
                         , text "Residual inerts =" <+> ppr ics' ]) }

       ; return (n_kicked, ics') }

kick_out_rewritable :: CtFlavourRole  -- Flavour/role of the equality that
                                      -- is being added to the inert set
                    -> TcTyVar        -- The new equality is tv ~ ty
                    -> InertCans
                    -> (WorkList, InertCans)
-- See Note [kickOutRewritable]
kick_out_rewritable new_fr new_tv
                    ics@(IC { inert_eqs      = tv_eqs
                            , inert_dicts    = dictmap
                            , inert_safehask = safehask
                            , inert_funeqs   = funeqmap
                            , inert_irreds   = irreds
                            , inert_insts    = old_insts
                            , inert_count    = n })
  | not (new_fr `eqMayRewriteFR` new_fr)
  = (emptyWorkList, ics)
        -- If new_fr can't rewrite itself, it can't rewrite
        -- anything else, so no need to kick out anything.
        -- (This is a common case: wanteds can't rewrite wanteds)
        -- Lemma (L2) in Note [Extending the inert equalities]

  | otherwise
  = (kicked_out, inert_cans_in)
  where
    inert_cans_in = IC { inert_eqs      = tv_eqs_in
                       , inert_dicts    = dicts_in
                       , inert_safehask = safehask   -- ??
                       , inert_funeqs   = feqs_in
                       , inert_irreds   = irs_in
                       , inert_insts    = insts_in
                       , inert_count    = n - workListWantedCount kicked_out }

    kicked_out :: WorkList
    -- NB: use extendWorkList to ensure that kicked-out equalities get priority
    -- See Note [Prioritise equality constraints] (Kick-out).
    -- The irreds may include non-canonical (hetero-kinded) equality
    -- constraints, which perhaps may have become soluble after new_tv
    -- is substituted; ditto the dictionaries, which may include (a~b)
    -- or (a~~b) constraints.
    kicked_out = foldrBag extendWorkListCt
                          (emptyWorkList { wl_eqs    = tv_eqs_out
                                         , wl_funeqs = feqs_out })
                          ((dicts_out `andCts` irs_out)
                            `extendCtsList` insts_out)

    (tv_eqs_out, tv_eqs_in) = foldDVarEnv kick_out_eqs ([], emptyDVarEnv) tv_eqs
    (feqs_out,   feqs_in)   = partitionFunEqs  kick_out_ct funeqmap
           -- See Note [Kicking out CFunEqCan for fundeps]
    (dicts_out,  dicts_in)  = partitionDicts   kick_out_ct dictmap
    (irs_out,    irs_in)    = partitionBag     kick_out_ct irreds
      -- Kick out even insolubles: See Note [Rewrite insolubles]
      -- Of course we must kick out irreducibles like (c a), in case
      -- we can rewrite 'c' to something more useful

    -- Kick-out for inert instances
    -- See Note [Quantified constraints] in TcCanonical
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

    fr_can_rewrite_ty :: EqRel -> Type -> Bool
    fr_can_rewrite_ty role ty = anyRewritableTyVar False role
                                                   fr_can_rewrite_tv ty
    fr_can_rewrite_tv :: EqRel -> TyVar -> Bool
    fr_can_rewrite_tv role tv = new_role `eqCanRewrite` role
                             && tv == new_tv

    fr_may_rewrite :: CtFlavourRole -> Bool
    fr_may_rewrite fs = new_fr `eqMayRewriteFR` fs
        -- Can the new item rewrite the inert item?

    kick_out_ct :: Ct -> Bool
    -- Kick it out if the new CTyEqCan can rewrite the inert one
    -- See Note [kickOutRewritable]
    kick_out_ct ct | let fs@(_,role) = ctFlavourRole ct
                   = fr_may_rewrite fs
                   && fr_can_rewrite_ty role (ctPred ct)
                  -- False: ignore casts and coercions
                  -- NB: this includes the fsk of a CFunEqCan.  It can't
                  --     actually be rewritten, but we need to kick it out
                  --     so we get to take advantage of injectivity
                  -- See Note [Kicking out CFunEqCan for fundeps]

    kick_out_eqs :: EqualCtList -> ([Ct], DTyVarEnv EqualCtList)
                 -> ([Ct], DTyVarEnv EqualCtList)
    kick_out_eqs eqs (acc_out, acc_in)
      = (eqs_out ++ acc_out, case eqs_in of
                               []      -> acc_in
                               (eq1:_) -> extendDVarEnv acc_in (cc_tyvar eq1) eqs_in)
      where
        (eqs_out, eqs_in) = partition kick_out_eq eqs

    -- Implements criteria K1-K3 in Note [Extending the inert equalities]
    kick_out_eq (CTyEqCan { cc_tyvar = tv, cc_rhs = rhs_ty
                          , cc_ev = ev, cc_eq_rel = eq_rel })
      | not (fr_may_rewrite fs)
      = False  -- Keep it in the inert set if the new thing can't rewrite it

      -- Below here (fr_may_rewrite fs) is True
      | tv == new_tv              = True        -- (K1)
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

        kick_out_for_completeness
          = case eq_rel of
              NomEq  -> rhs_ty `eqType` mkTyVarTy new_tv
              ReprEq -> isTyVarHead new_tv rhs_ty

    kick_out_eq ct = pprPanic "keep_eq" (ppr ct)

kickOutAfterUnification :: TcTyVar -> TcS Int
kickOutAfterUnification new_tv
  = do { ics <- getInertCans
       ; (n_kicked, ics2) <- kickOutRewritable (Given,NomEq)
                                                 new_tv ics
                     -- Given because the tv := xi is given; NomEq because
                     -- only nominal equalities are solved by unification

       ; setInertCans ics2
       ; return n_kicked }

{- Note [kickOutRewritable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also Note [inert_eqs: the inert equalities].

When we add a new inert equality (a ~N ty) to the inert set,
we must kick out any inert items that could be rewritten by the
new equality, to maintain the inert-set invariants.

  - We want to kick out an existing inert constraint if
    a) the new constraint can rewrite the inert one
    b) 'a' is free in the inert constraint (so that it *will*)
       rewrite it if we kick it out.

    For (b) we use tyCoVarsOfCt, which returns the type variables /and
    the kind variables/ that are directly visible in the type. Hence
    we will have exposed all the rewriting we care about to make the
    most precise kinds visible for matching classes etc. No need to
    kick out constraints that mention type variables whose kinds
    contain this variable!

  - A Derived equality can kick out [D] constraints in inert_eqs,
    inert_dicts, inert_irreds etc.

  - We don't kick out constraints from inert_solved_dicts, and
    inert_solved_funeqs optimistically. But when we lookup we have to
    take the substitution into account


Note [Rewrite insolubles]
~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have an insoluble alpha ~ [alpha], which is insoluble
because an occurs check.  And then we unify alpha := [Int].  Then we
really want to rewrite the insoluble to [Int] ~ [[Int]].  Now it can
be decomposed.  Otherwise we end up with a "Can't match [Int] ~
[[Int]]" which is true, but a bit confusing because the outer type
constructors match.

Similarly, if we have a CHoleCan, we'd like to rewrite it with any
Givens, to give as informative an error messasge as possible
(Trac #12468, #11325).

Hence:
 * In the main simlifier loops in TcSimplify (solveWanteds,
   simpl_loop), we feed the insolubles in solveSimpleWanteds,
   so that they get rewritten (albeit not solved).

 * We kick insolubles out of the inert set, if they can be
   rewritten (see TcSMonad.kick_out_rewritable)

 * We rewrite those insolubles in TcCanonical.
   See Note [Make sure that insolubles are fully rewritten]
-}



--------------
addInertSafehask :: InertCans -> Ct -> InertCans
addInertSafehask ics item@(CDictCan { cc_class = cls, cc_tyargs = tys })
  = ics { inert_safehask = addDict (inert_dicts ics) cls tys item }

addInertSafehask _ item
  = pprPanic "addInertSafehask: can't happen! Inserting " $ ppr item

insertSafeOverlapFailureTcS :: Ct -> TcS ()
-- See Note [Safe Haskell Overlapping Instances Implementation] in TcSimplify
insertSafeOverlapFailureTcS item
  = updInertCans (\ics -> addInertSafehask ics item)

getSafeOverlapFailures :: TcS Cts
-- See Note [Safe Haskell Overlapping Instances Implementation] in TcSimplify
getSafeOverlapFailures
 = do { IC { inert_safehask = safehask } <- getInertCans
      ; return $ foldDicts consCts safehask emptyCts }

--------------
addSolvedDict :: CtEvidence -> Class -> [Type] -> TcS ()
-- Add a new item in the solved set of the monad
-- See Note [Solved dictionaries]
addSolvedDict item cls tys
  | isIPPred (ctEvPred item)    -- Never cache "solved" implicit parameters (not sure why!)
  = return ()
  | otherwise
  = do { traceTcS "updSolvedSetTcs:" $ ppr item
       ; updInertTcS $ \ ics ->
             ics { inert_solved_dicts = addDict (inert_solved_dicts ics) cls tys item } }

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

updInertFunEqs :: (FunEqMap Ct -> FunEqMap Ct) -> TcS ()
-- Modify the inert set with the supplied function
updInertFunEqs upd_fn
  = updInertCans $ \ ics -> ics { inert_funeqs = upd_fn (inert_funeqs ics) }

updInertIrreds :: (Cts -> Cts) -> TcS ()
-- Modify the inert set with the supplied function
updInertIrreds upd_fn
  = updInertCans $ \ ics -> ics { inert_irreds = upd_fn (inert_irreds ics) }

getInertEqs :: TcS (DTyVarEnv EqualCtList)
getInertEqs = do { inert <- getInertCans; return (inert_eqs inert) }

getInertInsols :: TcS Cts
-- Returns insoluble equality constraints
-- specifically including Givens
getInertInsols = do { inert <- getInertCans
                    ; return (filterBag insolubleEqCt (inert_irreds inert)) }

getInertGivens :: TcS [Ct]
-- Returns the Given constraints in the inert set,
-- with type functions *not* unflattened
getInertGivens
  = do { inerts <- getInertCans
       ; let all_cts = foldDicts (:) (inert_dicts inerts)
                     $ foldFunEqs (:) (inert_funeqs inerts)
                     $ concat (dVarEnvElts (inert_eqs inerts))
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
    -- Note [The superclass story] in TcCanonical

getUnsolvedInerts :: TcS ( Bag Implication
                         , Cts     -- Tyvar eqs: a ~ ty
                         , Cts     -- Fun eqs:   F a ~ ty
                         , Cts )   -- All others
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
            unsolved_fun_eqs = foldFunEqs add_if_wanted fun_eqs emptyCts
            unsolved_irreds  = Bag.filterBag is_unsolved irreds
            unsolved_dicts   = foldDicts add_if_unsolved idicts emptyCts
            unsolved_others  = unsolved_irreds `unionBags` unsolved_dicts

      ; implics <- getWorkListImplics

      ; traceTcS "getUnsolvedInerts" $
        vcat [ text " tv eqs =" <+> ppr unsolved_tv_eqs
             , text "fun eqs =" <+> ppr unsolved_fun_eqs
             , text "others =" <+> ppr unsolved_others
             , text "implics =" <+> ppr implics ]

      ; return ( implics, unsolved_tv_eqs, unsolved_fun_eqs, unsolved_others) }
  where
    add_if_unsolved :: Ct -> Cts -> Cts
    add_if_unsolved ct cts | is_unsolved ct = ct `consCts` cts
                           | otherwise      = cts

    is_unsolved ct = not (isGivenCt ct)   -- Wanted or Derived

    -- For CFunEqCans we ignore the Derived ones, and keep
    -- only the Wanteds for flattening.  The Derived ones
    -- share a unification variable with the corresponding
    -- Wanted, so we definitely don't want to participate
    -- in unflattening
    -- See Note [Type family equations]
    add_if_wanted ct cts | isWantedCt ct = ct `consCts` cts
                         | otherwise     = cts

isInInertEqs :: DTyVarEnv EqualCtList -> TcTyVar -> TcType -> Bool
-- True if (a ~N ty) is in the inert set, in either Given or Wanted
isInInertEqs eqs tv rhs
  = case lookupDVarEnv eqs tv of
      Nothing  -> False
      Just cts -> any (same_pred rhs) cts
  where
    same_pred rhs ct
      | CTyEqCan { cc_rhs = rhs2, cc_eq_rel = eq_rel } <- ct
      , NomEq <- eq_rel
      , rhs `eqType` rhs2 = True
      | otherwise         = False

getNoGivenEqs :: TcLevel          -- TcLevel of this implication
               -> [TcTyVar]       -- Skolems of this implication
               -> TcS ( Bool      -- True <=> definitely no residual given equalities
                      , Cts )     -- Insoluble equalities arising from givens
-- See Note [When does an implication have given equalities?]
getNoGivenEqs tclvl skol_tvs
  = do { inerts@(IC { inert_eqs = ieqs, inert_irreds = irreds })
              <- getInertCans
       ; let has_given_eqs = foldrBag ((||) . ct_given_here) False irreds
                          || anyDVarEnv eqs_given_here ieqs
             insols = filterBag insolubleEqCt irreds
                      -- Specifically includes ones that originated in some
                      -- outer context but were refined to an insoluble by
                      -- a local equality; so do /not/ add ct_given_here.

       ; traceTcS "getNoGivenEqs" $
         vcat [ if has_given_eqs then text "May have given equalities"
                                 else text "No given equalities"
              , text "Skols:" <+> ppr skol_tvs
              , text "Inerts:" <+> ppr inerts
              , text "Insols:" <+> ppr insols]
       ; return (not has_given_eqs, insols) }
  where
    eqs_given_here :: EqualCtList -> Bool
    eqs_given_here [ct@(CTyEqCan { cc_tyvar = tv })]
                              -- Givens are always a sigleton
      = not (skolem_bound_here tv) && ct_given_here ct
    eqs_given_here _ = False

    ct_given_here :: Ct -> Bool
    -- True for a Given bound by the current implication,
    -- i.e. the current level
    ct_given_here ct =  isGiven ev
                     && tclvl == ctLocLevel (ctEvLoc ev)
        where
          ev = ctEvidence ct

    skol_tv_set = mkVarSet skol_tvs
    skolem_bound_here tv -- See Note [Let-bound skolems]
      = case tcTyVarDetails tv of
          SkolemTv {} -> tv `elemVarSet` skol_tv_set
          _           -> False

-- | Returns Given constraints that might,
-- potentially, match the given pred. This is used when checking to see if a
-- Given might overlap with an instance. See Note [Instance and Given overlap]
-- in TcInteract.
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
mightMatchLater given_pred given_loc wanted_pred wanted_loc
  =  not (prohibitedSuperClassSolve given_loc wanted_loc)
  && isJust (tcUnifyTys bind_meta_tv [given_pred] [wanted_pred])
  where
    bind_meta_tv :: TcTyVar -> BindFlag
    -- Any meta tyvar may be unified later, so we treat it as
    -- bindable when unifying with givens. That ensures that we
    -- conservatively assume that a meta tyvar might get unified with
    -- something that matches the 'given', until demonstrated
    -- otherwise.  More info in Note [Instance and Given overlap]
    -- in TcInteract
    bind_meta_tv tv | isMetaTyVar tv
                    , not (isFskTyVar tv) = BindMe
                    | otherwise           = Skolem

prohibitedSuperClassSolve :: CtLoc -> CtLoc -> Bool
-- See Note [Solving superclass constraints] in TcInstDcls
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

Note [When does an implication have given equalities?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider an implication
   beta => alpha ~ Int
where beta is a unification variable that has already been unified
to () in an outer scope.  Then we can float the (alpha ~ Int) out
just fine. So when deciding whether the givens contain an equality,
we should canonicalise first, rather than just looking at the original
givens (Trac #8644).

So we simply look at the inert, canonical Givens and see if there are
any equalities among them, the calculation of has_given_eqs.  There
are some wrinkles:

 * We must know which ones are bound in *this* implication and which
   are bound further out.  We can find that out from the TcLevel
   of the Given, which is itself recorded in the tcl_tclvl field
   of the TcLclEnv stored in the Given (ev_given_here).

   What about interactions between inner and outer givens?
      - Outer given is rewritten by an inner given, then there must
        have been an inner given equality, hence the “given-eq” flag
        will be true anyway.

      - Inner given rewritten by outer, retains its level (ie. The inner one)

 * We must take account of *potential* equalities, like the one above:
      beta => ...blah...
   If we still don't know what beta is, we conservatively treat it as potentially
   becoming an equality. Hence including 'irreds' in the calculation or has_given_eqs.

 * When flattening givens, we generate Given equalities like
     <F [a]> : F [a] ~ f,
   with Refl evidence, and we *don't* want those to count as an equality
   in the givens!  After all, the entire flattening business is just an
   internal matter, and the evidence does not mention any of the 'givens'
   of this implication.  So we do not treat inert_funeqs as a 'given equality'.

 * See Note [Let-bound skolems] for another wrinkle

 * We do *not* need to worry about representational equalities, because
   these do not affect the ability to float constraints.

Note [Let-bound skolems]
~~~~~~~~~~~~~~~~~~~~~~~~
If   * the inert set contains a canonical Given CTyEqCan (a ~ ty)
and  * 'a' is a skolem bound in this very implication,

then:
a) The Given is pretty much a let-binding, like
      f :: (a ~ b->c) => a -> a
   Here the equality constraint is like saying
      let a = b->c in ...
   It is not adding any new, local equality  information,
   and hence can be ignored by has_given_eqs

b) 'a' will have been completely substituted out in the inert set,
   so we can safely discard it.  Notably, it doesn't need to be
   returned as part of 'fsks'

For an example, see Trac #9211.

See also TcUnify Note [Deeper level on the left] for how we ensure
that the right variable is on the left of the equality when both are
tyvars.

You might wonder whether the skokem really needs to be bound "in the
very same implication" as the equuality constraint.
(c.f. Trac #15009) Consider this:

  data S a where
    MkS :: (a ~ Int) => S a

  g :: forall a. S a -> a -> blah
  g x y = let h = \z. ( z :: Int
                      , case x of
                           MkS -> [y,z])
          in ...

From the type signature for `g`, we get `y::a` .  Then when when we
encounter the `\z`, we'll assign `z :: alpha[1]`, say.  Next, from the
body of the lambda we'll get

  [W] alpha[1] ~ Int                             -- From z::Int
  [W] forall[2]. (a ~ Int) => [W] alpha[1] ~ a   -- From [y,z]

Now, suppose we decide to float `alpha ~ a` out of the implication
and then unify `alpha := a`.  Now we are stuck!  But if treat
`alpha ~ Int` first, and unify `alpha := Int`, all is fine.
But we absolutely cannot float that equality or we will get stuck.
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

    CFunEqCan { cc_fun  = tf,  cc_tyargs = tys } ->
      is { inert_funeqs = delFunEq (inert_funeqs is) tf tys }

    CTyEqCan  { cc_tyvar = x,  cc_rhs    = ty } ->
      is { inert_eqs    = delTyEq (inert_eqs is) x ty }

    CQuantCan {}     -> panic "removeInertCt: CQuantCan"
    CIrredCan {}     -> panic "removeInertCt: CIrredEvCan"
    CNonCanonical {} -> panic "removeInertCt: CNonCanonical"
    CHoleCan {}      -> panic "removeInertCt: CHoleCan"


lookupFlatCache :: TyCon -> [Type] -> TcS (Maybe (TcCoercion, TcType, CtFlavour))
lookupFlatCache fam_tc tys
  = do { IS { inert_flat_cache = flat_cache
            , inert_cans = IC { inert_funeqs = inert_funeqs } } <- getTcSInerts
       ; return (firstJusts [lookup_inerts inert_funeqs,
                             lookup_flats flat_cache]) }
  where
    lookup_inerts inert_funeqs
      | Just (CFunEqCan { cc_ev = ctev, cc_fsk = fsk, cc_tyargs = xis })
           <- findFunEq inert_funeqs fam_tc tys
      , tys `eqTypes` xis   -- The lookup might find a near-match; see
                            -- Note [Use loose types in inert set]
      = Just (ctEvCoercion ctev, mkTyVarTy fsk, ctEvFlavour ctev)
      | otherwise = Nothing

    lookup_flats flat_cache = findExactFunEq flat_cache fam_tc tys


lookupInInerts :: CtLoc -> TcPredType -> TcS (Maybe CtEvidence)
-- Is this exact predicate type cached in the solved or canonicals of the InertSet?
lookupInInerts loc pty
  | ClassPred cls tys <- classifyPredType pty
  = do { inerts <- getTcSInerts
       ; return (lookupSolvedDict inerts loc cls tys `mplus`
                 lookupInertDict (inert_cans inerts) loc cls tys) }
  | otherwise -- NB: No caching for equalities, IPs, holes, or errors
  = return Nothing

-- | Look up a dictionary inert. NB: the returned 'CtEvidence' might not
-- match the input exactly. Note [Use loose types in inert set].
lookupInertDict :: InertCans -> CtLoc -> Class -> [Type] -> Maybe CtEvidence
lookupInertDict (IC { inert_dicts = dicts }) loc cls tys
  = case findDict dicts loc cls tys of
      Just ct -> Just (ctEvidence ct)
      _       -> Nothing

-- | Look up a solved inert. NB: the returned 'CtEvidence' might not
-- match the input exactly. See Note [Use loose types in inert set].
lookupSolvedDict :: InertSet -> CtLoc -> Class -> [Type] -> Maybe CtEvidence
-- Returns just if exactly this predicate type exists in the solved.
lookupSolvedDict (IS { inert_solved_dicts = solved }) loc cls tys
  = case findDict solved loc cls tys of
      Just ev -> Just ev
      _       -> Nothing

{- *********************************************************************
*                                                                      *
                   Irreds
*                                                                      *
********************************************************************* -}

foldIrreds :: (Ct -> b -> b) -> Cts -> b -> b
foldIrreds k irreds z = foldrBag k z irreds


{- *********************************************************************
*                                                                      *
                   TcAppMap
*                                                                      *
************************************************************************

Note [Use loose types in inert set]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Say we know (Eq (a |> c1)) and we need (Eq (a |> c2)). One is clearly
solvable from the other. So, we do lookup in the inert set using
loose types, which omit the kind-check.

We must be careful when using the result of a lookup because it may
not match the requested info exactly!

-}

type TcAppMap a = UniqDFM (ListMap LooseTypeMap a)
    -- Indexed by tycon then the arg types, using "loose" matching, where
    -- we don't require kind equality. This allows, for example, (a |> co)
    -- to match (a).
    -- See Note [Use loose types in inert set]
    -- Used for types and classes; hence UniqDFM
    -- See Note [foldTM determinism] for why we use UniqDFM here

isEmptyTcAppMap :: TcAppMap a -> Bool
isEmptyTcAppMap m = isNullUDFM m

emptyTcAppMap :: TcAppMap a
emptyTcAppMap = emptyUDFM

findTcApp :: TcAppMap a -> Unique -> [Type] -> Maybe a
findTcApp m u tys = do { tys_map <- lookupUDFM m u
                       ; lookupTM tys tys_map }

delTcApp :: TcAppMap a -> Unique -> [Type] -> TcAppMap a
delTcApp m cls tys = adjustUDFM (deleteTM tys) m cls

insertTcApp :: TcAppMap a -> Unique -> [Type] -> a -> TcAppMap a
insertTcApp m cls tys ct = alterUDFM alter_tm m cls
  where
    alter_tm mb_tm = Just (insertTM tys ct (mb_tm `orElse` emptyTM))

-- mapTcApp :: (a->b) -> TcAppMap a -> TcAppMap b
-- mapTcApp f = mapUDFM (mapTM f)

filterTcAppMap :: (Ct -> Bool) -> TcAppMap Ct -> TcAppMap Ct
filterTcAppMap f m
  = mapUDFM do_tm m
  where
    do_tm tm = foldTM insert_mb tm emptyTM
    insert_mb ct tm
       | f ct      = insertTM tys ct tm
       | otherwise = tm
       where
         tys = case ct of
                CFunEqCan { cc_tyargs = tys } -> tys
                CDictCan  { cc_tyargs = tys } -> tys
                _ -> pprPanic "filterTcAppMap" (ppr ct)

tcAppMapToBag :: TcAppMap a -> Bag a
tcAppMapToBag m = foldTcAppMap consBag m emptyBag

foldTcAppMap :: (a -> b -> b) -> TcAppMap a -> b -> b
foldTcAppMap k m z = foldUDFM (foldTM k) z m


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
the intervening binding for (?x::Int).  Trac #14218.

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
  See Note [Overview of implicit CallStacks] in TcEvidence

* We cannonicalise such constraints, in TcCanonical.canClassNC, by
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
  | isCTupleClass cls
  , any hasIPPred tys   -- See Note [Tuples hiding implicit parameters]
  = Nothing

  | Just {} <- isCallStackPred cls tys
  , OccurrenceOf {} <- ctLocOrigin loc
  = Nothing             -- See Note [Solving CallStack constraints]

  | otherwise
  = findTcApp m (getUnique cls) tys

findDictsByClass :: DictMap a -> Class -> Bag a
findDictsByClass m cls
  | Just tm <- lookupUDFM m cls = foldTM consBag tm emptyBag
  | otherwise                  = emptyBag

delDict :: DictMap a -> Class -> [Type] -> DictMap a
delDict m cls tys = delTcApp m (getUnique cls) tys

addDict :: DictMap a -> Class -> [Type] -> a -> DictMap a
addDict m cls tys item = insertTcApp m (getUnique cls) tys item

addDictsByClass :: DictMap Ct -> Class -> Bag Ct -> DictMap Ct
addDictsByClass m cls items
  = addToUDFM m cls (foldrBag add emptyTM items)
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
findFunEq m tc tys = findTcApp m (getUnique tc) tys

funEqsToBag :: FunEqMap a -> Bag a
funEqsToBag m = foldTcAppMap consBag m emptyBag

findFunEqsByTyCon :: FunEqMap a -> TyCon -> [a]
-- Get inert function equation constraints that have the given tycon
-- in their head.  Not that the constraints remain in the inert set.
-- We use this to check for derived interactions with built-in type-function
-- constructors.
findFunEqsByTyCon m tc
  | Just tm <- lookupUDFM m tc = foldTM (:) tm []
  | otherwise                 = []

foldFunEqs :: (a -> b -> b) -> FunEqMap a -> b -> b
foldFunEqs = foldTcAppMap

-- mapFunEqs :: (a -> b) -> FunEqMap a -> FunEqMap b
-- mapFunEqs = mapTcApp

-- filterFunEqs :: (Ct -> Bool) -> FunEqMap Ct -> FunEqMap Ct
-- filterFunEqs = filterTcAppMap

insertFunEq :: FunEqMap a -> TyCon -> [Type] -> a -> FunEqMap a
insertFunEq m tc tys val = insertTcApp m (getUnique tc) tys val

partitionFunEqs :: (Ct -> Bool) -> FunEqMap Ct -> ([Ct], FunEqMap Ct)
-- Optimise for the case where the predicate is false
-- partitionFunEqs is called only from kick-out, and kick-out usually
-- kicks out very few equalities, so we want to optimise for that case
partitionFunEqs f m = (yeses, foldr del m yeses)
  where
    yeses = foldTcAppMap k m []
    k ct yeses | f ct      = ct : yeses
               | otherwise = yeses
    del (CFunEqCan { cc_fun = tc, cc_tyargs = tys }) m
        = delFunEq m tc tys
    del ct _ = pprPanic "partitionFunEqs" (ppr ct)

delFunEq :: FunEqMap a -> TyCon -> [Type] -> FunEqMap a
delFunEq m tc tys = delTcApp m (getUnique tc) tys

------------------------------
type ExactFunEqMap a = UniqFM (ListMap TypeMap a)

emptyExactFunEqs :: ExactFunEqMap a
emptyExactFunEqs = emptyUFM

findExactFunEq :: ExactFunEqMap a -> TyCon -> [Type] -> Maybe a
findExactFunEq m tc tys = do { tys_map <- lookupUFM m (getUnique tc)
                             ; lookupTM tys tys_map }

insertExactFunEq :: ExactFunEqMap a -> TyCon -> [Type] -> a -> ExactFunEqMap a
insertExactFunEq m tc tys val = alterUFM alter_tm m (getUnique tc)
  where alter_tm mb_tm = Just (insertTM tys val (mb_tm `orElse` emptyTM))

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

      tcs_count     :: IORef Int, -- Global step count

      tcs_inerts    :: IORef InertSet, -- Current inert set

      -- The main work-list and the flattening worklist
      -- See Note [Work list priorities] and
      tcs_worklist  :: IORef WorkList -- Current worklist
    }

---------------
newtype TcS a = TcS { unTcS :: TcSEnv -> TcM a }

instance Functor TcS where
  fmap f m = TcS $ fmap f . unTcS m

instance Applicative TcS where
  pure x = TcS (\_ -> return x)
  (<*>) = ap

instance Monad TcS where
  fail = MonadFail.fail
  m >>= k   = TcS (\ebs -> unTcS m ebs >>= \r -> unTcS (k r) ebs)

instance MonadFail.MonadFail TcS where
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
panicTcS doc = pprPanic "TcCanonical" doc

traceTcS :: String -> SDoc -> TcS ()
traceTcS herald doc = wrapTcS (TcM.traceTc herald doc)

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

csTraceTcM :: TcM SDoc -> TcM ()
-- Constraint-solver tracing, -ddump-cs-trace
csTraceTcM mk_doc
  = do { dflags <- getDynFlags
       ; when (  dopt Opt_D_dump_cs_trace dflags
                  || dopt Opt_D_dump_tc_trace dflags )
              ( do { msg <- mk_doc
                   ; TcM.traceTcRn Opt_D_dump_cs_trace msg }) }

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

runTcSWithEvBinds :: EvBindsVar
                  -> TcS a
                  -> TcM a
runTcSWithEvBinds ev_binds_var tcs
  = do { unified_var <- TcM.newTcRef 0
       ; step_count <- TcM.newTcRef 0
       ; inert_var <- TcM.newTcRef emptyInert
       ; wl_var <- TcM.newTcRef emptyWorkList
       ; let env = TcSEnv { tcs_ev_binds      = ev_binds_var
                          , tcs_unified       = unified_var
                          , tcs_count         = step_count
                          , tcs_inerts        = inert_var
                          , tcs_worklist      = wl_var }

             -- Run the computation
       ; res <- unTcS tcs env

       ; count <- TcM.readTcRef step_count
       ; when (count > 0) $
         csTraceTcM $ return (text "Constraint solver steps =" <+> int count)

       ; unflattenGivens inert_var

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
    is_co_bind (EvBind { eb_lhs = b }) = isEqPred (varType b)

    edges :: [ Node EvVar EvBind ]
    edges = [ DigraphNode bind bndr (nonDetEltsUniqSet (evVarsOfTerm rhs))
            | bind@(EvBind { eb_lhs = bndr, eb_rhs = rhs}) <- bagToList ev_binds ]
            -- It's OK to use nonDetEltsUFM here as
            -- stronglyConnCompFromEdgedVertices is still deterministic even
            -- if the edges are in nondeterministic order as explained in
            -- Note [Deterministic SCC] in Digraph.
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
                   } ->
    do { inerts <- TcM.readTcRef old_inert_var
       ; let nest_inert = emptyInert
                            { inert_cans = inert_cans inerts
                            , inert_solved_dicts = inert_solved_dicts inerts }
                              -- See Note [Do not inherit the flat cache]
       ; new_inert_var <- TcM.newTcRef nest_inert
       ; new_wl_var    <- TcM.newTcRef emptyWorkList
       ; let nest_env = TcSEnv { tcs_ev_binds      = ref
                               , tcs_unified       = unified_var
                               , tcs_count         = count
                               , tcs_inerts        = new_inert_var
                               , tcs_worklist      = new_wl_var }
       ; res <- TcM.setTcLevel inner_tclvl $
                thing_inside nest_env

       ; unflattenGivens new_inert_var

#if defined(DEBUG)
       -- Perform a check that the thing_inside did not cause cycles
       ; ev_binds <- TcM.getTcEvBindsMap ref
       ; checkForCyclicBinds ev_binds
#endif
       ; return res }

{- Note [Do not inherit the flat cache]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do not want to inherit the flat cache when processing nested
implications.  Consider
   a ~ F b, forall c. b~Int => blah
If we have F b ~ fsk in the flat-cache, and we push that into the
nested implication, we might miss that F b can be rewritten to F Int,
and hence perhpas solve it.  Moreover, the fsk from outside is
flattened out after solving the outer level, but and we don't
do that flattening recursively.
-}

nestTcS ::  TcS a -> TcS a
-- Use the current untouchables, augmenting the current
-- evidence bindings, and solved dictionaries
-- But have no effect on the InertCans, or on the inert_flat_cache
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

       -- we want to propogate the safe haskell failures
       ; let old_ic = inert_cans inerts
             new_ic = inert_cans new_inerts
             nxt_ic = old_ic { inert_safehask = inert_safehask new_ic }

       ; TcM.writeTcRef inerts_var  -- See Note [Propagate the solved dictionaries]
                        (inerts { inert_solved_dicts = inert_solved_dicts new_inerts
                                , inert_cans = nxt_ic })

       ; return res }

checkTvConstraintsTcS :: SkolemInfo
                      -> [TcTyVar]        -- Skolems
                      -> TcS (result, Cts)
                      -> TcS result
-- Just like TcUnify.checkTvConstraints, but
--   - In the TcS monnad
--   - The thing-inside should not put things in the work-list
--     Instead, it returns the Wanted constraints it needs
--   - No 'givens', and no TcEvBinds; this is type-level constraints only
checkTvConstraintsTcS skol_info skol_tvs (TcS thing_inside)
  = TcS $ \ tcs_env ->
    do { let wl_panic  = pprPanic "TcSMonad.buildImplication" $
                         ppr skol_info $$ ppr skol_tvs
                         -- This panic checks that the thing-inside
                         -- does not emit any work-list constraints
             new_tcs_env = tcs_env { tcs_worklist = wl_panic }

       ; (new_tclvl, (res, wanteds)) <- TcM.pushTcLevelM $
                                        thing_inside new_tcs_env

       ; unless (null wanteds) $
         do { ev_binds_var <- TcM.newNoTcEvBinds
            ; imp <- newImplication
            ; let wc = emptyWC { wc_simple = wanteds }
                  imp' = imp { ic_tclvl  = new_tclvl
                             , ic_skols  = skol_tvs
                             , ic_wanted = wc
                             , ic_binds  = ev_binds_var
                             , ic_info   = skol_info }

           -- Add the implication to the work-list
           ; TcM.updTcRef (tcs_worklist tcs_env)
                          (extendWorkListImplic (unitBag imp')) }

      ; return res }

checkConstraintsTcS :: SkolemInfo
                    -> [TcTyVar]        -- Skolems
                    -> [EvVar]          -- Givens
                    -> TcS (result, Cts)
                    -> TcS (result, TcEvBinds)
-- Just like checkConstraintsTcS, but
--   - In the TcS monnad
--   - The thing-inside should not put things in the work-list
--     Instead, it returns the Wanted constraints it needs
--   - I did not bother to put in the fast-path for
--     empty-skols/empty-givens, or for empty-wanteds, because
--     this function is used only for "quantified constraints" in
--     with both tests are pretty much guaranteed to fail
checkConstraintsTcS skol_info skol_tvs given (TcS thing_inside)
  = TcS $ \ tcs_env ->
    do { let wl_panic  = pprPanic "TcSMonad.buildImplication" $
                         ppr skol_info $$ ppr skol_tvs
                         -- This panic checks that the thing-inside
                         -- does not emit any work-list constraints
             new_tcs_env = tcs_env { tcs_worklist = wl_panic }

       ; (new_tclvl, (res, wanteds)) <- TcM.pushTcLevelM $
                                        thing_inside new_tcs_env

       ; ev_binds_var <- TcM.newTcEvBinds
       ; imp <- newImplication
       ; let wc = emptyWC { wc_simple = wanteds }
             imp' = imp { ic_tclvl  = new_tclvl
                        , ic_skols  = skol_tvs
                        , ic_given  = given
                        , ic_wanted = wc
                        , ic_binds  = ev_binds_var
                        , ic_info   = skol_info }

           -- Add the implication to the work-list
       ; TcM.updTcRef (tcs_worklist tcs_env)
                      (extendWorkListImplic (unitBag imp'))

       ; return (res, TcEvBinds ev_binds_var) }

{-
Note [Propagate the solved dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's really quite important that nestTcS does not discard the solved
dictionaries from the thing_inside.
Consider
   Eq [a]
   forall b. empty =>  Eq [a]
We solve the simple (Eq [a]), under nestTcS, and then turn our attention to
the implications.  It's definitely fine to use the solved dictionaries on
the inner implications, and it can make a signficant performance difference
if you do so.
-}

-- Getters and setters of TcEnv fields
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
emitWork cts
  = do { traceTcS "Emitting fresh work" (vcat (map ppr cts))
       ; updWorkListTcS (extendWorkListCts cts) }

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

{- *********************************************************************
*                                                                      *
*                Flatten skolems                                       *
*                                                                      *
********************************************************************* -}

newFlattenSkolem :: CtFlavour -> CtLoc
                 -> TyCon -> [TcType]                    -- F xis
                 -> TcS (CtEvidence, Coercion, TcTyVar)  -- [G/WD] x:: F xis ~ fsk
newFlattenSkolem flav loc tc xis
  = do { stuff@(ev, co, fsk) <- new_skolem
       ; let fsk_ty = mkTyVarTy fsk
       ; extendFlatCache tc xis (co, fsk_ty, ctEvFlavour ev)
       ; return stuff }
  where
    fam_ty = mkTyConApp tc xis

    new_skolem
      | Given <- flav
      = do { fsk <- wrapTcS (TcM.newFskTyVar fam_ty)

           -- Extend the inert_fsks list, for use by unflattenGivens
           ; updInertTcS $ \is -> is { inert_fsks = (fsk, fam_ty) : inert_fsks is }

           -- Construct the Refl evidence
           ; let pred = mkPrimEqPred fam_ty (mkTyVarTy fsk)
                 co   = mkNomReflCo fam_ty
           ; ev  <- newGivenEvVar loc (pred, evCoercion co)
           ; return (ev, co, fsk) }

      | otherwise  -- Generate a [WD] for both Wanted and Derived
                   -- See Note [No Derived CFunEqCans]
      = do { fmv <- wrapTcS (TcM.newFmvTyVar fam_ty)
           ; (ev, hole_co) <- newWantedEq loc Nominal fam_ty (mkTyVarTy fmv)
           ; return (ev, hole_co, fmv) }

----------------------------
unflattenGivens :: IORef InertSet -> TcM ()
-- Unflatten all the fsks created by flattening types in Given
-- constraints. We must be sure to do this, else we end up with
-- flatten-skolems buried in any residual Wanteds
--
-- NB: this is the /only/ way that a fsk (MetaDetails = FlatSkolTv)
--     is filled in. Nothing else does so.
--
-- It's here (rather than in TcFlatten) because the Right Places
-- to call it are in runTcSWithEvBinds/nestImplicTcS, where it
-- is nicely paired with the creation an empty inert_fsks list.
unflattenGivens inert_var
 = do { inerts <- TcM.readTcRef inert_var
       ; TcM.traceTc "unflattenGivens" (ppr (inert_fsks inerts))
       ; mapM_ flatten_one (inert_fsks inerts) }
  where
    flatten_one (fsk, ty) = TcM.writeMetaTyVar fsk ty

----------------------------
extendFlatCache :: TyCon -> [Type] -> (TcCoercion, TcType, CtFlavour) -> TcS ()
extendFlatCache tc xi_args stuff@(_, ty, fl)
  | isGivenOrWDeriv fl  -- Maintain the invariant that inert_flat_cache
                        -- only has [G] and [WD] CFunEqCans
  = do { dflags <- getDynFlags
       ; when (gopt Opt_FlatCache dflags) $
    do { traceTcS "extendFlatCache" (vcat [ ppr tc <+> ppr xi_args
                                          , ppr fl, ppr ty ])
            -- 'co' can be bottom, in the case of derived items
       ; updInertTcS $ \ is@(IS { inert_flat_cache = fc }) ->
            is { inert_flat_cache = insertExactFunEq fc tc xi_args stuff } } }

  | otherwise
  = return ()

----------------------------
unflattenFmv :: TcTyVar -> TcType -> TcS ()
-- Fill a flatten-meta-var, simply by unifying it.
-- This does NOT count as a unification in tcs_unified.
unflattenFmv tv ty
  = ASSERT2( isMetaTyVar tv, ppr tv )
    TcS $ \ _ ->
    do { TcM.traceTc "unflattenFmv" (ppr tv <+> text ":=" <+> ppr ty)
       ; TcM.writeMetaTyVar tv ty }

----------------------------
demoteUnfilledFmv :: TcTyVar -> TcS ()
-- If a flatten-meta-var is still un-filled,
-- turn it into an ordinary meta-var
demoteUnfilledFmv fmv
  = wrapTcS $ do { is_filled <- TcM.isFilledMetaTyVar fmv
                 ; unless is_filled $
                   do { tv_ty <- TcM.newFlexiTyVarTy (tyVarKind fmv)
                      ; TcM.writeMetaTyVar fmv tv_ty } }

-----------------------------
dischargeFunEq :: CtEvidence -> TcTyVar -> TcCoercion -> TcType -> TcS ()
-- (dischargeFunEq tv co ty)
--     Preconditions
--       - ev :: F tys ~ tv   is a CFunEqCan
--       - tv is a FlatMetaTv of FlatSkolTv
--       - co :: F tys ~ xi
--       - fmv/fsk `notElem` xi
--       - fmv not filled (for Wanteds)
--
-- Then for [W] or [WD], we actually fill in the fmv:
--      set fmv := xi,
--      set ev  := co
--      kick out any inert things that are now rewritable
--
-- For [D], we instead emit an equality that must ultimately hold
--      [D] xi ~ fmv
--      Does not evaluate 'co' if 'ev' is Derived
--
-- For [G], emit this equality
--     [G] (sym ev; co) :: fsk ~ xi

-- See TcFlatten Note [The flattening story],
-- especially "Ownership of fsk/fmv"
dischargeFunEq (CtGiven { ctev_evar = old_evar, ctev_loc = loc }) fsk co xi
  = do { new_ev <- newGivenEvVar loc ( new_pred, evCoercion new_co  )
       ; emitWorkNC [new_ev] }
  where
    new_pred = mkPrimEqPred (mkTyVarTy fsk) xi
    new_co   = mkTcSymCo (mkTcCoVarCo old_evar) `mkTcTransCo` co

dischargeFunEq ev@(CtWanted { ctev_dest = dest }) fmv co xi
  = ASSERT2( not (fmv `elemVarSet` tyCoVarsOfType xi), ppr ev $$ ppr fmv $$ ppr xi )
    do { setWantedEvTerm dest (evCoercion co)
       ; unflattenFmv fmv xi
       ; n_kicked <- kickOutAfterUnification fmv
       ; traceTcS "dischargeFmv" (ppr fmv <+> equals <+> ppr xi $$ pprKicked n_kicked) }

dischargeFunEq (CtDerived { ctev_loc = loc }) fmv _co xi
  = emitNewDerivedEq loc Nominal xi (mkTyVarTy fmv)
              -- FunEqs are always at Nominal role

pprKicked :: Int -> SDoc
pprKicked 0 = empty
pprKicked n = parens (int n <+> text "kicked out")

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
       ; wrapTcS $ TcM.fillCoercionHole hole co }
setWantedEq (EvVarDest ev) _ = pprPanic "setWantedEq" (ppr ev)

-- | Good for both equalities and non-equalities
setWantedEvTerm :: TcEvDest -> EvTerm -> TcS ()
setWantedEvTerm (HoleDest hole) tm
  | Just co <- evTermCoercion_maybe tm
  = do { useVars (coVarsOfCo co)
       ; wrapTcS $ TcM.fillCoercionHole hole co }
  | otherwise
  = do { let co_var = coHoleCoVar hole
       ; setEvBind (mkWantedEvBind co_var tm)
       ; wrapTcS $ TcM.fillCoercionHole hole (mkTcCoVarCo co_var) }

setWantedEvTerm (EvVarDest ev_id) tm
  = setEvBind (mkWantedEvBind ev_id tm)

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
-- See Note [Bind new Givens immediately] in TcRnTypes
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
  | otherwise
  = do { (ev, co) <- newWantedEq loc role ty1 ty2
       ; updWorkListTcS $
         extendWorkListEq (mkNonCanonical ev)
       ; return co }

-- | Make a new equality CtEvidence
newWantedEq :: CtLoc -> Role -> TcType -> TcType -> TcS (CtEvidence, Coercion)
newWantedEq loc role ty1 ty2
  = do { hole <- wrapTcS $ TcM.newCoercionHole pty
       ; traceTcS "Emitting new coercion hole" (ppr hole <+> dcolon <+> ppr pty)
       ; return ( CtWanted { ctev_pred = pty, ctev_dest = HoleDest hole
                           , ctev_nosh = WDeriv
                           , ctev_loc = loc}
                , mkHoleCo hole ) }
  where
    pty = mkPrimEqPredRole role ty1 ty2

-- no equalities here. Use newWantedEq instead
newWantedEvVarNC :: CtLoc -> TcPredType -> TcS CtEvidence
-- Don't look up in the solved/inerts; we know it's not there
newWantedEvVarNC loc pty
  = do { new_ev <- newEvVar pty
       ; traceTcS "Emitting new wanted" (ppr new_ev <+> dcolon <+> ppr pty $$
                                         pprCtLoc loc)
       ; return (CtWanted { ctev_pred = pty, ctev_dest = EvVarDest new_ev
                          , ctev_nosh = WDeriv
                          , ctev_loc = loc })}

newWantedEvVar :: CtLoc -> TcPredType -> TcS MaybeNew
-- For anything except ClassPred, this is the same as newWantedEvVarNC
newWantedEvVar loc pty
  = do { mb_ct <- lookupInInerts loc pty
       ; case mb_ct of
            Just ctev
              | not (isDerived ctev)
              -> do { traceTcS "newWantedEvVar/cache hit" $ ppr ctev
                    ; return $ Cached (ctEvExpr ctev) }
            _ -> do { ctev <- newWantedEvVarNC loc pty
                    ; return (Fresh ctev) } }

-- deals with both equalities and non equalities. Tries to look
-- up non-equalities in the cache
newWanted :: CtLoc -> PredType -> TcS MaybeNew
newWanted loc pty
  | Just (role, ty1, ty2) <- getEqPredTys_maybe pty
  = Fresh . fst <$> newWantedEq loc role ty1 ty2
  | otherwise
  = newWantedEvVar loc pty

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
  = do { -- checkReductionDepth loc pred
       ; return (CtDerived { ctev_pred = pred, ctev_loc = loc }) }

-- --------- Check done in TcInteract.selectNewWorkItem???? ---------
-- | Checks if the depth of the given location is too much. Fails if
-- it's too big, with an appropriate error message.
checkReductionDepth :: CtLoc -> TcType   -- ^ type being reduced
                    -> TcS ()
checkReductionDepth loc ty
  = do { dflags <- getDynFlags
       ; when (subGoalDepthExceeded dflags (ctLocDepth loc)) $
         wrapErrTcS $
         solverDepthErrorTcS loc ty }

matchFam :: TyCon -> [Type] -> TcS (Maybe (Coercion, TcType))
matchFam tycon args = wrapTcS $ matchFamTcM tycon args

matchFamTcM :: TyCon -> [Type] -> TcM (Maybe (Coercion, TcType))
-- Given (F tys) return (ty, co), where co :: F tys ~ ty
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
See TcSMonad.deferTcSForAllEq
-}
