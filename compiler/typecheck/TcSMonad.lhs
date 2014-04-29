\begin{code}
-- Type definitions for the constraint solver
{-# LANGUAGE TypeFamilies #-}
module TcSMonad (

       -- Canonical constraints, definition is now in TcRnTypes

    WorkList(..), isEmptyWorkList, emptyWorkList,
    workListFromEq, workListFromNonEq, workListFromCt,
    extendWorkListEq, extendWorkListFunEq,
    extendWorkListNonEq, extendWorkListCt,
    extendWorkListCts, extendWorkListEqs, appendWorkList, selectWorkItem,
    withWorkList, workListSize,

    updWorkListTcS, updWorkListTcS_return,

    updTcSImplics,

    Ct(..), Xi, tyVarsOfCt, tyVarsOfCts,
    emitInsoluble,

    isWanted, isDerived,
    isGivenCt, isWantedCt, isDerivedCt,

    canRewrite,
    mkGivenLoc,

    TcS, runTcS, runTcSWithEvBinds, failTcS, panicTcS, traceTcS, -- Basic functionality
    traceFireTcS, bumpStepCountTcS,
    tryTcS, nestTcS, nestImplicTcS, recoverTcS,
    wrapErrTcS, wrapWarnTcS,

    -- Getting and setting the flattening cache
    addSolvedDict, addSolvedFunEq, getGivenInfo,

    -- Marking stuff as used
    addUsedRdrNamesTcS,

    deferTcSForAllEq,

    setEvBind,
    XEvTerm(..),
    MaybeNew (..), isFresh, freshGoal, freshGoals, getEvTerm, getEvTerms,

    xCtEvidence,        -- Transform a CtEvidence during a step
    rewriteEvidence,    -- Specialized version of xCtEvidence for coercions
    rewriteEqEvidence,  -- Yet more specialised, for equality coercions
    maybeSym,

    newWantedEvVar, newWantedEvVarNC, newWantedEvVarNonrec, newDerived,
    instDFunConstraints,

       -- Creation of evidence variables
    setWantedTyBind, reportUnifications,

    getInstEnvs, getFamInstEnvs,                -- Getting the environments
    getTopEnv, getGblEnv, getTcEvBinds, getUntouchables,
    getTcEvBindsMap, getTcSTyBindsMap,

    lookupFlatEqn, newFlattenSkolem,            -- Flatten skolems

        -- Deque
    Deque(..), insertDeque, emptyDeque,

        -- Inerts
    InertSet(..), InertCans(..),
    getInertEqs,
    emptyInert, getTcSInerts, setTcSInerts,
    getInertUnsolved, checkAllSolved,
    prepareInertsForImplications,
    addInertCan, insertInertItemTcS,
    EqualCtList,
    lookupSolvedDict, extendFlatCache,

    findFunEq, findTyEqs,
    findDict, findDictsByClass, addDict, addDictsByClass, delDict, partitionDicts,
    findFunEqsByTyCon, findFunEqs, addFunEq, replaceFunEqs, partitionFunEqs,

    instDFunType,                              -- Instantiation
    newFlexiTcSTy, instFlexiTcS, instFlexiTcSHelperTcS,
    cloneMetaTyVar,

    Untouchables, isTouchableMetaTyVarTcS, isFilledMetaTyVar_maybe,
    zonkTyVarsAndFV,

    TN.ExtSolRes(..), extSolSend, extSolPush, extSolPop, extSolCheck,

    getDefaultInfo, getDynFlags, getGlobalRdrEnvTcS,

    matchFam, matchOpenFam,
    checkWellStagedDFun,
    pprEq                                    -- Smaller utils, re-exported from TcM
                                             -- TODO (DV): these are only really used in the
                                             -- instance matcher in TcSimplify. I am wondering
                                             -- if the whole instance matcher simply belongs
                                             -- here
) where

#include "HsVersions.h"

import HscTypes

import Inst
import InstEnv
import FamInst
import FamInstEnv

import qualified TcRnMonad as TcM
import qualified TcMType as TcM
import qualified TcEnv as TcM
       ( checkWellStaged, topIdLvl, tcGetDefaultTys )
import Kind
import TcType
import DynFlags
import Type
import CoAxiom(sfMatchFam)
import qualified TcTypeNats as TN

import TcEvidence
import Class
import TyCon

import Name
import RdrName (RdrName, GlobalRdrEnv)
import RnEnv (addUsedRdrNames)
import Var
import VarEnv
import Outputable
import Bag
import MonadUtils
import UniqSupply

import FastString
import Util
import Id
import TcRnTypes

import BasicTypes
import Unique
import UniqFM
import Maybes ( orElse, catMaybes, firstJusts )
import Pair ( pSnd )

import TrieMap
import Control.Monad( ap, when )
import Data.IORef
import Data.List( partition )

#ifdef DEBUG
import VarSet
import Digraph
#endif
\end{code}

%************************************************************************
%*                                                                      *
%*                            Worklists                                *
%*  Canonical and non-canonical constraints that the simplifier has to  *
%*  work on. Including their simplification depths.                     *
%*                                                                      *
%*                                                                      *
%************************************************************************

Note [WorkList priorities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
A WorkList contains canonical and non-canonical items (of all flavors).
Notice that each Ct now has a simplification depth. We may
consider using this depth for prioritization as well in the future.

As a simple form of priority queue, our worklist separates out
equalities (wl_eqs) from the rest of the canonical constraints,
so that it's easier to deal with them first, but the separation
is not strictly necessary. Notice that non-canonical constraints
are also parts of the worklist.


Note [NonCanonical Semantics]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note that canonical constraints involve a CNonCanonical constructor. In the worklist
we use this constructor for constraints that have not yet been canonicalized such as
   [Int] ~ [a]
In other words, all constraints start life as NonCanonicals.

On the other hand, in the Inert Set (see below) the presence of a NonCanonical somewhere
means that we have a ``frozen error''.

NonCanonical constraints never interact directly with other constraints -- but they can
be rewritten by equalities (for instance if a non canonical exists in the inert, we'd
better rewrite it as much as possible before reporting it as an error to the user)

\begin{code}
data Deque a = DQ [a] [a]   -- Insert in RH field, remove from LH field
                            -- First to remove is at head of LH field

instance Outputable a => Outputable (Deque a) where
  ppr (DQ as bs) = ppr (as ++ reverse bs)   -- Show first one to come out at the start

emptyDeque :: Deque a
emptyDeque = DQ [] []

isEmptyDeque :: Deque a -> Bool
isEmptyDeque (DQ as bs) = null as && null bs

dequeSize :: Deque a -> Int
dequeSize (DQ as bs) = length as + length bs

insertDeque :: a -> Deque a -> Deque a
insertDeque b (DQ as bs) = DQ as (b:bs)

appendDeque :: Deque a -> Deque a -> Deque a
appendDeque (DQ as1 bs1) (DQ as2 bs2) = DQ (as1 ++ reverse bs1 ++ as2) bs2

extractDeque :: Deque a -> Maybe (Deque a, a)
extractDeque (DQ [] [])     = Nothing
extractDeque (DQ (a:as) bs) = Just (DQ as bs, a)
extractDeque (DQ [] bs)     = case reverse bs of
                                (a:as) -> Just (DQ as [], a)
                                [] -> panic "extractDeque"

-- See Note [WorkList priorities]
data WorkList = WorkList { wl_eqs    :: [Ct]
                         , wl_funeqs :: Deque Ct
                         , wl_rest   :: [Ct]
                         }


appendWorkList :: WorkList -> WorkList -> WorkList
appendWorkList new_wl orig_wl
   = WorkList { wl_eqs    = wl_eqs new_wl    ++            wl_eqs orig_wl
              , wl_funeqs = wl_funeqs new_wl `appendDeque` wl_funeqs orig_wl
              , wl_rest   = wl_rest new_wl   ++            wl_rest orig_wl }


workListSize :: WorkList -> Int
workListSize (WorkList { wl_eqs = eqs, wl_funeqs = funeqs, wl_rest = rest })
  = length eqs + dequeSize funeqs + length rest

extendWorkListEq :: Ct -> WorkList -> WorkList
-- Extension by equality
extendWorkListEq ct wl
  | Just {} <- isCFunEqCan_maybe ct
  = extendWorkListFunEq ct wl
  | otherwise
  = wl { wl_eqs = ct : wl_eqs wl }

extendWorkListFunEq :: Ct -> WorkList -> WorkList
extendWorkListFunEq ct wl
  = wl { wl_funeqs = insertDeque ct (wl_funeqs wl) }

extendWorkListEqs :: [Ct] -> WorkList -> WorkList
-- Append a list of equalities
extendWorkListEqs cts wl = foldr extendWorkListEq wl cts

extendWorkListNonEq :: Ct -> WorkList -> WorkList
-- Extension by non equality
extendWorkListNonEq ct wl
  = wl { wl_rest = ct : wl_rest wl }

extendWorkListCt :: Ct -> WorkList -> WorkList
-- Agnostic
extendWorkListCt ct wl
 | isEqPred (ctPred ct) = extendWorkListEq ct wl
 | otherwise = extendWorkListNonEq ct wl

extendWorkListCts :: [Ct] -> WorkList -> WorkList
-- Agnostic
extendWorkListCts cts wl = foldr extendWorkListCt wl cts

isEmptyWorkList :: WorkList -> Bool
isEmptyWorkList wl
  = null (wl_eqs wl) &&  null (wl_rest wl) && isEmptyDeque (wl_funeqs wl)

emptyWorkList :: WorkList
emptyWorkList = WorkList { wl_eqs  = [], wl_rest = [], wl_funeqs = emptyDeque }

workListFromEq :: Ct -> WorkList
workListFromEq ct = extendWorkListEq ct emptyWorkList

workListFromNonEq :: Ct -> WorkList
workListFromNonEq ct = extendWorkListNonEq ct emptyWorkList

workListFromCt :: Ct -> WorkList
-- Agnostic
workListFromCt ct | isEqPred (ctPred ct) = workListFromEq ct
                  | otherwise            = workListFromNonEq ct


selectWorkItem :: WorkList -> (Maybe Ct, WorkList)
selectWorkItem wl@(WorkList { wl_eqs = eqs, wl_funeqs = feqs, wl_rest = rest })
  = case (eqs,feqs,rest) of
      (ct:cts,_,_)     -> (Just ct, wl { wl_eqs    = cts })
      (_,fun_eqs,_)    | Just (fun_eqs', ct) <- extractDeque fun_eqs
                       -> (Just ct, wl { wl_funeqs = fun_eqs' })
      (_,_,(ct:cts))   -> (Just ct, wl { wl_rest   = cts })
      (_,_,_)          -> (Nothing,wl)

-- Pretty printing
instance Outputable WorkList where
  ppr wl = vcat [ text "WorkList (eqs)   = " <+> ppr (wl_eqs wl)
                , text "WorkList (funeqs)= " <+> ppr (wl_funeqs wl)
                , text "WorkList (rest)  = " <+> ppr (wl_rest wl)
                ]
\end{code}

%************************************************************************
%*                                                                      *
%*                            Inert Sets                                *
%*                                                                      *
%*                                                                      *
%************************************************************************

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

  * Non-CTyEqCan constraints are fully rewritten with respect
    to the CTyEqCan equalities (modulo canRewrite of course;
    eg a wanted cannot rewrite a given)

  * CTyEqCan equalities _do_not_ form an idempotent substitution, but
    they are guaranteed to not have any occurs errors. Additional notes:

       - The lack of idempotence of the inert substitution implies
         that we must make sure that when we rewrite a constraint we
         apply the substitution /recursively/ to the types
         involved. Currently the one AND ONLY way in the whole
         constraint solver that we rewrite types and constraints wrt
         to the inert substitution is TcCanonical/flattenTyVar.

       - In the past we did try to have the inert substitution as
         idempotent as possible but this would only be true for
         constraints of the same flavor, so in total the inert
         substitution could not be idempotent, due to flavor-related
         issued.  Note [Non-idempotent inert substitution] explains
         what is going on.

       - Whenever a constraint ends up in the worklist we do
         recursively apply exhaustively the inert substitution to it
         to check for occurs errors.  But if an equality is already in
         the inert set and we can guarantee that adding a new equality
         will not cause the first equality to have an occurs check
         then we do not rewrite the inert equality.  This happens in
         TcInteract, rewriteInertEqsFromInertEq.

         See Note [Delicate equality kick-out] to see which inert
         equalities can safely stay in the inert set and which must be
         kicked out to be rewritten and re-checked for occurs errors.


Note [Solved constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~
When we take a step to simplify a constraint 'c', we call the original constraint "solved".
For example:   Wanted:    ev  :: [s] ~ [t]
               New goal:  ev1 :: s ~ t
               Then 'ev' is now "solved".

The reason for all this is simply to avoid re-solving goals we have solved already.

* A solved Wanted may depend on as-yet-unsolved goals, so (for example) we should not
  use it to rewrite a Given; in that sense the solved goal is still a Wanted

* A solved Given is just given

* A solved Derived in inert_solved is possible; purpose is to avoid
  creating tons of identical Derived goals.

  But there are no solved Deriveds in inert_solved_funeqs

Note [Type family equations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Type-family equations, of form (ev : F tys ~ ty), live in four places

  * The work-list, of course

  * The inert_flat_cache.  This is used when flattening, to get maximal
    sharing.  It contains lots of things that are still in the work-list.
    E.g Suppose we have (w1: F (G a) ~ Int), and (w2: H (G a) ~ Int) in the
        work list.  Then we flatten w1, dumping (w3: G a ~ f1) in the work
        list.  Now if we flatten w2 before we get to w3, we still want to
        share that (G a).

    Because it contains work-list things, DO NOT use the flat cache to solve
    a top-level goal.  Eg in the above example we don't want to solve w3
    using w3 itself!

  * The inert_solved_funeqs.  These are all "solved" goals (see Note [Solved constraints]),
    the result of using a top-level type-family instance.

  * THe inert_funeqs are un-solved but fully processed and in the InertCans.


\begin{code}
-- All Given (fully known) or Wanted or Derived
-- See Note [Detailed InertCans Invariants] for more
data InertCans
  = IC { inert_eqs :: TyVarEnv EqualCtList
              -- All CTyEqCans; index is the LHS tyvar
              -- Some Refl equalities are also in tcs_ty_binds
              -- see Note [Spontaneously solved in TyBinds] in TcInteract

       , inert_funeqs :: FunEqMap EqualCtList
              -- All CFunEqCans; index is the whole family head type.

       , inert_dicts :: DictMap Ct
              -- Dictionaries only, index is the class
              -- NB: index is /not/ the whole type because FD reactions
              -- need to match the class but not necessarily the whole type.

       , inert_irreds :: Cts
              -- Irreducible predicates

       , inert_insols :: Cts
              -- Frozen errors (as non-canonicals)

       , inert_no_eqs :: !Bool    
              -- Set to False when adding a new equality
              -- (eq/funeq) or potential equality (irred)
              -- whose evidence is not a constant
              -- See Note [When does an implication have given equalities?]
              -- in TcSimplify
       }

type EqualCtList = [Ct]
-- EqualCtList invariants:
--    * All are equalities
--    * All these equalities have the same LHS
--    * The list is never empty
--    * No element of the list can rewrite any other
--
-- From the fourth invariant it follows that the list is
--   - A single Given, or
--   - Multiple Wanteds, or
--   - Multiple Deriveds

-- The Inert Set
data InertSet
  = IS { inert_cans :: InertCans
              -- Canonical Given, Wanted, Derived (no Solved)
              -- Sometimes called "the inert set"

       , inert_flat_cache :: FunEqMap (CtEvidence, TcType)
              -- See Note [Type family equations]
              -- Just a hash-cons cache for use when flattening only
              -- These include entirely un-processed goals, so don't use
              -- them to solve a top-level goal, else you may end up solving
              -- (w:F ty ~ a) by setting w:=w!  We just use the flat-cache
              -- when allocating a new flatten-skolem.
              -- Not necessarily inert wrt top-level equations (or inert_cans)

       , inert_fsks :: [TcTyVar]  -- Rigid flatten-skolems (arising from givens)
                                  -- allocated in this local scope

       , inert_solved_funeqs :: FunEqMap (CtEvidence, TcType)
              -- See Note [Type family equations]
              -- Of form co :: F xis ~ xi
              -- Always the result of using a top-level family axiom F xis ~ tau
              -- No Deriveds
              -- Not necessarily fully rewritten (by type substitutions)

       , inert_solved_dicts   :: DictMap CtEvidence
              -- Of form ev :: C t1 .. tn
              -- Always the result of using a top-level instance declaration
              -- See Note [Solved constraints]
              -- - Used to avoid creating a new EvVar when we have a new goal
              --   that we have solved in the past
              -- - Stored not necessarily as fully rewritten
              --   (ToDo: rewrite lazily when we lookup)
       }


instance Outputable InertCans where
  ppr ics = vcat [ ptext (sLit "Equalities:")
                   <+> vcat (map ppr (varEnvElts (inert_eqs ics)))
                 , ptext (sLit "Type-function equalities:")
                   <+> vcat (map ppr (funEqsToList (inert_funeqs ics)))
                 , ptext (sLit "No-eqs:") <+> ppr (inert_no_eqs ics)
                 , ptext (sLit "Dictionaries:")
                   <+> vcat (map ppr (Bag.bagToList $ dictsToBag (inert_dicts ics)))
                 , ptext (sLit "Irreds:")
                   <+> vcat (map ppr (Bag.bagToList $ inert_irreds ics))
                 , text "Insolubles =" <+> -- Clearly print frozen errors
                    braces (vcat (map ppr (Bag.bagToList $ inert_insols ics)))
                 ]

instance Outputable InertSet where
  ppr is = vcat [ ppr $ inert_cans is
                , text "Solved dicts"  <+> int (sizeDictMap (inert_solved_dicts is))
                , text "Solved funeqs" <+> int (sizeFunEqMap (inert_solved_funeqs is))]

emptyInert :: InertSet
emptyInert
  = IS { inert_cans = IC { inert_eqs     = emptyVarEnv
                         , inert_dicts   = emptyDicts
                         , inert_funeqs  = emptyFunEqs
                         , inert_irreds  = emptyCts
                         , inert_insols  = emptyCts
                         , inert_no_eqs  = True
                         }
       , inert_fsks          = []
       , inert_flat_cache    = emptyFunEqs
       , inert_solved_funeqs = emptyFunEqs
       , inert_solved_dicts  = emptyDictMap }

---------------
addInertCan :: InertCans -> Ct -> InertCans
-- Precondition: item /is/ canonical
addInertCan ics item@(CTyEqCan { cc_ev = ev })
  = ics { inert_eqs = extendVarEnv_C (\eqs _ -> item : eqs)
                              (inert_eqs ics)
                              (cc_tyvar item) [item]
        , inert_no_eqs = isFlatSkolEv ev && inert_no_eqs ics }

addInertCan ics item@(CFunEqCan { cc_fun = tc, cc_tyargs = tys, cc_ev = ev })
  = ics { inert_funeqs = addFunEq (inert_funeqs ics) tc tys item
        , inert_no_eqs = isFlatSkolEv ev && inert_no_eqs ics }

addInertCan ics item@(CIrredEvCan {})
  = ics { inert_irreds = inert_irreds ics `Bag.snocBag` item
        , inert_no_eqs = False }
       -- The 'False' is because the irreducible constraint might later instantiate
       -- to an equality.
       -- But since we try to simplify first, if there's a constraint function FC with
       --    type instance FC Int = Show
       -- we'll reduce a constraint (FC Int a) to Show a, and never add an inert irreducible

addInertCan ics item@(CDictCan { cc_class = cls, cc_tyargs = tys })
  = ics { inert_dicts = addDict (inert_dicts ics) cls tys item }

addInertCan _ item
  = pprPanic "upd_inert set: can't happen! Inserting " $
    ppr item   -- Can't be CNonCanonical, CHoleCan,
               -- because they only land in inert_insols

isFlatSkolEv :: CtEvidence -> Bool
-- True if (a) it's a Given and (b) it is evidence for
-- (or derived from) a flatten-skolem equality.
-- See Note [When does an implication have given equalities?] in TcSimplify
isFlatSkolEv ev = case ctLocOrigin (ctev_loc ev) of
                    FlatSkolOrigin -> True
                    _              -> False

--------------
insertInertItemTcS :: Ct -> TcS ()
-- Add a new item in the inerts of the monad
insertInertItemTcS item
  = do { traceTcS "insertInertItemTcS {" $
         text "Trying to insert new inert item:" <+> ppr item

       ; updInertTcS (\ics -> ics { inert_cans = addInertCan (inert_cans ics) item })

       ; traceTcS "insertInertItemTcS }" $ empty }

addSolvedDict :: CtEvidence -> Class -> [Type] -> TcS ()
-- Add a new item in the solved set of the monad
addSolvedDict item cls tys
  | isIPPred (ctEvPred item)    -- Never cache "solved" implicit parameters (not sure why!)
  = return ()
  | otherwise
  = do { traceTcS "updSolvedSetTcs:" $ ppr item
       ; updInertTcS $ \ ics ->
             ics { inert_solved_dicts = addDict (inert_solved_dicts ics) cls tys item } }

addSolvedFunEq :: TyCon -> [TcType] -> CtEvidence -> TcType -> TcS ()
addSolvedFunEq fam_tc tys ev rhs_ty
  = updInertTcS $ \ inert ->
    inert { inert_solved_funeqs = insertFunEq (inert_solved_funeqs inert)
                                              fam_tc tys (ev, rhs_ty) }

updInertTcS :: (InertSet -> InertSet) -> TcS ()
-- Modify the inert set with the supplied function
updInertTcS upd
  = do { is_var <- getTcSInertsRef
       ; wrapTcS (do { curr_inert <- TcM.readTcRef is_var
                     ; TcM.writeTcRef is_var (upd curr_inert) }) }

prepareInertsForImplications :: InertSet -> InertSet
-- See Note [Preparing inert set for implications]
prepareInertsForImplications is
  = is { inert_cans    = getGivens (inert_cans is)
       , inert_fsks    = []
       , inert_flat_cache = emptyFunEqs }
  where
    getGivens (IC { inert_eqs    = eqs
                  , inert_irreds = irreds
                  , inert_funeqs = funeqs
                  , inert_dicts  = dicts })
      = IC { inert_eqs     = filterVarEnv is_given_eq eqs
           , inert_funeqs  = foldFunEqs given_from_wanted funeqs emptyFunEqs
           , inert_irreds  = Bag.filterBag isGivenCt irreds
           , inert_dicts   = filterDicts isGivenCt dicts
           , inert_insols  = emptyCts
           , inert_no_eqs  = True  -- Ready for each implication
           }

    is_given_eq :: [Ct] -> Bool
    is_given_eq (ct:rest) | isGivenCt ct = ASSERT( null rest ) True
    is_given_eq _                        = False

    given_from_wanted :: EqualCtList -> FunEqMap EqualCtList -> FunEqMap EqualCtList
    given_from_wanted (funeq:_) fhm  -- This is where the magic processing happens
                                     -- for type-function equalities
                                     -- Pick just the first
                                     -- See Note [Preparing inert set for implications]

      | isWanted ev  = insert_one (funeq { cc_ev = given_ev }) fhm
      | isGiven ev   = insert_one funeq fhm
      where
        ev = ctEvidence funeq
        given_ev = CtGiven { ctev_evtm = EvId (ctev_evar ev)
                           , ctev_pred = ctev_pred ev
                           , ctev_loc  = ctev_loc ev }

    given_from_wanted _ fhm = fhm -- Drop derived constraints

    insert_one :: Ct -> FunEqMap EqualCtList -> FunEqMap EqualCtList
    insert_one item@(CFunEqCan { cc_fun = tc, cc_tyargs = tys }) fhm
       = addFunEq fhm tc tys item
    insert_one item _ = pprPanic "insert_one" (ppr item)
\end{code}

Note [Preparing inert set for implications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Before solving the nested implications, we trim the inert set,
retaining only Givens.  These givens can be used when solving
the inner implications.

With one wrinkle!  We take all *wanted* *funeqs*, and turn them into givens.
Consider (Trac #4935)
   type instance F True a b = a
   type instance F False a b = b

   [w] F c a b ~ gamma
   (c ~ True) => a ~ gamma
   (c ~ False) => b ~ gamma

Obviously this is soluble with gamma := F c a b.  But
Since solveCTyFunEqs happens at the very end of solving, the only way
to solve the two implications is temporarily consider (F c a b ~ gamma)
as Given and push it inside the implications. Now, when we come
out again at the end, having solved the implications solveCTyFunEqs
will solve this equality.

Turning type-function equalities into Givens is easy becase they
*stay inert*.  No need to re-process them.

We don't try to turn any *other* Wanteds into Givens:

  * For example, we should not push given dictionaries in because
    of example LongWayOverlapping.hs, where we might get strange
    overlap errors between far-away constraints in the program.

There might be cases where interactions between wanteds can help
to solve a constraint. For example

        class C a b | a -> b
        (C Int alpha), (forall d. C d blah => C Int a)

If we push the (C Int alpha) inwards, as a given, it can produce a
fundep (alpha~a) and this can float out again and be used to fix
alpha.  (In general we can't float class constraints out just in case
(C d blah) might help to solve (C Int a).)  But we ignore this possiblity.

For Derived constraints we don't have evidence, so we do not turn
them into Givens.  There can *be* deriving CFunEqCans; see Trac #8129.

\begin{code}
getInertEqs :: TcS (TyVarEnv [Ct])
getInertEqs = do { inert <- getTcSInerts
                 ; return (inert_eqs (inert_cans inert)) }

getInertUnsolved :: TcS (Cts, Cts)
-- Return (unsolved-wanteds, insolubles)
-- Both consist of a mixture of Wanted and Derived
getInertUnsolved
 = do { is <- getTcSInerts

      ; let icans = inert_cans is
            unsolved_irreds = Bag.filterBag is_unsolved  (inert_irreds icans)
            unsolved_dicts  = foldDicts  add_if_unsolved  (inert_dicts icans)  emptyCts
            unsolved_funeqs = foldFunEqs add_if_unsolveds (inert_funeqs icans) emptyCts
            unsolved_eqs    = foldVarEnv add_if_unsolveds emptyCts (inert_eqs icans)

            unsolved_flats = unsolved_eqs `unionBags` unsolved_irreds `unionBags`
                             unsolved_dicts `unionBags` unsolved_funeqs

      ; return (unsolved_flats, inert_insols icans) }
  where
    add_if_unsolved :: Ct -> Cts -> Cts
    add_if_unsolved ct cts | is_unsolved ct = cts `extendCts` ct
                           | otherwise      = cts

    add_if_unsolveds :: [Ct] -> Cts -> Cts
    add_if_unsolveds eqs cts = foldr add_if_unsolved cts eqs

    is_unsolved ct = not (isGivenCt ct)   -- Wanted or Derived

checkAllSolved :: TcS Bool
-- True if there are no unsolved wanteds
-- Ignore Derived for this purpose, unless in insolubles
checkAllSolved
 = do { is <- getTcSInerts

      ; let icans = inert_cans is
            unsolved_irreds = Bag.anyBag isWantedCt (inert_irreds icans)
            unsolved_dicts  = foldDicts ((||)  . isWantedCt)     (inert_dicts icans)  False
            unsolved_funeqs = foldFunEqs ((||) . any isWantedCt) (inert_funeqs icans) False
            unsolved_eqs    = foldVarEnv ((||) . any isWantedCt) False (inert_eqs icans)

      ; return (not (unsolved_eqs || unsolved_irreds
                     || unsolved_dicts || unsolved_funeqs
                     || not (isEmptyBag (inert_insols icans)))) }

lookupFlatEqn :: TyCon -> [Type] -> TcS (Maybe (CtEvidence, TcType))
lookupFlatEqn fam_tc tys
  = do { IS { inert_solved_funeqs = solved_funeqs
            , inert_flat_cache = flat_cache
            , inert_cans = IC { inert_funeqs = inert_funeqs } } <- getTcSInerts
       ; return (firstJusts [findFunEq solved_funeqs fam_tc tys,
                             lookup_inerts inert_funeqs,
                             findFunEq flat_cache fam_tc tys]) }
  where
    lookup_inerts inert_funeqs
      | (ct:_) <- findFunEqs inert_funeqs fam_tc tys
      = Just (ctEvidence ct, cc_rhs ct)
      | otherwise
      = Nothing

lookupInInerts :: TcPredType -> TcS (Maybe CtEvidence)
-- Is this exact predicate type cached in the solved or canonicals of the InertSet?
lookupInInerts pty
  = do { IS { inert_solved_dicts = solved_dicts
            , inert_cans         = inert_cans }
            <- getTcSInerts
       ; return $ case (classifyPredType pty) of
           ClassPred cls tys
              | Just ctev <- findDict solved_dicts cls tys
                    -- I'm not sure why we check for solved dicts,
                    -- but not for solved funeqs
              -> Just ctev
              | Just ct <- findDict (inert_dicts inert_cans) cls tys
              -> Just (ctEvidence ct)

           EqPred ty1 _ty2
             | Just tv <- getTyVar_maybe ty1      -- Tyvar equation
             -> foldr exact_match Nothing (findTyEqs (inert_eqs inert_cans) tv)

             | Just (tc, tys) <- splitTyConApp_maybe ty1  -- Family equation
             -> foldr exact_match Nothing (findFunEqs (inert_funeqs inert_cans) tc tys)

           IrredPred {} -> foldrBag exact_match Nothing (inert_irreds inert_cans)

           _other -> Nothing -- NB: No caching for IPs or holes
      }
  where
    exact_match :: Ct -> Maybe CtEvidence -> Maybe CtEvidence
    exact_match ct deflt | let ctev = ctEvidence ct
                         , ctEvPred ctev `tcEqType` pty
                         = Just ctev
                         | otherwise
                         = deflt

lookupSolvedDict :: InertSet -> Class -> [Type] -> Maybe CtEvidence
-- Returns just if exactly this predicate type exists in the solved.
lookupSolvedDict (IS { inert_solved_dicts = solved }) cls tys
  = findDict solved cls tys
\end{code}


%************************************************************************
%*                                                                      *
                   TyEqMap
%*                                                                      *
%************************************************************************

\begin{code}
type TyEqMap a = TyVarEnv a

findTyEqs :: TyEqMap EqualCtList -> TyVar -> EqualCtList
findTyEqs m tv = lookupVarEnv m tv `orElse` []
\end{code}


%************************************************************************
%*                                                                      *
                   TcAppMap, DictMap, FunEqMap
%*                                                                      *
%************************************************************************

\begin{code}
type TcAppMap a = UniqFM (ListMap TypeMap a)
    -- Indexed by tycon then the arg types
    -- Used for types and classes; hence UniqFM

emptyTcAppMap :: TcAppMap a
emptyTcAppMap = emptyUFM

findTcApp :: TcAppMap a -> Unique -> [Type] -> Maybe a
findTcApp m u tys = do { tys_map <- lookupUFM m u
                       ; lookupTM tys tys_map }

delTcApp :: TcAppMap a -> Unique -> [Type] -> TcAppMap a
delTcApp m cls tys = adjustUFM (deleteTM tys) m cls

insertTcApp :: TcAppMap a -> Unique -> [Type] -> a -> TcAppMap a
insertTcApp m cls tys ct = alterUFM alter_tm m cls
  where
    alter_tm mb_tm = Just (insertTM tys ct (mb_tm `orElse` emptyTM))

tcAppMapToBag :: TcAppMap a -> Bag a
tcAppMapToBag m = foldTcAppMap consBag m emptyBag

foldTcAppMap :: (a -> b -> b) -> TcAppMap a -> b -> b
foldTcAppMap k m z = foldUFM (foldTM k) z m

-------------------------
type DictMap a = TcAppMap a

emptyDictMap :: DictMap a
emptyDictMap = emptyTcAppMap

sizeDictMap :: DictMap a -> Int
sizeDictMap m = foldDicts (\ _ x -> x+1) m 0

findDict :: DictMap a -> Class -> [Type] -> Maybe a
findDict m cls tys = findTcApp m (getUnique cls) tys

findDictsByClass :: DictMap a -> Class -> Bag a
findDictsByClass m cls
  | Just tm <- lookupUFM m cls = foldTM consBag tm emptyBag
  | otherwise                  = emptyBag

delDict :: DictMap a -> Class -> [Type] -> DictMap a
delDict m cls tys = delTcApp m (getUnique cls) tys

addDict :: DictMap a -> Class -> [Type] -> a -> DictMap a
addDict m cls tys item = insertTcApp m (getUnique cls) tys item

addDictsByClass :: DictMap Ct -> Class -> Bag Ct -> DictMap Ct
addDictsByClass m cls items
  = addToUFM m cls (foldrBag add emptyTM items)
  where
    add ct@(CDictCan { cc_tyargs = tys }) tm = insertTM tys ct tm
    add ct _ = pprPanic "addDictsByClass" (ppr ct)

filterDicts :: (Ct -> Bool) -> DictMap Ct -> DictMap Ct
filterDicts f m = mapUFM do_tm m
  where
    do_tm tm = foldTM insert_mb tm emptyTM
    insert_mb ct@(CDictCan { cc_tyargs = tys }) tm
       | f ct      = insertTM tys ct tm
       | otherwise = tm
    insert_mb ct _ = pprPanic "filterDicts" (ppr ct)

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

------------------------
type FunEqMap a = TcAppMap a  -- A map whose key is a (TyCon, [Type]) pair

emptyFunEqs :: TcAppMap a
emptyFunEqs = emptyTcAppMap

sizeFunEqMap :: FunEqMap a -> Int
sizeFunEqMap m = foldFunEqs (\ _ x -> x+1) m 0

findFunEq :: FunEqMap a -> TyCon -> [Type] -> Maybe a
findFunEq m tc tys = findTcApp m (getUnique tc) tys

findFunEqs :: FunEqMap [a] -> TyCon -> [Type] -> [a]
findFunEqs m tc tys = findTcApp m (getUnique tc) tys `orElse` []

funEqsToList :: FunEqMap [a] -> [a]
funEqsToList m = foldTcAppMap (++) m []

findFunEqsByTyCon :: FunEqMap [a] -> TyCon -> [a]
-- Get inert function equation constraints that have the given tycon
-- in their head.  Not that the constraints remain in the inert set.
-- We use this to check for derived interactions with built-in type-function
-- constructors.
findFunEqsByTyCon m tc
  | Just tm <- lookupUFM m tc = foldTM (++) tm []
  | otherwise                 = []

foldFunEqs :: (a -> b -> b) -> FunEqMap a -> b -> b
foldFunEqs = foldTcAppMap

insertFunEq :: FunEqMap a -> TyCon -> [Type] -> a -> FunEqMap a
insertFunEq m tc tys val = insertTcApp m (getUnique tc) tys val

addFunEq :: FunEqMap EqualCtList -> TyCon -> [Type] -> Ct -> FunEqMap EqualCtList
addFunEq m tc tys item
  = alterUFM alter_tm m (getUnique tc)
  where
    alter_tm mb_tm = Just (alterTM tys alter_cts (mb_tm `orElse` emptyTM))
    alter_cts Nothing       = Just [item]
    alter_cts (Just funeqs) = Just (item : funeqs)

replaceFunEqs :: FunEqMap EqualCtList -> TyCon -> [Type] -> Ct -> FunEqMap EqualCtList
replaceFunEqs m tc tys ct = insertTcApp m (getUnique tc) tys [ct]

partitionFunEqs :: (Ct -> Bool) -> FunEqMap EqualCtList -> (Bag Ct, FunEqMap EqualCtList)
partitionFunEqs f m = foldTcAppMap k m (emptyBag, emptyFunEqs)
  where
    k cts (yeses, noes)
      = ( case eqs_out of
            [] -> yeses
            _  -> yeses `unionBags` listToBag eqs_out
        , case eqs_in of
            CFunEqCan { cc_fun = tc, cc_tyargs = tys } : _
                -> insertTcApp noes (getUnique tc) tys eqs_in
            _  -> noes )
      where
        (eqs_out, eqs_in) = partition f cts
\end{code}


%************************************************************************
%*                                                                      *
%*              The TcS solver monad                                    *
%*                                                                      *
%************************************************************************

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

\begin{code}
data TcSEnv
  = TcSEnv {
      tcs_ev_binds    :: EvBindsVar,

      tcs_ty_binds :: IORef (Bool, TyVarEnv (TcTyVar, TcType)),
          -- Global type bindings for unification variables
          -- See Note [Spontaneously solved in TyBinds] in TcInteract
          -- The "dirty-flag" Bool is set True when we add a binding

      tcs_count    :: IORef Int, -- Global step count

      tcs_inerts   :: IORef InertSet, -- Current inert set
      tcs_worklist :: IORef WorkList, -- Current worklist

      -- Residual implication constraints that are generated
      -- while solving or canonicalising the current worklist.
      -- Specifically, when canonicalising (forall a. t1 ~ forall a. t2)
      -- from which we get the implication (forall a. t1 ~ t2)
      tcs_implics  :: IORef (Bag Implication),

      tcs_ext_solver :: TN.ExternalSolver
    }
\end{code}

\begin{code}

---------------
newtype TcS a = TcS { unTcS :: TcSEnv -> TcM a }

instance Functor TcS where
  fmap f m = TcS $ fmap f . unTcS m

instance Applicative TcS where
  pure  = return
  (<*>) = ap

instance Monad TcS where
  return x  = TcS (\_ -> return x)
  fail err  = TcS (\_ -> fail err)
  m >>= k   = TcS (\ebs -> unTcS m ebs >>= \r -> unTcS (k r) ebs)

instance MonadUnique TcS where
   getUniqueSupplyM = wrapTcS getUniqueSupplyM

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

failTcS, panicTcS :: SDoc -> TcS a
failTcS      = wrapTcS . TcM.failWith
panicTcS doc = pprPanic "TcCanonical" doc

traceTcS :: String -> SDoc -> TcS ()
traceTcS herald doc = wrapTcS (TcM.traceTc herald doc)

instance HasDynFlags TcS where
    getDynFlags = wrapTcS getDynFlags

getGlobalRdrEnvTcS :: TcS GlobalRdrEnv
getGlobalRdrEnvTcS = wrapTcS TcM.getGlobalRdrEnv

bumpStepCountTcS :: TcS ()
bumpStepCountTcS = TcS $ \env -> do { let ref = tcs_count env
                                    ; n <- TcM.readTcRef ref
                                    ; TcM.writeTcRef ref (n+1) }

traceFireTcS :: Ct -> SDoc -> TcS ()
-- Dump a rule-firing trace
traceFireTcS ct doc
  = TcS $ \env ->
    do { dflags <- getDynFlags
       ; when (dopt Opt_D_dump_cs_trace dflags && traceLevel dflags >= 1) $
    do { n <- TcM.readTcRef (tcs_count env)
       ; let msg = int n <> brackets (ppr (ctLocDepth (ctev_loc ev)))
                   <+> ppr ev <> colon <+> doc
       ; TcM.debugDumpTcRn msg } }
  where ev = cc_ev ct

runTcS :: TcS a                -- What to run
       -> TcM (a, Bag EvBind)
runTcS tcs
  = do { ev_binds_var <- TcM.newTcEvBinds
       ; res <- runTcSWithEvBinds ev_binds_var tcs
       ; ev_binds <- TcM.getTcEvBinds ev_binds_var
       ; return (res, ev_binds) }

runTcSWithEvBinds :: EvBindsVar
                  -> TcS a
                  -> TcM a
runTcSWithEvBinds ev_binds_var tcs
  = do { ty_binds_var <- TcM.newTcRef (False, emptyVarEnv)
       ; step_count <- TcM.newTcRef 0
       ; inert_var <- TcM.newTcRef is

       ; extSol <- liftIO $
                   TN.newExternalSolver "cvc4" [ "--incremental"
                                               , "--lang=smtlib2" ]

       ; let env = TcSEnv { tcs_ev_binds = ev_binds_var
                          , tcs_ty_binds = ty_binds_var
                          , tcs_count    = step_count
                          , tcs_inerts   = inert_var
                          , tcs_ext_solver = extSol
                          , tcs_worklist    = panic "runTcS: worklist"
                          , tcs_implics     = panic "runTcS: implics" }
                               -- NB: Both these are initialised by withWorkList

             -- Run the computation
       ; res <- unTcS tcs env
             -- Perform the type unifications required
       ; (_, ty_binds) <- TcM.readTcRef ty_binds_var
       ; mapM_ do_unification (varEnvElts ty_binds)

       ; TcM.whenDOptM Opt_D_dump_cs_trace $
         do { count <- TcM.readTcRef step_count
            ; when (count > 0) $
              TcM.debugDumpTcRn (ptext (sLit "Constraint solver steps =") <+> int count ) }

#ifdef DEBUG
       ; ev_binds <- TcM.getTcEvBinds ev_binds_var
       ; checkForCyclicBinds ev_binds
#endif
       ; liftIO $ TN.extSolStop extSol

       ; return res }
  where
    do_unification (tv,ty) = TcM.writeMetaTyVar tv ty
    is = emptyInert

#ifdef DEBUG
checkForCyclicBinds :: Bag EvBind -> TcM ()
checkForCyclicBinds ev_binds
  | null cycles
  = return ()
  | null coercion_cycles
  = TcM.traceTc "Cycle in evidence binds" $ ppr cycles
  | otherwise
  = pprPanic "Cycle in coercion bindings" $ ppr coercion_cycles
  where
    cycles :: [[EvBind]]
    cycles = [c | CyclicSCC c <- stronglyConnCompFromEdgedVertices edges]

    coercion_cycles = [c | c <- cycles, any is_co_bind c]
    is_co_bind (EvBind b _) = isEqVar b

    edges :: [(EvBind, EvVar, [EvVar])]
    edges = [(bind, bndr, varSetElems (evVarsOfTerm rhs)) | bind@(EvBind bndr rhs) <- bagToList ev_binds]
#endif

nestImplicTcS :: EvBindsVar -> Untouchables -> InertSet -> TcS a -> TcS a
nestImplicTcS ref inner_untch inerts (TcS thing_inside)
  = TcS $ \ TcSEnv { tcs_ty_binds = ty_binds
                   , tcs_ext_solver = outer_solver
                   , tcs_count = count } ->
    do { new_inert_var <- TcM.newTcRef inerts
       ; liftIO (TN.extSolPush outer_solver)
       ; let nest_env = TcSEnv { tcs_ev_binds    = ref
                               , tcs_ty_binds    = ty_binds
                               , tcs_count       = count
                               , tcs_ext_solver  = outer_solver
                               , tcs_inerts      = new_inert_var
                               , tcs_worklist    = panic "nextImplicTcS: worklist"
                               , tcs_implics     = panic "nextImplicTcS: implics"
                               -- NB: Both these are initialised by withWorkList
                               }
       ; res <- TcM.setUntouchables inner_untch $
                thing_inside nest_env
       ; liftIO (TN.extSolPop outer_solver)

#ifdef DEBUG
       -- Perform a check that the thing_inside did not cause cycles
       ; ev_binds <- TcM.getTcEvBinds ref
       ; checkForCyclicBinds ev_binds
#endif

       ; return res }

recoverTcS :: TcS a -> TcS a -> TcS a
recoverTcS (TcS recovery_code) (TcS thing_inside)
  = TcS $ \ env ->
    TcM.recoverM (recovery_code env) (thing_inside env)

nestTcS ::  TcS a -> TcS a
-- Use the current untouchables, augmenting the current
-- evidence bindings, ty_binds, and solved caches
-- But have no effect on the InertCans or insolubles
nestTcS (TcS thing_inside)
  = TcS $ \ env@(TcSEnv { tcs_inerts = inerts_var }) ->
    do { inerts <- TcM.readTcRef inerts_var
       ; new_inert_var <- TcM.newTcRef inerts
       ; let nest_env = env { tcs_inerts   = new_inert_var
                            , tcs_worklist = panic "nextImplicTcS: worklist"
                            , tcs_implics  = panic "nextImplicTcS: implics" }
       ; thing_inside nest_env }

tryTcS :: TcS a -> TcS a
-- Like runTcS, but from within the TcS monad
-- Completely fresh inerts and worklist, be careful!
-- Moreover, we will simply throw away all the evidence generated.
-- We have a completely empty tcs_ty_binds too, so make sure the
-- input stuff is fully rewritten wrt any outer inerts
tryTcS (TcS thing_inside)
  = TcS $ \env ->
    do { is_var <- TcM.newTcRef emptyInert
       ; ty_binds_var <- TcM.newTcRef (False, emptyVarEnv)
       ; ev_binds_var <- TcM.newTcEvBinds

       ; let nest_env = env { tcs_ev_binds = ev_binds_var
                            , tcs_ty_binds = ty_binds_var
                            , tcs_inerts   = is_var
                            , tcs_worklist = panic "nextImplicTcS: worklist"
                            , tcs_implics  = panic "nextImplicTcS: implics" }
       ; thing_inside nest_env }

-- Getters and setters of TcEnv fields
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Getter of inerts and worklist
getTcSInertsRef :: TcS (IORef InertSet)
getTcSInertsRef = TcS (return . tcs_inerts)

getTcSWorkListRef :: TcS (IORef WorkList)
getTcSWorkListRef = TcS (return . tcs_worklist)

getTcSInerts :: TcS InertSet
getTcSInerts = getTcSInertsRef >>= wrapTcS . (TcM.readTcRef)

setTcSInerts :: InertSet -> TcS ()
setTcSInerts ics = do { r <- getTcSInertsRef; wrapTcS (TcM.writeTcRef r ics) }

updWorkListTcS :: (WorkList -> WorkList) -> TcS ()
updWorkListTcS f
  = do { wl_var <- getTcSWorkListRef
       ; wl_curr <- wrapTcS (TcM.readTcRef wl_var)
       ; let new_work = f wl_curr
       ; wrapTcS (TcM.writeTcRef wl_var new_work) }

updWorkListTcS_return :: (WorkList -> (a,WorkList)) -> TcS a
-- Process the work list, returning a depleted work list,
-- plus a value extracted from it (typically a work item removed from it)
updWorkListTcS_return f
  = do { wl_var <- getTcSWorkListRef
       ; wl_curr <- wrapTcS (TcM.readTcRef wl_var)
       ; let (res,new_work) = f wl_curr
       ; wrapTcS (TcM.writeTcRef wl_var new_work)
       ; return res }

withWorkList :: Cts -> TcS () -> TcS (Bag Implication)
-- Use 'thing_inside' to solve 'work_items', extending the
-- ambient InertSet, and returning any residual implications
-- (arising from polytype equalities)
-- We do this with fresh work list and residual-implications variables
withWorkList work_items (TcS thing_inside)
  = TcS $ \ tcs_env ->
    do { let init_work_list = foldrBag extendWorkListCt emptyWorkList work_items
       ; new_wl_var <- TcM.newTcRef init_work_list
       ; new_implics_var <- TcM.newTcRef emptyBag
       ; thing_inside (tcs_env { tcs_worklist = new_wl_var
                               , tcs_implics = new_implics_var })
       ; final_wl <- TcM.readTcRef new_wl_var
       ; implics  <- TcM.readTcRef new_implics_var
       ; ASSERT( isEmptyWorkList final_wl )
         return implics }

updTcSImplics :: (Bag Implication -> Bag Implication) -> TcS ()
updTcSImplics f
 = do { impl_ref <- getTcSImplicsRef
      ; wrapTcS $ do { implics <- TcM.readTcRef impl_ref
                     ; TcM.writeTcRef impl_ref (f implics) } }

emitInsoluble :: Ct -> TcS ()
-- Emits a non-canonical constraint that will stand for a frozen error in the inerts.
emitInsoluble ct
  = do { traceTcS "Emit insoluble" (ppr ct)
       ; updInertTcS add_insol }
  where
    this_pred = ctPred ct
    add_insol is@(IS { inert_cans = ics@(IC { inert_insols = old_insols }) })
      | already_there = is
      | otherwise     = is { inert_cans = ics { inert_insols = extendCts old_insols ct } }
      where
        already_there = not (isWantedCt ct) && anyBag (tcEqType this_pred . ctPred) old_insols
             -- See Note [Do not add duplicate derived insolubles]

getTcSImplicsRef :: TcS (IORef (Bag Implication))
getTcSImplicsRef = TcS (return . tcs_implics)

getTcEvBinds :: TcS EvBindsVar
getTcEvBinds = TcS (return . tcs_ev_binds)

getUntouchables :: TcS Untouchables
getUntouchables = wrapTcS TcM.getUntouchables

getGivenInfo :: TcS a -> TcS (Bool, [TcTyVar], a)
-- Run thing_inside, returning info on
--  a) whether we got any new equalities
--  b) which new (given) flatten skolems were generated
getGivenInfo thing_inside
  = do { updInertTcS reset_vars
       ; res <- thing_inside
       ; is  <- getTcSInerts
       ; return (inert_no_eqs (inert_cans is), inert_fsks is, res) }
  where
    reset_vars :: InertSet -> InertSet
    reset_vars is = is { inert_cans = (inert_cans is) { inert_no_eqs = True }
                       , inert_fsks = [] }

getTcSTyBinds :: TcS (IORef (Bool, TyVarEnv (TcTyVar, TcType)))
getTcSTyBinds = TcS (return . tcs_ty_binds)

getTcSTyBindsMap :: TcS (TyVarEnv (TcTyVar, TcType))
getTcSTyBindsMap = do { ref <- getTcSTyBinds
                      ; wrapTcS $ do { (_, binds) <- TcM.readTcRef ref
                                     ; return binds } }

getTcEvBindsMap :: TcS EvBindMap
getTcEvBindsMap
  = do { EvBindsVar ev_ref _ <- getTcEvBinds
       ; wrapTcS $ TcM.readTcRef ev_ref }

setWantedTyBind :: TcTyVar -> TcType -> TcS ()
-- Add a type binding
-- We never do this twice!
setWantedTyBind tv ty
  = ASSERT2( isMetaTyVar tv, ppr tv )
    do { ref <- getTcSTyBinds
       ; wrapTcS $
         do { (_dirty, ty_binds) <- TcM.readTcRef ref
            ; when debugIsOn $
                  TcM.checkErr (not (tv `elemVarEnv` ty_binds)) $
                  vcat [ text "TERRIBLE ERROR: double set of meta type variable"
                       , ppr tv <+> text ":=" <+> ppr ty
                       , text "Old value =" <+> ppr (lookupVarEnv_NF ty_binds tv)]
            ; TcM.traceTc "setWantedTyBind" (ppr tv <+> text ":=" <+> ppr ty)
            ; TcM.writeTcRef ref (True, extendVarEnv ty_binds tv (tv,ty)) } }

reportUnifications :: TcS a -> TcS (Bool, a)
reportUnifications thing_inside
  = do { ty_binds_var <- getTcSTyBinds
       ; outer_dirty <- wrapTcS $
            do { (outer_dirty, binds1) <- TcM.readTcRef ty_binds_var
               ; TcM.writeTcRef ty_binds_var (False, binds1)
               ; return outer_dirty }
      ; res <- thing_inside
      ; wrapTcS $
        do { (inner_dirty, binds2) <- TcM.readTcRef ty_binds_var
           ; if inner_dirty then
                 return (True, res)
             else
                do { TcM.writeTcRef ty_binds_var (outer_dirty, binds2)
                   ; return (False, res) } } }
\end{code}

\begin{code}
getDefaultInfo ::  TcS ([Type], (Bool, Bool))
getDefaultInfo = wrapTcS TcM.tcGetDefaultTys

-- Just get some environments needed for instance looking up and matching
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

getInstEnvs :: TcS (InstEnv, InstEnv)
getInstEnvs = wrapTcS $ Inst.tcGetInstEnvs

getFamInstEnvs :: TcS (FamInstEnv, FamInstEnv)
getFamInstEnvs = wrapTcS $ FamInst.tcGetFamInstEnvs

getTopEnv :: TcS HscEnv
getTopEnv = wrapTcS $ TcM.getTopEnv

getGblEnv :: TcS TcGblEnv
getGblEnv = wrapTcS $ TcM.getGblEnv

-- Setting names as used (used in the deriving of Coercible evidence)
-- Too hackish to expose it to TcS? In that case somehow extract the used
-- constructors from the result of solveInteract
addUsedRdrNamesTcS :: [RdrName] -> TcS ()
addUsedRdrNamesTcS names = wrapTcS  $ addUsedRdrNames names

-- Various smaller utilities [TODO, maybe will be absorbed in the instance matcher]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

checkWellStagedDFun :: PredType -> DFunId -> CtLoc -> TcS ()
checkWellStagedDFun pred dfun_id loc
  = wrapTcS $ TcM.setCtLoc loc $
    do { use_stage <- TcM.getStage
       ; TcM.checkWellStaged pp_thing bind_lvl (thLevel use_stage) }
  where
    pp_thing = ptext (sLit "instance for") <+> quotes (ppr pred)
    bind_lvl = TcM.topIdLvl dfun_id

pprEq :: TcType -> TcType -> SDoc
pprEq ty1 ty2 = pprType $ mkEqPred ty1 ty2

isTouchableMetaTyVarTcS :: TcTyVar -> TcS Bool
isTouchableMetaTyVarTcS tv
  = do { untch <- getUntouchables
       ; return $ isTouchableMetaTyVar untch tv }

isFilledMetaTyVar_maybe :: TcTyVar -> TcS (Maybe Type)
isFilledMetaTyVar_maybe tv
 = ASSERT2( isTcTyVar tv, ppr tv )
   case tcTyVarDetails tv of
     MetaTv { mtv_ref = ref }
        -> do { cts <- wrapTcS (TcM.readTcRef ref)
              ; case cts of
                  Indirect ty -> return (Just ty)
                  Flexi       -> return Nothing }
     _ -> return Nothing

zonkTyVarsAndFV :: TcTyVarSet -> TcS TcTyVarSet
zonkTyVarsAndFV tvs = wrapTcS (TcM.zonkTyVarsAndFV tvs)
\end{code}

Note [Do not add duplicate derived insolubles]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general we *must* add an insoluble (Int ~ Bool) even if there is
one such there already, because they may come from distinct call
sites.  Not only do we want an error message for each, but with
-fdefer-type-errors we must generate evidence for each.  But for
*derived* insolubles, we only want to report each one once.  Why?

(a) A constraint (C r s t) where r -> s, say, may generate the same fundep
    equality many times, as the original constraint is sucessively rewritten.

(b) Ditto the successive iterations of the main solver itself, as it traverses
    the constraint tree. See example below.

Also for *given* insolubles we may get repeated errors, as we
repeatedly traverse the constraint tree.  These are relatively rare
anyway, so removing duplicates seems ok.  (Alternatively we could take
the SrcLoc into account.)

Note that the test does not need to be particularly efficient because
it is only used if the program has a type error anyway.

Example of (b): assume a top-level class and instance declaration:

  class D a b | a -> b
  instance D [a] [a]

Assume we have started with an implication:

  forall c. Eq c => { wc_flat = D [c] c [W] }

which we have simplified to:

  forall c. Eq c => { wc_flat = D [c] c [W]
                    , wc_insols = (c ~ [c]) [D] }

For some reason, e.g. because we floated an equality somewhere else,
we might try to re-solve this implication. If we do not do a
dropDerivedWC, then we will end up trying to solve the following
constraints the second time:

  (D [c] c) [W]
  (c ~ [c]) [D]

which will result in two Deriveds to end up in the insoluble set:

  wc_flat   = D [c] c [W]
  wc_insols = (c ~ [c]) [D], (c ~ [c]) [D]



\begin{code}
-- Flatten skolems
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
newFlattenSkolem :: CtEvidence
                 -> TcType                      -- F xis
                 -> TcS (CtEvidence, TcType)    -- co :: F xis ~ ty
-- We have already looked up in the cache; no need to so so again
newFlattenSkolem ev fam_ty
  | isGiven ev
  = do { tv <- wrapTcS $
               do { uniq <- TcM.newUnique
                  ; let name = TcM.mkTcTyVarName uniq (fsLit "f")
                  ; return $ mkTcTyVar name (typeKind fam_ty) (FlatSkol fam_ty) }
       ; traceTcS "New Flatten Skolem Born" $
         ppr tv <+> text "[:= " <+> ppr fam_ty <+> text "]"

       ; updInertTcS $ \ is -> is { inert_fsks = tv : inert_fsks is }

       ; let rhs_ty = mkTyVarTy tv
             ctev = CtGiven { ctev_pred = mkTcEqPred fam_ty rhs_ty
                            , ctev_evtm = EvCoercion (mkTcNomReflCo fam_ty)
                            , ctev_loc =  (ctev_loc ev) { ctl_origin = FlatSkolOrigin } }
       ; return (ctev, rhs_ty) }

  | otherwise  -- Wanted or Derived: make new unification variable
  = do { rhs_ty <- newFlexiTcSTy (typeKind fam_ty)
       ; ctev <- newWantedEvVarNC (ctev_loc ev) (mkTcEqPred fam_ty rhs_ty)
                          -- NC (no-cache) version because we've already
                          -- looked in the solved goals and inerts (lookupFlatEqn)
       ; return (ctev, rhs_ty) }


extendFlatCache :: TyCon -> [Type] -> CtEvidence -> TcType -> TcS ()
extendFlatCache tc xi_args ev rhs_xi
  = do { dflags <- getDynFlags
       ; when (gopt Opt_FlatCache dflags) $
         updInertTcS $ \ is@(IS { inert_flat_cache = fc }) ->
            is { inert_flat_cache = insertFunEq fc tc xi_args (ev, rhs_xi) } }

-- Instantiations
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

instDFunType :: DFunId -> [DFunInstType] -> TcS ([TcType], TcType)
instDFunType dfun_id mb_inst_tys
  = wrapTcS $ go dfun_tvs mb_inst_tys (mkTopTvSubst [])
  where
    (dfun_tvs, dfun_phi) = tcSplitForAllTys (idType dfun_id)

    go :: [TyVar] -> [DFunInstType] -> TvSubst -> TcM ([TcType], TcType)
    go [] [] subst = return ([], substTy subst dfun_phi)
    go (tv:tvs) (Just ty : mb_tys) subst
      = do { (tys, phi) <- go tvs mb_tys (extendTvSubst subst tv ty)
           ; return (ty : tys, phi) }
    go (tv:tvs) (Nothing : mb_tys) subst
      = do { ty <- instFlexiTcSHelper (tyVarName tv) (substTy subst (tyVarKind tv))
                         -- Don't forget to instantiate the kind!
                         -- cf TcMType.tcInstTyVarX
           ; (tys, phi) <- go tvs mb_tys (extendTvSubst subst tv ty)
           ; return (ty : tys, phi) }
    go _ _ _ = pprPanic "instDFunTypes" (ppr dfun_id $$ ppr mb_inst_tys)

newFlexiTcSTy :: Kind -> TcS TcType
newFlexiTcSTy knd = wrapTcS (TcM.newFlexiTyVarTy knd)

cloneMetaTyVar :: TcTyVar -> TcS TcTyVar
cloneMetaTyVar tv = wrapTcS (TcM.cloneMetaTyVar tv)

instFlexiTcS :: [TKVar] -> TcS (TvSubst, [TcType])
instFlexiTcS tvs = wrapTcS (mapAccumLM inst_one emptyTvSubst tvs)
  where
     inst_one subst tv
         = do { ty' <- instFlexiTcSHelper (tyVarName tv)
                                          (substTy subst (tyVarKind tv))
              ; return (extendTvSubst subst tv ty', ty') }

instFlexiTcSHelper :: Name -> Kind -> TcM TcType
instFlexiTcSHelper tvname kind
  = do { uniq <- TcM.newUnique
       ; details <- TcM.newMetaDetails TauTv
       ; let name = setNameUnique tvname uniq
       ; return (mkTyVarTy (mkTcTyVar name kind details)) }

instFlexiTcSHelperTcS :: Name -> Kind -> TcS TcType
instFlexiTcSHelperTcS n k = wrapTcS (instFlexiTcSHelper n k)


-- Creating and setting evidence variables and CtFlavors
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data XEvTerm
  = XEvTerm { ev_preds  :: [PredType]           -- New predicate types
            , ev_comp   :: [EvTerm] -> EvTerm   -- How to compose evidence
            , ev_decomp :: EvTerm -> [EvTerm]   -- How to decompose evidence
            }

data MaybeNew = Fresh CtEvidence | Cached EvTerm

isFresh :: MaybeNew -> Bool
isFresh (Fresh {}) = True
isFresh _ = False

getEvTerm :: MaybeNew -> EvTerm
getEvTerm (Fresh ctev) = ctEvTerm ctev
getEvTerm (Cached tm)  = tm

getEvTerms :: [MaybeNew] -> [EvTerm]
getEvTerms = map getEvTerm

freshGoal :: MaybeNew -> Maybe CtEvidence
freshGoal (Fresh ctev) = Just ctev
freshGoal _ = Nothing

freshGoals :: [MaybeNew] -> [CtEvidence]
freshGoals mns = [ ctev | Fresh ctev <- mns ]

setEvBind :: EvVar -> EvTerm -> TcS ()
setEvBind the_ev tm
  = do { traceTcS "setEvBind" $ vcat [ text "ev =" <+> ppr the_ev
                                     , text "tm  =" <+> ppr tm ]
       ; tc_evbinds <- getTcEvBinds
       ; wrapTcS $ TcM.addTcEvBind tc_evbinds the_ev tm }

newGivenEvVar :: CtLoc -> (TcPredType, EvTerm) -> TcS CtEvidence
-- Make a new variable of the given PredType,
-- immediately bind it to the given term
-- and return its CtEvidence
newGivenEvVar loc (pred, rhs)
  = do { new_ev <- wrapTcS $ TcM.newEvVar pred
       ; setEvBind new_ev rhs
       ; return (CtGiven { ctev_pred = pred, ctev_evtm = EvId new_ev, ctev_loc = loc }) }

newWantedEvVarNC :: CtLoc -> TcPredType -> TcS CtEvidence
-- Don't look up in the solved/inerts; we know it's not there
newWantedEvVarNC loc pty
  = do { new_ev <- wrapTcS $ TcM.newEvVar pty
       ; return (CtWanted { ctev_pred = pty, ctev_evar = new_ev, ctev_loc = loc })}

-- | Variant of newWantedEvVar that has a lower bound on the depth of the result
--   (see Note [Preventing recursive dictionaries])
newWantedEvVarNonrec :: CtLoc -> TcPredType -> TcS MaybeNew
newWantedEvVarNonrec loc pty
  = do { mb_ct <- lookupInInerts pty
       ; case mb_ct of
            Just ctev | not (isDerived ctev) && ctEvCheckDepth (ctLocDepth loc) ctev
                      -> do { traceTcS "newWantedEvVarNonrec/cache hit" $ ppr ctev
                            ; return (Cached (ctEvTerm ctev)) }
            _ -> do { ctev <- newWantedEvVarNC loc pty
                    ; traceTcS "newWantedEvVarNonrec/cache miss" $ ppr ctev
                    ; return (Fresh ctev) } }

newWantedEvVar :: CtLoc -> TcPredType -> TcS MaybeNew
newWantedEvVar loc pty
  = do { mb_ct <- lookupInInerts pty
       ; case mb_ct of
            Just ctev | not (isDerived ctev)
                      -> do { traceTcS "newWantedEvVar/cache hit" $ ppr ctev
                            ; return (Cached (ctEvTerm ctev)) }
            _ -> do { ctev <- newWantedEvVarNC loc pty
                    ; traceTcS "newWantedEvVar/cache miss" $ ppr ctev
                    ; return (Fresh ctev) } }

newDerived :: CtLoc -> TcPredType -> TcS (Maybe CtEvidence)
-- Returns Nothing    if cached,
--         Just pred  if not cached
newDerived loc pty
  = do { mb_ct <- lookupInInerts pty
       ; return (case mb_ct of
                    Just {} -> Nothing
                    Nothing -> Just (CtDerived { ctev_pred = pty, ctev_loc = loc })) }

instDFunConstraints :: CtLoc -> TcThetaType -> TcS [MaybeNew]
instDFunConstraints loc = mapM (newWantedEvVar loc)
\end{code}


Note [xCFlavor]
~~~~~~~~~~~~~~~
A call might look like this:

    xCtFlavor ev subgoal-preds evidence-transformer

  ev is Given   => use ev_decomp to create new Givens for subgoal-preds,
                   and return them

  ev is Wanted  => create new wanteds for subgoal-preds,
                   use ev_comp to bind ev,
                   return fresh wanteds (ie ones not cached in inert_cans or solved)

  ev is Derived => create new deriveds for subgoal-preds
                      (unless cached in inert_cans or solved)

Note: The [CtEvidence] returned is a subset of the subgoal-preds passed in
      Ones that are already cached are not returned

Example
    ev : Tree a b ~ Tree c d
    xCtEvidence ev [a~c, b~d] (XEvTerm { ev_comp = \[c1 c2]. <Tree> c1 c2
                                       , ev_decomp = \c. [nth 1 c, nth 2 c] })
              (\fresh-goals.  stuff)

Note [Bind new Givens immediately]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For Givens we make new EvVars and bind them immediately. We don't worry
about caching, but we don't expect complicated calculations among Givens.
It is important to bind each given:
      class (a~b) => C a b where ....
      f :: C a b => ....
Then in f's Givens we have g:(C a b) and the superclass sc(g,0):a~b.
But that superclass selector can't (yet) appear in a coercion
(see evTermCoercion), so the easy thing is to bind it to an Id.

See Note [Coercion evidence terms] in TcEvidence.

Note [Do not create Given kind equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do not want to create a Given kind equality like

   [G]  kv ~ k   -- kv is a skolem kind variable
                 -- Reason we don't yet support non-Refl kind equalities

This showed up in Trac #8566, where we had a data type
   data I (u :: U *) (r :: [*]) :: * where
        A :: I (AA t as) r                  -- Existential k
so A has type
   A :: forall (u:U *) (r:[*])                  Universal
        (k:BOX) (t:k) (as:[U *]).        Existential
        (u ~ AA * k t as) => I u r

There is no direct kind equality, but in a pattern match where 'u' is
instantiated to, say, (AA * kk (t1:kk) as1), we'd decompose to get
   k ~ kk, t ~ t1, as ~ as1
This is bad.  We "fix" this by simply ignoring the Given kind equality
But the Right Thing is to add kind equalities!

But note (Trac #8705) that we *do* create Given (non-canonical) equalities
with un-equal kinds, e.g.
   [G]  t1::k1 ~ t2::k2   -- k1 and k2 are un-equal kinds
Reason: k1 or k2 might be unification variables that have already been
unified (at this point we have not canonicalised the types), so we want
to emit this t1~t2 as a (non-canonical) Given in the work-list. If k1/k2 
have been unified, we'll find that when we canonicalise it, and the 
t1~t2 information may be crucial (Trac #8705 is an example).

If it turns out that k1 and k2 are really un-equal, then it'll end up
as an Irreducible (see Note [Equalities with incompatible kinds] in
TcCanonical), and will do no harm.

\begin{code}
xCtEvidence :: CtEvidence            -- Original flavor
            -> XEvTerm               -- Instructions about how to manipulate evidence
            -> TcS [CtEvidence]

xCtEvidence (CtGiven { ctev_evtm = tm, ctev_loc = loc })
            (XEvTerm { ev_preds = ptys, ev_decomp = decomp_fn })
  = ASSERT( equalLength ptys (decomp_fn tm) )
    mapM (newGivenEvVar loc)     -- See Note [Bind new Givens immediately]
         (filterOut bad_given_pred (ptys `zip` decomp_fn tm))
  where
    -- See Note [Do not create Given kind equalities]
    bad_given_pred (pred_ty, _)
      | EqPred t1 _ <- classifyPredType pred_ty
      = isKind t1
      | otherwise
      = False

xCtEvidence (CtWanted { ctev_evar = evar, ctev_loc = loc })
            (XEvTerm { ev_preds = ptys, ev_comp = comp_fn })
  = do { new_evars <- mapM (newWantedEvVar loc) ptys
       ; setEvBind evar (comp_fn (getEvTerms new_evars))
       ; return (freshGoals new_evars) }

xCtEvidence (CtDerived { ctev_loc = loc })
            (XEvTerm { ev_preds = ptys })
  = do { ders <- mapM (newDerived loc) ptys
       ; return (catMaybes ders) }

-----------------------------
rewriteEvidence :: CtEvidence   -- old evidence
                -> TcPredType   -- new predicate
                -> TcCoercion   -- Of type :: new predicate ~ <type of old evidence>
                -> TcS (Maybe CtEvidence)
-- Returns Just new_ev iff either (i)  'co' is reflexivity
--                             or (ii) 'co' is not reflexivity, and 'new_pred' not cached
-- In either case, there is nothing new to do with new_ev
{-
     rewriteEvidence old_ev new_pred co
Main purpose: create new evidence for new_pred;
              unless new_pred is cached already
* Returns a new_ev : new_pred, with same wanted/given/derived flag as old_ev
* If old_ev was wanted, create a binding for old_ev, in terms of new_ev
* If old_ev was given, AND not cached, create a binding for new_ev, in terms of old_ev
* Returns Nothing if new_ev is already cached

        Old evidence    New predicate is               Return new evidence
        flavour                                        of same flavor
        -------------------------------------------------------------------
        Wanted          Already solved or in inert     Nothing
        or Derived      Not                            Just new_evidence

        Given           Already in inert               Nothing
                        Not                            Just new_evidence

Note [Rewriting with Refl]
~~~~~~~~~~~~~~~~~~~~~~~~~~
If the coercion is just reflexivity then you may re-use the same
variable.  But be careful!  Although the coercion is Refl, new_pred
may reflect the result of unification alpha := ty, so new_pred might
not _look_ the same as old_pred, and it's vital to proceed from now on
using new_pred.

The flattener preserves type synonyms, so they should appear in new_pred
as well as in old_pred; that is important for good error messages.
 -}


rewriteEvidence (CtDerived { ctev_loc = loc }) new_pred _co
  = -- If derived, don't even look at the coercion.
    -- This is very important, DO NOT re-order the equations for
    -- rewriteEvidence to put the isTcReflCo test first!
    -- Why?  Because for *Derived* constraints, c, the coercion, which
    -- was produced by flattening, may contain suspended calls to
    -- (ctEvTerm c), which fails for Derived constraints.
    -- (Getting this wrong caused Trac #7384.)
    newDerived loc new_pred

rewriteEvidence old_ev new_pred co
  | isTcReflCo co -- See Note [Rewriting with Refl]
  = return (Just (old_ev { ctev_pred = new_pred }))

rewriteEvidence (CtGiven { ctev_evtm = old_tm , ctev_loc = loc }) new_pred co
  = do { new_ev <- newGivenEvVar loc (new_pred, new_tm)  -- See Note [Bind new Givens immediately]
       ; return (Just new_ev) }
  where
    new_tm = mkEvCast old_tm (mkTcSubCo (mkTcSymCo co))  -- mkEvCast optimises ReflCo

rewriteEvidence (CtWanted { ctev_evar = evar, ctev_loc = loc }) new_pred co
  = do { new_evar <- newWantedEvVar loc new_pred
       ; MASSERT( tcCoercionRole co == Nominal )
       ; setEvBind evar (mkEvCast (getEvTerm new_evar) (mkTcSubCo co))
       ; return (freshGoal new_evar) }


rewriteEqEvidence :: CtEvidence         -- Old evidence :: olhs ~ orhs (not swapped)
                                        --              or orhs ~ olhs (swapped)
                  -> SwapFlag
                  -> TcType -> TcType   -- New predicate  nlhs ~ nrhs
                                        -- Should be zonked, because we use typeKind on nlhs/nrhs
                  -> TcCoercion         -- lhs_co, of type :: nlhs ~ olhs
                  -> TcCoercion         -- rhs_co, of type :: nrhs ~ orhs
                  -> TcS (Maybe CtEvidence)  -- Of type nlhs ~ nrhs
-- For (rewriteEqEvidence (Given g olhs orhs) False nlhs nrhs lhs_co rhs_co)
-- we generate
-- If not swapped
--      g1 : nlhs ~ nrhs = lhs_co ; g ; sym rhs_co
-- If 'swapped'
--      g1 : nlhs ~ nrhs = lhs_co ; Sym g ; sym rhs_co
--
-- For (Wanted w) we do the dual thing.
-- New  w1 : nlhs ~ nrhs
-- If not swapped
--      w : olhs ~ orhs = sym lhs_co ; w1 ; rhs_co
-- If swapped
--      w : orhs ~ olhs = sym rhs_co ; sym w1 ; lhs_co
--
-- It's all a form of rewwriteEvidence, specialised for equalities
rewriteEqEvidence old_ev swapped nlhs nrhs lhs_co rhs_co
  | CtDerived { ctev_loc = loc } <- old_ev
  = newDerived loc (mkEqPred nlhs nrhs)

  | NotSwapped <- swapped
  , isTcReflCo lhs_co      -- See Note [Rewriting with Refl]
  , isTcReflCo rhs_co
  = return (Just (old_ev { ctev_pred = new_pred }))

  | CtGiven { ctev_evtm = old_tm , ctev_loc = loc } <- old_ev
  = do { let new_tm = EvCoercion (lhs_co 
                                  `mkTcTransCo` maybeSym swapped (evTermCoercion old_tm)
                                  `mkTcTransCo` mkTcSymCo rhs_co)
       ; new_ev <- newGivenEvVar loc (new_pred, new_tm)  -- See Note [Bind new Givens immediately]
       ; return (Just new_ev) }

  | CtWanted { ctev_evar = evar, ctev_loc = loc } <- old_ev
  = do { new_evar <- newWantedEvVar loc new_pred
       ; let co = maybeSym swapped $
                  mkTcSymCo lhs_co
                  `mkTcTransCo` evTermCoercion (getEvTerm new_evar)
                  `mkTcTransCo` rhs_co
       ; setEvBind evar (EvCoercion co)
       ; traceTcS "rewriteEqEvidence" (vcat [ppr old_ev, ppr nlhs, ppr nrhs, ppr co])
       ; return (freshGoal new_evar) }

  | otherwise
  = panic "rewriteEvidence"
  where
    new_pred = mkEqPred nlhs nrhs

maybeSym :: SwapFlag -> TcCoercion -> TcCoercion 
maybeSym IsSwapped  co = mkTcSymCo co
maybeSym NotSwapped co = co


matchOpenFam :: TyCon -> [Type] -> TcS (Maybe FamInstMatch)
matchOpenFam tycon args = wrapTcS $ tcLookupFamInst tycon args

matchFam :: TyCon -> [Type] -> TcS (Maybe (TcCoercion, TcType))
-- Given (F tys) return (ty, co), where co :: F tys ~ ty
matchFam tycon args
  | isOpenSynFamilyTyCon tycon
  = do { maybe_match <- matchOpenFam tycon args
       ; case maybe_match of
           Nothing -> return Nothing
           Just (FamInstMatch { fim_instance = famInst
                              , fim_tys      = inst_tys })
             -> let co = mkTcUnbranchedAxInstCo Nominal (famInstAxiom famInst) inst_tys
                    ty = pSnd $ tcCoercionKind co
                in return $ Just (co, ty) }

  | Just ax <- isClosedSynFamilyTyCon_maybe tycon
  , Just (ind, inst_tys) <- chooseBranch ax args
  = let co = mkTcAxInstCo Nominal ax ind inst_tys
        ty = pSnd (tcCoercionKind co)
    in return $ Just (co, ty)

  | Just ops <- isBuiltInSynFamTyCon_maybe tycon =
    return $ do (r,ts,ty) <- sfMatchFam ops args
                return (mkTcAxiomRuleCo r ts [], ty)

  | otherwise
  = return Nothing

\end{code}

\begin{code}
-- Deferring forall equalities as implications
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

deferTcSForAllEq :: Role -- Nominal or Representational
                 -> CtLoc  -- Original wanted equality flavor
                 -> ([TyVar],TcType)   -- ForAll tvs1 body1
                 -> ([TyVar],TcType)   -- ForAll tvs2 body2
                 -> TcS EvTerm
-- Some of this functionality is repeated from TcUnify,
-- consider having a single place where we create fresh implications.
deferTcSForAllEq role loc (tvs1,body1) (tvs2,body2)
 = do { (subst1, skol_tvs) <- wrapTcS $ TcM.tcInstSkolTyVars tvs1
      ; let tys  = mkTyVarTys skol_tvs
            phi1 = Type.substTy subst1 body1
            phi2 = Type.substTy (zipTopTvSubst tvs2 tys) body2
            skol_info = UnifyForAllSkol skol_tvs phi1
        ; mev <- newWantedEvVar loc $ case role of
                Nominal ->          mkTcEqPred      phi1 phi2
                Representational -> mkCoerciblePred phi1 phi2
                Phantom ->          panic "deferTcSForAllEq Phantom"
        ; coe_inside <- case mev of
            Cached ev_tm -> return (evTermCoercion ev_tm)
            Fresh ctev   -> do { ev_binds_var <- wrapTcS $ TcM.newTcEvBinds
                               ; env <- wrapTcS $ TcM.getLclEnv
                               ; let ev_binds = TcEvBinds ev_binds_var
                                     new_ct = mkNonCanonical ctev
                                     new_co = evTermCoercion (ctEvTerm ctev)
                                     new_untch = pushUntouchables (tcl_untch env)
                               ; let wc = WC { wc_flat  = singleCt new_ct
                                             , wc_impl  = emptyBag
                                             , wc_insol = emptyCts }
                                     imp = Implic { ic_untch  = new_untch
                                                  , ic_skols  = skol_tvs
                                                  , ic_fsks   = []
                                                  , ic_no_eqs = True
                                                  , ic_given  = []
                                                  , ic_wanted = wc
                                                  , ic_insol  = False
                                                  , ic_binds  = ev_binds_var
                                                  , ic_env    = env
                                                  , ic_info   = skol_info }
                               ; updTcSImplics (consBag imp)
                               ; return (TcLetCo ev_binds new_co) }

        ; return $ EvCoercion (foldr mkTcForAllCo coe_inside skol_tvs)
        }
\end{code}

Interaction with an External SMT Solver
---------------------------------------

\begin{code}
extSolSend  :: Ct -> TcS Bool
extSolSend ct = withExtSol (\s -> TN.extSolSend s ct)

extSolPush  :: TcS ()
extSolPush = withExtSol TN.extSolPop

extSolPop   :: TcS ()
extSolPop = withExtSol TN.extSolPop

extSolCheck :: TcS TN.ExtSolRes
extSolCheck = withExtSol TN.extSolCheck

withExtSol :: (TN.ExternalSolver -> IO a) -> TcS a
withExtSol m = TcS (liftIO . m . tcs_ext_solver)



\end{code}
