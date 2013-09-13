\begin{code}
{-# OPTIONS -fno-warn-tabs -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

-- Type definitions for the constraint solver
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

    canRewrite, canSolve,
    mkGivenLoc, 

    TcS, runTcS, runTcSWithEvBinds, failTcS, panicTcS, traceTcS, -- Basic functionality 
    traceFireTcS, bumpStepCountTcS, 
    tryTcS, nestTcS, nestImplicTcS, recoverTcS,
    wrapErrTcS, wrapWarnTcS,

    -- Getting and setting the flattening cache
    addSolvedDict, addSolvedFunEq, getFlattenSkols,
    
    deferTcSForAllEq, 
    
    setEvBind,
    XEvTerm(..),
    MaybeNew (..), isFresh, freshGoals, getEvTerms,

    xCtFlavor,        -- Transform a CtEvidence during a step 
    rewriteCtFlavor,  -- Specialized version of xCtFlavor for coercions
    newWantedEvVar, newWantedEvVarNC, instDFunConstraints,
    newDerived,
    
       -- Creation of evidence variables
    setWantedTyBind,

    getInstEnvs, getFamInstEnvs,                -- Getting the environments
    getTopEnv, getGblEnv, getTcEvBinds, getUntouchables,
    getTcEvBindsMap, getTcSTyBinds, getTcSTyBindsMap,


    lookupFlatEqn, newFlattenSkolem,            -- Flatten skolems 

        -- Deque
    Deque(..), insertDeque, emptyDeque,

        -- Inerts 
    InertSet(..), InertCans(..), 
    getInertEqs, 
    emptyInert, getTcSInerts, lookupInInerts, 
    getInertUnsolved, checkAllSolved, 
    prepareInertsForImplications,
    modifyInertTcS,
    insertInertItemTcS, partitionCCanMap, partitionEqMap,
    getRelevantCts, extractRelevantInerts,
    getInertsFunEqTyCon,
    CCanMap(..), CtTypeMap, CtFamHeadMap, CtPredMap,
    PredMap, FamHeadMap,
    partCtFamHeadMap, lookupFamHead, lookupSolvedDict,
    filterSolved,

    instDFunType,                              -- Instantiation
    newFlexiTcSTy, instFlexiTcS, instFlexiTcSHelperTcS,
    cloneMetaTyVar,

    Untouchables, isTouchableMetaTyVarTcS, isFilledMetaTyVar_maybe,
    zonkTyVarsAndFV,

    getDefaultInfo, getDynFlags,

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

import TcEvidence
import Class
import TyCon

import Name
import Var
import VarEnv
import Outputable
import Bag
import MonadUtils

import FastString
import Util
import Id 
import TcRnTypes

import Unique 
import UniqFM
import Maybes ( orElse, catMaybes, firstJust )
import Pair ( pSnd )

import Control.Monad( unless, when, zipWithM )
import Data.IORef
import TrieMap

#ifdef DEBUG
import StaticFlags( opt_PprStyle_Debug )
import VarSet
import Digraph
#endif
\end{code}

%************************************************************************
%*									*
%*                            Worklists                                *
%*  Canonical and non-canonical constraints that the simplifier has to  *
%*  work on. Including their simplification depths.                     *
%*                                                                      *
%*									*
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
  | Just {} <- isCFunEqCan_Maybe ct
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


-- Canonical constraint maps
data CCanMap a 
  = CCanMap { cts_given   :: UniqFM Cts   -- All Given
            , cts_derived :: UniqFM Cts   -- All Derived
            , cts_wanted  :: UniqFM Cts } -- All Wanted

keepGivenCMap :: CCanMap a -> CCanMap a
keepGivenCMap cc = emptyCCanMap { cts_given = cts_given cc }

instance Outputable (CCanMap a) where
  ppr (CCanMap given derived wanted) = ptext (sLit "CCanMap") <+> (ppr given) <+> (ppr derived) <+> (ppr wanted)

cCanMapToBag :: CCanMap a -> Cts 
cCanMapToBag cmap = foldUFM unionBags rest_wder (cts_given cmap)
  where rest_wder = foldUFM unionBags rest_der  (cts_wanted cmap) 
        rest_der  = foldUFM unionBags emptyCts  (cts_derived cmap)

emptyCCanMap :: CCanMap a 
emptyCCanMap = CCanMap { cts_given = emptyUFM, cts_derived = emptyUFM, cts_wanted = emptyUFM } 

updCCanMap:: Uniquable a => (a,Ct) -> CCanMap a -> CCanMap a 
updCCanMap (a,ct) cmap 
  = case cc_ev ct of 
      CtWanted {}  -> cmap { cts_wanted  = insert_into (cts_wanted cmap)  } 
      CtGiven {}   -> cmap { cts_given   = insert_into (cts_given cmap)   }
      CtDerived {} -> cmap { cts_derived = insert_into (cts_derived cmap) }
  where 
    insert_into m = addToUFM_C unionBags m a (singleCt ct)

getRelevantCts :: Uniquable a => a -> CCanMap a -> (Cts, CCanMap a) 
-- Gets the relevant constraints and returns the rest of the CCanMap
getRelevantCts a cmap 
    = let relevant = lookup (cts_wanted cmap) `unionBags`
                     lookup (cts_given cmap)  `unionBags`
                     lookup (cts_derived cmap) 
          residual_map = cmap { cts_wanted  = delFromUFM (cts_wanted cmap) a
                              , cts_given   = delFromUFM (cts_given cmap) a
                              , cts_derived = delFromUFM (cts_derived cmap) a }
      in (relevant, residual_map) 
  where
    lookup map = lookupUFM map a `orElse` emptyCts

lookupCCanMap :: Uniquable a => a -> (CtEvidence -> Bool) -> CCanMap a -> Maybe CtEvidence
lookupCCanMap a pick_me map
  = findEvidence pick_me possible_cts
  where
     possible_cts = lookupUFM (cts_given map)   a `plus` (
                    lookupUFM (cts_wanted map)  a `plus` (
                    lookupUFM (cts_derived map) a `plus` emptyCts))

     plus Nothing     cts2 = cts2
     plus (Just cts1) cts2 = cts1 `unionBags` cts2

findEvidence :: (CtEvidence -> Bool) -> Cts -> Maybe CtEvidence
findEvidence pick_me cts
  = foldrBag pick Nothing cts
  where
     pick :: Ct -> Maybe CtEvidence -> Maybe CtEvidence
     pick ct deflt | let ctev = cc_ev ct, pick_me ctev = Just ctev
                   | otherwise                             = deflt

partitionCCanMap :: (Ct -> Bool) -> CCanMap a -> (Cts,CCanMap a) 
-- All constraints that /match/ the predicate go in the bag, the rest remain in the map
partitionCCanMap pred cmap
  = let (ws_map,ws) = foldUFM_Directly aux (emptyUFM,emptyCts) (cts_wanted cmap) 
        (ds_map,ds) = foldUFM_Directly aux (emptyUFM,emptyCts) (cts_derived cmap)
        (gs_map,gs) = foldUFM_Directly aux (emptyUFM,emptyCts) (cts_given cmap) 
    in (ws `andCts` ds `andCts` gs, cmap { cts_wanted  = ws_map
                                         , cts_given   = gs_map
                                         , cts_derived = ds_map }) 
  where aux k this_cts (mp,acc_cts) = (new_mp, new_acc_cts)
                                    where new_mp      = addToUFM mp k cts_keep
                                          new_acc_cts = acc_cts `andCts` cts_out
                                          (cts_out, cts_keep) = partitionBag pred this_cts

partitionEqMap :: (Ct -> Bool) -> TyVarEnv (Ct,TcCoercion) -> ([Ct], TyVarEnv (Ct,TcCoercion))
partitionEqMap pred isubst 
  = let eqs_out = foldVarEnv extend_if_pred [] isubst
        eqs_in  = filterVarEnv_Directly (\_ (ct,_) -> not (pred ct)) isubst
    in (eqs_out, eqs_in)
  where extend_if_pred (ct,_) cts = if pred ct then ct : cts else cts

extractUnsolvedCMap :: CCanMap a -> Cts
-- Gets the wanted or derived constraints
extractUnsolvedCMap cmap = foldUFM unionBags emptyCts (cts_wanted cmap)
              `unionBags`  foldUFM unionBags emptyCts (cts_derived cmap)

-- Maps from PredTypes to Constraints
type CtTypeMap    = TypeMap    Ct
type CtPredMap    = PredMap    Ct
type CtFamHeadMap = FamHeadMap Ct

newtype PredMap    a = PredMap    { unPredMap    :: TypeMap a } -- Indexed by TcPredType
newtype FamHeadMap a = FamHeadMap { unFamHeadMap :: TypeMap a } -- Indexed by family head

instance Outputable a => Outputable (PredMap a) where
   ppr (PredMap m) = ppr (foldTM (:) m [])

instance Outputable a => Outputable (FamHeadMap a) where
   ppr (FamHeadMap m) = ppr (foldTM (:) m [])

sizePredMap :: PredMap a -> Int
sizePredMap (PredMap m) = foldTypeMap (\_ x -> x+1) 0 m

emptyFamHeadMap :: FamHeadMap a
emptyFamHeadMap = FamHeadMap emptyTM

sizeFamHeadMap :: FamHeadMap a -> Int
sizeFamHeadMap (FamHeadMap m) = foldTypeMap (\_ x -> x+1) 0 m

ctTypeMapCts :: TypeMap Ct -> Cts
ctTypeMapCts ctmap = foldTM (\ct cts -> extendCts cts ct) ctmap emptyCts

lookupFamHead :: FamHeadMap a -> TcType -> Maybe a
lookupFamHead (FamHeadMap m) key = lookupTM key m

insertFamHead :: FamHeadMap a -> TcType -> a -> FamHeadMap a
insertFamHead (FamHeadMap m) key value = FamHeadMap (alterTM key (const (Just value)) m)

delFamHead :: FamHeadMap a -> TcType -> FamHeadMap a
delFamHead (FamHeadMap m) key = FamHeadMap (alterTM key (const Nothing) m)

anyFamHeadMap :: (Ct -> Bool) -> CtFamHeadMap -> Bool
anyFamHeadMap f ctmap = foldTM ((||) . f) (unFamHeadMap ctmap) False

partCtFamHeadMap :: (Ct -> Bool) 
                 -> CtFamHeadMap 
                 -> (Cts, CtFamHeadMap)
partCtFamHeadMap f ctmap
  = let (cts,tymap_final) = foldTM upd_acc tymap_inside (emptyBag, tymap_inside)
    in (cts, FamHeadMap tymap_final)
  where
    tymap_inside = unFamHeadMap ctmap 
    upd_acc ct (cts,acc_map)
         | f ct      = (extendCts cts ct, alterTM ct_key (\_ -> Nothing) acc_map)
         | otherwise = (cts,acc_map)
         where ct_key | EqPred ty1 _ <- classifyPredType (ctPred ct)
                      = ty1 
                      | otherwise 
                      = panic "partCtFamHeadMap, encountered non equality!"

filterSolved :: (CtEvidence -> Bool) -> PredMap CtEvidence -> PredMap CtEvidence
filterSolved p (PredMap mp) = PredMap (foldTM upd mp emptyTM)
  where upd a m = if p a then alterTM (ctEvPred a) (\_ -> Just a) m
                         else m
\end{code}

%************************************************************************
%*									*
%*                            Inert Sets                                *
%*                                                                      *
%*									*
%************************************************************************

Note [Detailed InertCans Invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The InertCans represents a collection of constraints with the following properties:
  1 All canonical
  2 All Given or Wanted or Derived. No (partially) Solved
  3 No two dictionaries with the same head
  4 No two family equations with the same head 
      NB: This is enforced by construction since we use a CtFamHeadMap for inert_funeqs
  5 Family equations inert wrt top-level family axioms
  6 Dictionaries have no matching top-level instance 
  
  7 Non-equality constraints are fully rewritten with respect to the equalities (CTyEqCan)

  8 Equalities _do_not_ form an idempotent substitution, but they are
    guaranteed to not have any occurs errors. Additional notes: 

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

  9 Given family or dictionary constraints don't mention touchable unification variables

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
  = IC { inert_eqs :: TyVarEnv Ct
              -- Must all be CTyEqCans! If an entry exists of the form: 
              --   a |-> ct,co
              -- Then ct = CTyEqCan { cc_tyvar = a, cc_rhs = xi } 
              -- And  co : a ~ xi
       , inert_dicts :: CCanMap Class
              -- Dictionaries only, index is the class
              -- NB: index is /not/ the whole type because FD reactions 
              -- need to match the class but not necessarily the whole type.
       , inert_funeqs :: CtFamHeadMap
              -- Family equations, index is the whole family head type.
       , inert_irreds :: Cts       
              -- Irreducible predicates

       , inert_insols :: Cts       
              -- Frozen errors (as non-canonicals)
       }
    
                     
-- The Inert Set
data InertSet
  = IS { inert_cans :: InertCans
              -- Canonical Given, Wanted, Derived (no Solved)
	      -- Sometimes called "the inert set"

       , inert_flat_cache :: FamHeadMap (CtEvidence, TcType)
              -- See Note [Type family equations]
              -- Just a hash-cons cache for use when flattening only
              -- These include entirely un-processed goals, so don't use
              -- them to solve a top-level goal, else you may end up solving
              -- (w:F ty ~ a) by setting w:=w!  We just use the flat-cache
              -- when allocating a new flatten-skolem.
              -- Not necessarily inert wrt top-level equations (or inert_cans)
 
       , inert_fsks :: [TcTyVar]  -- Rigid flatten-skolems (arising from givens)
                                  -- allocated in this local scope

       , inert_solved_funeqs :: FamHeadMap (CtEvidence, TcType)
              -- See Note [Type family equations]
              -- Of form co :: F xis ~ xi 
              -- Always the result of using a top-level family axiom F xis ~ tau
              -- No Deriveds 
              -- Not necessarily fully rewritten (by type substitutions)

       , inert_solved_dicts   :: PredMap CtEvidence 
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
                   <+> vcat (map ppr (Bag.bagToList $ 
                                  ctTypeMapCts (unFamHeadMap $ inert_funeqs ics)))
                 , ptext (sLit "Dictionaries:")
                   <+> vcat (map ppr (Bag.bagToList $ cCanMapToBag (inert_dicts ics)))
                 , ptext (sLit "Irreds:")
                   <+> vcat (map ppr (Bag.bagToList $ inert_irreds ics))
                 , text "Insolubles =" <+> -- Clearly print frozen errors
                    braces (vcat (map ppr (Bag.bagToList $ inert_insols ics)))
                 ]
            
instance Outputable InertSet where 
  ppr is = vcat [ ppr $ inert_cans is
                , text "Solved dicts"  <+> int (sizePredMap (inert_solved_dicts is))
                , text "Solved funeqs" <+> int (sizeFamHeadMap (inert_solved_funeqs is))]

emptyInert :: InertSet
emptyInert
  = IS { inert_cans = IC { inert_eqs    = emptyVarEnv
                         , inert_dicts  = emptyCCanMap
                         , inert_funeqs = emptyFamHeadMap
                         , inert_irreds = emptyCts
                         , inert_insols = emptyCts }
       , inert_fsks          = []
       , inert_flat_cache    = emptyFamHeadMap
       , inert_solved_dicts  = PredMap emptyTM 
       , inert_solved_funeqs = emptyFamHeadMap }

insertInertItem :: Ct -> InertSet -> InertSet 
-- Add a new inert element to the inert set. 
insertInertItem item is
  = -- A canonical Given, Wanted, or Derived
    is { inert_cans = upd_inert_cans (inert_cans is) item }
  
  where upd_inert_cans :: InertCans -> Ct -> InertCans
        -- Precondition: item /is/ canonical
        upd_inert_cans ics item
          | isCTyEqCan item                     
          = let upd_err a b = pprPanic "insertInertItem" $
                              vcat [ text "Multiple inert equalities:"
                                   , text "Old (already inert):" <+> ppr a
                                   , text "Trying to insert   :" <+> ppr b ]
        
                eqs'     = extendVarEnv_C upd_err (inert_eqs ics) 
                                                  (cc_tyvar item) item        

            in ics { inert_eqs = eqs' }

          | isCIrredEvCan item                  -- Presently-irreducible evidence
          = ics { inert_irreds = inert_irreds ics `Bag.snocBag` item }

          | Just cls <- isCDictCan_Maybe item   -- Dictionary 
          = ics { inert_dicts = updCCanMap (cls,item) (inert_dicts ics) }

          | Just _tc <- isCFunEqCan_Maybe item  -- Function equality
          = let fam_head = mkTyConApp (cc_fun item) (cc_tyargs item)
                upd_funeqs Nothing = Just item
                upd_funeqs (Just _already_there) 
                  = panic "insertInertItem: item already there!"
            in ics { inert_funeqs = FamHeadMap 
                                      (alterTM fam_head upd_funeqs $ 
                                         (unFamHeadMap $ inert_funeqs ics)) }
          | otherwise
          = pprPanic "upd_inert set: can't happen! Inserting " $ 
            ppr item   -- Can't be CNonCanonical, CHoleCan, 
                       -- because they only land in inert_insols


insertInertItemTcS :: Ct -> TcS ()
-- Add a new item in the inerts of the monad
insertInertItemTcS item
  = do { traceTcS "insertInertItemTcS {" $ 
         text "Trying to insert new inert item:" <+> ppr item

       ; updInertTcS (insertInertItem item) 
                        
       ; traceTcS "insertInertItemTcS }" $ empty }

addSolvedDict :: CtEvidence -> TcS ()
-- Add a new item in the solved set of the monad
addSolvedDict item
  | isIPPred (ctEvPred item)    -- Never cache "solved" implicit parameters (not sure why!)
  = return () 
  | otherwise
  = do { traceTcS "updSolvedSetTcs:" $ ppr item
       ; updInertTcS upd_solved_dicts }
  where
    upd_solved_dicts is 
      = is { inert_solved_dicts = PredMap $ alterTM pred upd_solved $ 
                                  unPredMap $ inert_solved_dicts is }
    pred = ctEvPred item
    upd_solved _ = Just item

addSolvedFunEq :: TcType -> CtEvidence -> TcType -> TcS ()
addSolvedFunEq fam_ty ev rhs_ty
  = updInertTcS $ \ inert -> 
    inert { inert_solved_funeqs = insertFamHead (inert_solved_funeqs inert) 
                                                fam_ty (ev, rhs_ty) }

modifyInertTcS :: (InertSet -> (a,InertSet)) -> TcS a 
-- Modify the inert set with the supplied function
modifyInertTcS upd 
  = do { is_var <- getTcSInertsRef
       ; curr_inert <- wrapTcS (TcM.readTcRef is_var)
       ; let (a, new_inert) = upd curr_inert
       ; wrapTcS (TcM.writeTcRef is_var new_inert)
       ; return a }

updInertTcS :: (InertSet -> InertSet) -> TcS () 
-- Modify the inert set with the supplied function
updInertTcS upd 
  = do { is_var <- getTcSInertsRef
       ; curr_inert <- wrapTcS (TcM.readTcRef is_var)
       ; let new_inert = upd curr_inert
       ; wrapTcS (TcM.writeTcRef is_var new_inert) }

prepareInertsForImplications :: InertSet -> InertSet
-- See Note [Preparing inert set for implications]
prepareInertsForImplications is
  = is { inert_cans   = getGivens (inert_cans is)
       , inert_fsks   = []
       , inert_flat_cache = emptyFamHeadMap }
  where
    getGivens (IC { inert_eqs    = eqs
                  , inert_irreds = irreds
                  , inert_funeqs = FamHeadMap funeqs
                  , inert_dicts  = dicts })
      = IC { inert_eqs    = filterVarEnv_Directly (\_ ct -> isGivenCt ct) eqs 
           , inert_funeqs = FamHeadMap (mapTM given_from_wanted funeqs)
           , inert_irreds = Bag.filterBag isGivenCt irreds
           , inert_dicts  = keepGivenCMap dicts
           , inert_insols = emptyCts }

    given_from_wanted funeq   -- This is where the magic processing happens 
      | isGiven ev = funeq    -- for type-function equalities
                              -- See Note [Preparing inert set for implications]
      | otherwise  = funeq { cc_ev = given_ev }
      where
        ev = ctEvidence funeq
        given_ev = CtGiven { ctev_evtm = EvId (ctev_evar ev)
                           , ctev_pred = ctev_pred ev }
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


\begin{code}
getInertEqs :: TcS (TyVarEnv Ct)
getInertEqs = do { inert <- getTcSInerts
                 ; return (inert_eqs (inert_cans inert)) }

getInertUnsolved :: TcS (Cts, Cts)
-- Return (unsolved-wanteds, insolubles)
-- Both consist of a mixture of Wanted and Derived
getInertUnsolved
 = do { is <- getTcSInerts

      ; let icans = inert_cans is
            unsolved_irreds = Bag.filterBag is_unsolved (inert_irreds icans)
            unsolved_dicts  = extractUnsolvedCMap (inert_dicts icans)
            (unsolved_funeqs,_) = partCtFamHeadMap is_unsolved (inert_funeqs icans)
            unsolved_eqs = foldVarEnv add_if_unsolved emptyCts (inert_eqs icans)

            unsolved_flats = unsolved_eqs `unionBags` unsolved_irreds `unionBags` 
                             unsolved_dicts `unionBags` unsolved_funeqs

      ; return (unsolved_flats, inert_insols icans) }
  where
    add_if_unsolved ct cts
      | is_unsolved ct = cts `extendCts` ct
      | otherwise      = cts

    is_unsolved ct = not (isGivenCt ct)   -- Wanted or Derived

checkAllSolved :: TcS Bool
-- True if there are no unsolved wanteds
-- Ignore Derived for this purpose, unless in insolubles
checkAllSolved
 = do { is <- getTcSInerts

      ; let icans = inert_cans is
            unsolved_irreds = Bag.anyBag isWantedCt (inert_irreds icans)
            unsolved_dicts  = not (isNullUFM (cts_wanted (inert_dicts icans)))
            unsolved_funeqs = anyFamHeadMap isWantedCt (inert_funeqs icans)
            unsolved_eqs    = foldVarEnv ((||) . isWantedCt) False (inert_eqs icans)

      ; return (not (unsolved_eqs || unsolved_irreds
                     || unsolved_dicts || unsolved_funeqs
                     || not (isEmptyBag (inert_insols icans)))) }


{- Get inert function equation constraints that have the given tycon
in their head.  Not that the constraints remain in the inert set.
We use this to check for derived interactions with built-in type-function
constructors. -}
getInertsFunEqTyCon :: TyCon -> TcS [Ct]
getInertsFunEqTyCon tc =
  do is <- getTcSInerts
     let mp = unFamHeadMap $ inert_funeqs $ inert_cans is
     return $ lookupTypeMapTyCon mp tc


extractRelevantInerts :: Ct -> TcS Cts
-- Returns the constraints from the inert set that are 'relevant' to react with 
-- this constraint. The monad is left with the 'thinner' inerts. 
-- NB: This function contains logic specific to the constraint solver, maybe move there?
extractRelevantInerts wi 
  = modifyInertTcS (extract_relevants wi)
  where 
        extract_relevants :: Ct -> InertSet -> (Cts,InertSet)
        extract_relevants wi is 
          = let (cts,ics') = extract_ics_relevants wi (inert_cans is)
            in (cts, is { inert_cans = ics' }) 
            
        extract_ics_relevants :: Ct -> InertCans -> (Cts, InertCans)
        extract_ics_relevants (CDictCan {cc_class = cl}) ics = 
            let (cts,dict_map) = getRelevantCts cl (inert_dicts ics) 
            in (cts, ics { inert_dicts = dict_map })

        extract_ics_relevants ct@(CFunEqCan {}) ics@(IC { inert_funeqs = funeq_map })
            | Just ct <- lookupFamHead funeq_map fam_head
            = (singleCt ct, ics { inert_funeqs = delFamHead funeq_map fam_head })
            | otherwise
            = (emptyCts, ics)
            where
              fam_head = mkTyConApp (cc_fun ct) (cc_tyargs ct)

        extract_ics_relevants (CHoleCan {}) ics
            = pprPanic "extractRelevantInerts" (ppr wi)
              -- Holes are put straight into inert_frozen, so never get here

        extract_ics_relevants (CIrredEvCan { }) ics = 
            let cts = inert_irreds ics 
            in (cts, ics { inert_irreds = emptyCts })

        extract_ics_relevants _ ics = (emptyCts,ics)
        

lookupFlatEqn :: TcType -> TcS (Maybe (CtEvidence, TcType))
lookupFlatEqn fam_ty 
  = do { IS { inert_solved_funeqs = solved_funeqs
            , inert_flat_cache = flat_cache
            , inert_cans = IC { inert_funeqs = inert_funeqs } } <- getTcSInerts
       ; return (lookupFamHead solved_funeqs fam_ty `firstJust` 
                 lookup_in_inerts inert_funeqs    `firstJust`
                 lookupFamHead flat_cache fam_ty) }
  where
    lookup_in_inerts inert_funeqs 
        = case lookupFamHead inert_funeqs fam_ty of
            Nothing -> Nothing
            Just ct -> Just (ctEvidence ct, cc_rhs ct)

lookupInInerts :: TcPredType -> TcS (Maybe CtEvidence)
-- Is this exact predicate type cached in the solved or canonicals of the InertSet
lookupInInerts pty
  = do { inerts <- getTcSInerts
       ; case lookupSolvedDict inerts pty of
           Just ctev -> return (Just ctev)
           Nothing   -> return (lookupInInertCans inerts pty) }

lookupSolvedDict :: InertSet -> TcPredType -> Maybe CtEvidence
-- Returns just if exactly this predicate type exists in the solved.
lookupSolvedDict (IS { inert_solved_dicts = solved }) pty 
  = lookupTM pty (unPredMap solved)

lookupInInertCans :: InertSet -> TcPredType -> Maybe CtEvidence
-- Returns Just if exactly this pred type exists in the inert canonicals
lookupInInertCans (IS { inert_cans = ics }) pty
  = case (classifyPredType pty) of
      ClassPred cls _ 
         -> lookupCCanMap cls (\ct -> ctEvPred ct `eqType` pty) (inert_dicts ics)

      EqPred ty1 _ty2 
         | Just tv <- getTyVar_maybe ty1      -- Tyvar equation
         , Just ct <- lookupVarEnv (inert_eqs ics) tv
      	 , let ctev = ctEvidence ct
      	 , ctEvPred ctev `eqType` pty
      	 -> Just ctev

      	 | Just _ <- splitTyConApp_maybe ty1  -- Family equation
      	 , Just ct <- lookupTM ty1 (unFamHeadMap $ inert_funeqs ics)
      	 , let ctev = ctEvidence ct
      	 , ctEvPred ctev `eqType` pty
      	 -> Just ctev

      IrredPred {} -> findEvidence (\ct -> ctEvPred ct `eqType` pty) (inert_irreds ics)
    
      _other -> Nothing -- NB: No caching for IPs or holes
\end{code}




%************************************************************************
%*									*
%*		The TcS solver monad                                    *
%*									*
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
      
      tcs_ty_binds :: IORef (TyVarEnv (TcTyVar, TcType)),
          -- Global type bindings
                     
      tcs_count      :: IORef Int, -- Global step count

      tcs_inerts   :: IORef InertSet, -- Current inert set
      tcs_worklist :: IORef WorkList, -- Current worklist
      
      -- Residual implication constraints that are generated 
      -- while solving or canonicalising the current worklist.
      -- Specifically, when canonicalising (forall a. t1 ~ forall a. t2)
      -- from which we get the implication (forall a. t1 ~ t2)
      tcs_implics  :: IORef (Bag Implication)
    }
\end{code}

\begin{code}

---------------
newtype TcS a = TcS { unTcS :: TcSEnv -> TcM a } 

instance Functor TcS where
  fmap f m = TcS $ fmap f . unTcS m

instance Monad TcS where 
  return x  = TcS (\_ -> return x) 
  fail err  = TcS (\_ -> fail err) 
  m >>= k   = TcS (\ebs -> unTcS m ebs >>= \r -> unTcS (k r) ebs)

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

bumpStepCountTcS :: TcS ()
bumpStepCountTcS = TcS $ \env -> do { let ref = tcs_count env
                                    ; n <- TcM.readTcRef ref
                                    ; TcM.writeTcRef ref (n+1) }

traceFireTcS :: Ct -> SDoc -> TcS ()
-- Dump a rule-firing trace
traceFireTcS ct doc 
  = TcS $ \env -> 
    TcM.whenDOptM Opt_D_dump_cs_trace $ 
    do { n <- TcM.readTcRef (tcs_count env)
       ; let msg = int n <> brackets (int (ctLocDepth (cc_loc ct))) <+> doc
       ; TcM.dumpTcRn msg }

runTcS :: TcS a		       -- What to run
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
  = do { ty_binds_var <- TcM.newTcRef emptyVarEnv
       ; step_count <- TcM.newTcRef 0
       ; inert_var <- TcM.newTcRef is 

       ; let env = TcSEnv { tcs_ev_binds = ev_binds_var
                          , tcs_ty_binds = ty_binds_var
			  , tcs_count    = step_count
                          , tcs_inerts   = inert_var
                          , tcs_worklist    = panic "runTcS: worklist"
                          , tcs_implics     = panic "runTcS: implics" }
                               -- NB: Both these are initialised by withWorkList

	     -- Run the computation
       ; res <- unTcS tcs env
	     -- Perform the type unifications required
       ; ty_binds <- TcM.readTcRef ty_binds_var
       ; mapM_ do_unification (varEnvElts ty_binds)

#ifdef DEBUG
       ; count <- TcM.readTcRef step_count
       ; when (opt_PprStyle_Debug && count > 0) $
         TcM.debugDumpTcRn (ptext (sLit "Constraint solver steps =") <+> int count )

       ; ev_binds <- TcM.getTcEvBinds ev_binds_var
       ; checkForCyclicBinds ev_binds
#endif

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
                   , tcs_count = count } -> 
    do { new_inert_var <- TcM.newTcRef inerts
       ; let nest_env = TcSEnv { tcs_ev_binds    = ref
                               , tcs_ty_binds    = ty_binds
                               , tcs_count       = count
                               , tcs_inerts      = new_inert_var
                               , tcs_worklist    = panic "nextImplicTcS: worklist"
                               , tcs_implics     = panic "nextImplicTcS: implics"
                               -- NB: Both these are initialised by withWorkList
                               }
       ; res <- TcM.setUntouchables inner_untch $
                thing_inside nest_env
                
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
-- Completely afresh inerts and worklist, be careful! 
-- Moreover, we will simply throw away all the evidence generated. 
tryTcS (TcS thing_inside)
  = TcS $ \env -> 
    do { is_var <- TcM.newTcRef emptyInert
       ; ty_binds_var <- TcM.newTcRef emptyVarEnv
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
        already_there = not (isWantedCt ct) && anyBag (eqType this_pred . ctPred) old_insols
	     -- See Note [Do not add duplicate derived insolubles]

getTcSImplicsRef :: TcS (IORef (Bag Implication))
getTcSImplicsRef = TcS (return . tcs_implics) 

getTcEvBinds :: TcS EvBindsVar
getTcEvBinds = TcS (return . tcs_ev_binds) 

getUntouchables :: TcS Untouchables
getUntouchables = wrapTcS TcM.getUntouchables

getFlattenSkols :: TcS [TcTyVar]
getFlattenSkols = do { is <- getTcSInerts; return (inert_fsks is) }

getTcSTyBinds :: TcS (IORef (TyVarEnv (TcTyVar, TcType)))
getTcSTyBinds = TcS (return . tcs_ty_binds)

getTcSTyBindsMap :: TcS (TyVarEnv (TcTyVar, TcType))
getTcSTyBindsMap = getTcSTyBinds >>= wrapTcS . (TcM.readTcRef) 

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
         do { ty_binds <- TcM.readTcRef ref
            ; when debugIsOn $
                  TcM.checkErr (not (tv `elemVarEnv` ty_binds)) $
                  vcat [ text "TERRIBLE ERROR: double set of meta type variable"
                       , ppr tv <+> text ":=" <+> ppr ty
                       , text "Old value =" <+> ppr (lookupVarEnv_NF ty_binds tv)]
            ; TcM.traceTc "setWantedTyBind" (ppr tv <+> text ":=" <+> ppr ty)
            ; TcM.writeTcRef ref (extendVarEnv ty_binds tv (tv,ty)) } }
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
newFlattenSkolem :: CtFlavour 
                 -> TcType                      -- F xis
                 -> TcS (CtEvidence, TcType)    -- co :: F xis ~ ty
-- We have already looked up in the cache; no need to so so again
newFlattenSkolem Given fam_ty
  = do { tv <- wrapTcS $ 
               do { uniq <- TcM.newUnique
                  ; let name = TcM.mkTcTyVarName uniq (fsLit "f")
                  ; return $ mkTcTyVar name (typeKind fam_ty) (FlatSkol fam_ty) } 
       ; traceTcS "New Flatten Skolem Born" $
         ppr tv <+> text "[:= " <+> ppr fam_ty <+> text "]"

       ; let rhs_ty = mkTyVarTy tv
             ctev = CtGiven { ctev_pred = mkTcEqPred fam_ty rhs_ty
                            , ctev_evtm = EvCoercion (mkTcReflCo fam_ty) }
       ; dflags <- getDynFlags
       ; updInertTcS $ \ is@(IS { inert_fsks = fsks }) -> 
            extendFlatCache dflags fam_ty ctev rhs_ty
            is { inert_fsks       = tv : fsks }

       ; return (ctev, rhs_ty) }

newFlattenSkolem _ fam_ty  -- Wanted or Derived: make new unification variable
  = do { rhs_ty <- newFlexiTcSTy (typeKind fam_ty)
       ; ctev <- newWantedEvVarNC (mkTcEqPred fam_ty rhs_ty)
                                   -- NC (no-cache) version because we've already
                                   -- looked in the solved goals an inerts (lookupFlatEqn)
       ; dflags <- getDynFlags
       ; updInertTcS $ extendFlatCache dflags fam_ty ctev rhs_ty
       ; return (ctev, rhs_ty) }

extendFlatCache :: DynFlags -> TcType -> CtEvidence -> TcType
                -> InertSet -> InertSet
extendFlatCache dflags
  | not (gopt Opt_FlatCache dflags)
  = \ _ _ _ is -> is
  | otherwise
  = \ fam_ty ctev rhs_ty is@(IS { inert_flat_cache = fc }) -> 
      is { inert_flat_cache = insertFamHead fc fam_ty (ctev,rhs_ty) }

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

data XEvTerm = 
  XEvTerm { ev_comp   :: [EvTerm] -> EvTerm
                         -- How to compose evidence 
          , ev_decomp :: EvTerm -> [EvTerm]
                         -- How to decompose evidence 
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

freshGoals :: [MaybeNew] -> [CtEvidence]
freshGoals mns = [ ctev | Fresh ctev <- mns ]

setEvBind :: EvVar -> EvTerm -> TcS ()
setEvBind the_ev tm
  = do { traceTcS "setEvBind" $ vcat [ text "ev =" <+> ppr the_ev
                                     , text "tm  =" <+> ppr tm ]
       ; tc_evbinds <- getTcEvBinds
       ; wrapTcS $ TcM.addTcEvBind tc_evbinds the_ev tm }

newGivenEvVar :: TcPredType -> EvTerm -> TcS CtEvidence
-- Make a new variable of the given PredType, 
-- immediately bind it to the given term
-- and return its CtEvidence
newGivenEvVar pred rhs
  = do { new_ev <- wrapTcS $ TcM.newEvVar pred
       ; setEvBind new_ev rhs
       ; return (CtGiven { ctev_pred = pred, ctev_evtm = EvId new_ev }) }

newWantedEvVarNC :: TcPredType -> TcS CtEvidence
-- Don't look up in the solved/inerts; we know it's not there
newWantedEvVarNC pty
  = do { new_ev <- wrapTcS $ TcM.newEvVar pty
       ; return (CtWanted { ctev_pred = pty, ctev_evar = new_ev })}

newWantedEvVar :: TcPredType -> TcS MaybeNew
newWantedEvVar pty
  = do { mb_ct <- lookupInInerts pty
       ; case mb_ct of
            Just ctev | not (isDerived ctev) 
                      -> do { traceTcS "newWantedEvVar/cache hit" $ ppr ctev
                            ; return (Cached (ctEvTerm ctev)) }
            _ -> do { ctev <- newWantedEvVarNC pty
                    ; traceTcS "newWantedEvVar/cache miss" $ ppr ctev
                    ; return (Fresh ctev) } }

newDerived :: TcPredType -> TcS (Maybe CtEvidence)
-- Returns Nothing    if cached, 
--         Just pred  if not cached
newDerived pty
  = do { mb_ct <- lookupInInerts pty
       ; return (case mb_ct of
                    Just {} -> Nothing
                    Nothing -> Just (CtDerived { ctev_pred = pty })) }

instDFunConstraints :: TcThetaType -> TcS [MaybeNew]
instDFunConstraints = mapM newWantedEvVar
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
    xCtFlavor ev [a~c, b~d] (XEvTerm { ev_comp = \[c1 c2]. <Tree> c1 c2
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


\begin{code}
xCtFlavor :: CtEvidence            -- Original flavor   
          -> [TcPredType]          -- New predicate types
          -> XEvTerm               -- Instructions about how to manipulate evidence
          -> TcS [CtEvidence]

xCtFlavor (CtGiven { ctev_evtm = tm }) ptys xev
  = ASSERT( equalLength ptys (ev_decomp xev tm) )
    zipWithM newGivenEvVar ptys (ev_decomp xev tm)
    -- See Note [Bind new Givens immediately]
  
xCtFlavor ctev@(CtWanted { ctev_evar = evar }) ptys xev
  = do { new_evars <- mapM newWantedEvVar ptys
       ; setEvBind evar (ev_comp xev (getEvTerms new_evars))
       ; return (freshGoals new_evars) }
    
xCtFlavor (CtDerived {}) ptys _xev
  = do { ders <- mapM newDerived ptys
       ; return (catMaybes ders) }

-----------------------------
rewriteCtFlavor :: CtEvidence
                -> TcPredType   -- new predicate
                -> TcCoercion   -- new ~ old     
                -> TcS (Maybe CtEvidence)
-- Returns Just new_fl iff either (i)  'co' is reflexivity
--                             or (ii) 'co' is not reflexivity, and 'new_pred' not cached
-- In either case, there is nothing new to do with new_fl
{- 
     rewriteCtFlavor old_fl new_pred co
Main purpose: create new evidence for new_pred;
              unless new_pred is cached already
* Returns a new_fl : new_pred, with same wanted/given/derived flag as old_fl
* If old_fl was wanted, create a binding for old_fl, in terms of new_fl
* If old_fl was given, AND not cached, create a binding for new_fl, in terms of old_fl
* Returns Nothing if new_fl is already cached


        Old evidence    New predicate is               Return new evidence
        flavour                                        of same flavor
        -------------------------------------------------------------------
        Wanted          Already solved or in inert     Nothing
        or Derived      Not                            Just new_evidence

        Given           Already in inert               Nothing
                        Not                            Just new_evidence
-}


rewriteCtFlavor (CtDerived {}) new_pred _co
  = -- If derived, don't even look at the coercion.
    -- This is very important, DO NOT re-order the equations for
    -- rewriteCtFlavor to put the isTcReflCo test first!  
    -- Why?  Because for *Derived* constraints, c, the coercion, which 
    -- was produced by flattening, may contain suspended calls to 
    -- (ctEvTerm c), which fails for Derived constraints.
    -- (Getting this wrong caused Trac #7384.)
    newDerived new_pred
        
rewriteCtFlavor old_ev new_pred co
  | isTcReflCo co -- If just reflexivity then you may re-use the same variable
  = return (Just (if ctEvPred old_ev `eqType` new_pred
                  then old_ev
                  else old_ev { ctev_pred = new_pred }))
       -- Even if the coercion is Refl, it might reflect the result of unification alpha := ty
       -- so old_pred and new_pred might not *look* the same, and it's vital to proceed from
       -- now on using new_pred.
       -- However, if they *do* look the same, we'd prefer to stick with old_pred
       -- then retain the old type, so that error messages come out mentioning synonyms

rewriteCtFlavor (CtGiven { ctev_evtm = old_tm }) new_pred co
  = do { new_ev <- newGivenEvVar new_pred new_tm  -- See Note [Bind new Givens immediately]
       ; return (Just new_ev) }
  where
    new_tm = mkEvCast old_tm (mkTcSymCo co)  -- mkEvCast optimises ReflCo
  
rewriteCtFlavor (CtWanted { ctev_evar = evar, ctev_pred = old_pred }) new_pred co
  = do { new_evar <- newWantedEvVar new_pred
       ; setEvBind evar (mkEvCast (getEvTerm new_evar) co)
       ; case new_evar of
            Fresh ctev -> return (Just ctev) 
            _          -> return Nothing }



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
             -> let co = mkTcUnbranchedAxInstCo (famInstAxiom famInst) inst_tys
                    ty = pSnd $ tcCoercionKind co
                in return $ Just (co, ty) }

  | Just ax <- isClosedSynFamilyTyCon_maybe tycon
  , Just (ind, inst_tys) <- chooseBranch ax args
  = let co = mkTcAxInstCo ax ind inst_tys
        ty = pSnd (tcCoercionKind co)
    in return $ Just (co, ty)

  | Just ops <- isBuiltInSynFamTyCon_maybe tycon = return (sfMatchFam ops args)

  | otherwise
  = return Nothing
       
\end{code}

\begin{code}
-- Deferring forall equalities as implications
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

deferTcSForAllEq :: (CtLoc,EvVar)  -- Original wanted equality flavor
                 -> ([TyVar],TcType)   -- ForAll tvs1 body1
                 -> ([TyVar],TcType)   -- ForAll tvs2 body2
                 -> TcS ()
-- Some of this functionality is repeated from TcUnify, 
-- consider having a single place where we create fresh implications. 
deferTcSForAllEq (loc,orig_ev) (tvs1,body1) (tvs2,body2)
 = do { (subst1, skol_tvs) <- wrapTcS $ TcM.tcInstSkolTyVars tvs1
      ; let tys  = mkTyVarTys skol_tvs
            phi1 = Type.substTy subst1 body1
            phi2 = Type.substTy (zipTopTvSubst tvs2 tys) body2
            skol_info = UnifyForAllSkol skol_tvs phi1
        ; mev <- newWantedEvVar (mkTcEqPred phi1 phi2)
        ; coe_inside <- case mev of
            Cached ev_tm -> return (evTermCoercion ev_tm)
            Fresh ctev   -> do { ev_binds_var <- wrapTcS $ TcM.newTcEvBinds
                               ; env <- wrapTcS $ TcM.getLclEnv
                               ; let ev_binds = TcEvBinds ev_binds_var
                                     new_ct = mkNonCanonical loc ctev
              			     new_co = evTermCoercion (ctEvTerm ctev)
                                     new_untch = pushUntouchables (tcl_untch env)
                               ; let wc = WC { wc_flat  = singleCt new_ct 
                                             , wc_impl  = emptyBag
                                             , wc_insol = emptyCts }
                                     imp = Implic { ic_untch  = new_untch
                                                  , ic_skols  = skol_tvs
                                                  , ic_fsks   = []
                                                  , ic_given  = []
                                                  , ic_wanted = wc 
                                                  , ic_insol  = False
                                                  , ic_binds  = ev_binds_var
                                                  , ic_env    = env
                                                  , ic_info   = skol_info }
                               ; updTcSImplics (consBag imp) 
                               ; return (TcLetCo ev_binds new_co) }

        ; setEvBind orig_ev $
          EvCoercion (foldr mkTcForAllCo coe_inside skol_tvs)
        }
\end{code}

