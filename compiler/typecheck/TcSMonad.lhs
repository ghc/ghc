\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
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
    extendWorkListEq, extendWorkListNonEq, extendWorkListCt, 
    appendWorkListCt, appendWorkListEqs, unionWorkList, selectWorkItem,

    getTcSWorkList, updWorkListTcS, updWorkListTcS_return,
    getTcSWorkListTvs, 
    
    getTcSImplics, updTcSImplics, emitTcSImplication, 

    Ct(..), Xi, tyVarsOfCt, tyVarsOfCts, tyVarsOfCDicts, 
    emitFrozenError,

    isWanted, isDerived, 
    isGivenCt, isWantedCt, isDerivedCt, pprFlavorArising,

    isFlexiTcsTv, instFlexiTcSHelperTcS,

    canRewrite, canSolve,
    mkGivenLoc, ctWantedLoc,

    TcS, runTcS, runTcSWithEvBinds, failTcS, panicTcS, traceTcS, -- Basic functionality 
    traceFireTcS, bumpStepCountTcS, doWithInert,
    tryTcS, nestImplicTcS, recoverTcS,
    wrapErrTcS, wrapWarnTcS,

    -- Getting and setting the flattening cache
    getFlatCache, updFlatCache, addSolvedDict, addSolvedFunEq,
    
    deferTcSForAllEq, 
    
    setEvBind,
    XEvTerm(..),
    MaybeNew (..), isFresh, freshGoals, getEvTerms,

    xCtFlavor,        -- Transform a CtEvidence during a step 
    rewriteCtFlavor,  -- Specialized version of xCtFlavor for coercions
    newWantedEvVar, instDFunConstraints,
    newDerived,
    
       -- Creation of evidence variables
    setWantedTyBind,

    getInstEnvs, getFamInstEnvs,                -- Getting the environments
    getTopEnv, getGblEnv, getTcEvBinds, getUntouchables,
    getTcEvBindsMap, getTcSTyBinds, getTcSTyBindsMap,


    newFlattenSkolemTy,                         -- Flatten skolems 

        -- Inerts 
    InertSet(..), InertCans(..), 
    getInertEqs, getCtCoercion,
    emptyInert, getTcSInerts, lookupInInerts, 
    getInertUnsolved, getInertInsols, splitInertsForImplications,
    modifyInertTcS,
    updInertSetTcS, partitionCCanMap, partitionEqMap,
    getRelevantCts, extractRelevantInerts,
    CCanMap(..), CtTypeMap, CtFamHeadMap, CtPredMap,
    PredMap, FamHeadMap,
    partCtFamHeadMap, lookupFamHead,
    filterSolved,

    instDFunType,                              -- Instantiation
    newFlexiTcSTy, instFlexiTcS,

    compatKind, mkKindErrorCtxtTcS,

    TcsUntouchables,
    isTouchableMetaTyVar,
    isTouchableMetaTyVar_InRange, 

    getDefaultInfo, getDynFlags,

    matchClass, matchFam, MatchInstResult (..), 
    checkWellStagedDFun, 
    warnTcS,
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
import {-# SOURCE #-} qualified TcUnify as TcM ( mkKindErrorCtxt )
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
import VarSet

import FastString
import Util
import Id 
import TcRnTypes

import Unique 
import UniqFM
#ifdef DEBUG
import Digraph
#endif
import Maybes ( orElse, catMaybes )


import Control.Monad( unless, when, zipWithM )
import StaticFlags( opt_PprStyle_Debug )
import Data.IORef
import TrieMap

\end{code}


\begin{code}
compatKind :: Kind -> Kind -> Bool
compatKind k1 k2 = k1 `tcIsSubKind` k2 || k2 `tcIsSubKind` k1 

mkKindErrorCtxtTcS :: Type -> Kind 
                   -> Type -> Kind 
                   -> ErrCtxt
mkKindErrorCtxtTcS ty1 ki1 ty2 ki2
  = (False,TcM.mkKindErrorCtxt ty1 ty2 ki1 ki2)

\end{code}

%************************************************************************
%*									*
%*                            Worklists                                *
%*  Canonical and non-canonical constraints that the simplifier has to  *
%*  work on. Including their simplification depths.                     *
%*                                                                      *
%*									*
%************************************************************************

Note [WorkList]
~~~~~~~~~~~~~~~
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

-- See Note [WorkList]
data WorkList = WorkList { wl_eqs    :: [Ct]
                         , wl_funeqs :: [Ct]
                         , wl_rest   :: [Ct] 
                         }


unionWorkList :: WorkList -> WorkList -> WorkList
unionWorkList new_wl orig_wl = 
   WorkList { wl_eqs    = wl_eqs new_wl ++ wl_eqs orig_wl
            , wl_funeqs = wl_funeqs new_wl ++ wl_funeqs orig_wl
            , wl_rest   = wl_rest new_wl ++ wl_rest orig_wl }


extendWorkListEq :: Ct -> WorkList -> WorkList
-- Extension by equality
extendWorkListEq ct wl 
  | Just {} <- isCFunEqCan_Maybe ct
  = wl { wl_funeqs = ct : wl_funeqs wl }
  | otherwise
  = wl { wl_eqs = ct : wl_eqs wl }

extendWorkListNonEq :: Ct -> WorkList -> WorkList
-- Extension by non equality
extendWorkListNonEq ct wl 
  = wl { wl_rest = ct : wl_rest wl }

extendWorkListCt :: Ct -> WorkList -> WorkList
-- Agnostic
extendWorkListCt ct wl
 | isEqPred (ctPred ct) = extendWorkListEq ct wl
 | otherwise = extendWorkListNonEq ct wl

appendWorkListCt :: [Ct] -> WorkList -> WorkList
-- Agnostic
appendWorkListCt cts wl = foldr extendWorkListCt wl cts

appendWorkListEqs :: [Ct] -> WorkList -> WorkList
-- Append a list of equalities
appendWorkListEqs cts wl = foldr extendWorkListEq wl cts

isEmptyWorkList :: WorkList -> Bool
isEmptyWorkList wl 
  = null (wl_eqs wl) &&  null (wl_rest wl) && null (wl_funeqs wl)

emptyWorkList :: WorkList
emptyWorkList = WorkList { wl_eqs  = [], wl_rest = [], wl_funeqs = [] }

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
      (_,(ct:cts),_)   -> (Just ct, wl { wl_funeqs = cts })
      (_,_,(ct:cts))   -> (Just ct, wl { wl_rest   = cts })
      (_,_,_)          -> (Nothing,wl)

-- Pretty printing 
instance Outputable WorkList where 
  ppr wl = vcat [ text "WorkList (eqs)   = " <+> ppr (wl_eqs wl)
                , text "WorkList (funeqs)= " <+> ppr (wl_funeqs wl)
                , text "WorkList (rest)  = " <+> ppr (wl_rest wl)
                ]


-- Canonical constraint maps
data CCanMap a = CCanMap { cts_given   :: UniqFM Cts
                                          -- Invariant: all Given
                         , cts_derived :: UniqFM Cts 
                                          -- Invariant: all Derived
                         , cts_wanted  :: UniqFM Cts } 
                                          -- Invariant: all Wanted

cCanMapToBag :: CCanMap a -> Cts 
cCanMapToBag cmap = foldUFM unionBags rest_wder (cts_given cmap)
  where rest_wder = foldUFM unionBags rest_der  (cts_wanted cmap) 
        rest_der  = foldUFM unionBags emptyCts  (cts_derived cmap)

emptyCCanMap :: CCanMap a 
emptyCCanMap = CCanMap { cts_given = emptyUFM, cts_derived = emptyUFM, cts_wanted = emptyUFM } 

updCCanMap:: Uniquable a => (a,Ct) -> CCanMap a -> CCanMap a 
updCCanMap (a,ct) cmap 
  = case cc_ev ct of 
      Wanted {}  -> cmap { cts_wanted  = insert_into (cts_wanted cmap)  } 
      Given {}   -> cmap { cts_given   = insert_into (cts_given cmap)   }
      Derived {} -> cmap { cts_derived = insert_into (cts_derived cmap) }
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


extractUnsolvedCMap :: CCanMap a -> (Cts, CCanMap a)
-- Gets the wanted or derived constraints and returns a residual
-- CCanMap with only givens.
extractUnsolvedCMap cmap =
  let wntd = foldUFM unionBags emptyCts (cts_wanted cmap)
      derd = foldUFM unionBags emptyCts (cts_derived cmap)
  in (wntd `unionBags` derd, 
      cmap { cts_wanted = emptyUFM, cts_derived = emptyUFM })

extractWantedCMap :: CCanMap a -> (Cts, CCanMap a)
-- Gets the wanted /only/ constraints and returns a residual
-- CCanMap with only givens or derived
extractWantedCMap cmap =
  let wntd = foldUFM unionBags emptyCts (cts_wanted cmap)
  in (wntd, cmap { cts_wanted = emptyUFM })


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

sizeFamHeadMap :: FamHeadMap a -> Int
sizeFamHeadMap (FamHeadMap m) = foldTypeMap (\_ x -> x+1) 0 m

ctTypeMapCts :: TypeMap Ct -> Cts
ctTypeMapCts ctmap = foldTM (\ct cts -> extendCts cts ct) ctmap emptyCts

lookupFamHead :: FamHeadMap a -> TcType -> Maybe a
lookupFamHead (FamHeadMap m) key = lookupTM key m

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

\begin{code}
-- All Given (fully known) or Wanted or Derived
-- See Note [Detailed InertCans Invariants] for more
data InertCans 
  = IC { inert_eqs :: TyVarEnv Ct
              -- Must all be CTyEqCans! If an entry exists of the form: 
              --   a |-> ct,co
              -- Then ct = CTyEqCan { cc_tyvar = a, cc_rhs = xi } 
              -- And  co : a ~ xi
       , inert_eq_tvs :: InScopeSet
              -- Superset of the type variables of inert_eqs
       , inert_dicts :: CCanMap Class
              -- Dictionaries only, index is the class
              -- NB: index is /not/ the whole type because FD reactions 
              -- need to match the class but not necessarily the whole type.
       , inert_funeqs :: CtFamHeadMap
              -- Family equations, index is the whole family head type.
       , inert_irreds :: Cts       
              -- Irreducible predicates
       }
    
                     
\end{code}

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

  8 Equalities _do_not_ form an idempotent substitution but they are guarranteed to not have
    any occurs errors. Additional notes: 

       - The lack of idempotence of the inert substitution implies that we must make sure 
         that when we rewrite a constraint we apply the substitution /recursively/ to the 
         types involved. Currently the one AND ONLY way in the whole constraint solver 
         that we rewrite types and constraints wrt to the inert substitution is 
         TcCanonical/flattenTyVar.

       - In the past we did try to have the inert substituion as idempotent as possible but
         this would only be true for constraints of the same flavor, so in total the inert 
         substitution could not be idempotent, due to flavor-related issued. 
         Note [Non-idempotent inert substitution] explains what is going on. 

       - Whenever a constraint ends up in the worklist we do recursively apply exhaustively
         the inert substitution to it to check for occurs errors but if an equality is already
         in the inert set and we can guarantee that adding a new equality will not cause the
         first equality to have an occurs check then we do not rewrite the inert equality. 
         This happens in TcInteract, rewriteInertEqsFromInertEq. 
         
         See Note [Delicate equality kick-out] to see which inert equalities can safely stay
         in the inert set and which must be kicked out to be rewritten and re-checked for 
         occurs errors. 

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


\begin{code}
-- The Inert Set
data InertSet
  = IS { inert_cans :: InertCans
              -- Canonical Given, Wanted, Derived (no Solved)
	      -- Sometimes called "the inert set"

       , inert_frozen :: Cts       
              -- Frozen errors (as non-canonicals)
                               
       , inert_flat_cache :: CtFamHeadMap 
              -- All ``flattening equations'' are kept here. 
              -- Always canonical CTyFunEqs (Given or Wanted only!)
              -- Key is by family head. We use this field during flattening only
              -- Not necessarily inert wrt top-level equations (or inert_cans)

       , inert_solved_funeqs :: FamHeadMap CtEvidence  -- Of form co :: F xis ~ xi 
                                                       -- No Deriveds 

       , inert_solved_dicts :: PredMap    CtEvidence  -- All others
       	      -- These two fields constitute a cache of solved (only!) constraints
              -- See Note [Solved constraints]
       	      -- - Constraints of form (F xis ~ xi) live in inert_solved_funeqs, 
       	      --   all the others are in inert_solved
       	      -- - Used to avoid creating a new EvVar when we have a new goal that we
       	      --   have solvedin the past
       	      -- - Stored not necessarily as fully rewritten 
       	      --   (ToDo: rewrite lazily when we lookup)
       }


instance Outputable InertCans where 
  ppr ics = vcat [ vcat (map ppr (varEnvElts (inert_eqs ics)))
                 , vcat (map ppr (Bag.bagToList $ cCanMapToBag (inert_dicts ics)))
                 , vcat (map ppr (Bag.bagToList $ 
                                  ctTypeMapCts (unFamHeadMap $ inert_funeqs ics)))
                 , vcat (map ppr (Bag.bagToList $ inert_irreds ics))
                 ]
            
instance Outputable InertSet where 
  ppr is = vcat [ ppr $ inert_cans is
                , text "Frozen errors =" <+> -- Clearly print frozen errors
                    braces (vcat (map ppr (Bag.bagToList $ inert_frozen is)))
                , text "Solved dicts"  <+> int (sizePredMap (inert_solved_dicts is))
                , text "Solved funeqs" <+> int (sizeFamHeadMap (inert_solved_funeqs is))]

emptyInert :: InertSet
emptyInert
  = IS { inert_cans = IC { inert_eqs    = emptyVarEnv
                         , inert_eq_tvs = emptyInScopeSet
                         , inert_dicts  = emptyCCanMap
                         , inert_funeqs = FamHeadMap emptyTM 
                         , inert_irreds = emptyCts }
       , inert_frozen        = emptyCts
       , inert_flat_cache    = FamHeadMap emptyTM
       , inert_solved_dicts  = PredMap emptyTM 
       , inert_solved_funeqs = FamHeadMap emptyTM }


updInertSet :: InertSet -> Ct -> InertSet 
-- Add a new inert element to the inert set. 
updInertSet is item 
  | isCNonCanonical item 
    -- NB: this may happen if we decide to kick some frozen error 
    -- out to rewrite him. Frozen errors are just NonCanonicals
  = is { inert_frozen = inert_frozen is `Bag.snocBag` item }
    
  | otherwise  
    -- A canonical Given, Wanted, or Derived
  = is { inert_cans = upd_inert_cans (inert_cans is) item }
  
  where upd_inert_cans :: InertCans -> Ct -> InertCans
        -- Precondition: item /is/ canonical
        upd_inert_cans ics item
          | isCTyEqCan item                     
          = let upd_err a b = pprPanic "updInertSet" $
                              vcat [ text "Multiple inert equalities:"
                                   , text "Old (already inert):" <+> ppr a
                                   , text "Trying to insert   :" <+> ppr b ]
        
                eqs'     = extendVarEnv_C upd_err (inert_eqs ics) 
                                                  (cc_tyvar item) item        
                inscope' = extendInScopeSetSet (inert_eq_tvs ics)
                                               (tyVarsOfCt item)
                
            in ics { inert_eqs = eqs', inert_eq_tvs = inscope' }

          | isCIrredEvCan item                  -- Presently-irreducible evidence
          = ics { inert_irreds = inert_irreds ics `Bag.snocBag` item }

          | Just cls <- isCDictCan_Maybe item   -- Dictionary 
          = ics { inert_dicts = updCCanMap (cls,item) (inert_dicts ics) }

          | Just _tc <- isCFunEqCan_Maybe item  -- Function equality
          = let fam_head = mkTyConApp (cc_fun item) (cc_tyargs item)
                upd_funeqs Nothing = Just item
                upd_funeqs (Just _already_there) 
                  = panic "updInertSet: item already there!"
            in ics { inert_funeqs = FamHeadMap 
                                      (alterTM fam_head upd_funeqs $ 
                                         (unFamHeadMap $ inert_funeqs ics)) }
          | otherwise
          = pprPanic "upd_inert set: can't happen! Inserting " $ 
            ppr item 

updInertSetTcS :: Ct -> TcS ()
-- Add a new item in the inerts of the monad
updInertSetTcS item
  = do { traceTcS "updInertSetTcs {" $ 
         text "Trying to insert new inert item:" <+> ppr item

       ; modifyInertTcS (\is -> ((), updInertSet is item)) 
                        
       ; traceTcS "updInertSetTcs }" $ empty }


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

addSolvedFunEq :: CtEvidence -> TcS ()
addSolvedFunEq fun_eq
  = updInertTcS upd_inert
  where 
    upd_inert inert 
      = let slvd = unFamHeadMap (inert_solved_funeqs inert)
        in inert { inert_solved_funeqs =
                      FamHeadMap (alterTM key upd_funeqs slvd) }       
    upd_funeqs Nothing    = Just fun_eq
    upd_funeqs (Just _ct) = Just fun_eq 
         -- Or _ct? depends on which caches more steps of computation
    key = ctEvPred fun_eq

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

splitInertsForImplications :: InertSet -> ([Ct],InertSet)
-- Converts the Wanted of the original inert to Given and removes 
-- all Wanted and Derived from the inerts.
-- DV: Is the removal of Derived essential? 
splitInertsForImplications is
  = let (cts,is') = extractWanted is
    in  (givens_from_unsolved cts,is')
  where givens_from_unsolved = foldrBag get_unsolved []
        get_unsolved cc rest_givens
            | pushable_wanted cc
            = let fl   = ctEvidence cc
                  gfl  = Given { ctev_gloc = setCtLocOrigin (ctev_wloc fl) UnkSkol
                               , ctev_evtm = EvId (ctev_evar fl)
                               , ctev_pred = ctev_pred fl }
                  this_given = cc { cc_ev = gfl }
              in this_given : rest_givens
            | otherwise = rest_givens 

        pushable_wanted :: Ct -> Bool 
        pushable_wanted cc 
         = isEqPred (ctPred cc) -- see Note [Preparing inert set for implications]

        -- Returns Wanted constraints and a Derived/Given InertSet
        extractWanted (IS { inert_cans = IC { inert_eqs    = eqs
                                            , inert_eq_tvs = eq_tvs
                                            , inert_irreds = irreds
                                            , inert_funeqs = funeqs
                                            , inert_dicts  = dicts
                                            }
                          , inert_frozen = _frozen
                          , inert_solved_dicts = solved
                          , inert_flat_cache = flat_cache 
                          , inert_solved_funeqs = funeq_cache
                          })
          
          = let is_solved  = IS { inert_cans = IC { inert_eqs    = solved_eqs
                                                  , inert_eq_tvs = eq_tvs
                                                  , inert_dicts  = solved_dicts
                                                  , inert_irreds = solved_irreds
                                                  , inert_funeqs = solved_funeqs }
                                , inert_frozen = emptyCts -- All out
                                                 
                                      -- At some point, I used to flush all the solved, in 
                                      -- fear of evidence loops. But I think we are safe, 
                                      -- flushing is why T3064 had become slower
                                , inert_solved_dicts  = solved      -- PredMap emptyTM
                                , inert_flat_cache    = flat_cache  -- FamHeadMap emptyTM
                                , inert_solved_funeqs = funeq_cache -- FamHeadMap emptyTM
                                }
            in (wanted, is_solved)

          where gd_eqs = filterVarEnv_Directly (\_ ct -> not (isWantedCt ct)) eqs
                wanted_eqs = foldVarEnv (\ct cts -> cts `extendCts` ct) emptyCts $ 
                             eqs `minusVarEnv` gd_eqs
                
                (wanted_irreds, gd_irreds) = Bag.partitionBag isWantedCt irreds
                (wanted_dicts,  gd_dicts)  = extractWantedCMap dicts
                (wanted_funeqs, gd_funeqs) = partCtFamHeadMap isWantedCt funeqs

                -- Is this all necessary? 
                solved_eqs        = filterVarEnv_Directly (\_ ct -> isGivenCt ct) gd_eqs 
                solved_irreds     = Bag.filterBag isGivenCt gd_irreds
                (_,solved_dicts)  = extractUnsolvedCMap gd_dicts
                (_,solved_funeqs) = partCtFamHeadMap (not . isGivenCt) gd_funeqs

                wanted = wanted_eqs `unionBags` wanted_irreds `unionBags`
                         wanted_dicts `unionBags` wanted_funeqs


getInertInsols :: InertSet -> Cts
-- Insolubles only
getInertInsols is = inert_frozen is

getInertUnsolved :: InertSet -> Cts
-- Unsolved Wanted or Derived only 
getInertUnsolved (IS { inert_cans = icans }) 
  = let unsolved_eqs = foldVarEnv add_if_not_given emptyCts (inert_eqs icans)
        add_if_not_given ct cts
            | isGivenCt ct = cts
            | otherwise    = cts `extendCts` ct
        (unsolved_irreds,_) = Bag.partitionBag (not . isGivenCt) (inert_irreds icans)
        (unsolved_dicts,_)  = extractUnsolvedCMap (inert_dicts icans)
        (unsolved_funeqs,_) = partCtFamHeadMap (not . isGivenCt) (inert_funeqs icans)
    in unsolved_eqs `unionBags` unsolved_irreds `unionBags` 
       unsolved_dicts `unionBags` unsolved_funeqs



extractRelevantInerts :: Ct -> TcS Cts
-- Returns the constraints from the inert set that are 'relevant' to react with 
-- this constraint. The monad is left with the 'thinner' inerts. 
-- NB: This function contains logic specific to the constraint solver, maybe move there?
extractRelevantInerts wi 
  = modifyInertTcS (extract_relevants wi)
  where extract_relevants wi is 
          = let (cts,ics') = extract_ics_relevants wi (inert_cans is)
            in (cts, is { inert_cans = ics' }) 
            
        extract_ics_relevants (CDictCan {cc_class = cl}) ics = 
            let (cts,dict_map) = getRelevantCts cl (inert_dicts ics) 
            in (cts, ics { inert_dicts = dict_map })
        extract_ics_relevants ct@(CFunEqCan {}) ics = 
            let (cts,feqs_map)  = 
                  let funeq_map = unFamHeadMap $ inert_funeqs ics
                      fam_head = mkTyConApp (cc_fun ct) (cc_tyargs ct)
                      lkp = lookupTM fam_head funeq_map
                      new_funeq_map = alterTM fam_head xtm funeq_map
                      xtm Nothing    = Nothing
                      xtm (Just _ct) = Nothing
                  in case lkp of 
                    Nothing -> (emptyCts, funeq_map)
                    Just ct -> (singleCt ct, new_funeq_map)
            in (cts, ics { inert_funeqs = FamHeadMap feqs_map })
        extract_ics_relevants (CIrredEvCan { }) ics = 
            let cts = inert_irreds ics 
            in (cts, ics { inert_irreds = emptyCts })
        extract_ics_relevants _ ics = (emptyCts,ics)
        

lookupInInerts :: InertSet -> TcPredType -> Maybe CtEvidence
-- Is this exact predicate type cached in the solved or canonicals of the InertSet
lookupInInerts (IS { inert_solved_dicts = solved, inert_cans = ics }) pty
  = case lookupInSolved solved pty of
      Just ctev -> return ctev
      Nothing   -> lookupInInertCans ics pty

lookupInSolved :: PredMap CtEvidence -> TcPredType -> Maybe CtEvidence
-- Returns just if exactly this predicate type exists in the solved.
lookupInSolved tm pty = lookupTM pty $ unPredMap tm

lookupInInertCans :: InertCans -> TcPredType -> Maybe CtEvidence
-- Returns Just if exactly this pred type exists in the inert canonicals
lookupInInertCans ics pty
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
    
      _other -> Nothing -- NB: No caching for IPs
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
                     
      tcs_untch :: TcsUntouchables,

      tcs_ic_depth   :: Int,       -- Implication nesting depth
      tcs_count      :: IORef Int, -- Global step count

      tcs_inerts   :: IORef InertSet, -- Current inert set
      tcs_worklist :: IORef WorkList, -- Current worklist
      
      -- Residual implication constraints that are generated 
      -- while solving the current worklist.
      tcs_implics  :: IORef (Bag Implication)
    }

type TcsUntouchables = (Untouchables,TcTyVarSet)
-- Like the TcM Untouchables, 
-- but records extra TcsTv variables generated during simplification
-- See Note [Extra TcsTv untouchables] in TcSimplify
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

bumpStepCountTcS :: TcS ()
bumpStepCountTcS = TcS $ \env -> do { let ref = tcs_count env
                                    ; n <- TcM.readTcRef ref
                                    ; TcM.writeTcRef ref (n+1) }

traceFireTcS :: SubGoalDepth -> SDoc -> TcS ()
-- Dump a rule-firing trace
traceFireTcS depth doc 
  = TcS $ \env -> 
    TcM.ifDOptM Opt_D_dump_cs_trace $ 
    do { n <- TcM.readTcRef (tcs_count env)
       ; let msg = int n 
                <> text (replicate (tcs_ic_depth env) '>')
                <> brackets (int depth) <+> doc
       ; TcM.dumpTcRn msg }

runTcSWithEvBinds :: EvBindsVar
                  -> TcS a 
                  -> TcM a
runTcSWithEvBinds ev_binds_var tcs
  = do { ty_binds_var <- TcM.newTcRef emptyVarEnv
       ; impl_var <- TcM.newTcRef emptyBag
       ; step_count <- TcM.newTcRef 0

       ; inert_var <- TcM.newTcRef is 
       ; wl_var <- TcM.newTcRef wl

       -- The "low end" of the untouchable range should come from the
       -- ambient tcl_untch; the high end is the highest allocated to
       -- date. 'untouch' used (in 7.6.1, entirely wrongly) to be
       -- set to NoUntouchables, causing #7453.
       -- All this is done much better in 7.8.
       ; tc_lenv <- TcM.getLclEnv
       ; tcm_high <- TcM.readTcRef (tcl_meta tc_lenv)
       ; let untouch = TouchableRange tcm_low tcm_high
             tcm_low = tcl_untch tc_lenv
       ; let env = TcSEnv { tcs_ev_binds = ev_binds_var
                          , tcs_ty_binds = ty_binds_var
                          , tcs_untch    = (untouch, emptyVarSet) -- No Tcs untouchables yet
			  , tcs_count    = step_count
			  , tcs_ic_depth = 0
                          , tcs_inerts   = inert_var
                          , tcs_worklist = wl_var 
                          , tcs_implics  = impl_var }

	     -- Run the computation
       ; res <- unTcS tcs env
	     -- Perform the type unifications required
       ; ty_binds <- TcM.readTcRef ty_binds_var
       ; mapM_ do_unification (varEnvElts ty_binds)

       ; when debugIsOn $ 
         do { count <- TcM.readTcRef step_count
            ; when (opt_PprStyle_Debug && count > 0) $
              TcM.debugDumpTcRn (ptext (sLit "Constraint solver steps =") <+> int count ) }
             -- And return
       ; ev_binds <- TcM.getTcEvBinds ev_binds_var
       ; checkForCyclicBinds ev_binds
       ; return res }
  where
    do_unification (tv,ty) = TcM.writeMetaTyVar tv ty
    is = emptyInert
    wl = emptyWorkList
    
runTcS :: TcS a		       -- What to run
       -> TcM (a, Bag EvBind)
runTcS tcs
  = do { ev_binds_var <- TcM.newTcEvBinds
       ; res <- runTcSWithEvBinds ev_binds_var tcs
       ; ev_binds <- TcM.getTcEvBinds ev_binds_var
       ; return (res, ev_binds) }

checkForCyclicBinds :: Bag EvBind -> TcM ()
#ifndef DEBUG
checkForCyclicBinds _ = return ()
#else
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

doWithInert :: InertSet -> TcS a -> TcS a 
doWithInert inert (TcS action)
  = TcS $ \env -> do { new_inert_var <- TcM.newTcRef inert
                     ; action (env { tcs_inerts = new_inert_var }) }

nestImplicTcS :: EvBindsVar -> TcsUntouchables -> TcS a -> TcS a 
nestImplicTcS ref (inner_range, inner_tcs) (TcS thing_inside) 
  = TcS $ \ TcSEnv { tcs_ty_binds = ty_binds
                   , tcs_untch = (_outer_range, outer_tcs)
                   , tcs_count = count
                   , tcs_ic_depth = idepth
                   , tcs_inerts = inert_var
                   , tcs_worklist = wl_var 
                   , tcs_implics = _impl_var } -> 
    do { let inner_untch = (inner_range, outer_tcs `unionVarSet` inner_tcs)
       		   -- The inner_range should be narrower than the outer one
		   -- (thus increasing the set of untouchables) but 
		   -- the inner Tcs-untouchables must be unioned with the
		   -- outer ones!

         -- Inherit the inerts from the outer scope
       ; orig_inerts <- TcM.readTcRef inert_var
       ; new_inert_var <- TcM.newTcRef orig_inerts
         -- Inherit residual implications from outer scope (?) or create
         -- fresh var?                 
--     ; orig_implics <- TcM.readTcRef impl_var                   
       ; new_implics_var <- TcM.newTcRef emptyBag
                           
       ; let nest_env = TcSEnv { tcs_ev_binds    = ref
                               , tcs_ty_binds    = ty_binds
                               , tcs_untch       = inner_untch
                               , tcs_count       = count
                               , tcs_ic_depth    = idepth+1
                               , tcs_inerts      = new_inert_var
                               , tcs_worklist    = wl_var 
                               -- NB: worklist is going to be empty anyway, 
                               -- so reuse the same ref cell
                               , tcs_implics     = new_implics_var
                               }
       ; res <- thing_inside nest_env 
                
       -- Perform a check that the thing_inside did not cause cycles
       ; ev_binds <- TcM.getTcEvBinds ref
       ; checkForCyclicBinds ev_binds
         
       ; return res }

recoverTcS :: TcS a -> TcS a -> TcS a
recoverTcS (TcS recovery_code) (TcS thing_inside)
  = TcS $ \ env ->
    TcM.recoverM (recovery_code env) (thing_inside env)

tryTcS :: TcS a -> TcS a
-- Like runTcS, but from within the TcS monad 
-- Completely afresh inerts and worklist, be careful! 
-- Moreover, we will simply throw away all the evidence generated. 
tryTcS tcs
  = TcS (\env -> 
             do { wl_var <- TcM.newTcRef emptyWorkList
                ; is_var <- TcM.newTcRef emptyInert

                ; ty_binds_var <- TcM.newTcRef emptyVarEnv
                ; ev_binds_var <- TcM.newTcEvBinds

                ; let env1 = env { tcs_ev_binds = ev_binds_var
                                 , tcs_ty_binds = ty_binds_var
                                 , tcs_inerts   = is_var
                                 , tcs_worklist = wl_var } 
                ; unTcS tcs env1 })

-- Getters and setters of TcEnv fields
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Getter of inerts and worklist
getTcSInertsRef :: TcS (IORef InertSet)
getTcSInertsRef = TcS (return . tcs_inerts)

getTcSWorkListRef :: TcS (IORef WorkList) 
getTcSWorkListRef = TcS (return . tcs_worklist) 

getTcSInerts :: TcS InertSet 
getTcSInerts = getTcSInertsRef >>= wrapTcS . (TcM.readTcRef) 


getTcSImplicsRef :: TcS (IORef (Bag Implication))
getTcSImplicsRef = TcS (return . tcs_implics) 

getTcSImplics :: TcS (Bag Implication)
getTcSImplics = getTcSImplicsRef >>= wrapTcS . (TcM.readTcRef)

getTcSWorkList :: TcS WorkList
getTcSWorkList = getTcSWorkListRef >>= wrapTcS . (TcM.readTcRef) 


getTcSWorkListTvs :: TcS TyVarSet
-- Return the variables of the worklist
getTcSWorkListTvs 
  = do { wl <- getTcSWorkList
       ; return $
         cts_tvs (wl_eqs wl) `unionVarSet` cts_tvs (wl_funeqs wl) `unionVarSet` cts_tvs (wl_rest wl) }
  where cts_tvs = foldr (unionVarSet . tyVarsOfCt) emptyVarSet 


updWorkListTcS :: (WorkList -> WorkList) -> TcS () 
updWorkListTcS f 
  = updWorkListTcS_return (\w -> ((),f w))

updWorkListTcS_return :: (WorkList -> (a,WorkList)) -> TcS a
updWorkListTcS_return f
  = do { wl_var <- getTcSWorkListRef
       ; wl_curr <- wrapTcS (TcM.readTcRef wl_var)
       ; let (res,new_work) = f wl_curr
       ; wrapTcS (TcM.writeTcRef wl_var new_work)
       ; return res }
    

updTcSImplics :: (Bag Implication -> Bag Implication) -> TcS ()
updTcSImplics f 
 = do { impl_ref <- getTcSImplicsRef
      ; implics <- wrapTcS (TcM.readTcRef impl_ref)
      ; let new_implics = f implics
      ; wrapTcS (TcM.writeTcRef impl_ref new_implics) }

emitTcSImplication :: Implication -> TcS ()
emitTcSImplication imp = updTcSImplics (consBag imp)


emitFrozenError :: CtEvidence -> SubGoalDepth -> TcS ()
-- Emits a non-canonical constraint that will stand for a frozen error in the inerts. 
emitFrozenError fl depth 
  = do { traceTcS "Emit frozen error" (ppr (ctEvPred fl))
       ; inert_ref <- getTcSInertsRef 
       ; wrapTcS $ do
       { inerts <- TcM.readTcRef inert_ref
       ; let old_insols = inert_frozen inerts
             ct = CNonCanonical { cc_ev = fl, cc_depth = depth } 
             inerts_new = inerts { inert_frozen = extendCts old_insols ct } 
             this_pred = ctEvPred fl
             already_there = not (isWanted fl) && anyBag (eqType this_pred . ctPred) old_insols
	     -- See Note [Do not add duplicate derived insolubles]
       ; unless already_there $
         TcM.writeTcRef inert_ref inerts_new } }

instance HasDynFlags TcS where
    getDynFlags = wrapTcS getDynFlags


getTcEvBinds :: TcS EvBindsVar
getTcEvBinds = TcS (return . tcs_ev_binds) 

getFlatCache :: TcS CtTypeMap 
getFlatCache = getTcSInerts >>= (return . unFamHeadMap . inert_flat_cache)

updFlatCache :: Ct -> TcS ()
-- Pre: constraint is a flat family equation (equal to a flatten skolem)
updFlatCache flat_eq@(CFunEqCan { cc_ev = fl, cc_fun = tc, cc_tyargs = xis })
  = modifyInertTcS upd_inert_cache
  where upd_inert_cache is = ((), is { inert_flat_cache = FamHeadMap new_fc })
                           where new_fc = alterTM pred_key upd_cache fc
                                 fc = unFamHeadMap $ inert_flat_cache is
        pred_key = mkTyConApp tc xis
        upd_cache (Just ct) | cc_ev ct `canSolve` fl = Just ct 
        upd_cache (Just _ct) = Just flat_eq 
        upd_cache Nothing    = Just flat_eq
updFlatCache other_ct = pprPanic "updFlatCache: non-family constraint" $
                        ppr other_ct
                        

getUntouchables :: TcS TcsUntouchables
getUntouchables = TcS (return . tcs_untch)

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
  = do { ref <- getTcSTyBinds
       ; wrapTcS $ 
         do { ty_binds <- TcM.readTcRef ref
            ; when debugIsOn $
                  TcM.checkErr (not (tv `elemVarEnv` ty_binds)) $
                  vcat [ text "TERRIBLE ERROR: double set of meta type variable"
                       , ppr tv <+> text ":=" <+> ppr ty
                       , text "Old value =" <+> ppr (lookupVarEnv_NF ty_binds tv)]
            ; TcM.writeTcRef ref (extendVarEnv ty_binds tv (tv,ty)) } }


\end{code}

\begin{code}
warnTcS :: CtLoc orig -> Bool -> SDoc -> TcS ()
warnTcS loc warn_if doc 
  | warn_if   = wrapTcS $ TcM.setCtLoc loc $ TcM.addWarnTc doc
  | otherwise = return ()

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

checkWellStagedDFun :: PredType -> DFunId -> WantedLoc -> TcS () 
checkWellStagedDFun pred dfun_id loc 
  = wrapTcS $ TcM.setCtLoc loc $ 
    do { use_stage <- TcM.getStage
       ; TcM.checkWellStaged pp_thing bind_lvl (thLevel use_stage) }
  where
    pp_thing = ptext (sLit "instance for") <+> quotes (ppr pred)
    bind_lvl = TcM.topIdLvl dfun_id

pprEq :: TcType -> TcType -> SDoc
pprEq ty1 ty2 = pprType $ mkEqPred ty1 ty2

isTouchableMetaTyVar :: TcTyVar -> TcS Bool
isTouchableMetaTyVar tv 
  = do { untch <- getUntouchables
       ; return $ isTouchableMetaTyVar_InRange untch tv } 

isTouchableMetaTyVar_InRange :: TcsUntouchables -> TcTyVar -> Bool 
isTouchableMetaTyVar_InRange (untch,untch_tcs) tv 
  = ASSERT2 ( isTcTyVar tv, ppr tv )
    case tcTyVarDetails tv of 
      MetaTv TcsTv _ -> not (tv `elemVarSet` untch_tcs)
                        -- See Note [Touchable meta type variables] 
      MetaTv {}      -> inTouchableRange untch tv && not (tv `elemVarSet` untch_tcs)
      _              -> False 


\end{code}

Note [Do not add duplicate derived insolubles]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general we do want to add an insoluble (Int ~ Bool) even if there is one
such there already, because they may come from distinct call sites.  But for
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
keepWanted, then we will end up trying to solve the following
constraints the second time:

  (D [c] c) [W]
  (c ~ [c]) [D]

which will result in two Deriveds to end up in the insoluble set:

  wc_flat   = D [c] c [W]
  wc_insols = (c ~ [c]) [D], (c ~ [c]) [D]

Note [Touchable meta type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Meta type variables allocated *by the constraint solver itself* are always
touchable.  Example: 
   instance C a b => D [a] where...
if we use this instance declaration we "make up" a fresh meta type
variable for 'b', which we must later guess.  (Perhaps C has a
functional dependency.)  But since we aren't in the constraint *generator*
we can't allocate a Unique in the touchable range for this implication
constraint.  Instead, we mark it as a "TcsTv", which makes it always-touchable.


\begin{code}
-- Flatten skolems
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

newFlattenSkolemTy :: TcType -> TcS TcType
newFlattenSkolemTy ty = mkTyVarTy <$> newFlattenSkolemTyVar ty

newFlattenSkolemTyVar :: TcType -> TcS TcTyVar
newFlattenSkolemTyVar ty
  = do { tv <- wrapTcS $ 
               do { uniq <- TcM.newUnique
                  ; let name = TcM.mkTcTyVarName uniq (fsLit "f")
                  ; return $ mkTcTyVar name (typeKind ty) (FlatSkol ty) } 
       ; traceTcS "New Flatten Skolem Born" $
         ppr tv <+> text "[:= " <+> ppr ty <+> text "]"
       ; return tv }

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

instFlexiTcS :: [TKVar] -> TcS (TvSubst, [TcType])
-- Like TcM.instMetaTyVar but the variable that is created is 
-- always touchable; we are supposed to guess its instantiation.
-- See Note [Touchable meta type variables] 
instFlexiTcS tvs = wrapTcS (mapAccumLM inst_one emptyTvSubst tvs)
  where
     inst_one subst tv = do { ty' <- instFlexiTcSHelper (tyVarName tv) 
                                                        (substTy subst (tyVarKind tv))
                            ; return (extendTvSubst subst tv ty', ty') }

newFlexiTcSTy :: Kind -> TcS TcType  
newFlexiTcSTy knd 
  = wrapTcS $
    do { uniq <- TcM.newUnique 
       ; ref  <- TcM.newMutVar  Flexi 
       ; let name = TcM.mkTcTyVarName uniq (fsLit "uf")
       ; return $ mkTyVarTy (mkTcTyVar name knd (MetaTv TcsTv ref)) }

isFlexiTcsTv :: TyVar -> Bool
isFlexiTcsTv tv
  | not (isTcTyVar tv)                  = False
  | MetaTv TcsTv _ <- tcTyVarDetails tv = True
  | otherwise                           = False

instFlexiTcSHelper :: Name -> Kind -> TcM TcType
instFlexiTcSHelper tvname tvkind
  = do { uniq <- TcM.newUnique 
       ; ref  <- TcM.newMutVar  Flexi 
       ; let name = setNameUnique tvname uniq 
             kind = tvkind 
       ; return (mkTyVarTy (mkTcTyVar name kind (MetaTv TcsTv ref))) }

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

newGivenEvVar :: GivenLoc -> TcPredType -> EvTerm -> TcS CtEvidence
-- Make a new variable of the given PredType, 
-- immediately bind it to the given term
-- and return its CtEvidence
newGivenEvVar gloc pred rhs
  = do { new_ev <- wrapTcS $ TcM.newEvVar pred
       ; setEvBind new_ev rhs
       ; return (Given { ctev_gloc = gloc, ctev_pred = pred, ctev_evtm = EvId new_ev }) }

newWantedEvVar :: WantedLoc -> TcPredType -> TcS MaybeNew
newWantedEvVar loc pty
  = do { is <- getTcSInerts
       ; case lookupInInerts is pty of
            Just ctev | not (isDerived ctev) 
                      -> do { traceTcS "newWantedEvVar/cache hit" $ ppr ctev
                            ; return (Cached (ctEvTerm ctev)) }
            _ -> do { new_ev <- wrapTcS $ TcM.newEvVar pty
                    ; traceTcS "newWantedEvVar/cache miss" $ ppr new_ev
                    ; let ctev = Wanted { ctev_wloc = loc
                                        , ctev_pred = pty
                                        , ctev_evar = new_ev }
                    ; return (Fresh ctev) } }

newDerived :: WantedLoc -> TcPredType -> TcS (Maybe CtEvidence)
-- Returns Nothing    if cached, 
--         Just pred  if not cached
newDerived loc pty
  = do { is <- getTcSInerts
       ; case lookupInInerts is pty of
            Just {} -> return Nothing
            _       -> return (Just Derived { ctev_wloc = loc
                                            , ctev_pred = pty }) }

instDFunConstraints :: WantedLoc -> TcThetaType -> TcS [MaybeNew]
instDFunConstraints wl = mapM (newWantedEvVar wl)
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
xCtFlavor :: CtEvidence              -- Original flavor   
          -> [TcPredType]          -- New predicate types
          -> XEvTerm               -- Instructions about how to manipulate evidence
          -> TcS [CtEvidence]
xCtFlavor (Given { ctev_gloc = gl, ctev_evtm = tm }) ptys xev
  = ASSERT( equalLength ptys (ev_decomp xev tm) )
    zipWithM (newGivenEvVar gl) ptys (ev_decomp xev tm)
    -- See Note [Bind new Givens immediately]
  
xCtFlavor (Wanted { ctev_wloc = wl, ctev_evar = evar }) ptys xev
  = do { new_evars <- mapM (newWantedEvVar wl) ptys
       ; setEvBind evar (ev_comp xev (getEvTerms new_evars))
       ; return (freshGoals new_evars) }
    
xCtFlavor (Derived { ctev_wloc = wl }) ptys _xev
  = do { ders <- mapM (newDerived wl) ptys
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
Main purpose: create a new identity (flavor) for new_pred;
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

        Solved          NEVER HAPPENS
-}

-- Returns Just new_fl iff either (i)  'co' is reflexivity
--                             or (ii) 'co' is not reflexivity, and 'new_pred' not cached
-- In either case, there is nothing new to do with new_fl

-- If derived, don't even look at the coercion
-- NB: this allows us to sneak away with ``error'' thunks for 
-- coercions that come from derived ids (which don't exist!) 
rewriteCtFlavor (Derived { ctev_wloc = wl }) pty_new _co
  = newDerived wl pty_new
        
rewriteCtFlavor (Given { ctev_gloc = gl, ctev_evtm = old_tm }) pty_new co
  = do { new_ev <- newGivenEvVar gl pty_new new_tm  -- See Note [Bind new Givens immediately]
       ; return (Just new_ev) }
  where
    new_tm = mkEvCast old_tm (mkTcSymCo co)  -- mkEvCase optimises ReflCo
  
rewriteCtFlavor ctev@(Wanted { ctev_wloc = wl, ctev_evar = evar, ctev_pred = pty_old }) pty_new co
  | isTcReflCo co  -- If just reflexivity then you may re-use the same variable
  = return (Just (if pty_old `eqType` pty_new 
                  then ctev 
                  else ctev { ctev_pred = pty_new }))
       -- If the old and new types compare equal (eqType looks through synonyms)
       -- then retain the old type, so that error messages come out mentioning synonyms

  | otherwise
  = do { new_evar <- newWantedEvVar wl pty_new
       ; setEvBind evar (mkEvCast (getEvTerm new_evar) co)
       ; case new_evar of
            Fresh ctev -> return (Just ctev) 
            _          -> return Nothing }



-- Matching and looking up classes and family instances
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data MatchInstResult mi
  = MatchInstNo         -- No matching instance 
  | MatchInstSingle mi  -- Single matching instance
  | MatchInstMany       -- Multiple matching instances


matchClass :: Class -> [Type] -> TcS (MatchInstResult (DFunId, [Maybe TcType])) 
-- Look up a class constraint in the instance environment
matchClass clas tys
  = do	{ let pred = mkClassPred clas tys 
        ; instEnvs <- getInstEnvs
        ; case lookupInstEnv instEnvs clas tys of {
            ([], _unifs, _)               -- Nothing matches  
                -> do { traceTcS "matchClass not matching" $ 
                        vcat [ text "dict" <+> ppr pred
                             , ppr instEnvs ]
                        
                      ; return MatchInstNo  
                      } ;  
	    ([(ispec, inst_tys)], [], _) -- A single match 
		-> do	{ let dfun_id = is_dfun ispec
			; traceTcS "matchClass success" $
                          vcat [text "dict" <+> ppr pred, 
                                text "witness" <+> ppr dfun_id
                                               <+> ppr (idType dfun_id) ]
				  -- Record that this dfun is needed
                        ; return $ MatchInstSingle (dfun_id, inst_tys)
                        } ;
     	    (matches, _unifs, _)          -- More than one matches 
		-> do	{ traceTcS "matchClass multiple matches, deferring choice" $
                          vcat [text "dict" <+> ppr pred,
                                text "matches" <+> ppr matches]
                        ; return MatchInstMany 
		        }
	}
        }

matchFam :: TyCon -> [Type] -> TcS (Maybe (FamInst, [Type]))
matchFam tycon args = wrapTcS $ tcLookupFamInst tycon args
\end{code}

\begin{code}
-- Deferring forall equalities as implications
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

deferTcSForAllEq :: (WantedLoc,EvVar)  -- Original wanted equality flavor
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
        ; mev <- newWantedEvVar loc (mkTcEqPred phi1 phi2)
        ; coe_inside <- case mev of
            Cached ev_tm -> return (evTermCoercion ev_tm)
            Fresh ctev   -> do { ev_binds_var <- wrapTcS $ TcM.newTcEvBinds
                               ; let ev_binds = TcEvBinds ev_binds_var
                                     new_ct = mkNonCanonical ctev
              			     new_co = evTermCoercion (ctEvTerm ctev)
                               ; lcl_env <- wrapTcS $ TcM.getLclTypeEnv
                               ; loc <- wrapTcS $ TcM.getCtLoc skol_info
                               ; let wc = WC { wc_flat  = singleCt new_ct 
                                             , wc_impl  = emptyBag
                                             , wc_insol = emptyCts }
                                     imp = Implic { ic_untch  = all_untouchables
                                                  , ic_env    = lcl_env
                                                  , ic_skols  = skol_tvs
                                                  , ic_given  = []
                                                  , ic_wanted = wc 
                                                  , ic_insol  = False
                                                  , ic_binds  = ev_binds_var
                                                  , ic_loc    = loc }
                               ; updTcSImplics (consBag imp) 
                               ; return (TcLetCo ev_binds new_co) }

        ; setEvBind orig_ev $
          EvCoercion (foldr mkTcForAllCo coe_inside skol_tvs)
        }
 where all_untouchables = TouchableRange u u
       u = idUnique orig_ev -- HACK: empty range

\end{code}



-- Rewriting with respect to the inert equalities 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
getInertEqs :: TcS (TyVarEnv Ct, InScopeSet)
getInertEqs = do { inert <- getTcSInerts
                 ; let ics = inert_cans inert
                 ; return (inert_eqs ics, inert_eq_tvs ics) }

getCtCoercion :: EvBindMap -> Ct -> TcCoercion
-- Precondition: A CTyEqCan which is either Wanted or Given, never Derived or Solved!
getCtCoercion _bs ct 
  = ASSERT( not (isDerivedCt ct) )
    evTermCoercion (ctEvTerm (ctEvidence ct))
\end{code}


