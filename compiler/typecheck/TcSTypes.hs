{-# LANGUAGE TypeFamilies #-}
module TcSTypes where

import TyCon
import Class
import TcRnTypes
import TcEvidence
import TcType

import VarEnv
import Var

import Maybes
import Unique
import UniqFM
import Outputable
import FastString
import Bag
import TrieMap

import Control.Applicative
import Control.Monad
import Data.IORef



data Deque a = DQ [a] [a]   -- Insert in RH field, remove from LH field
                            -- First to remove is at head of LH field

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



instance Outputable a => Outputable (Deque a) where
  ppr (DQ as bs) = ppr (as ++ reverse bs)   -- Show first one to come out at the start
-- See Note [WorkList priorities]
data WorkList = WorkList { wl_eqs    :: [Ct]
                         , wl_funeqs :: Deque Ct
                         , wl_rest   :: [Ct]
                         }




-- All Given (fully known) or Wanted or Derived
-- See Note [Detailed InertCans Invariants] for more
data InertCans
  = IC { inert_eqs :: TyVarEnv EqualCtList
              -- All CTyEqCans; index is the LHS tyvar
              -- Some Refl equalities are also in tcs_ty_binds
              -- see Note [Spontaneously solved in TyBinds] in TcInteract

       , inert_funeqs :: FunEqMap Ct
              -- All CFunEqCans; index is the whole family head type.

       , inert_dicts :: DictMap Ct
              -- Dictionaries only, index is the class
              -- NB: index is /not/ the whole type because FD reactions
              -- need to match the class but not necessarily the whole type.

       , inert_irreds :: Cts
              -- Irreducible predicates

       , inert_insols :: Cts
              -- Frozen errors (as non-canonicals)
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

       , inert_flat_cache :: FunEqMap (TcCoercion, TcTyVar)
              -- See Note [Type family equations]
              -- If    F tys :-> (co, fsk), 
              -- then  co :: F tys ~ fsk
              -- Just a hash-cons cache for use when flattening only
              -- These include entirely un-processed goals, so don't use
              -- them to solve a top-level goal, else you may end up solving
              -- (w:F ty ~ a) by setting w:=w!  We just use the flat-cache
              -- when allocating a new flatten-skolem.
              -- Not necessarily inert wrt top-level equations (or inert_cans)

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
                 , ptext (sLit "Dictionaries:")
                   <+> vcat (map ppr (Bag.bagToList $ dictsToBag (inert_dicts ics)))
                 , ptext (sLit "Irreds:")
                   <+> vcat (map ppr (Bag.bagToList $ inert_irreds ics))
                 , text "Insolubles =" <+> -- Clearly print frozen errors
                    braces (vcat (map ppr (Bag.bagToList $ inert_insols ics)))
                 ]

instance Outputable InertSet where
  ppr is = vcat [ ppr $ inert_cans is
                , text "Solved dicts"  <+> int (sizeDictMap (inert_solved_dicts is)) ]


{-
%************************************************************************
%*                                                                      *
                   TyEqMap
%*                                                                      *
%************************************************************************
-}
type TyEqMap a = TyVarEnv a

findTyEqs :: TyEqMap EqualCtList -> TyVar -> EqualCtList
findTyEqs m tv = lookupVarEnv m tv `orElse` []




{-
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
-}

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
      tcs_implics  :: IORef (Bag Implication)
    }

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


data XEvTerm
  = XEvTerm { ev_preds  :: [PredType]           -- New predicate types
            , ev_comp   :: [EvTerm] -> EvTerm   -- How to compose evidence
            , ev_decomp :: EvTerm -> [EvTerm]   -- How to decompose evidence
            -- In both ev_comp and ev_decomp, the [EvTerm] is 1-1 with ev_preds
            -- and each EvTerm has type of the corresponding EvPred
            }

data MaybeNew = Fresh CtEvidence | Cached EvTerm




{-
%************************************************************************
%*                                                                      *
                   TcAppMap, DictMap, FunEqMap
%*                                                                      *
%************************************************************************
-}

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

-- mapTcApp :: (a->b) -> TcAppMap a -> TcAppMap b
-- mapTcApp f = mapUFM (mapTM f)

filterTcAppMap :: (Ct -> Bool) -> TcAppMap Ct -> TcAppMap Ct
filterTcAppMap f m
  = mapUFM do_tm m
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

------------------------
type FunEqMap a = TcAppMap a  -- A map whose key is a (TyCon, [Type]) pair

emptyFunEqs :: TcAppMap a
emptyFunEqs = emptyTcAppMap

sizeFunEqMap :: FunEqMap a -> Int
sizeFunEqMap m = foldFunEqs (\ _ x -> x+1) m 0

findFunEq :: FunEqMap a -> TyCon -> [Type] -> Maybe a
findFunEq m tc tys = findTcApp m (getUnique tc) tys

findFunEqs :: FunEqMap a -> TyCon -> [Type] -> Maybe a
findFunEqs m tc tys = findTcApp m (getUnique tc) tys

funEqsToList :: FunEqMap a -> [a]
funEqsToList m = foldTcAppMap (:) m []

findFunEqsByTyCon :: FunEqMap a -> TyCon -> [a]
-- Get inert function equation constraints that have the given tycon
-- in their head.  Not that the constraints remain in the inert set.
-- We use this to check for derived interactions with built-in type-function
-- constructors.
findFunEqsByTyCon m tc
  | Just tm <- lookupUFM m tc = foldTM (:) tm []
  | otherwise                 = []

foldFunEqs :: (a -> b -> b) -> FunEqMap a -> b -> b
foldFunEqs = foldTcAppMap

-- mapFunEqs :: (a -> b) -> FunEqMap a -> FunEqMap b
-- mapFunEqs = mapTcApp

filterFunEqs :: (Ct -> Bool) -> FunEqMap Ct -> FunEqMap Ct
filterFunEqs = filterTcAppMap

insertFunEq :: FunEqMap a -> TyCon -> [Type] -> a -> FunEqMap a
insertFunEq m tc tys val = insertTcApp m (getUnique tc) tys val

insertFunEqCt :: FunEqMap Ct -> Ct -> FunEqMap Ct
insertFunEqCt m ct@(CFunEqCan { cc_fun = tc, cc_tyargs = tys })
  = insertFunEq m tc tys ct
insertFunEqCt _ ct = pprPanic "insertFunEqCt" (ppr ct)

partitionFunEqs :: (Ct -> Bool) -> FunEqMap Ct -> (Bag Ct, FunEqMap Ct)
partitionFunEqs f m = foldTcAppMap k m (emptyBag, emptyFunEqs)
  where
    k ct (yeses, noes)
      | f ct      = (yeses `snocBag` ct, noes)
      | otherwise = (yeses, insertFunEqCt noes ct)




