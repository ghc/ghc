{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | Utility types used within the constraint solver
module GHC.Tc.Solver.Types (
    -- Inert CDictCans
    DictMap, emptyDictMap, findDictsByClass, addDict,
    addDictsByClass, delDict, foldDicts, filterDicts, findDict,
    dictsToBag, partitionDicts,

    FunEqMap, emptyFunEqs, foldFunEqs, findFunEq, insertFunEq,
    findFunEqsByTyCon,

    TcAppMap, emptyTcAppMap, isEmptyTcAppMap,
    insertTcApp, alterTcApp, filterTcAppMap,
    tcAppMapToBag, foldTcAppMap,

    EqualCtList, filterEqualCtList, addToEqualCtList
  ) where

import GHC.Prelude

import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcType

import GHC.Core.Class
import GHC.Core.Map.Type
import GHC.Core.Predicate
import GHC.Core.TyCon
import GHC.Core.TyCon.Env

import GHC.Data.Bag
import GHC.Data.Maybe
import GHC.Data.TrieMap
import GHC.Utils.Constants
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain

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

alterTcApp :: forall a. TcAppMap a -> TyCon -> [Type] -> XT a -> TcAppMap a
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

type DictMap a = TcAppMap a

emptyDictMap :: DictMap a
emptyDictMap = emptyTcAppMap

findDict :: DictMap a -> CtLoc -> Class -> [Type] -> Maybe a
findDict m loc cls tys
  | hasIPSuperClasses cls tys -- See Note [Tuples hiding implicit parameters]
  = Nothing

  | Just {} <- isCallStackPred cls tys
  , isPushCallStackOrigin (ctLocOrigin loc)
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
partitionDicts f m = foldTcAppMap k m (emptyBag, emptyDictMap)
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

{- Note [Tuples hiding implicit parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   f,g :: (?x::Int, C a) => a -> a
   f v = let ?x = 4 in g v

The call to 'g' gives rise to a Wanted constraint (?x::Int, C a).
We must /not/ solve this from the Given (?x::Int, C a), because of
the intervening binding for (?x::Int).  #14218.

We deal with this by arranging that we always fail when looking up a
tuple constraint that hides an implicit parameter. Note that this applies
  * both to the inert_dicts (lookupInertDict)
  * and to the solved_dicts (looukpSolvedDict)
An alternative would be not to extend these sets with such tuple
constraints, but it seemed more direct to deal with the lookup.

Note [Solving CallStack constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also Note [Overview of implicit CallStacks] in GHc.Tc.Types.Evidence.

Suppose f :: HasCallStack => blah.  Then

* Each call to 'f' gives rise to
    [W] s1 :: IP "callStack" CallStack    -- CtOrigin = OccurrenceOf f
  with a CtOrigin that says "OccurrenceOf f".
  Remember that HasCallStack is just shorthand for
    IP "callStack" CallStack
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
up such constraints.
-}

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
-- We use this to check for wanted interactions with built-in type-function
-- constructors.
findFunEqsByTyCon m tc
  | Just tm <- lookupDTyConEnv m tc = foldTM (:) tm []
  | otherwise                       = []

foldFunEqs :: (a -> b -> b) -> FunEqMap a -> b -> b
foldFunEqs = foldTcAppMap

insertFunEq :: FunEqMap a -> TyCon -> [Type] -> a -> FunEqMap a
insertFunEq m tc tys val = insertTcApp m tc tys val

{- *********************************************************************
*                                                                      *
                   EqualCtList
*                                                                      *
********************************************************************* -}

{-
Note [EqualCtList invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    * All are equalities
    * All these equalities have the same LHS
    * No element of the list can rewrite any other

Accordingly, this list is either empty, contains one element, or
contains a Given representational equality and a Wanted nominal one.
-}

type EqualCtList = [Ct]
  -- See Note [EqualCtList invariants]

addToEqualCtList :: Ct -> EqualCtList -> EqualCtList
-- See Note [EqualCtList invariants]
addToEqualCtList ct old_eqs
  | debugIsOn
  = case ct of
      CEqCan { cc_lhs = TyVarLHS tv } ->
        let shares_lhs (CEqCan { cc_lhs = TyVarLHS old_tv }) = tv == old_tv
            shares_lhs _other                                = False
        in
        assert (all shares_lhs old_eqs) $
        assert (null ([ (ct1, ct2) | ct1 <- ct : old_eqs
                                   , ct2 <- ct : old_eqs
                                   , let { fr1 = ctFlavourRole ct1
                                         ; fr2 = ctFlavourRole ct2 }
                                   , fr1 `eqCanRewriteFR` fr2 ])) $
        (ct : old_eqs)

      _ -> pprPanic "addToEqualCtList not CEqCan" (ppr ct)

  | otherwise
  = ct : old_eqs

-- returns Nothing when the new list is empty, to keep the environments smaller
filterEqualCtList :: (Ct -> Bool) -> EqualCtList -> Maybe EqualCtList
filterEqualCtList pred cts
  | null new_list
  = Nothing
  | otherwise
  = Just new_list
  where
    new_list = filter pred cts
