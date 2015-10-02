{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE CPP #-}

module VarSet (
        -- * Var, Id and TyVar set types
        VarSet, IdSet, TyVarSet, CoVarSet,

        -- ** Manipulating these sets
        emptyVarSet, unitVarSet, mkVarSet,
        extendVarSet, extendVarSetList, extendVarSet_C,
        elemVarSet, varSetElems, subVarSet,
        unionVarSet, unionVarSets, mapUnionVarSet,
        intersectVarSet, intersectsVarSet, disjointVarSet,
        isEmptyVarSet, delVarSet, delVarSetList, delVarSetByKey,
        minusVarSet, foldVarSet, filterVarSet,
        transCloVarSet, fixVarSet,
        lookupVarSet, mapVarSet, sizeVarSet, seqVarSet,
        elemVarSetByKey, partitionVarSet
    ) where

#include "HsVersions.h"

import Var      ( Var, TyVar, CoVar, Id )
import Unique
import UniqSet
import UniqFM( disjointUFM )

{-
************************************************************************
*                                                                      *
\subsection{@VarSet@s}
*                                                                      *
************************************************************************
-}

type VarSet       = UniqSet Var
type IdSet        = UniqSet Id
type TyVarSet     = UniqSet TyVar
type CoVarSet     = UniqSet CoVar

emptyVarSet     :: VarSet
intersectVarSet :: VarSet -> VarSet -> VarSet
unionVarSet     :: VarSet -> VarSet -> VarSet
unionVarSets    :: [VarSet] -> VarSet

mapUnionVarSet  :: (a -> VarSet) -> [a] -> VarSet
-- ^ map the function oer the list, and union the results

varSetElems     :: VarSet -> [Var]
unitVarSet      :: Var -> VarSet
extendVarSet    :: VarSet -> Var -> VarSet
extendVarSetList:: VarSet -> [Var] -> VarSet
elemVarSet      :: Var -> VarSet -> Bool
delVarSet       :: VarSet -> Var -> VarSet
delVarSetList   :: VarSet -> [Var] -> VarSet
minusVarSet     :: VarSet -> VarSet -> VarSet
isEmptyVarSet   :: VarSet -> Bool
mkVarSet        :: [Var] -> VarSet
foldVarSet      :: (Var -> a -> a) -> a -> VarSet -> a
lookupVarSet    :: VarSet -> Var -> Maybe Var
                        -- Returns the set element, which may be
                        -- (==) to the argument, but not the same as
mapVarSet       :: (Var -> Var) -> VarSet -> VarSet
sizeVarSet      :: VarSet -> Int
filterVarSet    :: (Var -> Bool) -> VarSet -> VarSet
extendVarSet_C  :: (Var->Var->Var) -> VarSet -> Var -> VarSet

delVarSetByKey  :: VarSet -> Unique -> VarSet
elemVarSetByKey :: Unique -> VarSet -> Bool
partitionVarSet :: (Var -> Bool) -> VarSet -> (VarSet, VarSet)

emptyVarSet     = emptyUniqSet
unitVarSet      = unitUniqSet
extendVarSet    = addOneToUniqSet
extendVarSetList= addListToUniqSet
intersectVarSet = intersectUniqSets

intersectsVarSet:: VarSet -> VarSet -> Bool     -- True if non-empty intersection
disjointVarSet  :: VarSet -> VarSet -> Bool     -- True if empty intersection
subVarSet       :: VarSet -> VarSet -> Bool     -- True if first arg is subset of second
        -- (s1 `intersectsVarSet` s2) doesn't compute s2 if s1 is empty;
        -- ditto disjointVarSet, subVarSet

unionVarSet     = unionUniqSets
unionVarSets    = unionManyUniqSets
varSetElems     = uniqSetToList
elemVarSet      = elementOfUniqSet
minusVarSet     = minusUniqSet
delVarSet       = delOneFromUniqSet
delVarSetList   = delListFromUniqSet
isEmptyVarSet   = isEmptyUniqSet
mkVarSet        = mkUniqSet
foldVarSet      = foldUniqSet
lookupVarSet    = lookupUniqSet
mapVarSet       = mapUniqSet
sizeVarSet      = sizeUniqSet
filterVarSet    = filterUniqSet
extendVarSet_C  = addOneToUniqSet_C
delVarSetByKey  = delOneFromUniqSet_Directly
elemVarSetByKey = elemUniqSet_Directly
partitionVarSet = partitionUniqSet

mapUnionVarSet get_set xs = foldr (unionVarSet . get_set) emptyVarSet xs

-- See comments with type signatures
intersectsVarSet s1 s2 = not (s1 `disjointVarSet` s2)
disjointVarSet   s1 s2 = disjointUFM s1 s2
subVarSet        s1 s2 = isEmptyVarSet (s1 `minusVarSet` s2)

fixVarSet :: (VarSet -> VarSet)   -- Map the current set to a new set
          -> VarSet -> VarSet
-- (fixVarSet f s) repeatedly applies f to the set s, 
-- until it reaches a fixed point.
fixVarSet fn vars
  | new_vars `subVarSet` vars = vars
  | otherwise                 = fixVarSet fn new_vars
  where
    new_vars = fn vars

transCloVarSet :: (VarSet -> VarSet)
                  -- Map some variables in the set to
                  -- extra variables that should be in it
               -> VarSet -> VarSet
-- (transCloVarSet f s) repeatedly applies f to new candidates, adding any
-- new variables to s that it finds thereby, until it reaches a fixed point.
--
-- The function fn could be (Var -> VarSet), but we use (VarSet -> VarSet)
-- for efficiency, so that the test can be batched up.
-- It's essential that fn will work fine if given new candidates
-- one at at time; ie  fn {v1,v2} = fn v1 `union` fn v2
-- Use fixVarSet if the function needs to see the whole set all at once
transCloVarSet fn seeds
  = go seeds seeds
  where
    go :: VarSet  -- Accumulating result
       -> VarSet  -- Work-list; un-processed subset of accumulating result
       -> VarSet
    -- Specification: go acc vs = acc `union` transClo fn vs

    go acc candidates
       | isEmptyVarSet new_vs = acc
       | otherwise            = go (acc `unionVarSet` new_vs) new_vs
       where
         new_vs = fn candidates `minusVarSet` acc

seqVarSet :: VarSet -> ()
seqVarSet s = sizeVarSet s `seq` ()
