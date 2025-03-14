{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}


module GHC.Core.TyCon.Set (
        -- * TyCons set type
        TyConSet,

        -- ** Manipulating these sets
        emptyTyConSet, unitTyConSet, mkTyConSet, unionTyConSet, unionTyConSets,
        minusTyConSet, elemTyConSet, extendTyConSet, extendTyConSetList,
        delFromTyConSet, delListFromTyConSet, isEmptyTyConSet, filterTyConSet,
        intersectsTyConSet, disjointTyConSet, intersectTyConSet,
        nameSetAny, nameSetAll
    ) where

import GHC.Prelude

import GHC.Types.Unique.Set
import GHC.Core.TyCon (TyCon)

type TyConSet = UniqSet TyCon

emptyTyConSet       :: TyConSet
unitTyConSet        :: TyCon -> TyConSet
extendTyConSetList   :: TyConSet -> [TyCon] -> TyConSet
extendTyConSet    :: TyConSet -> TyCon -> TyConSet
mkTyConSet          :: [TyCon] -> TyConSet
unionTyConSet      :: TyConSet -> TyConSet -> TyConSet
unionTyConSets  :: [TyConSet] -> TyConSet
minusTyConSet       :: TyConSet -> TyConSet -> TyConSet
elemTyConSet        :: TyCon -> TyConSet -> Bool
isEmptyTyConSet     :: TyConSet -> Bool
delFromTyConSet     :: TyConSet -> TyCon -> TyConSet
delListFromTyConSet :: TyConSet -> [TyCon] -> TyConSet
filterTyConSet      :: (TyCon -> Bool) -> TyConSet -> TyConSet
intersectTyConSet   :: TyConSet -> TyConSet -> TyConSet
intersectsTyConSet  :: TyConSet -> TyConSet -> Bool
-- ^ True if there is a non-empty intersection.
-- @s1 `intersectsTyConSet` s2@ doesn't compute @s2@ if @s1@ is empty
disjointTyConSet    :: TyConSet -> TyConSet -> Bool

isEmptyTyConSet    = isEmptyUniqSet
emptyTyConSet      = emptyUniqSet
unitTyConSet       = unitUniqSet
mkTyConSet         = mkUniqSet
extendTyConSetList = addListToUniqSet
extendTyConSet     = addOneToUniqSet
unionTyConSet      = unionUniqSets
unionTyConSets     = unionManyUniqSets
minusTyConSet      = minusUniqSet
elemTyConSet       = elementOfUniqSet
delFromTyConSet    = delOneFromUniqSet
filterTyConSet     = filterUniqSet
intersectTyConSet  = intersectUniqSets
disjointTyConSet   = disjointUniqSets


delListFromTyConSet set ns = foldl' delFromTyConSet set ns

intersectsTyConSet s1 s2 = not (isEmptyTyConSet (s1 `intersectTyConSet` s2))

nameSetAny :: (TyCon -> Bool) -> TyConSet -> Bool
nameSetAny = uniqSetAny

nameSetAll :: (TyCon -> Bool) -> TyConSet -> Bool
nameSetAll = uniqSetAll
