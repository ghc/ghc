{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[TyConEnv]{@TyConEnv@: tyCon environments}
-}


{-# LANGUAGE ScopedTypeVariables #-}


module GHC.Core.TyCon.Env (
        -- * TyCon environment (map)
        TyConEnv,

        -- ** Manipulating these environments
        mkTyConEnv, mkTyConEnvWith,
        emptyTyConEnv, isEmptyTyConEnv,
        unitTyConEnv, nonDetTyConEnvElts,
        extendTyConEnv_C, extendTyConEnv_Acc, extendTyConEnv,
        extendTyConEnvList, extendTyConEnvList_C,
        filterTyConEnv, anyTyConEnv,
        plusTyConEnv, plusTyConEnv_C, plusTyConEnv_CD, plusTyConEnv_CD2, alterTyConEnv,
        lookupTyConEnv, lookupTyConEnv_NF, delFromTyConEnv, delListFromTyConEnv,
        elemTyConEnv, mapTyConEnv, disjointTyConEnv,

        DTyConEnv,

        emptyDTyConEnv, isEmptyDTyConEnv,
        lookupDTyConEnv,
        delFromDTyConEnv, filterDTyConEnv,
        mapDTyConEnv, mapMaybeDTyConEnv,
        adjustDTyConEnv, alterDTyConEnv, extendDTyConEnv, foldDTyConEnv
    ) where

import GHC.Prelude

import GHC.Types.Unique.FM
import GHC.Types.Unique.DFM
import GHC.Core.TyCon (TyCon)

import GHC.Data.Maybe

{-
************************************************************************
*                                                                      *
\subsection{TyCon environment}
*                                                                      *
************************************************************************
-}

-- | TyCon Environment
type TyConEnv a = UniqFM TyCon a       -- Domain is TyCon

emptyTyConEnv       :: TyConEnv a
isEmptyTyConEnv     :: TyConEnv a -> Bool
mkTyConEnv          :: [(TyCon,a)] -> TyConEnv a
mkTyConEnvWith      :: (a -> TyCon) -> [a] -> TyConEnv a
nonDetTyConEnvElts  :: TyConEnv a -> [a]
alterTyConEnv       :: (Maybe a-> Maybe a) -> TyConEnv a -> TyCon -> TyConEnv a
extendTyConEnv_C    :: (a->a->a) -> TyConEnv a -> TyCon -> a -> TyConEnv a
extendTyConEnv_Acc  :: (a->b->b) -> (a->b) -> TyConEnv b -> TyCon -> a -> TyConEnv b
extendTyConEnv      :: TyConEnv a -> TyCon -> a -> TyConEnv a
plusTyConEnv        :: TyConEnv a -> TyConEnv a -> TyConEnv a
plusTyConEnv_C      :: (a->a->a) -> TyConEnv a -> TyConEnv a -> TyConEnv a
plusTyConEnv_CD     :: (a->a->a) -> TyConEnv a -> a -> TyConEnv a -> a -> TyConEnv a
plusTyConEnv_CD2    :: (Maybe a->Maybe a->a) -> TyConEnv a -> TyConEnv a -> TyConEnv a
extendTyConEnvList  :: TyConEnv a -> [(TyCon,a)] -> TyConEnv a
extendTyConEnvList_C :: (a->a->a) -> TyConEnv a -> [(TyCon,a)] -> TyConEnv a
delFromTyConEnv     :: TyConEnv a -> TyCon -> TyConEnv a
delListFromTyConEnv :: TyConEnv a -> [TyCon] -> TyConEnv a
elemTyConEnv        :: TyCon -> TyConEnv a -> Bool
unitTyConEnv        :: TyCon -> a -> TyConEnv a
lookupTyConEnv      :: TyConEnv a -> TyCon -> Maybe a
lookupTyConEnv_NF   :: TyConEnv a -> TyCon -> a
filterTyConEnv      :: (elt -> Bool) -> TyConEnv elt -> TyConEnv elt
anyTyConEnv         :: (elt -> Bool) -> TyConEnv elt -> Bool
mapTyConEnv         :: (elt1 -> elt2) -> TyConEnv elt1 -> TyConEnv elt2
disjointTyConEnv    :: TyConEnv a -> TyConEnv a -> Bool

nonDetTyConEnvElts x   = nonDetEltsUFM x
emptyTyConEnv          = emptyUFM
isEmptyTyConEnv        = isNullUFM
unitTyConEnv x y       = unitUFM x y
extendTyConEnv x y z   = addToUFM x y z
extendTyConEnvList x l = addListToUFM x l
lookupTyConEnv x y     = lookupUFM x y
alterTyConEnv          = alterUFM
mkTyConEnv     l       = listToUFM l
mkTyConEnvWith f       = mkTyConEnv . map (\a -> (f a, a))
elemTyConEnv x y          = elemUFM x y
plusTyConEnv x y          = plusUFM x y
plusTyConEnv_C f x y      = plusUFM_C f x y
plusTyConEnv_CD f x d y b = plusUFM_CD f x d y b
plusTyConEnv_CD2 f x y    = plusUFM_CD2 f x y
extendTyConEnv_C f x y z  = addToUFM_C f x y z
mapTyConEnv f x           = mapUFM f x
extendTyConEnv_Acc x y z a b  = addToUFM_Acc x y z a b
extendTyConEnvList_C x y z = addListToUFM_C x y z
delFromTyConEnv x y      = delFromUFM x y
delListFromTyConEnv x y  = delListFromUFM x y
filterTyConEnv x y       = filterUFM x y
anyTyConEnv f x          = foldUFM ((||) . f) False x
disjointTyConEnv x y     = disjointUFM x y

lookupTyConEnv_NF env n = expectJust "lookupTyConEnv_NF" (lookupTyConEnv env n)

-- | Deterministic TyCon Environment
--
-- See Note [Deterministic UniqFM] in "GHC.Types.Unique.DFM" for explanation why
-- we need DTyConEnv.
type DTyConEnv a = UniqDFM TyCon a

emptyDTyConEnv :: DTyConEnv a
emptyDTyConEnv = emptyUDFM

isEmptyDTyConEnv :: DTyConEnv a -> Bool
isEmptyDTyConEnv = isNullUDFM

lookupDTyConEnv :: DTyConEnv a -> TyCon -> Maybe a
lookupDTyConEnv = lookupUDFM

delFromDTyConEnv :: DTyConEnv a -> TyCon -> DTyConEnv a
delFromDTyConEnv = delFromUDFM

filterDTyConEnv :: (a -> Bool) -> DTyConEnv a -> DTyConEnv a
filterDTyConEnv = filterUDFM

mapDTyConEnv :: (a -> b) -> DTyConEnv a -> DTyConEnv b
mapDTyConEnv = mapUDFM

mapMaybeDTyConEnv :: (a -> Maybe b) -> DTyConEnv a -> DTyConEnv b
mapMaybeDTyConEnv = mapMaybeUDFM

adjustDTyConEnv :: (a -> a) -> DTyConEnv a -> TyCon -> DTyConEnv a
adjustDTyConEnv = adjustUDFM

alterDTyConEnv :: (Maybe a -> Maybe a) -> DTyConEnv a -> TyCon -> DTyConEnv a
alterDTyConEnv = alterUDFM

extendDTyConEnv :: DTyConEnv a -> TyCon -> a -> DTyConEnv a
extendDTyConEnv = addToUDFM

foldDTyConEnv :: (elt -> a -> a) -> a -> DTyConEnv elt -> a
foldDTyConEnv = foldUDFM
