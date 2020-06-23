{-
%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
-}

-- | FastStringEnv: FastString environments
module GHC.Data.FastString.Env (
        -- * FastString environments (maps)
        FastStringEnv,

        -- ** Manipulating these environments
        mkFsEnv,
        emptyFsEnv, unitFsEnv,
        extendFsEnv_C, extendFsEnv_Acc, extendFsEnv,
        extendFsEnvList, extendFsEnvList_C,
        filterFsEnv,
        plusFsEnv, plusFsEnv_C, alterFsEnv,
        lookupFsEnv, lookupFsEnv_NF, delFromFsEnv, delListFromFsEnv,
        elemFsEnv, mapFsEnv,

        -- * Deterministic FastString environments (maps)
        DFastStringEnv,

        -- ** Manipulating these environments
        mkDFsEnv, emptyDFsEnv, dFsEnvElts, lookupDFsEnv
    ) where

import GHC.Prelude

import GHC.Types.Unique.FM
import GHC.Types.Unique.DFM
import GHC.Data.Maybe
import GHC.Data.FastString


-- | A non-deterministic set of FastStrings.
-- See Note [Deterministic UniqFM] in "GHC.Types.Unique.DFM" for explanation why it's not
-- deterministic and why it matters. Use DFastStringEnv if the set eventually
-- gets converted into a list or folded over in a way where the order
-- changes the generated code.
type FastStringEnv a = UniqFM FastString a  -- Domain is FastString

emptyFsEnv         :: FastStringEnv a
mkFsEnv            :: [(FastString,a)] -> FastStringEnv a
alterFsEnv         :: (Maybe a-> Maybe a) -> FastStringEnv a -> FastString -> FastStringEnv a
extendFsEnv_C      :: (a->a->a) -> FastStringEnv a -> FastString -> a -> FastStringEnv a
extendFsEnv_Acc    :: (a->b->b) -> (a->b) -> FastStringEnv b -> FastString -> a -> FastStringEnv b
extendFsEnv        :: FastStringEnv a -> FastString -> a -> FastStringEnv a
plusFsEnv          :: FastStringEnv a -> FastStringEnv a -> FastStringEnv a
plusFsEnv_C        :: (a->a->a) -> FastStringEnv a -> FastStringEnv a -> FastStringEnv a
extendFsEnvList    :: FastStringEnv a -> [(FastString,a)] -> FastStringEnv a
extendFsEnvList_C  :: (a->a->a) -> FastStringEnv a -> [(FastString,a)] -> FastStringEnv a
delFromFsEnv       :: FastStringEnv a -> FastString -> FastStringEnv a
delListFromFsEnv   :: FastStringEnv a -> [FastString] -> FastStringEnv a
elemFsEnv          :: FastString -> FastStringEnv a -> Bool
unitFsEnv          :: FastString -> a -> FastStringEnv a
lookupFsEnv        :: FastStringEnv a -> FastString -> Maybe a
lookupFsEnv_NF     :: FastStringEnv a -> FastString -> a
filterFsEnv        :: (elt -> Bool) -> FastStringEnv elt -> FastStringEnv elt
mapFsEnv           :: (elt1 -> elt2) -> FastStringEnv elt1 -> FastStringEnv elt2

emptyFsEnv                = emptyUFM
unitFsEnv x y             = unitUFM x y
extendFsEnv x y z         = addToUFM x y z
extendFsEnvList x l       = addListToUFM x l
lookupFsEnv x y           = lookupUFM x y
alterFsEnv                = alterUFM
mkFsEnv     l             = listToUFM l
elemFsEnv x y             = elemUFM x y
plusFsEnv x y             = plusUFM x y
plusFsEnv_C f x y         = plusUFM_C f x y
extendFsEnv_C f x y z     = addToUFM_C f x y z
mapFsEnv f x              = mapUFM f x
extendFsEnv_Acc x y z a b = addToUFM_Acc x y z a b
extendFsEnvList_C x y z   = addListToUFM_C x y z
delFromFsEnv x y          = delFromUFM x y
delListFromFsEnv x y      = delListFromUFM x y
filterFsEnv x y           = filterUFM x y

lookupFsEnv_NF env n = expectJust "lookupFsEnv_NF" (lookupFsEnv env n)

-- Deterministic FastStringEnv
-- See Note [Deterministic UniqFM] in GHC.Types.Unique.DFM for explanation why we need
-- DFastStringEnv.

type DFastStringEnv a = UniqDFM FastString a  -- Domain is FastString

emptyDFsEnv :: DFastStringEnv a
emptyDFsEnv = emptyUDFM

dFsEnvElts :: DFastStringEnv a -> [a]
dFsEnvElts = eltsUDFM

mkDFsEnv :: [(FastString,a)] -> DFastStringEnv a
mkDFsEnv l = listToUDFM l

lookupDFsEnv :: DFastStringEnv a -> FastString -> Maybe a
lookupDFsEnv = lookupUDFM
