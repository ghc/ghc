{-# LANGUAGE DeriveDataTypeable #-}
{-
%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[FastStringEnv]{@FastStringEnv@: FastString environments}
-}

module FastStringEnv (
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
        mkDFsEnv, emptyDFsEnv, dFsEnvElts, lookupDFsEnv,
        addToDFsEnv, elemDFsEnv, addListToDFsEnv, delFromDFsEnv,
        FastStringU(..), dFsEnvToDsEnv
    ) where

import GhcPrelude

import UniqMap
import Data.Data
import UniqDFM
import Maybes
import FastString
import Unique
import Outputable

import Data.Coerce


-- Uniquable is very bad because it means that we lose the reference to the
-- fast string which means that it gets GCd. FastStringEnv ensures that
-- the map also contains a copy of the key so we retain a reference.
newtype FastStringU = FastStringU FastString
      deriving Data

instance Outputable FastStringU where
  ppr (FastStringU f) = ppr f

instance Uniquable FastStringU where
  getUnique (FastStringU fs) = mkUniqueGrimily (uniqueOfFS fs)


-- | A non-deterministic set of FastStrings.
-- See Note [Deterministic UniqFM] in UniqDFM for explanation why it's not
-- deterministic and why it matters. Use DFastStringEnv if the set eventually
-- gets converted into a list or folded over in a way where the order
-- changes the generated code.
type FastStringEnv a = UniqMap FastStringU a  -- Domain is FastString

emptyFsEnv         :: FastStringEnv a
mkFsEnv            :: HasFastString f => [(f,a)] -> FastStringEnv a
alterFsEnv         :: HasFastString f => (Maybe a-> Maybe a) -> FastStringEnv a -> f -> FastStringEnv a
extendFsEnv_C      :: HasFastString f => (a->a->a) -> FastStringEnv a -> f -> a -> FastStringEnv a
extendFsEnv_Acc    :: HasFastString f => (a->b->b) -> (a->b) -> FastStringEnv b -> f -> a -> FastStringEnv b
extendFsEnv        :: HasFastString f => FastStringEnv a -> f -> a -> FastStringEnv a
plusFsEnv          :: FastStringEnv a -> FastStringEnv a -> FastStringEnv a
plusFsEnv_C        :: (a->a->a) -> FastStringEnv a -> FastStringEnv a -> FastStringEnv a
extendFsEnvList    :: HasFastString f => FastStringEnv a -> [(f,a)] -> FastStringEnv a
extendFsEnvList_C  :: HasFastString f => (a->a->a) -> FastStringEnv a -> [(f,a)] -> FastStringEnv a
delFromFsEnv       :: HasFastString f => FastStringEnv a -> f -> FastStringEnv a
delListFromFsEnv   :: HasFastString f => FastStringEnv a -> [f] -> FastStringEnv a
elemFsEnv          :: HasFastString f => f -> FastStringEnv a -> Bool
unitFsEnv          :: HasFastString f => f -> a -> FastStringEnv a
lookupFsEnv        :: HasFastString f => FastStringEnv a -> f -> Maybe a
lookupFsEnv_NF     :: HasFastString f => FastStringEnv a -> f -> a
filterFsEnv        :: (elt -> Bool) -> FastStringEnv elt -> FastStringEnv elt
mapFsEnv           :: (elt1 -> elt2) -> FastStringEnv elt1 -> FastStringEnv elt2

coerceF :: HasFastString f => f -> FastStringU
coerceF = coerce . getFastString
coerceFL :: HasFastString f => [f] -> [FastStringU]
coerceFL = coerce . map getFastString

coerceFT :: HasFastString f => [(f, a)] -> [(FastStringU, a)]
coerceFT = coerce . map (\(a, b) -> (getFastString a, b))

emptyFsEnv                = emptyUniqMap
unitFsEnv x y             = unitUniqMap (coerceF x) y
extendFsEnv x y z         = addToUniqMap x (coerceF y) z
extendFsEnvList x l       = addListToUniqMap x (coerceFT l)
lookupFsEnv x y           = lookupUniqMap x (coerceF y)
alterFsEnv  x y a         = alterUniqMap x y (coerceF a)
mkFsEnv     l             = listToUniqMap (coerceFT l)
elemFsEnv x y             = elemUniqMap (coerceF x) y
plusFsEnv x y             = plusUniqMap x y
plusFsEnv_C f x y         = plusUniqMap_C f x y
extendFsEnv_C f x y z     = addToUniqMap_C f x (coerceF y) z
mapFsEnv f x              = mapUniqMap f x
extendFsEnv_Acc x y z a b = addToUniqMap_Acc x y z (coerceF a) b
extendFsEnvList_C x y z   = addListToUniqMap_C x y (coerceFT z)
delFromFsEnv x y          = delFromUniqMap x (coerceF y)
delListFromFsEnv x y      = delListFromUniqMap x (coerceFL y)
filterFsEnv x y           = filterUniqMap x y

lookupFsEnv_NF env n = expectJust "lookupFsEnv_NF" (lookupFsEnv env n)

-- Deterministic FastStringEnv
-- See Note [Deterministic UniqFM] in UniqDFM for explanation why we need
-- DFastStringEnv.

type DFastStringEnv a = UniqDFM (FastString, a)  -- Domain is FastString

dFsEnvToDsEnv :: DFastStringEnv a -> FastStringEnv a
dFsEnvToDsEnv = UniqMap . udfmToUfm . coerce

emptyDFsEnv :: DFastStringEnv a
emptyDFsEnv = emptyUDFM

dFsEnvElts :: DFastStringEnv a -> [a]
dFsEnvElts = map snd . eltsUDFM

addToDFsEnv :: 
  HasFastString f => DFastStringEnv a -> f -> a -> DFastStringEnv a
addToDFsEnv m f a = addToUDFM m (FastStringU $ getFastString f) (getFastString f, a)

addListToDFsEnv :: 
  HasFastString f => DFastStringEnv a -> [(f,a)] -> DFastStringEnv a
addListToDFsEnv m fs = 
    addListToUDFM m (map (\(f,a) -> ((FastStringU $ getFastString f), (getFastString f, a))) fs)


mkDFsEnv :: HasFastString f => [(f,a)] -> DFastStringEnv a
mkDFsEnv l = listToUDFM 
  (map (\(f, x) -> (FastStringU (getFastString f), (getFastString f, x))) l)

lookupDFsEnv :: HasFastString f => DFastStringEnv a -> f -> Maybe a
lookupDFsEnv d f = snd <$> lookupUDFM d (FastStringU (getFastString f))

elemDFsEnv :: HasFastString f => f -> DFastStringEnv a -> Bool
elemDFsEnv f e = elemUDFM (FastStringU (getFastString f)) e

delFromDFsEnv :: HasFastString f => DFastStringEnv a -> f -> DFastStringEnv a
delFromDFsEnv e f = delFromUDFM e (FastStringU (getFastString f))
