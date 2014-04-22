%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[FastStringEnv]{@FastStringEnv@: FastString environments}

\begin{code}
module FastStringEnv (
        -- * FastString environments (maps)
        FastStringEnv,

        -- ** Manipulating these environments
        mkFsEnv,
        emptyFsEnv, unitFsEnv, fsEnvElts, fsEnvUniqueElts,
        extendFsEnv_C, extendFsEnv_Acc, extendFsEnv,
        extendFsEnvList, extendFsEnvList_C,
        foldFsEnv, filterFsEnv,
        plusFsEnv, plusFsEnv_C, alterFsEnv,
        lookupFsEnv, lookupFsEnv_NF, delFromFsEnv, delListFromFsEnv,
        elemFsEnv, mapFsEnv,
    ) where

#include "HsVersions.h"

import Unique
import UniqFM
import Maybes
import FastString


type FastStringEnv a = UniqFM a  -- Domain is FastString

emptyFsEnv         :: FastStringEnv a
mkFsEnv            :: [(FastString,a)] -> FastStringEnv a
fsEnvElts          :: FastStringEnv a -> [a]
fsEnvUniqueElts    :: FastStringEnv a -> [(Unique, a)]
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
foldFsEnv          :: (a -> b -> b) -> b -> FastStringEnv a -> b
filterFsEnv        :: (elt -> Bool) -> FastStringEnv elt -> FastStringEnv elt
mapFsEnv           :: (elt1 -> elt2) -> FastStringEnv elt1 -> FastStringEnv elt2

fsEnvElts x               = eltsUFM x
emptyFsEnv                = emptyUFM
unitFsEnv x y             = unitUFM x y
extendFsEnv x y z         = addToUFM x y z
extendFsEnvList x l       = addListToUFM x l
lookupFsEnv x y           = lookupUFM x y
alterFsEnv                = alterUFM
mkFsEnv     l             = listToUFM l
elemFsEnv x y             = elemUFM x y
foldFsEnv a b c           = foldUFM a b c
plusFsEnv x y             = plusUFM x y
plusFsEnv_C f x y         = plusUFM_C f x y
extendFsEnv_C f x y z     = addToUFM_C f x y z
mapFsEnv f x              = mapUFM f x
fsEnvUniqueElts x         = ufmToList x
extendFsEnv_Acc x y z a b = addToUFM_Acc x y z a b
extendFsEnvList_C x y z   = addListToUFM_C x y z
delFromFsEnv x y          = delFromUFM x y
delListFromFsEnv x y      = delListFromUFM x y
filterFsEnv x y           = filterUFM x y

lookupFsEnv_NF env n = expectJust "lookupFsEnv_NF" (lookupFsEnv env n)
\end{code}
