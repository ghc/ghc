%
% (c) The AQUA Project, Glasgow University, 1995
%
\section[IdEnv]{Lookup tables that have @Id@ keys}

An interface to the @FiniteMap@ machinery, which exports
a ``personality'' the same as that of the old @IdEnv@ module.

\begin{code}
#include "HsVersions.h"

module IdEnv (
	IdEnv(..),  -- abstract: NOT

	lookupIdEnv, lookupNoFailIdEnv,
	nullIdEnv, unitIdEnv, mkIdEnv, growIdEnv, growIdEnvList,
	isNullIdEnv,
	addOneToIdEnv,
	delOneFromIdEnv, delManyFromIdEnv, --UNUSED: minusIdEnv,
	modifyIdEnv, combineIdEnvs,
	rngIdEnv,
	mapIdEnv,
-- UNUSED:	filterIdEnv,

	-- and to make the interface self-sufficient...
	UniqFM,
	Id, Unique, Maybe(..)
	
	-- and for pragma-friendliness...
#ifdef USE_ATTACK_PRAGMAS
	, addToUFM, plusUFM_C, delListFromUFM, delFromUFM, plusUFM,
	lookupUFM, mapUFM, filterUFM, minusUFM, listToUFM, emptyUFM,
	eltsUFM, singletonUFM,
	u2i
#endif
    ) where

import UniqFM
import Id
import IdInfo
import Maybes		( Maybe(..), MaybeErr(..) )
import Outputable
import Unique		( Unique, u2i )
import Util
\end{code}

\begin{code}
type IdEnv elt = UniqFM elt
\end{code}

Signatures:
\begin{code}
addOneToIdEnv :: IdEnv a -> Id -> a -> IdEnv a
combineIdEnvs :: (a -> a -> a) -> IdEnv a -> IdEnv a -> IdEnv a
delManyFromIdEnv :: IdEnv a -> [Id] -> IdEnv a
delOneFromIdEnv :: IdEnv a -> Id -> IdEnv a
growIdEnv :: IdEnv a -> IdEnv a -> IdEnv a
growIdEnvList :: IdEnv a -> [(Id, a)] -> IdEnv a
isNullIdEnv :: IdEnv a -> Bool
lookupIdEnv :: IdEnv a -> Id -> Maybe a
lookupNoFailIdEnv :: IdEnv a -> Id -> a
mapIdEnv :: (a -> b) -> IdEnv a -> IdEnv b
--filterIdEnv :: (a -> Bool) -> IdEnv a -> IdEnv a
--minusIdEnv :: IdEnv a -> IdEnv a -> IdEnv a
mkIdEnv :: [(Id, a)] -> IdEnv a
modifyIdEnv :: IdEnv a -> (a -> a) -> Id -> IdEnv a
nullIdEnv :: IdEnv a
rngIdEnv :: IdEnv a -> [a]
unitIdEnv :: Id -> a -> IdEnv a
\end{code}

\begin{code}
addOneToIdEnv env id elt = addToUFM env id elt

combineIdEnvs combiner env1 env2 = plusUFM_C combiner env1 env2

delManyFromIdEnv env ids = delListFromUFM env ids

delOneFromIdEnv env id = delFromUFM env id

growIdEnv old_env new_stuff = plusUFM old_env new_stuff

growIdEnvList old_env pairs = plusUFM old_env (listToUFM pairs)

isNullIdEnv env = sizeUFM env == 0

lookupIdEnv env id = lookupUFM env id

lookupNoFailIdEnv env id = case (lookupIdEnv env id) of { Just xx -> xx }

mapIdEnv f env = mapUFM f env

{- UNUSED:
filterIdEnv p env = filterUFM p env
minusIdEnv env1 env2 = minusUFM env1 env2
-}

mkIdEnv stuff = listToUFM stuff

-- modifyIdEnv: Look up a thing in the IdEnv, then mash it with the
-- modify function, and put it back.

modifyIdEnv env mangle_fn key
  = case (lookupIdEnv env key) of
      Nothing -> env
      Just xx -> addOneToIdEnv env key (mangle_fn xx)

nullIdEnv = emptyUFM

rngIdEnv env = eltsUFM env

unitIdEnv id elt = singletonUFM id elt
\end{code}
