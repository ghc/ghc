%
% (c) The AQUA Project, Glasgow University, 1994
%
\section[TyVarEnv]{Lookup tables that have @TyVar@ keys}

An interface to the @FiniteMap@ machinery, which exports
a ``personality'' the same as that of the old @TyVarEnv@ module.

\begin{code}
#include "HsVersions.h"

module TyVarEnv (
	TyVarEnv(..),  -- abstract: NOT

	TypeEnv(..),	-- most common/important kind of TyVarEnv

	mkTyVarEnv,
	lookupTyVarEnv,
	nullTyVarEnv, growTyVarEnvList,
	isNullTyVarEnv,
	addOneToTyVarEnv,

	-- and to make the interface self-sufficient...
	UniqFM,
	TyVar, Unique, Maybe(..)
	
#ifdef USE_ATTACK_PRAGMAS
	, addToUFM, plusUFM_C, delListFromUFM, delFromUFM, plusUFM,
	lookupUFM, mapUFM, minusUFM, listToUFM, emptyUFM, eltsUFM,
	singletonUFM,
	u2i
#endif
    ) where

import AbsUniType
import UniqFM
import Maybes		( Maybe(..) )
import Outputable
import Unique		( Unique, u2i )
import Util
\end{code}

\begin{code}
type TyVarEnv elt = UniqFM elt

type TypeEnv = TyVarEnv UniType -- most common flavo(u)r
\end{code}

Signatures:
\begin{code}
mkTyVarEnv :: [(TyVar, a)] -> TyVarEnv a
addOneToTyVarEnv :: TyVarEnv a -> TyVar -> a -> TyVarEnv a
growTyVarEnvList :: TyVarEnv a -> [(TyVar, a)] -> TyVarEnv a
isNullTyVarEnv :: TyVarEnv a -> Bool
lookupTyVarEnv :: TyVarEnv a -> TyVar -> Maybe a
nullTyVarEnv :: TyVarEnv a
\end{code}

\begin{code}
mkTyVarEnv stuff = listToUFM stuff

addOneToTyVarEnv env id elt = addToUFM env id elt

growTyVarEnvList env pairs = plusUFM env (listToUFM pairs)

isNullTyVarEnv env = sizeUFM env == 0

lookupTyVarEnv env id = lookupUFM env id

nullTyVarEnv = emptyUFM
\end{code}
