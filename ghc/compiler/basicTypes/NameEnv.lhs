%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[NameEnv]{@NameEnv@: name environments}

\begin{code}
module NameEnv (
	NameEnv, mkNameEnv,
	emptyNameEnv, unitNameEnv, nameEnvElts, 
	extendNameEnv_C, extendNameEnv_Acc, extendNameEnv, extendNameEnvList, 
	foldNameEnv, filterNameEnv,
	plusNameEnv, plusNameEnv_C, 
	lookupNameEnv, lookupNameEnv_NF, delFromNameEnv, delListFromNameEnv,
	elemNameEnv, mapNameEnv
    ) where

#include "HsVersions.h"

import Name	( Name )
import UniqFM
import Maybes	( expectJust )
\end{code}

%************************************************************************
%*									*
\subsection{Name environment}
%*									*
%************************************************************************

\begin{code}
type NameEnv a = UniqFM a	-- Domain is Name

emptyNameEnv   	   :: NameEnv a
mkNameEnv	   :: [(Name,a)] -> NameEnv a
nameEnvElts    	   :: NameEnv a -> [a]
extendNameEnv_C    :: (a->a->a) -> NameEnv a -> Name -> a -> NameEnv a
extendNameEnv_Acc  :: (a->b->b) -> (a->b) -> NameEnv b -> Name -> a -> NameEnv b
extendNameEnv  	   :: NameEnv a -> Name -> a -> NameEnv a
plusNameEnv    	   :: NameEnv a -> NameEnv a -> NameEnv a
plusNameEnv_C  	   :: (a->a->a) -> NameEnv a -> NameEnv a -> NameEnv a
extendNameEnvList  :: NameEnv a -> [(Name,a)] -> NameEnv a
delFromNameEnv 	   :: NameEnv a -> Name -> NameEnv a
delListFromNameEnv :: NameEnv a -> [Name] -> NameEnv a
elemNameEnv    	   :: Name -> NameEnv a -> Bool
unitNameEnv    	   :: Name -> a -> NameEnv a
lookupNameEnv  	   :: NameEnv a -> Name -> Maybe a
lookupNameEnv_NF   :: NameEnv a -> Name -> a
foldNameEnv	   :: (a -> b -> b) -> b -> NameEnv a -> b
filterNameEnv	   :: (elt -> Bool) -> NameEnv elt -> NameEnv elt
mapNameEnv	   :: (elt1 -> elt2) -> NameEnv elt1 -> NameEnv elt2

emptyNameEnv   	    = emptyUFM
foldNameEnv	    = foldUFM
mkNameEnv	    = listToUFM
nameEnvElts    	    = eltsUFM
extendNameEnv_C     = addToUFM_C
extendNameEnv_Acc   = addToUFM_Acc
extendNameEnv  	    = addToUFM
plusNameEnv    	    = plusUFM
plusNameEnv_C  	    = plusUFM_C
extendNameEnvList   = addListToUFM
delFromNameEnv 	    = delFromUFM
delListFromNameEnv  = delListFromUFM
elemNameEnv    	    = elemUFM
unitNameEnv    	    = unitUFM
filterNameEnv	    = filterUFM
mapNameEnv	    = mapUFM

lookupNameEnv  	       = lookupUFM
lookupNameEnv_NF env n = expectJust "lookupNameEnv_NF" (lookupUFM env n)
\end{code}

