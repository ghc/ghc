%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[NameEnv]{@NameEnv@: name environments}

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module NameEnv (
	NameEnv, mkNameEnv,
	emptyNameEnv, unitNameEnv, nameEnvElts, nameEnvUniqueElts,
	extendNameEnv_C, extendNameEnv_Acc, extendNameEnv,
        extendNameEnvList, extendNameEnvList_C,
	foldNameEnv, filterNameEnv,
	plusNameEnv, plusNameEnv_C, 
	lookupNameEnv, lookupNameEnv_NF, delFromNameEnv, delListFromNameEnv,
	elemNameEnv, mapNameEnv
    ) where

#include "HsVersions.h"

import Name
import Unique(Unique)
import UniqFM
import Maybes
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection{Name environment}
%*									*
%************************************************************************

\begin{code}
newtype NameEnv a = A (UniqFM a)	-- Domain is Name

emptyNameEnv   	   :: NameEnv a
mkNameEnv	   :: [(Name,a)] -> NameEnv a
nameEnvElts    	   :: NameEnv a -> [a]
nameEnvUniqueElts  :: NameEnv a -> [(Unique, a)]
extendNameEnv_C    :: (a->a->a) -> NameEnv a -> Name -> a -> NameEnv a
extendNameEnv_Acc  :: (a->b->b) -> (a->b) -> NameEnv b -> Name -> a -> NameEnv b
extendNameEnv  	   :: NameEnv a -> Name -> a -> NameEnv a
plusNameEnv    	   :: NameEnv a -> NameEnv a -> NameEnv a
plusNameEnv_C  	   :: (a->a->a) -> NameEnv a -> NameEnv a -> NameEnv a
extendNameEnvList  :: NameEnv a -> [(Name,a)] -> NameEnv a
extendNameEnvList_C :: (a->a->a) -> NameEnv a -> [(Name,a)] -> NameEnv a
delFromNameEnv 	   :: NameEnv a -> Name -> NameEnv a
delListFromNameEnv :: NameEnv a -> [Name] -> NameEnv a
elemNameEnv    	   :: Name -> NameEnv a -> Bool
unitNameEnv    	   :: Name -> a -> NameEnv a
lookupNameEnv  	   :: NameEnv a -> Name -> Maybe a
lookupNameEnv_NF   :: NameEnv a -> Name -> a
foldNameEnv	   :: (a -> b -> b) -> b -> NameEnv a -> b
filterNameEnv	   :: (elt -> Bool) -> NameEnv elt -> NameEnv elt
mapNameEnv	   :: (elt1 -> elt2) -> NameEnv elt1 -> NameEnv elt2

nameEnvElts (A x) = eltsUFM x
emptyNameEnv  	 = A emptyUFM
unitNameEnv x y = A $ unitUFM x y 
extendNameEnv (A x) y z = A $ addToUFM x y z
extendNameEnvList (A x) l = A $ addListToUFM x l
lookupNameEnv (A x) y = lookupUFM x y
mkNameEnv     l    = A $ listToUFM l
elemNameEnv x (A y) 	 = elemUFM x y
foldNameEnv a b (A c)	 = foldUFM a b c 
occEnvElts (A x)	 = eltsUFM x
plusNameEnv (A x) (A y)	 = A $ plusUFM x y 
plusNameEnv_C f (A x) (A y)	 = A $ plusUFM_C f x y 
extendNameEnv_C f (A x) y z   = A $ addToUFM_C f x y z
mapNameEnv f (A x)	 = A $ mapUFM f x
mkNameEnv_C comb l = A $ addListToUFM_C comb emptyUFM l
nameEnvUniqueElts (A x)  = ufmToList x
extendNameEnv_Acc x y (A z) a b  = A $ addToUFM_Acc x y z a b
extendNameEnvList_C x (A y) z = A $ addListToUFM_C x y z
delFromNameEnv (A x) y    = A $ delFromUFM x y
delListFromNameEnv (A x) y  = A $ delListFromUFM x y
filterNameEnv x (A y)       = A $ filterUFM x y

lookupNameEnv_NF env n = expectJust "lookupNameEnv_NF" (lookupNameEnv env n)

instance Outputable a => Outputable (NameEnv a) where
    ppr (A x) = ppr x
\end{code}

