{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[NameEnv]{@NameEnv@: name environments}
-}

{-# LANGUAGE CPP #-}
module NameEnv (
        -- * Var, Id and TyVar environments (maps)
        NameEnv,

        -- ** Manipulating these environments
        mkNameEnv,
        emptyNameEnv, isEmptyNameEnv,
        unitNameEnv, nameEnvElts,
        extendNameEnv_C, extendNameEnv_Acc, extendNameEnv,
        extendNameEnvList, extendNameEnvList_C,
        filterNameEnv, anyNameEnv,
        plusNameEnv, plusNameEnv_C, alterNameEnv,
        lookupNameEnv, lookupNameEnv_NF, delFromNameEnv, delListFromNameEnv,
        elemNameEnv, mapNameEnv, disjointNameEnv,

        DNameEnv,

        emptyDNameEnv,
        lookupDNameEnv,
        mapDNameEnv,
        alterDNameEnv,
        -- ** Dependency analysis
        depAnal
    ) where

#include "HsVersions.h"

import GhcPrelude

import Digraph
import Name
import UniqFM
import UniqDFM
import Maybes

{-
************************************************************************
*                                                                      *
\subsection{Name environment}
*                                                                      *
************************************************************************
-}

{-
Note [depAnal determinism]
~~~~~~~~~~~~~~~~~~~~~~~~~~
depAnal is deterministic provided it gets the nodes in a deterministic order.
The order of lists that get_defs and get_uses return doesn't matter, as these
are only used to construct the edges, and stronglyConnCompFromEdgedVertices is
deterministic even when the edges are not in deterministic order as explained
in Note [Deterministic SCC] in Digraph.
-}

depAnal :: (node -> [Name])      -- Defs
        -> (node -> [Name])      -- Uses
        -> [node]
        -> [SCC node]
-- Perform dependency analysis on a group of definitions,
-- where each definition may define more than one Name
--
-- The get_defs and get_uses functions are called only once per node
depAnal get_defs get_uses nodes
  = stronglyConnCompFromEdgedVerticesUniq (map mk_node keyed_nodes)
  where
    keyed_nodes = nodes `zip` [(1::Int)..]
    mk_node (node, key) =
      DigraphNode node key (mapMaybe (lookupNameEnv key_map) (get_uses node))

    key_map :: NameEnv Int   -- Maps a Name to the key of the decl that defines it
    key_map = mkNameEnv [(name,key) | (node, key) <- keyed_nodes, name <- get_defs node]

{-
************************************************************************
*                                                                      *
\subsection{Name environment}
*                                                                      *
************************************************************************
-}

-- | Name Environment
type NameEnv a = UniqFM a       -- Domain is Name

emptyNameEnv       :: NameEnv a
isEmptyNameEnv     :: NameEnv a -> Bool
mkNameEnv          :: [(Name,a)] -> NameEnv a
nameEnvElts        :: NameEnv a -> [a]
alterNameEnv       :: (Maybe a-> Maybe a) -> NameEnv a -> Name -> NameEnv a
extendNameEnv_C    :: (a->a->a) -> NameEnv a -> Name -> a -> NameEnv a
extendNameEnv_Acc  :: (a->b->b) -> (a->b) -> NameEnv b -> Name -> a -> NameEnv b
extendNameEnv      :: NameEnv a -> Name -> a -> NameEnv a
plusNameEnv        :: NameEnv a -> NameEnv a -> NameEnv a
plusNameEnv_C      :: (a->a->a) -> NameEnv a -> NameEnv a -> NameEnv a
extendNameEnvList  :: NameEnv a -> [(Name,a)] -> NameEnv a
extendNameEnvList_C :: (a->a->a) -> NameEnv a -> [(Name,a)] -> NameEnv a
delFromNameEnv     :: NameEnv a -> Name -> NameEnv a
delListFromNameEnv :: NameEnv a -> [Name] -> NameEnv a
elemNameEnv        :: Name -> NameEnv a -> Bool
unitNameEnv        :: Name -> a -> NameEnv a
lookupNameEnv      :: NameEnv a -> Name -> Maybe a
lookupNameEnv_NF   :: NameEnv a -> Name -> a
filterNameEnv      :: (elt -> Bool) -> NameEnv elt -> NameEnv elt
anyNameEnv         :: (elt -> Bool) -> NameEnv elt -> Bool
mapNameEnv         :: (elt1 -> elt2) -> NameEnv elt1 -> NameEnv elt2
disjointNameEnv    :: NameEnv a -> NameEnv a -> Bool

nameEnvElts x         = eltsUFM x
emptyNameEnv          = emptyUFM
isEmptyNameEnv        = isNullUFM
unitNameEnv x y       = unitUFM x y
extendNameEnv x y z   = addToUFM x y z
extendNameEnvList x l = addListToUFM x l
lookupNameEnv x y     = lookupUFM x y
alterNameEnv          = alterUFM
mkNameEnv     l       = listToUFM l
elemNameEnv x y          = elemUFM x y
plusNameEnv x y          = plusUFM x y
plusNameEnv_C f x y      = plusUFM_C f x y
extendNameEnv_C f x y z  = addToUFM_C f x y z
mapNameEnv f x           = mapUFM f x
extendNameEnv_Acc x y z a b  = addToUFM_Acc x y z a b
extendNameEnvList_C x y z = addListToUFM_C x y z
delFromNameEnv x y      = delFromUFM x y
delListFromNameEnv x y  = delListFromUFM x y
filterNameEnv x y       = filterUFM x y
anyNameEnv f x          = foldUFM ((||) . f) False x
disjointNameEnv x y     = isNullUFM (intersectUFM x y)

lookupNameEnv_NF env n = expectJust "lookupNameEnv_NF" (lookupNameEnv env n)

-- | Deterministic Name Environment
--
-- See Note [Deterministic UniqFM] in UniqDFM for explanation why we need
-- DNameEnv.
type DNameEnv a = UniqDFM a

emptyDNameEnv :: DNameEnv a
emptyDNameEnv = emptyUDFM

lookupDNameEnv :: DNameEnv a -> Name -> Maybe a
lookupDNameEnv = lookupUDFM

mapDNameEnv :: (a -> b) -> DNameEnv a -> DNameEnv b
mapDNameEnv = mapUDFM

alterDNameEnv :: (Maybe a -> Maybe a) -> DNameEnv a -> Name -> DNameEnv a
alterDNameEnv = alterUDFM
