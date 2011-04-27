-- Cmm representations using Hoopl's Graph CmmNode e x.
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#if __GLASGOW_HASKELL__ >= 701
-- GHC 7.0.1 improved incomplete pattern warnings with GADTs
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
#endif

module Cmm
  ( CmmGraph, GenCmmGraph(..), CmmBlock
  , CmmStackInfo(..), CmmTopInfo(..), Cmm, CmmTop
  , CmmReplGraph, CmmFwdRewrite, CmmBwdRewrite

  , modifyGraph
  , lastNode, replaceLastNode, insertBetween
  , ofBlockMap, toBlockMap, insertBlock
  , ofBlockList, toBlockList, bodyToBlockList
  , foldGraphBlocks, mapGraphNodes, postorderDfs

  , analFwd, analBwd, analRewFwd, analRewBwd
  , dataflowPassFwd, dataflowPassBwd
  , module CmmNode
  )
where

import BlockId
import CmmDecl
import CmmNode
import OptimizationFuel as F
import SMRep
import UniqSupply

import Compiler.Hoopl
import Control.Monad
import Data.Maybe
import Panic

#include "HsVersions.h"

-------------------------------------------------
-- CmmBlock, CmmGraph and Cmm

type CmmGraph = GenCmmGraph CmmNode
data GenCmmGraph n = CmmGraph { g_entry :: BlockId, g_graph :: Graph n C C }
type CmmBlock = Block CmmNode C C

type CmmReplGraph e x = FuelUniqSM (Maybe (Graph CmmNode e x))
type CmmFwdRewrite f = FwdRewrite FuelUniqSM CmmNode f
type CmmBwdRewrite f = BwdRewrite FuelUniqSM CmmNode f

data CmmStackInfo = StackInfo {arg_space :: ByteOff, updfr_space :: Maybe ByteOff}
data CmmTopInfo   = TopInfo {info_tbl :: CmmInfoTable, stack_info :: CmmStackInfo}
type Cmm          = GenCmm    CmmStatic CmmTopInfo CmmGraph
type CmmTop       = GenCmmTop CmmStatic CmmTopInfo CmmGraph

-------------------------------------------------
-- Manipulating CmmGraphs

modifyGraph :: (Graph n C C -> Graph n' C C) -> GenCmmGraph n -> GenCmmGraph n'
modifyGraph f g = CmmGraph {g_entry=g_entry g, g_graph=f (g_graph g)}

toBlockMap :: CmmGraph -> LabelMap CmmBlock
toBlockMap (CmmGraph {g_graph=GMany NothingO body NothingO}) = body

ofBlockMap :: BlockId -> LabelMap CmmBlock -> CmmGraph
ofBlockMap entry bodyMap = CmmGraph {g_entry=entry, g_graph=GMany NothingO bodyMap NothingO}

insertBlock :: CmmBlock -> LabelMap CmmBlock -> LabelMap CmmBlock
insertBlock block map =
  ASSERT (isNothing $ mapLookup id map)
  mapInsert id block map
  where id = entryLabel block

toBlockList :: CmmGraph -> [CmmBlock]
toBlockList g = mapElems $ toBlockMap g

ofBlockList :: BlockId -> [CmmBlock] -> CmmGraph
ofBlockList entry blocks = CmmGraph {g_entry=entry, g_graph=GMany NothingO body NothingO}
  where body = foldr addBlock emptyBody blocks

bodyToBlockList :: Body CmmNode -> [CmmBlock]
bodyToBlockList body = mapElems body

mapGraphNodes :: ( CmmNode C O -> CmmNode C O
                 , CmmNode O O -> CmmNode O O
                 , CmmNode O C -> CmmNode O C)
              -> CmmGraph -> CmmGraph
mapGraphNodes funs@(mf,_,_) g =
  ofBlockMap (entryLabel $ mf $ CmmEntry $ g_entry g) $ mapMap (blockMapNodes3 funs) $ toBlockMap g

foldGraphBlocks :: (CmmBlock -> a -> a) -> a -> CmmGraph -> a
foldGraphBlocks k z g = mapFold k z $ toBlockMap g

postorderDfs :: CmmGraph -> [CmmBlock]
postorderDfs g = postorder_dfs_from (toBlockMap g) (g_entry g)

-------------------------------------------------
-- Manipulating CmmBlocks

lastNode :: CmmBlock -> CmmNode O C
lastNode block = foldBlockNodesF3 (nothing, nothing, const) block ()
  where nothing :: a -> b -> ()
        nothing _ _ = ()

replaceLastNode :: Block CmmNode e C -> CmmNode O C -> Block CmmNode e C
replaceLastNode block last = blockOfNodeList (first, middle, JustC last)
  where (first, middle, _) = blockToNodeList block

----------------------------------------------------------------------
----- Splicing between blocks
-- Given a middle node, a block, and a successor BlockId,
-- we can insert the middle node between the block and the successor.
-- We return the updated block and a list of new blocks that must be added
-- to the graph.
-- The semantics is a bit tricky. We consider cases on the last node:
-- o For a branch, we can just insert before the branch,
--   but sometimes the optimizer does better if we actually insert
--   a fresh basic block, enabling some common blockification.
-- o For a conditional branch, switch statement, or call, we must insert
--   a new basic block.
-- o For a jump or return, this operation is impossible.

insertBetween :: MonadUnique m => CmmBlock -> [CmmNode O O] -> BlockId -> m (CmmBlock, [CmmBlock])
insertBetween b ms succId = insert $ lastNode b
  where insert :: MonadUnique m => CmmNode O C -> m (CmmBlock, [CmmBlock])
        insert (CmmBranch bid) =
          if bid == succId then
            do (bid', bs) <- newBlocks
               return (replaceLastNode b (CmmBranch bid'), bs)
          else panic "tried invalid block insertBetween"
        insert (CmmCondBranch c t f) =
          do (t', tbs) <- if t == succId then newBlocks else return $ (t, [])
             (f', fbs) <- if f == succId then newBlocks else return $ (f, [])
             return (replaceLastNode b (CmmCondBranch c t' f'), tbs ++ fbs)
        insert (CmmSwitch e ks) =
          do (ids, bs) <- mapAndUnzipM mbNewBlocks ks
             return (replaceLastNode b (CmmSwitch e ids), join bs)
        insert (CmmCall {}) =
          panic "unimp: insertBetween after a call -- probably not a good idea"
        insert (CmmForeignCall {}) =
          panic "unimp: insertBetween after a foreign call -- probably not a good idea"

        newBlocks :: MonadUnique m => m (BlockId, [CmmBlock])
        newBlocks = do id <- liftM mkBlockId $ getUniqueM
                       return $ (id, [blockOfNodeList (JustC (CmmEntry id), ms, JustC (CmmBranch succId))])
        mbNewBlocks :: MonadUnique m => Maybe BlockId -> m (Maybe BlockId, [CmmBlock])
        mbNewBlocks (Just k) = if k == succId then liftM fstJust newBlocks
                               else return (Just k, [])
        mbNewBlocks Nothing  = return (Nothing, [])
        fstJust (id, bs) = (Just id, bs)

-------------------------------------------------
-- Running dataflow analysis and/or rewrites

-- Constructing forward and backward analysis-only pass
analFwd    :: Monad m => DataflowLattice f -> FwdTransfer n f -> FwdPass m n f
analBwd    :: Monad m => DataflowLattice f -> BwdTransfer n f -> BwdPass m n f

analFwd lat xfer = analRewFwd lat xfer noFwdRewrite
analBwd lat xfer = analRewBwd lat xfer noBwdRewrite

-- Constructing forward and backward analysis + rewrite pass
analRewFwd :: Monad m => DataflowLattice f -> FwdTransfer n f -> FwdRewrite m n f -> FwdPass m n f
analRewBwd :: Monad m => DataflowLattice f -> BwdTransfer n f -> BwdRewrite m n f -> BwdPass m n f

analRewFwd lat xfer rew = FwdPass {fp_lattice = lat, fp_transfer = xfer, fp_rewrite = rew}
analRewBwd lat xfer rew = BwdPass {bp_lattice = lat, bp_transfer = xfer, bp_rewrite = rew}

-- Running forward and backward dataflow analysis + optional rewrite
dataflowPassFwd :: NonLocal n => GenCmmGraph n -> [(BlockId, f)] -> FwdPass FuelUniqSM n f -> FuelUniqSM (GenCmmGraph n, BlockEnv f)
dataflowPassFwd (CmmGraph {g_entry=entry, g_graph=graph}) facts fwd = do
  (graph, facts, NothingO) <- analyzeAndRewriteFwd fwd (JustC [entry]) graph (mkFactBase (fp_lattice fwd) facts)
  return (CmmGraph {g_entry=entry, g_graph=graph}, facts)

dataflowPassBwd :: NonLocal n => GenCmmGraph n -> [(BlockId, f)] -> BwdPass FuelUniqSM n f -> FuelUniqSM (GenCmmGraph n, BlockEnv f)
dataflowPassBwd (CmmGraph {g_entry=entry, g_graph=graph}) facts bwd = do
  (graph, facts, NothingO) <- analyzeAndRewriteBwd bwd (JustC [entry]) graph (mkFactBase (bp_lattice bwd) facts)
  return (CmmGraph {g_entry=entry, g_graph=graph}, facts)
