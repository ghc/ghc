{-# LANGUAGE GADTs #-}

module GHC.Cmm.CommonBlockElim
  ( elimCommonBlocks
  )
where


import GHC.Prelude hiding (iterate, succ, unzip, zip)

import GHC.Cmm.BlockId
import GHC.Cmm
import GHC.Cmm.Utils
import GHC.Cmm.Switch (eqSwitchTargetWith)
import GHC.Cmm.ContFlowOpt

import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import Data.Functor.Classes (liftEq)
import Data.Maybe (mapMaybe)
import qualified Data.List as List
import Data.Word
import qualified Data.Map as M
import qualified GHC.Data.TrieMap as TM
import GHC.Types.Unique.FM
import GHC.Types.Unique
import GHC.Utils.Word64 (truncateWord64ToWord32)
import Control.Arrow (first, second)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Type.Equality

-- -----------------------------------------------------------------------------
-- Eliminate common blocks

-- If two blocks are identical except for the label on the first node,
-- then we can eliminate one of the blocks. To ensure that the semantics
-- of the program are preserved, we have to rewrite each predecessor of the
-- eliminated block to proceed with the block we keep.

-- The algorithm iterates over the blocks in the graph,
-- checking whether it has seen another block that is equal modulo labels.
-- If so, then it adds an entry in a map indicating that the new block
-- is made redundant by the old block.
-- Otherwise, it is added to the useful blocks.

-- To avoid comparing every block with every other block repeatedly, we group
-- them by
--   * a hash of the block, ignoring labels (explained below)
--   * the list of outgoing labels
-- The hash is invariant under relabeling, so we only ever compare within
-- the same group of blocks.
--
-- The list of outgoing labels is updated as we merge blocks (that is why they
-- are not included in the hash, which we want to calculate only once).
--
-- All in all, two blocks should never be compared if they have different
-- hashes, and at most once otherwise. Previously, we were slower, and people
-- rightfully complained: #10397

-- TODO: Use optimization fuel
elimCommonBlocks :: CmmGraph -> CmmGraph
elimCommonBlocks g = replaceLabels env $ copyTicks env g
  where
     env = iterate mapEmpty blocks_with_key
     -- The order of blocks doesn't matter here. While we could use
     -- revPostorder which drops unreachable blocks this is done in
     -- ContFlowOpt already which runs before this pass. So we use
     -- toBlockList since it is faster.
     groups = groupByInt hash_block (toBlockList g) :: [[CmmBlock]]
     blocks_with_key = [ [ (successors b, [b]) | b <- bs] | bs <- groups]

-- Invariant: The blocks in the list are pairwise distinct
-- (so avoid comparing them again)
type DistinctBlocks = [CmmBlock]
type Key = [Label]
type Subst = LabelMap BlockId

-- The outer list groups by hash. We retain this grouping throughout.
iterate :: Subst -> [[(Key, DistinctBlocks)]] -> Subst
iterate subst blocks
    | mapNull new_substs = subst
    | otherwise = iterate subst' updated_blocks
  where
    grouped_blocks :: [[(Key, NonEmpty DistinctBlocks)]]
    grouped_blocks = map groupByLabel blocks

    merged_blocks :: [[(Key, DistinctBlocks)]]
    (new_substs, merged_blocks) = List.mapAccumL (List.mapAccumL go) mapEmpty grouped_blocks
      where
        go !new_subst1 (k,dbs) = (new_subst1 `mapUnion` new_subst2, (k,db))
          where
            (new_subst2, db) = mergeBlockList subst dbs

    subst' = subst `mapUnion` new_substs
    updated_blocks = map (map (first (map (lookupBid subst')))) merged_blocks

-- Combine two lists of blocks.
-- While they are internally distinct they can still share common blocks.
mergeBlocks :: Subst -> DistinctBlocks -> DistinctBlocks -> (Subst, DistinctBlocks)
mergeBlocks subst existing new = go new
  where
    go [] = (mapEmpty, existing)
    go (b:bs) = case List.find (eqBlockBodyWith (eqBid subst) b) existing of
        -- This block is a duplicate. Drop it, and add it to the substitution
        Just b' -> first (mapInsert (entryLabel b) (entryLabel b')) $ go bs
        -- This block is not a duplicate, keep it.
        Nothing -> second (b:) $ go bs

mergeBlockList :: Subst -> NonEmpty DistinctBlocks -> (Subst, DistinctBlocks)
mergeBlockList subst (b:|bs) = go mapEmpty b bs
  where
    go !new_subst1 b [] = (new_subst1, b)
    go !new_subst1 b1 (b2:bs) = go new_subst b bs
      where
        (new_subst2, b) =  mergeBlocks subst b1 b2
        new_subst = new_subst1 `mapUnion` new_subst2


-- -----------------------------------------------------------------------------
-- Hashing and equality on blocks

-- Below here is mostly boilerplate: hashing blocks ignoring labels,
-- and comparing blocks modulo a label mapping.

-- To speed up comparisons, we hash each basic block modulo jump labels.
-- The hashing is a bit arbitrary (the numbers are completely arbitrary),
-- but it should be fast and good enough.

-- We want to get as many small buckets as possible, as comparing blocks is
-- expensive. So include as much as possible in the hash. Ideally everything
-- that is compared with (==) in eqBlockBodyWith.

type HashCode = Int

hash_block :: CmmBlock -> HashCode
hash_block block =
  fromIntegral (foldBlockNodesB3 (hash_fst, hash_mid, hash_lst) block (0 :: Word32) .&. (0x7fffffff :: Word32))
  -- UniqFM doesn't like negative Ints
  where hash_fst _ h = h
        hash_mid m h = hash_node m + h `shiftL` 1
        hash_lst m h = hash_node m + h `shiftL` 1

        hash_node :: CmmNode O x -> Word32
        hash_node n | dont_care n = 0 -- don't care
        hash_node (CmmAssign r e) = hash_reg r + hash_e e
        hash_node (CmmStore e e' _) = hash_e e + hash_e e'
        hash_node (CmmUnsafeForeignCall t _ as) = hash_tgt t + hash_list hash_e as
        hash_node (CmmBranch _) = 23 -- NB. ignore the label
        hash_node (CmmCondBranch p _ _ _) = hash_e p
        hash_node (CmmCall e _ _ _ _ _) = hash_e e
        hash_node (CmmForeignCall t _ _ _ _ _ _) = hash_tgt t
        hash_node (CmmSwitch e _) = hash_e e
        hash_node _ = error "hash_node: unknown Cmm node!"

        hash_reg :: CmmReg -> Word32
        hash_reg   (CmmLocal localReg) = hash_unique localReg -- important for performance, see #10397
        hash_reg   (CmmGlobal _)    = 19

        hash_e :: CmmExpr -> Word32
        hash_e (CmmLit l) = hash_lit l
        hash_e (CmmLoad e _ _) = 67 + hash_e e
        hash_e (CmmReg r) = hash_reg r
        hash_e (CmmMachOp _ es) = hash_list hash_e es -- pessimal - no operator check
        hash_e (CmmRegOff r i) = hash_reg r + cvt i
        hash_e (CmmStackSlot _ _) = 13

        hash_lit :: CmmLit -> Word32
        hash_lit (CmmInt i _) = fromInteger i
        hash_lit (CmmFloat r _) = truncate r
        hash_lit (CmmVec ls) = hash_list hash_lit ls
        hash_lit (CmmLabel _) = 119 -- ugh
        hash_lit (CmmLabelOff _ i) = cvt $ 199 + i
        hash_lit (CmmLabelDiffOff _ _ i _) = cvt $ 299 + i
        hash_lit (CmmBlock _) = 191 -- ugh
        hash_lit (CmmHighStackMark) = cvt 313

        hash_tgt (ForeignTarget e _) = hash_e e
        hash_tgt (PrimTarget _) = 31 -- lots of these

        hash_list f = foldl' (\z x -> f x + z) (0::Word32)

        cvt = fromInteger . toInteger

        -- Since we are hashing, we can savely downcast Word64 to Word32 here.
        -- Although a different hashing function may be more effective.
        hash_unique :: Uniquable a => a -> Word32
        hash_unique = truncateWord64ToWord32 . getKey . getUnique

-- | Ignore these node types for equality
dont_care :: CmmNode O x -> Bool
dont_care CmmComment {}  = True
dont_care CmmTick {}     = True
dont_care CmmUnwind {}   = True
dont_care _other         = False

-- Utilities: equality and substitution on the graph.

-- Given a map ``subst'' from BlockID -> BlockID, we define equality.
eqBid :: LabelMap BlockId -> BlockId -> BlockId -> Bool
eqBid subst bid bid' = lookupBid subst bid == lookupBid subst bid'
lookupBid :: LabelMap BlockId -> BlockId -> BlockId
lookupBid subst bid = case mapLookup bid subst of
                        Just bid  -> lookupBid subst bid
                        Nothing -> bid

-- Middle nodes and expressions can contain BlockIds, in particular in
-- CmmStackSlot and CmmBlock, so we have to use a special equality for
-- these.
--
eqMiddleWith :: (BlockId -> BlockId -> Bool)
             -> CmmNode O O -> CmmNode O O -> Bool
eqMiddleWith eqBid (CmmAssign r1 e1) (CmmAssign r2 e2)
  = r1 == r2 && eqExprWith eqBid e1 e2
eqMiddleWith eqBid (CmmStore l1 r1 _) (CmmStore l2 r2 _)
  = eqExprWith eqBid l1 l2 && eqExprWith eqBid r1 r2
eqMiddleWith eqBid (CmmUnsafeForeignCall t1 r1 a1)
                   (CmmUnsafeForeignCall t2 r2 a2)
  = t1 == t2 && r1 == r2 && liftEq (eqExprWith eqBid) a1 a2
eqMiddleWith _ _ _ = False

eqExprWith :: (BlockId -> BlockId -> Bool)
           -> CmmExpr -> CmmExpr -> Bool
eqExprWith eqBid = eq
 where
  CmmLit l1          `eq` CmmLit l2          = eqLit l1 l2
  CmmLoad e1 t1 a1   `eq` CmmLoad e2 t2 a2   = t1 `cmmEqType` t2 && e1 `eq` e2 && a1==a2
  CmmReg r1          `eq` CmmReg r2          = r1==r2
  CmmRegOff r1 i1    `eq` CmmRegOff r2 i2    = r1==r2 && i1==i2
  CmmMachOp op1 es1  `eq` CmmMachOp op2 es2  =
    case testEquality (sizeOfTupleGADT es1) (sizeOfTupleGADT es2) of
      Just Refl -> op1==op2 && liftEq eq es1 es2
      Nothing   -> False
  CmmStackSlot a1 i1 `eq` CmmStackSlot a2 i2 = eqArea a1 a2 && i1==i2
  _e1                `eq` _e2                = False

  eqLit (CmmBlock id1) (CmmBlock id2) = eqBid id1 id2
  eqLit l1 l2 = l1 == l2

  eqArea Old Old = True
  eqArea (Young id1) (Young id2) = eqBid id1 id2
  eqArea _ _ = False

-- Equality on the body of a block, modulo a function mapping block
-- IDs to block IDs.
eqBlockBodyWith :: (BlockId -> BlockId -> Bool) -> CmmBlock -> CmmBlock -> Bool
eqBlockBodyWith eqBid block block'
  {-
  | equal     = pprTrace "equal" (vcat [ppr block, ppr block']) True
  | otherwise = pprTrace "not equal" (vcat [ppr block, ppr block']) False
  -}
  = equal
  where (_,m,l)   = blockSplit block
        nodes     = filter (not . dont_care) (blockToList m)
        (_,m',l') = blockSplit block'
        nodes'    = filter (not . dont_care) (blockToList m')

        equal = liftEq (eqMiddleWith eqBid) nodes nodes' &&
                eqLastWith eqBid l l'


eqLastWith :: (BlockId -> BlockId -> Bool) -> CmmNode O C -> CmmNode O C -> Bool
eqLastWith eqBid (CmmBranch bid1) (CmmBranch bid2) = eqBid bid1 bid2
eqLastWith eqBid (CmmCondBranch c1 t1 f1 l1) (CmmCondBranch c2 t2 f2 l2) =
  c1 == c2 && l1 == l2 && eqBid t1 t2 && eqBid f1 f2
eqLastWith eqBid (CmmCall t1 c1 g1 a1 r1 u1) (CmmCall t2 c2 g2 a2 r2 u2) =
  t1 == t2 && liftEq eqBid c1 c2 && a1 == a2 && r1 == r2 && u1 == u2 && g1 == g2
eqLastWith eqBid (CmmSwitch e1 ids1) (CmmSwitch e2 ids2) =
  e1 == e2 && eqSwitchTargetWith eqBid ids1 ids2
eqLastWith _ _ _ = False

-- | Given a block map, ensure that all "target" blocks are covered by
-- the same ticks as the respective "source" blocks. This not only
-- means copying ticks, but also adjusting tick scopes where
-- necessary.
copyTicks :: LabelMap BlockId -> CmmGraph -> CmmGraph
copyTicks env g
  | mapNull env = g
  | otherwise   = ofBlockMap (g_entry g) $ mapMap copyTo blockMap
  where -- Reverse block merge map
        blockMap = toBlockMap g
        revEnv = mapFoldlWithKey insertRev M.empty env
        insertRev m k x = M.insertWith (const (k:)) x [k] m
        -- Copy ticks and scopes into the given block
        copyTo block = case M.lookup (entryLabel block) revEnv of
          Nothing -> block
          Just ls -> foldr copy block $ mapMaybe (flip mapLookup blockMap) ls
        copy from to =
          let ticks = blockTicks from
              CmmEntry  _   scp0        = firstNode from
              (CmmEntry lbl scp1, code) = blockSplitHead to
          in CmmEntry lbl (combineTickScopes scp0 scp1) `blockJoinHead`
             foldr blockCons code (map CmmTick ticks)

-- Group by [Label]
-- See Note [Compressed TrieMap] in GHC.Core.Map.Expr about the usage of GenMap.
groupByLabel :: [(Key, DistinctBlocks)] -> [(Key, NonEmpty DistinctBlocks)]
groupByLabel =
  go (TM.emptyTM :: TM.ListMap (TM.GenMap LabelMap) (Key, NonEmpty DistinctBlocks))
    where
      go !m [] = TM.foldTM (:) m []
      go !m ((k,v) : entries) = go (TM.alterTM k adjust m) entries
        where --k' = map (getKey . getUnique) k
              adjust Nothing       = Just (k, pure v)
              adjust (Just (_,vs)) = Just (k, v NE.<| vs)

groupByInt :: (a -> Int) -> [a] -> [[a]]
groupByInt f xs = nonDetEltsUFM $ List.foldl' go emptyUFM xs
   -- See Note [Unique Determinism and code generation]
  where
    go m x = alterUFM addEntry m (f x)
      where
        addEntry xs = Just $! maybe [x] (x:) xs
