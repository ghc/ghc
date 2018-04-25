{-# LANGUAGE GADTs, BangPatterns #-}
module CmmCommonBlockElim
  ( elimCommonBlocks
  )
where


import GhcPrelude hiding (iterate, succ, unzip, zip)

import BlockId
import Cmm
import CmmUtils
import CmmSwitch (eqSwitchTargetWith)
import CmmContFlowOpt
-- import PprCmm ()

import Hoopl.Block
import Hoopl.Graph
import Hoopl.Label
import Hoopl.Collections
import Data.Bits
import Data.Maybe (mapMaybe)
import qualified Data.List as List
import Data.Word
import qualified Data.Map as M
import Outputable
import DynFlags (DynFlags)
import UniqFM
import UniqDFM
import qualified TrieMap as TM
import Unique
import Control.Arrow (first, second)

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
elimCommonBlocks :: DynFlags -> CmmGraph -> CmmGraph
elimCommonBlocks dflags g = replaceLabels env $ copyTicks env g
  where
     env = iterate dflags mapEmpty blocks_with_key
     groups = groupByInt (hash_block dflags) (postorderDfs g)
     blocks_with_key = [ [ (successors b, [b]) | b <- bs] | bs <- groups]

-- Invariant: The blocks in the list are pairwise distinct
-- (so avoid comparing them again)
type DistinctBlocks = [CmmBlock]
type Key = [Label]
type Subst = LabelMap BlockId

-- The outer list groups by hash. We retain this grouping throughout.
iterate :: DynFlags -> Subst -> [[(Key, DistinctBlocks)]] -> Subst
iterate dflags subst blocks
    | mapNull new_substs = subst
    | otherwise = iterate dflags subst' updated_blocks
  where
    grouped_blocks :: [[(Key, [DistinctBlocks])]]
    grouped_blocks = map groupByLabel blocks

    merged_blocks :: [[(Key, DistinctBlocks)]]
    (new_substs, merged_blocks) =
        List.mapAccumL (List.mapAccumL go) mapEmpty grouped_blocks
      where
        go !new_subst1 (k,dbs) = (new_subst1 `mapUnion` new_subst2, (k,db))
          where
            (new_subst2, db) = mergeBlockList dflags subst dbs

    subst' = subst `mapUnion` new_substs
    updated_blocks = map (map (first (map (lookupBid subst')))) merged_blocks

mergeBlocks :: DynFlags -> Subst
            -> DistinctBlocks -> DistinctBlocks
            -> (Subst, DistinctBlocks)
mergeBlocks dflags subst existing new = go new
  where
    go [] = (mapEmpty, existing)
    go (b:bs) =
        case List.find (eqBlockBodyWith dflags (eqBid subst) b) existing of
          -- This block is a duplicate. Drop it, and add it to the substitution
          Just b' -> first (mapInsert (entryLabel b) (entryLabel b')) $ go bs
          -- This block is not a duplicate, keep it.
          Nothing -> second (b:) $ go bs

mergeBlockList :: DynFlags -> Subst -> [DistinctBlocks]
               -> (Subst, DistinctBlocks)
mergeBlockList _      _     [] = pprPanic "mergeBlockList" empty
mergeBlockList dflags subst (b:bs) = go mapEmpty b bs
  where
    go !new_subst1 b [] = (new_subst1, b)
    go !new_subst1 b1 (b2:bs) = go new_subst b bs
      where
        (new_subst2, b) =  mergeBlocks dflags subst b1 b2
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

{-
Note [Equivalence up to local registers in CBE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CBE treats two blocks which are equivalent up to alpha-renaming of locally-bound
local registers as equivalent. This was not always the case (see #14226) but is
quite important for effective CBE. For instance, consider the blocks,

    c2VZ: // global
        _c2Yd::I64 = _s2Se::I64 + 1;
        _s2Sx::I64 = _c2Yd::I64;
        _s2Se::I64 = _s2Sx::I64;
        goto c2TE;

    c2VY: // global
        _c2Yb::I64 = _s2Se::I64 + 1;
        _s2Sw::I64 = _c2Yb::I64;
        _s2Se::I64 = _s2Sw::I64;
        goto c2TE;

These clearly implement precisely the same logic, differing only register
naming. This happens quite often in the code produced by GHC.

This alpha-equivalence relation must be accounted for in two places:

 1. the block hash function (hash_block), which we use for approximate "binning"
 2. the exact block comparison function, which computes pair-wise equivalence

In (1) we maintain a de Bruijn numbering of each block's locally-bound local
registers and compute the hash relative to this numbering.

For (2) we maintain a substitution which maps the local registers of one block
onto those of the other. We then compare local registers modulo this
substitution.

-}

type HashCode = Int

type LocalRegEnv a = UniqFM a
type DeBruijn = Int

-- | Maintains a de Bruijn numbering of local registers bound within a block.
--
-- See Note [Equivalence up to local registers in CBE]
data HashEnv = HashEnv { localRegHashEnv :: !(LocalRegEnv DeBruijn)
                       , nextIndex       :: !DeBruijn
                       }

hash_block :: DynFlags -> CmmBlock -> HashCode
hash_block dflags block =
  --pprTrace "hash_block" (ppr (entryLabel block) $$ ppr hash)
  hash
  where hash_fst _ (env, h) = (env, h)
        hash_mid m (env, h) = let (env', h') = hash_node env m
                              in (env', h' + h `shiftL` 1)
        hash_lst m (env, h) = let (env', h') = hash_node env m
                              in (env', h' + h `shiftL` 1)

        hash =
            let (_, raw_hash) =
                    foldBlockNodesF3 (hash_fst, hash_mid, hash_lst)
                                     block
                                     (emptyEnv, 0 :: Word32)
                emptyEnv = HashEnv mempty 0
            in fromIntegral (raw_hash .&. (0x7fffffff :: Word32))
               -- UniqFM doesn't like negative Ints

        hash_node :: HashEnv -> CmmNode O x -> (HashEnv, Word32)
        hash_node env n =
            (env', hash)
          where
            hash =
              case n of
                n | dont_care n -> 0  -- don't care
                -- don't include register as it is a binding occurrence
                CmmAssign (CmmLocal _) e -> hash_e env e
                CmmAssign r e   -> hash_reg env r + hash_e env e
                CmmStore e e'   -> hash_e env e + hash_e env e'
                CmmUnsafeForeignCall t _ as
                                -> hash_tgt env t + hash_list (hash_e env) as
                CmmBranch _     ->  23 -- NB. ignore the label
                CmmCondBranch p _ _ _ -> hash_e env p
                CmmCall e _ _ _ _ _   -> hash_e env e
                CmmForeignCall t _ _ _ _ _ _ -> hash_tgt env t
                CmmSwitch e _   -> hash_e env e
                _               -> error "hash_node: unknown Cmm node!"
            env' = foldLocalRegsDefd dflags (flip bind_local_reg) env n

        hash_reg :: HashEnv -> CmmReg -> Word32
        hash_reg env (CmmLocal localReg)
          | Just idx <- lookupUFM (localRegHashEnv env) localReg
          = fromIntegral idx
          | otherwise
          = hash_unique localReg -- important for performance, see #10397
        hash_reg _  (CmmGlobal _)    = 19

        hash_e :: HashEnv -> CmmExpr -> Word32
        hash_e _   (CmmLit l) = hash_lit l
        hash_e env (CmmLoad e _) = 67 + hash_e env e
        hash_e env (CmmReg r) = hash_reg env r
        hash_e env (CmmMachOp _ es) = hash_list (hash_e env) es -- pessimal - no operator check
        hash_e env (CmmRegOff r i) = hash_reg env r + cvt i
        hash_e _   (CmmStackSlot _ _) = 13

        hash_lit :: CmmLit -> Word32
        hash_lit (CmmInt i _) = fromInteger i
        hash_lit (CmmFloat r _) = truncate r
        hash_lit (CmmVec ls) = hash_list hash_lit ls
        hash_lit (CmmLabel _) = 119 -- ugh
        hash_lit (CmmLabelOff _ i) = cvt $ 199 + i
        hash_lit (CmmLabelDiffOff _ _ i) = cvt $ 299 + i
        hash_lit (CmmBlock _) = 191 -- ugh
        hash_lit (CmmHighStackMark) = cvt 313

        hash_tgt :: HashEnv -> ForeignTarget -> Word32
        hash_tgt env (ForeignTarget e _) = hash_e env e
        hash_tgt _   (PrimTarget _) = 31 -- lots of these

        hash_list f = List.foldl' (\z x -> f x + z) (0::Word32)

        cvt = fromInteger . toInteger

        bind_local_reg :: LocalReg -> HashEnv -> HashEnv
        bind_local_reg reg env =
            env { localRegHashEnv =
                    addToUFM (localRegHashEnv env) reg (nextIndex env)
                , nextIndex = nextIndex env + 1
                }

        hash_unique :: Uniquable a => a -> Word32
        hash_unique = cvt . getKey . getUnique

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

-- | Maps the local registers of one block to those of another
--
-- See Note [Equivalence up to local registers in CBE]
type LocalRegMapping = LocalRegEnv LocalReg

-- Middle nodes and expressions can contain BlockIds, in particular in
-- CmmStackSlot and CmmBlock, so we have to use a special equality for
-- these.
--
eqMiddleWith :: DynFlags
             -> (BlockId -> BlockId -> Bool)
             -> LocalRegMapping
             -> CmmNode O O -> CmmNode O O
             -> (LocalRegMapping, Bool)
eqMiddleWith dflags eqBid env a b =
  case (a, b) of
     -- registers aren't compared since they are binding occurrences
    (CmmAssign (CmmLocal _) e1,  CmmAssign (CmmLocal _) e2) ->
        let eq = eqExprWith eqBid env e1 e2
        in (env', eq)

    (CmmAssign r1 e1,  CmmAssign r2 e2) ->
        let eq = r1 == r2
              && eqExprWith eqBid env e1 e2
        in (env', eq)

    (CmmStore l1 r1,  CmmStore l2 r2) ->
        let eq = eqExprWith eqBid env l1 l2
              && eqExprWith eqBid env r1 r2
        in (env', eq)

     -- result registers aren't compared since they are binding occurrences
    (CmmUnsafeForeignCall t1 _ a1,  CmmUnsafeForeignCall t2 _ a2) ->
        let eq = t1 == t2
              && eqLists (eqExprWith eqBid env) a1 a2
        in (env', eq)

    _ -> (env, False)
  where
    env' = List.foldl' (\acc (ra,rb) -> addToUFM acc ra rb) emptyUFM
           $ List.zip defd_a defd_b
    defd_a = foldLocalRegsDefd dflags (flip (:)) [] a
    defd_b = foldLocalRegsDefd dflags (flip (:)) [] b

eqLists :: (a -> b -> Bool) -> [a] -> [b] -> Bool
eqLists f (a:as) (b:bs) = f a b && eqLists f as bs
eqLists _ []     []     = True
eqLists _ _      _      = False

eqExprWith :: (BlockId -> BlockId -> Bool)
           -> LocalRegMapping
           -> CmmExpr -> CmmExpr
           -> Bool
eqExprWith eqBid env = eq
 where
  CmmLit l1          `eq` CmmLit l2          = eqLit l1 l2
  CmmLoad e1 _       `eq` CmmLoad e2 _       = e1 `eq` e2
  CmmReg r1          `eq` CmmReg r2          = r1 `eqReg` r2
  CmmRegOff r1 i1    `eq` CmmRegOff r2 i2    = r1 `eqReg` r2 && i1==i2
  CmmMachOp op1 es1  `eq` CmmMachOp op2 es2  = op1==op2 && es1 `eqs` es2
  CmmStackSlot a1 i1 `eq` CmmStackSlot a2 i2 = eqArea a1 a2 && i1==i2
  _e1                `eq` _e2                = False

  xs `eqs` ys = eqLists eq xs ys

  -- See Note [Equivalence up to local registers in CBE]
  CmmLocal a `eqReg` CmmLocal b
    | Just a' <- lookupUFM env a
    = a' == b
  a `eqReg` b = a == b

  eqLit (CmmBlock id1) (CmmBlock id2) = eqBid id1 id2
  eqLit l1 l2 = l1 == l2

  eqArea Old Old = True
  eqArea (Young id1) (Young id2) = eqBid id1 id2
  eqArea _ _ = False

-- Equality on the body of a block, modulo a function mapping block
-- IDs to block IDs.
eqBlockBodyWith :: DynFlags
                -> (BlockId -> BlockId -> Bool)
                -> CmmBlock -> CmmBlock -> Bool
eqBlockBodyWith dflags eqBid block block'
  {-
  | equal     = pprTrace "equal" (vcat [ppr block, ppr block']) True
  | otherwise = pprTrace "not equal" (vcat [ppr block, ppr block']) False
  -}
  = equal
  where (_,m,l)   = blockSplit block
        nodes     = filter (not . dont_care) (blockToList m)
        (_,m',l') = blockSplit block'
        nodes'    = filter (not . dont_care) (blockToList m')

        eqMids :: LocalRegMapping -> [CmmNode O O] -> [CmmNode O O] -> Bool
        eqMids env (a:as) (b:bs)
          | eq = eqMids env' as bs
          where
            (env', eq) = eqMiddleWith dflags eqBid env a b
        eqMids env [] [] = eqLastWith eqBid env l l'
        eqMids _ _ _ = False

        equal = eqMids emptyUFM nodes nodes'


eqLastWith :: (BlockId -> BlockId -> Bool) -> LocalRegMapping
           -> CmmNode O C -> CmmNode O C -> Bool
eqLastWith eqBid env a b =
    case (a, b) of
      (CmmBranch bid1, CmmBranch bid2) -> eqBid bid1 bid2
      (CmmCondBranch c1 t1 f1 l1, CmmCondBranch c2 t2 f2 l2) ->
          eqExprWith eqBid env c1 c2 && l1 == l2 && eqBid t1 t2 && eqBid f1 f2
      (CmmCall t1 c1 g1 a1 r1 u1, CmmCall t2 c2 g2 a2 r2 u2) ->
             t1 == t2
          && eqMaybeWith eqBid c1 c2
          && a1 == a2 && r1 == r2 && u1 == u2 && g1 == g2
      (CmmSwitch e1 ids1, CmmSwitch e2 ids2) ->
          eqExprWith eqBid env e1 e2 && eqSwitchTargetWith eqBid ids1 ids2
      -- result registers aren't compared since they are binding occurrences
      (CmmForeignCall t1 _ a1 s1 ret_args1 ret_off1 intrbl1,
       CmmForeignCall t2 _ a2 s2 ret_args2 ret_off2 intrbl2) ->
             t1 == t2
          && eqLists (eqExprWith eqBid env) a1 a2
          && s1 == s2
          && ret_args1 == ret_args2
          && ret_off1 == ret_off2
          && intrbl1 == intrbl2
      _ -> False

eqMaybeWith :: (a -> b -> Bool) -> Maybe a -> Maybe b -> Bool
eqMaybeWith eltEq (Just e) (Just e') = eltEq e e'
eqMaybeWith _ Nothing Nothing = True
eqMaybeWith _ _ _ = False

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
        revEnv = mapFoldWithKey insertRev M.empty env
        insertRev k x = M.insertWith (const (k:)) x [k]
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
groupByLabel :: [(Key, a)] -> [(Key, [a])]
groupByLabel = go (TM.emptyTM :: TM.ListMap UniqDFM a)
  where
    go !m [] = TM.foldTM (:) m []
    go !m ((k,v) : entries) = go (TM.alterTM k' adjust m) entries
      where k' = map getUnique k
            adjust Nothing       = Just (k,[v])
            adjust (Just (_,vs)) = Just (k,v:vs)


groupByInt :: (a -> Int) -> [a] -> [[a]]
groupByInt f xs = nonDetEltsUFM $ List.foldl' go emptyUFM xs
  -- See Note [Unique Determinism and code generation]
  where go m x = alterUFM (Just . maybe [x] (x:)) m (f x)
