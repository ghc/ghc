{-# LANGUAGE GADTs #-}
module CmmCommonBlockElim
  ( elimCommonBlocks
  )
where


import BlockId
import Cmm
import CmmUtils
import CmmContFlowOpt
import Prelude hiding (iterate, succ, unzip, zip)

import Hoopl hiding (ChangeFlag)
import Data.Bits
import qualified Data.List as List
import Data.Word
import Outputable
import UniqFM

my_trace :: String -> SDoc -> a -> a
my_trace = if False then pprTrace else \_ _ a -> a

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

-- TODO: Use optimization fuel
elimCommonBlocks :: CmmGraph -> CmmGraph
elimCommonBlocks g = replaceLabels env g
  where
     env = iterate hashed_blocks mapEmpty
     hashed_blocks = map (\b -> (hash_block b, b)) $ postorderDfs g

-- Iterate over the blocks until convergence
iterate :: [(HashCode,CmmBlock)] -> BlockEnv BlockId -> BlockEnv BlockId
iterate blocks subst =
  case foldl common_block (False, emptyUFM, subst) blocks of
    (changed,  _, subst)
       | changed   -> iterate blocks subst
       | otherwise -> subst

type State  = (ChangeFlag, UniqFM [CmmBlock], BlockEnv BlockId)

type ChangeFlag = Bool
type HashCode = Int

-- Try to find a block that is equal (or ``common'') to b.
common_block :: State -> (HashCode, CmmBlock) -> State
common_block (old_change, bmap, subst) (hash, b) =
  case lookupUFM bmap hash of
    Just bs -> case (List.find (eqBlockBodyWith (eqBid subst) b) bs,
                     mapLookup bid subst) of
                 (Just b', Nothing)                         -> addSubst b'
                 (Just b', Just b'') | entryLabel b' /= b'' -> addSubst b'
                                     | otherwise -> (old_change, bmap, subst)
                 _ -> (old_change, addToUFM bmap hash (b : bs), subst)
    Nothing -> (old_change, addToUFM bmap hash [b], subst)
  where bid = entryLabel b
        addSubst b' = my_trace "found new common block" (ppr bid <> char '=' <> ppr (entryLabel b')) $
                      (True, bmap, mapInsert bid (entryLabel b') subst)


-- -----------------------------------------------------------------------------
-- Hashing and equality on blocks

-- Below here is mostly boilerplate: hashing blocks ignoring labels,
-- and comparing blocks modulo a label mapping.

-- To speed up comparisons, we hash each basic block modulo labels.
-- The hashing is a bit arbitrary (the numbers are completely arbitrary),
-- but it should be fast and good enough.
hash_block :: CmmBlock -> HashCode
hash_block block =
  fromIntegral (foldBlockNodesB3 (hash_fst, hash_mid, hash_lst) block (0 :: Word32) .&. (0x7fffffff :: Word32))
  -- UniqFM doesn't like negative Ints
  where hash_fst _ h = h
        hash_mid m h = hash_node m + h `shiftL` 1
        hash_lst m h = hash_node m + h `shiftL` 1

        hash_node :: CmmNode O x -> Word32
        hash_node (CmmComment _) = 0 -- don't care
        hash_node (CmmAssign r e) = hash_reg r + hash_e e
        hash_node (CmmStore e e') = hash_e e + hash_e e'
        hash_node (CmmUnsafeForeignCall t _ as) = hash_tgt t + hash_list hash_e as
        hash_node (CmmBranch _) = 23 -- NB. ignore the label
        hash_node (CmmCondBranch p _ _) = hash_e p
        hash_node (CmmCall e _ _ _ _ _) = hash_e e
        hash_node (CmmForeignCall t _ _ _ _ _) = hash_tgt t
        hash_node (CmmSwitch e _) = hash_e e

        hash_reg :: CmmReg -> Word32
        hash_reg   (CmmLocal _) = 117
        hash_reg   (CmmGlobal _)    = 19

        hash_e :: CmmExpr -> Word32
        hash_e (CmmLit l) = hash_lit l
        hash_e (CmmLoad e _) = 67 + hash_e e
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
        hash_lit (CmmLabelDiffOff _ _ i) = cvt $ 299 + i
        hash_lit (CmmBlock _) = 191 -- ugh
        hash_lit (CmmHighStackMark) = cvt 313

        hash_tgt (ForeignTarget e _) = hash_e e
        hash_tgt (PrimTarget _) = 31 -- lots of these

        hash_list f = foldl (\z x -> f x + z) (0::Word32)

        cvt = fromInteger . toInteger
-- Utilities: equality and substitution on the graph.

-- Given a map ``subst'' from BlockID -> BlockID, we define equality.
eqBid :: BlockEnv BlockId -> BlockId -> BlockId -> Bool
eqBid subst bid bid' = lookupBid subst bid == lookupBid subst bid'
lookupBid :: BlockEnv BlockId -> BlockId -> BlockId
lookupBid subst bid = case mapLookup bid subst of
                        Just bid  -> lookupBid subst bid
                        Nothing -> bid

-- Middle nodes and expressions can contain BlockIds, in particular in
-- CmmStackSlot and CmmBlock, so we have to use a special equality for
-- these.
--
eqMiddleWith :: (BlockId -> BlockId -> Bool)
             -> CmmNode O O -> CmmNode O O -> Bool
eqMiddleWith _ (CmmComment _) (CmmComment _) = True
eqMiddleWith eqBid (CmmAssign r1 e1) (CmmAssign r2 e2)
  = r1 == r2 && eqExprWith eqBid e1 e2
eqMiddleWith eqBid (CmmStore l1 r1) (CmmStore l2 r2)
  = eqExprWith eqBid l1 l2 && eqExprWith eqBid r1 r2
eqMiddleWith eqBid (CmmUnsafeForeignCall t1 r1 a1)
                   (CmmUnsafeForeignCall t2 r2 a2)
  = t1 == t2 && r1 == r2 && and (zipWith (eqExprWith eqBid) a1 a2)
eqMiddleWith _ _ _ = False

eqExprWith :: (BlockId -> BlockId -> Bool)
           -> CmmExpr -> CmmExpr -> Bool
eqExprWith eqBid = eq
 where
  CmmLit l1          `eq` CmmLit l2          = eqLit l1 l2
  CmmLoad e1 _       `eq` CmmLoad e2 _       = e1 `eq` e2
  CmmReg r1          `eq` CmmReg r2          = r1==r2
  CmmRegOff r1 i1    `eq` CmmRegOff r2 i2    = r1==r2 && i1==i2
  CmmMachOp op1 es1  `eq` CmmMachOp op2 es2  = op1==op2 && es1 `eqs` es2
  CmmStackSlot a1 i1 `eq` CmmStackSlot a2 i2 = eqArea a1 a2 && i1==i2
  _e1                `eq` _e2                = False

  xs `eqs` ys = and (zipWith eq xs ys)

  eqLit (CmmBlock id1) (CmmBlock id2) = eqBid id1 id2
  eqLit l1 l2 = l1 == l2

  eqArea Old Old = True
  eqArea (Young id1) (Young id2) = eqBid id1 id2
  eqArea _ _ = False

-- Equality on the body of a block, modulo a function mapping block
-- IDs to block IDs.
eqBlockBodyWith :: (BlockId -> BlockId -> Bool) -> CmmBlock -> CmmBlock -> Bool
eqBlockBodyWith eqBid block block'
  = and (zipWith (eqMiddleWith eqBid) (blockToList m) (blockToList m')) &&
    eqLastWith eqBid l l'
  where (_,m,l)   = blockSplit block
        (_,m',l') = blockSplit block'



eqLastWith :: (BlockId -> BlockId -> Bool) -> CmmNode O C -> CmmNode O C -> Bool
eqLastWith eqBid (CmmBranch bid1) (CmmBranch bid2) = eqBid bid1 bid2
eqLastWith eqBid (CmmCondBranch c1 t1 f1) (CmmCondBranch c2 t2 f2) =
  c1 == c2 && eqBid t1 t2 && eqBid f1 f2
eqLastWith eqBid (CmmCall t1 c1 g1 a1 r1 u1) (CmmCall t2 c2 g2 a2 r2 u2) =
  t1 == t2 && eqMaybeWith eqBid c1 c2 && a1 == a2 && r1 == r2 && u1 == u2 && g1 == g2
eqLastWith eqBid (CmmSwitch e1 bs1) (CmmSwitch e2 bs2) =
  e1 == e2 && eqListWith (eqMaybeWith eqBid) bs1 bs2
eqLastWith _ _ _ = False

eqListWith :: (a -> b -> Bool) -> [a] -> [b] -> Bool
eqListWith eltEq es es' = all (uncurry eltEq) (List.zip es es')

eqMaybeWith :: (a -> b -> Bool) -> Maybe a -> Maybe b -> Bool
eqMaybeWith eltEq (Just e) (Just e') = eltEq e e'
eqMaybeWith _ Nothing Nothing = True
eqMaybeWith _ _ _ = False
