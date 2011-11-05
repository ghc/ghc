{-# LANGUAGE GADTs #-}
-- ToDo: remove -fno-warn-warnings-deprecations
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-- ToDo: remove -fno-warn-incomplete-patterns
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module CmmCommonBlockElim
  ( elimCommonBlocks
  )
where


import BlockId
import Cmm
import CmmUtils
import Prelude hiding (iterate, succ, unzip, zip)

import Compiler.Hoopl
import Data.Bits
import qualified Data.List as List
import Data.Word
import FastString
import Control.Monad
import Outputable
import UniqFM
import Unique

my_trace :: String -> SDoc -> a -> a
my_trace = if False then pprTrace else \_ _ a -> a

-- Eliminate common blocks:
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
elimCommonBlocks g =
    upd_graph g . snd $ iterate common_block reset hashed_blocks
                                (emptyUFM, mapEmpty)
      where hashed_blocks    = map (\b -> (hash_block b, b)) (reverse (postorderDfs g))
            reset (_, subst) = (emptyUFM, subst)

-- Iterate over the blocks until convergence
iterate :: (t -> a -> (Bool, t)) -> (t -> t) -> [a] -> t -> t
iterate upd reset blocks state =
  case foldl upd' (False, state) blocks of
    (True,  state') -> iterate upd reset blocks (reset state')
    (False, state') -> state'
  where upd' (b, s) a = let (b', s') = upd s a in (b || b', s') -- lift to track changes

-- Try to find a block that is equal (or ``common'') to b.
type BidMap = BlockEnv BlockId
type State  = (UniqFM [CmmBlock], BidMap)
common_block :: (Outputable h, Uniquable h) =>  State -> (h, CmmBlock) -> (Bool, State)
common_block (bmap, subst) (hash, b) =
  case lookupUFM bmap hash of
    Just bs -> case (List.find (eqBlockBodyWith (eqBid subst) b) bs,
                     mapLookup bid subst) of
                 (Just b', Nothing)                         -> addSubst b'
                 (Just b', Just b'') | entryLabel b' /= b'' -> addSubst b'
                 _ -> (False, (addToUFM bmap hash (b : bs), subst))
    Nothing -> (False, (addToUFM bmap hash [b], subst))
  where bid = entryLabel b
        addSubst b' = my_trace "found new common block" (ppr (entryLabel b')) $
                      (True, (bmap, mapInsert bid (entryLabel b') subst))

-- Given the map ``subst'' from BlockId -> BlockId, we rewrite the graph.
upd_graph :: CmmGraph -> BidMap -> CmmGraph
upd_graph g subst = mapGraphNodes (id, middle, last) g
  where middle = mapExpDeep exp
        last l = last' (mapExpDeep exp l)
        last' :: CmmNode O C -> CmmNode O C
        last' (CmmBranch bid)              = CmmBranch $ sub bid
        last' (CmmCondBranch p t f)        = cond p (sub t) (sub f)
        last' (CmmCall t (Just bid) a r o) = CmmCall t (Just $ sub bid) a r o
        last' l@(CmmCall _ Nothing _ _ _)  = l
        last' (CmmForeignCall t r a bid u i) = CmmForeignCall t r a (sub bid) u i
        last' (CmmSwitch e bs)             = CmmSwitch e $ map (liftM sub) bs
        cond p t f = if t == f then CmmBranch t else CmmCondBranch p t f
        exp (CmmStackSlot (CallArea (Young id))       off) =
             CmmStackSlot (CallArea (Young (sub id))) off
        exp (CmmLit (CmmBlock id)) = CmmLit (CmmBlock (sub id))
        exp e = e
        sub = lookupBid subst

-- To speed up comparisons, we hash each basic block modulo labels.
-- The hashing is a bit arbitrary (the numbers are completely arbitrary),
-- but it should be fast and good enough.
hash_block :: CmmBlock -> Int
hash_block block =
  fromIntegral (foldBlockNodesB3 (hash_fst, hash_mid, hash_lst) block (0 :: Word32) .&. (0x7fffffff :: Word32))
  -- UniqFM doesn't like negative Ints
  where hash_fst _ h = h
        hash_mid m h = hash_node m + h `shiftL` 1
        hash_lst m h = hash_node m + h `shiftL` 1

        hash_node :: CmmNode O x -> Word32
        hash_node (CmmComment (FastString u _ _ _ _)) = cvt u
        hash_node (CmmAssign r e) = hash_reg r + hash_e e
        hash_node (CmmStore e e') = hash_e e + hash_e e'
        hash_node (CmmUnsafeForeignCall t _ as) = hash_tgt t + hash_list hash_e as
        hash_node (CmmBranch _) = 23 -- would be great to hash these properly
        hash_node (CmmCondBranch p _ _) = hash_e p
        hash_node (CmmCall e _ _ _ _) = hash_e e
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
eqBid :: BidMap -> BlockId -> BlockId -> Bool
eqBid subst bid bid' = lookupBid subst bid == lookupBid subst bid'
lookupBid :: BidMap -> BlockId -> BlockId
lookupBid subst bid = case mapLookup bid subst of
                        Just bid  -> lookupBid subst bid
                        Nothing -> bid

-- Equality on the body of a block, modulo a function mapping block IDs to block IDs.
eqBlockBodyWith :: (BlockId -> BlockId -> Bool) -> CmmBlock -> CmmBlock -> Bool
eqBlockBodyWith eqBid block block' = middles == middles' && eqLastWith eqBid last last'
  where (_, middles , JustC last  :: MaybeC C (CmmNode O C)) = blockToNodeList block
        (_, middles', JustC last' :: MaybeC C (CmmNode O C)) = blockToNodeList block'

eqLastWith :: (BlockId -> BlockId -> Bool) -> CmmNode O C -> CmmNode O C -> Bool
eqLastWith eqBid (CmmBranch bid1) (CmmBranch bid2) = eqBid bid1 bid2
eqLastWith eqBid (CmmCondBranch c1 t1 f1) (CmmCondBranch c2 t2 f2) =
  c1 == c2 && eqBid t1 t2 && eqBid f1 f2
eqLastWith eqBid (CmmCall t1 c1 a1 r1 u1) (CmmCall t2 c2 a2 r2 u2) =
  t1 == t2 && eqMaybeWith eqBid c1 c2 && a1 == a2 && r1 == r2 && u1 == u2
eqLastWith eqBid (CmmSwitch e1 bs1) (CmmSwitch e2 bs2) =
  e1 == e2 && eqListWith (eqMaybeWith eqBid) bs1 bs2
eqLastWith _ _ _ = False

eqListWith :: (a -> b -> Bool) -> [a] -> [b] -> Bool
eqListWith eltEq es es' = all (uncurry eltEq) (List.zip es es')

eqMaybeWith :: (a -> b -> Bool) -> Maybe a -> Maybe b -> Bool
eqMaybeWith eltEq (Just e) (Just e') = eltEq e e'
eqMaybeWith _ Nothing Nothing = True
eqMaybeWith _ _ _ = False
