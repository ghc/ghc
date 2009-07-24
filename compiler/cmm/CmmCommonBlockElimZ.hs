module CmmCommonBlockElimZ
  ( elimCommonBlocks
  )
where


import BlockId
import CmmExpr
import Prelude hiding (iterate, zip, unzip)
import ZipCfg
import ZipCfgCmmRep

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
                                (emptyUFM, emptyBlockEnv)
      where hashed_blocks    = map (\b -> (hash_block b, b)) (reverse (postorder_dfs g))
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
                     lookupBlockEnv subst bid) of
                 (Just b', Nothing)                      -> addSubst b'
                 (Just b', Just b'') | blockId b' /= b'' -> addSubst b'
                 _ -> (False, (addToUFM bmap hash (b : bs), subst))
    Nothing -> (False, (addToUFM bmap hash [b], subst))
  where bid = blockId b
        addSubst b' = my_trace "found new common block" (ppr (blockId b')) $
                      (True, (bmap, extendBlockEnv subst bid (blockId b')))

-- Given the map ``subst'' from BlockId -> BlockId, we rewrite the graph.
upd_graph :: CmmGraph -> BidMap -> CmmGraph
upd_graph g subst = map_nodes id middle last g
  where middle = mapExpDeepMiddle exp
        last l = last' (mapExpDeepLast exp l)
        last' (LastBranch bid)            = LastBranch $ sub bid
        last' (LastCondBranch p t f)      = cond p (sub t) (sub f)
        last' (LastCall t (Just bid) args res u) = LastCall t (Just $ sub bid) args res u
        last' l@(LastCall _ Nothing _ _ _)  = l
        last' (LastSwitch e bs)           = LastSwitch e $ map (liftM sub) bs
        cond p t f = if t == f then LastBranch t else LastCondBranch p t f
        exp (CmmStackSlot (CallArea (Young id))       off) =
             CmmStackSlot (CallArea (Young (sub id))) off
        exp (CmmLit (CmmBlock id)) = CmmLit (CmmBlock (sub id))
        exp e = e
        sub = lookupBid subst

-- To speed up comparisons, we hash each basic block modulo labels.
-- The hashing is a bit arbitrary (the numbers are completely arbitrary),
-- but it should be fast and good enough.
hash_block :: CmmBlock -> Int
hash_block (Block _ t) =
  fromIntegral (hash_tail t (0 :: Word32) .&. (0x7fffffff :: Word32))
  -- UniqFM doesn't like negative Ints
  where hash_mid   (MidComment (FastString u _ _ _ _)) = cvt u
        hash_mid   (MidAssign r e) = hash_reg r + hash_e e
        hash_mid   (MidStore e e') = hash_e e + hash_e e'
        hash_mid   (MidForeignCall _ t _ as) = hash_tgt t + hash_lst hash_e as
        hash_reg :: CmmReg -> Word32
        hash_reg   (CmmLocal l) = hash_local l
        hash_reg   (CmmGlobal _)    = 19
        hash_local (LocalReg _ _) = 117
        hash_e :: CmmExpr -> Word32
        hash_e (CmmLit l) = hash_lit l
        hash_e (CmmLoad e _) = 67 + hash_e e
        hash_e (CmmReg r) = hash_reg r
        hash_e (CmmMachOp _ es) = hash_lst hash_e es -- pessimal - no operator check
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
        hash_lst f = foldl (\z x -> f x + z) (0::Word32)
        hash_last (LastBranch _) = 23 -- would be great to hash these properly
        hash_last (LastCondBranch p _ _) = hash_e p 
        hash_last (LastCall e _ _ _ _) = hash_e e
        hash_last (LastSwitch e _) = hash_e e
        hash_tail (ZLast LastExit) v = 29 + v `shiftL` 1
        hash_tail (ZLast (LastOther l)) v = hash_last l + (v `shiftL` 1)
        hash_tail (ZTail m t) v = hash_tail t (hash_mid m + (v `shiftL` 1))
        cvt = fromInteger . toInteger
-- Utilities: equality and substitution on the graph.

-- Given a map ``subst'' from BlockID -> BlockID, we define equality.
eqBid :: BidMap -> BlockId -> BlockId -> Bool
eqBid subst bid bid' = lookupBid subst bid == lookupBid subst bid'
lookupBid :: BidMap -> BlockId -> BlockId
lookupBid subst bid = case lookupBlockEnv subst bid of
                        Just bid  -> lookupBid subst bid
                        Nothing -> bid

-- Equality on the body of a block, modulo a function mapping block IDs to block IDs.
eqBlockBodyWith :: (BlockId -> BlockId -> Bool) -> CmmBlock -> CmmBlock -> Bool
eqBlockBodyWith eqBid (Block _ t) (Block _ t') = eqTailWith eqBid t t'

type CmmTail = ZTail Middle Last
eqTailWith :: (BlockId -> BlockId -> Bool) -> CmmTail -> CmmTail -> Bool
eqTailWith eqBid (ZTail m t) (ZTail m' t') = m == m' && eqTailWith eqBid t t'
eqTailWith _ (ZLast LastExit) (ZLast LastExit) = True
eqTailWith eqBid (ZLast (LastOther l)) (ZLast (LastOther l')) = eqLastWith eqBid l l'
eqTailWith _ _ _ = False

eqLastWith :: (BlockId -> BlockId -> Bool) -> Last -> Last -> Bool
eqLastWith eqBid (LastBranch bid1) (LastBranch bid2) = eqBid bid1 bid2
eqLastWith eqBid (LastCondBranch c1 t1 f1) (LastCondBranch c2 t2 f2) =
  c1 == c2 && eqBid t1 t2 && eqBid f1 f2
eqLastWith eqBid (LastCall t1 c1 a1 r1 u1) (LastCall t2 c2 a2 r2 u2) =
  t1 == t2 && eqMaybeWith eqBid c1 c2 && a1 == a2 && r1 == r2 && u1 == u2
eqLastWith eqBid (LastSwitch e1 bs1) (LastSwitch e2 bs2) =
  e1 == e2 && eqLstWith (eqMaybeWith eqBid) bs1 bs2
eqLastWith _ _ _ = False

eqLstWith :: (a -> b -> Bool) -> [a] -> [b] -> Bool
eqLstWith eltEq es es' = all (uncurry eltEq) (List.zip es es')

eqMaybeWith :: (a -> b -> Bool) -> Maybe a -> Maybe b -> Bool
eqMaybeWith eltEq (Just e) (Just e') = eltEq e e'
eqMaybeWith _ Nothing Nothing = True
eqMaybeWith _ _ _ = False
