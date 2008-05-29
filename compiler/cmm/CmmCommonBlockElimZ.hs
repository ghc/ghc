module CmmCommonBlockElimZ
  ( elimCommonBlocks
  )
where


import BlockId
import Cmm hiding (blockId)
import CmmExpr
import Prelude hiding (iterate, zip, unzip)
import ZipCfg
import ZipCfgCmmRep

import FastString
import FiniteMap
import List hiding (iterate)
import Monad
import Outputable
import UniqFM
import Unique

my_trace :: String -> SDoc -> a -> a
my_trace = if True then pprTrace else \_ _ a -> a

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
    upd_graph g . snd $ iterate common_block reset hashed_blocks (emptyUFM, emptyFM)
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
type BidMap = FiniteMap BlockId BlockId
type State  = (UniqFM [CmmBlock], BidMap)
common_block :: (Outputable h, Uniquable h) =>  State -> (h, CmmBlock) -> (Bool, State)
common_block (bmap, subst) (hash, b) =
  case lookupUFM bmap $ my_trace "common_block" (ppr bid <+> ppr subst <+> ppr hash) $ hash of
    Just bs -> case (find (eqBlockBodyWith (eqBid subst) b) bs, lookupFM subst bid) of
                 (Just b', Nothing)                      -> addSubst b'
                 (Just b', Just b'') | blockId b' /= b'' -> addSubst b'
                 _ -> (False, (addToUFM bmap hash (b : bs), subst))
    Nothing -> (False, (addToUFM bmap hash [b], subst))
  where bid = blockId b
        addSubst b' = my_trace "found new common block" (ppr (blockId b')) $
                      (True, (bmap, addToFM subst bid (blockId b')))

-- Given the map ``subst'' from BlockId -> BlockId, we rewrite the graph.
upd_graph :: CmmGraph -> BidMap -> CmmGraph
upd_graph g subst = map_nodes id middle last g
  where middle m = m
        last (LastBranch bid)       = LastBranch $ sub bid
        last (LastCondBranch p t f) = cond p (sub t) (sub f)
        last (LastCall t bid)       = LastCall   t $ liftM sub bid
        last (LastSwitch e bs)      = LastSwitch e $ map (liftM sub) bs
        last l = l
        cond p t f = if t == f then LastBranch t else LastCondBranch p t f
        sub = lookupBid subst

-- To speed up comparisons, we hash each basic block modulo labels.
-- The hashing is a bit arbitrary (the numbers are completely arbitrary),
-- but it should be fast and good enough.
hash_block :: CmmBlock -> Int
hash_block (Block _ t) = hash_tail t 0
  where hash_mid   (MidComment (FastString u _ _ _ _)) = u
        hash_mid   (MidAssign r e) = hash_reg r + hash_e e
        hash_mid   (MidStore e e') = hash_e e + hash_e e'
        hash_mid   (MidUnsafeCall t _ as) = hash_tgt t + hash_as as
        hash_mid   (MidAddToContext e es) = hash_e e + hash_lst hash_e es
        hash_mid   (CopyIn _ fs _) = hash_fs fs
        hash_mid   (CopyOut _ as) = hash_as as
        hash_reg   (CmmLocal l) = hash_local l
        hash_reg   (CmmGlobal _)    = 19
        hash_local (LocalReg _ _ _) = 117
        hash_e (CmmLit l) = hash_lit l
        hash_e (CmmLoad e _) = 67 + hash_e e
        hash_e (CmmReg r) = hash_reg r
        hash_e (CmmMachOp _ es) = hash_lst hash_e es -- pessimal - no operator check
        hash_e (CmmRegOff r i) = hash_reg r + i
        hash_e (CmmStackSlot _ _) = 13
        hash_lit (CmmInt i _) = fromInteger i
        hash_lit (CmmFloat r _) = truncate r
        hash_lit (CmmLabel _) = 119 -- ugh
        hash_lit (CmmLabelOff _ i) = 199 + i
        hash_lit (CmmLabelDiffOff _ _ i) = 299 + i
        hash_tgt (CmmCallee e _) = hash_e e
        hash_tgt (CmmPrim _) = 31 -- lots of these
        hash_as = hash_lst $ hash_kinded hash_e
        hash_fs = hash_lst $ hash_kinded hash_local
        hash_kinded f (CmmKinded x _) = f x
        hash_lst f = foldl (\z x -> f x + z) 0
        hash_last (LastBranch _) = 23 -- would be great to hash these properly
        hash_last (LastCondBranch p _ _) = hash_e p 
        hash_last LastReturn = 17 -- better ideas?
        hash_last (LastJump e) = hash_e e
        hash_last (LastCall e _) = hash_e e
        hash_last (LastSwitch e _) = hash_e e
        hash_tail (ZLast LastExit) v = 29 + v * 2
        hash_tail (ZLast (LastOther l)) v = hash_last l + (v * 2)
        hash_tail (ZTail m t) v = hash_tail t (hash_mid m + (v * 2))

-- Utilities: equality and substitution on the graph.

-- Given a map ``subst'' from BlockID -> BlockID, we define equality.
eqBid :: BidMap -> BlockId -> BlockId -> Bool
eqBid subst bid bid' = lookupBid subst bid == lookupBid subst bid'
lookupBid :: BidMap -> BlockId -> BlockId
lookupBid subst bid = case lookupFM subst bid of
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
eqLastWith eqBid (LastBranch bid) (LastBranch bid') = eqBid bid bid'
eqLastWith eqBid c@(LastCondBranch _ _ _) c'@(LastCondBranch _ _ _) =
  eqBid (cml_true c) (cml_true c')  && eqBid (cml_false c) (cml_false c') 
eqLastWith _ LastReturn LastReturn = True
eqLastWith _ (LastJump e) (LastJump e') = e == e'
eqLastWith eqBid c@(LastCall _ _) c'@(LastCall _ _) =
  cml_target c == cml_target c' && eqMaybeWith eqBid (cml_cont c) (cml_cont c')
eqLastWith eqBid (LastSwitch e bs) (LastSwitch e' bs') =
  e == e' && eqLstWith (eqMaybeWith eqBid) bs bs'
eqLastWith _ _ _ = False

eqLstWith :: (a -> b -> Bool) -> [a] -> [b] -> Bool
eqLstWith eltEq es es' = all (uncurry eltEq) (List.zip es es')

eqMaybeWith :: (a -> b -> Bool) -> Maybe a -> Maybe b -> Bool
eqMaybeWith eltEq (Just e) (Just e') = eltEq e e'
eqMaybeWith _ Nothing Nothing = True
eqMaybeWith _ _ _ = False
