{-# LANGUAGE GADTs #-}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

{-# OPTIONS_GHC -fno-warn-deprecations #-}
-- Warnings from deprecated blockToNodeList
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#if __GLASGOW_HASKELL__ >= 703
-- GHC 7.0.1 improved incomplete pattern warnings with GADTs
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
#endif


-----------------------------------------------------------------------------
--
-- Cmm utilities.
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module CmmUtils( 
        -- CmmType
	primRepCmmType, primRepForeignHint,
	typeCmmType, typeForeignHint,

	-- CmmLit
	zeroCLit, mkIntCLit, 
	mkWordCLit, packHalfWordsCLit,
	mkByteStringCLit, 
        mkDataLits, mkRODataLits,

	-- CmmExpr
	mkLblExpr,
	cmmRegOff,  cmmOffset,  cmmLabelOff,  cmmOffsetLit,  cmmOffsetExpr, 
	cmmRegOffB, cmmOffsetB, cmmLabelOffB, cmmOffsetLitB, cmmOffsetExprB,
	cmmRegOffW, cmmOffsetW, cmmLabelOffW, cmmOffsetLitW, cmmOffsetExprW,
	cmmIndex, cmmIndexExpr, cmmLoadIndex, cmmLoadIndexW,
	cmmNegate, 
  	cmmULtWord, cmmUGeWord, cmmUGtWord, cmmSubWord,
  	cmmNeWord, cmmEqWord, cmmOrWord, cmmAndWord,
  	cmmUShrWord, cmmAddWord, cmmMulWord,

	isTrivialCmmExpr, hasNoGlobalRegs,
	
	-- Statics
	blankWord,

	-- Tagging
	cmmTagMask, cmmPointerMask, cmmUntag, cmmGetTag, cmmIsTagged,
	cmmConstrTag, cmmConstrTag1,

        -- Liveness and bitmaps
        mkLiveness,

        -- * Operations that probably don't belong here
        modifyGraph,

        lastNode, replaceLastNode, insertBetween,
        ofBlockMap, toBlockMap, insertBlock,
        ofBlockList, toBlockList, bodyToBlockList,
        foldGraphBlocks, mapGraphNodes, postorderDfs, mapGraphNodes1,
      
        analFwd, analBwd, analRewFwd, analRewBwd,
        dataflowPassFwd, dataflowPassBwd
  ) where

#include "HsVersions.h"

import TyCon	( PrimRep(..) )
import Type	( UnaryType, typePrimRep )

import SMRep
import Cmm
import BlockId
import CLabel
import Outputable
import OptimizationFuel as F
import Unique
import UniqSupply
import Constants( wORD_SIZE, tAG_MASK )
import Util

import Data.Word
import Data.Maybe
import Data.Bits
import Control.Monad
import Compiler.Hoopl hiding ( Unique )

---------------------------------------------------
--
--	CmmTypes
--
---------------------------------------------------

primRepCmmType :: PrimRep -> CmmType
primRepCmmType VoidRep    = panic "primRepCmmType:VoidRep"
primRepCmmType PtrRep     = gcWord
primRepCmmType IntRep	  = bWord
primRepCmmType WordRep	  = bWord
primRepCmmType Int64Rep   = b64
primRepCmmType Word64Rep  = b64
primRepCmmType AddrRep    = bWord
primRepCmmType FloatRep   = f32
primRepCmmType DoubleRep  = f64

typeCmmType :: UnaryType -> CmmType
typeCmmType ty = primRepCmmType (typePrimRep ty)

primRepForeignHint :: PrimRep -> ForeignHint
primRepForeignHint VoidRep	= panic "primRepForeignHint:VoidRep"
primRepForeignHint PtrRep	= AddrHint
primRepForeignHint IntRep	= SignedHint
primRepForeignHint WordRep	= NoHint
primRepForeignHint Int64Rep	= SignedHint
primRepForeignHint Word64Rep	= NoHint
primRepForeignHint AddrRep      = AddrHint -- NB! AddrHint, but NonPtrArg
primRepForeignHint FloatRep	= NoHint
primRepForeignHint DoubleRep	= NoHint

typeForeignHint :: UnaryType -> ForeignHint
typeForeignHint = primRepForeignHint . typePrimRep

---------------------------------------------------
--
--	CmmLit
--
---------------------------------------------------

mkIntCLit :: Int -> CmmLit
mkIntCLit i = CmmInt (toInteger i) wordWidth

zeroCLit :: CmmLit
zeroCLit = CmmInt 0 wordWidth

mkByteStringCLit :: Unique -> [Word8] -> (CmmLit, GenCmmDecl CmmStatics info stmt)
-- We have to make a top-level decl for the string, 
-- and return a literal pointing to it
mkByteStringCLit uniq bytes
  = (CmmLabel lbl, CmmData ReadOnlyData $ Statics lbl [CmmString bytes])
  where
    lbl = mkStringLitLabel uniq
mkDataLits :: Section -> CLabel -> [CmmLit] -> GenCmmDecl CmmStatics info stmt
-- Build a data-segment data block
mkDataLits section lbl lits
  = CmmData section (Statics lbl $ map CmmStaticLit lits)

mkRODataLits :: CLabel -> [CmmLit] -> GenCmmDecl CmmStatics info stmt
-- Build a read-only data block
mkRODataLits lbl lits
  = mkDataLits section lbl lits
  where 
    section | any needsRelocation lits = RelocatableReadOnlyData
            | otherwise                = ReadOnlyData
    needsRelocation (CmmLabel _)      = True
    needsRelocation (CmmLabelOff _ _) = True
    needsRelocation _                 = False

mkWordCLit :: StgWord -> CmmLit
mkWordCLit wd = CmmInt (fromIntegral wd) wordWidth

packHalfWordsCLit :: (Integral a, Integral b) => a -> b -> CmmLit
-- Make a single word literal in which the lower_half_word is
-- at the lower address, and the upper_half_word is at the 
-- higher address
-- ToDo: consider using half-word lits instead
-- 	 but be careful: that's vulnerable when reversed
packHalfWordsCLit lower_half_word upper_half_word
#ifdef WORDS_BIGENDIAN
   = mkWordCLit ((fromIntegral lower_half_word `shiftL` hALF_WORD_SIZE_IN_BITS)
		 .|. fromIntegral upper_half_word)
#else 
   = mkWordCLit ((fromIntegral lower_half_word) 
		 .|. (fromIntegral upper_half_word `shiftL` hALF_WORD_SIZE_IN_BITS))
#endif

---------------------------------------------------
--
--	CmmExpr
--
---------------------------------------------------

mkLblExpr :: CLabel -> CmmExpr
mkLblExpr lbl = CmmLit (CmmLabel lbl)

cmmOffsetExpr :: CmmExpr -> CmmExpr -> CmmExpr
-- assumes base and offset have the same CmmType
cmmOffsetExpr e (CmmLit (CmmInt n _)) = cmmOffset e (fromInteger n)
cmmOffsetExpr e byte_off = CmmMachOp (MO_Add (cmmExprWidth e)) [e, byte_off]

-- NB. Do *not* inspect the value of the offset in these smart constructors!!!
-- because the offset is sometimes involved in a loop in the code generator
-- (we don't know the real Hp offset until we've generated code for the entire
-- basic block, for example).  So we cannot eliminate zero offsets at this
-- stage; they're eliminated later instead (either during printing or
-- a later optimisation step on Cmm).
--
cmmOffset :: CmmExpr -> Int -> CmmExpr
cmmOffset e                 0        = e
cmmOffset (CmmReg reg)      byte_off = cmmRegOff reg byte_off
cmmOffset (CmmRegOff reg m) byte_off = cmmRegOff reg (m+byte_off)
cmmOffset (CmmLit lit)      byte_off = CmmLit (cmmOffsetLit lit byte_off)
cmmOffset (CmmMachOp (MO_Add rep) [expr, CmmLit (CmmInt byte_off1 _rep)]) byte_off2
  = CmmMachOp (MO_Add rep) 
	      [expr, CmmLit (CmmInt (byte_off1 + toInteger byte_off2) rep)]
cmmOffset expr byte_off
  = CmmMachOp (MO_Add width) [expr, CmmLit (CmmInt (toInteger byte_off) width)]
  where
    width = cmmExprWidth expr

-- Smart constructor for CmmRegOff.  Same caveats as cmmOffset above.
cmmRegOff :: CmmReg -> Int -> CmmExpr
cmmRegOff reg byte_off = CmmRegOff reg byte_off

cmmOffsetLit :: CmmLit -> Int -> CmmLit
cmmOffsetLit (CmmLabel l)      byte_off = cmmLabelOff	l byte_off
cmmOffsetLit (CmmLabelOff l m) byte_off = cmmLabelOff	l (m+byte_off)
cmmOffsetLit (CmmInt m rep)    byte_off = CmmInt (m + fromIntegral byte_off) rep
cmmOffsetLit _    	       byte_off = pprPanic "cmmOffsetLit" (ppr byte_off)

cmmLabelOff :: CLabel -> Int -> CmmLit
-- Smart constructor for CmmLabelOff
cmmLabelOff lbl 0        = CmmLabel lbl
cmmLabelOff lbl byte_off = CmmLabelOff lbl byte_off

-- | Useful for creating an index into an array, with a staticaly known offset.
-- The type is the element type; used for making the multiplier
cmmIndex :: Width	-- Width w
	 -> CmmExpr	-- Address of vector of items of width w
	 -> Int		-- Which element of the vector (0 based)
	 -> CmmExpr	-- Address of i'th element
cmmIndex width base idx = cmmOffset base (idx * widthInBytes width)

-- | Useful for creating an index into an array, with an unknown offset.
cmmIndexExpr :: Width		-- Width w
	     -> CmmExpr		-- Address of vector of items of width w
	     -> CmmExpr		-- Which element of the vector (0 based)
	     -> CmmExpr		-- Address of i'th element
cmmIndexExpr width base (CmmLit (CmmInt n _)) = cmmIndex width base (fromInteger n)
cmmIndexExpr width base idx =
  cmmOffsetExpr base byte_off
  where
    idx_w = cmmExprWidth idx
    byte_off = CmmMachOp (MO_Shl idx_w) [idx, CmmLit (mkIntCLit (widthInLog width))]

cmmLoadIndex :: CmmType -> CmmExpr -> Int -> CmmExpr
cmmLoadIndex ty expr ix = CmmLoad (cmmIndex (typeWidth ty) expr ix) ty

-- The "B" variants take byte offsets
cmmRegOffB :: CmmReg -> ByteOff -> CmmExpr
cmmRegOffB = cmmRegOff

cmmOffsetB :: CmmExpr -> ByteOff -> CmmExpr
cmmOffsetB = cmmOffset

cmmOffsetExprB :: CmmExpr -> CmmExpr -> CmmExpr
cmmOffsetExprB = cmmOffsetExpr

cmmLabelOffB :: CLabel -> ByteOff -> CmmLit
cmmLabelOffB = cmmLabelOff

cmmOffsetLitB :: CmmLit -> ByteOff -> CmmLit
cmmOffsetLitB = cmmOffsetLit

-----------------------
-- The "W" variants take word offsets
cmmOffsetExprW :: CmmExpr -> CmmExpr -> CmmExpr
-- The second arg is a *word* offset; need to change it to bytes
cmmOffsetExprW e (CmmLit (CmmInt n _)) = cmmOffsetW e (fromInteger n)
cmmOffsetExprW e wd_off = cmmIndexExpr wordWidth e wd_off

cmmOffsetW :: CmmExpr -> WordOff -> CmmExpr
cmmOffsetW e n = cmmOffsetB e (wORD_SIZE * n)

cmmRegOffW :: CmmReg -> WordOff -> CmmExpr
cmmRegOffW reg wd_off = cmmRegOffB reg (wd_off * wORD_SIZE)

cmmOffsetLitW :: CmmLit -> WordOff -> CmmLit
cmmOffsetLitW lit wd_off = cmmOffsetLitB lit (wORD_SIZE * wd_off)

cmmLabelOffW :: CLabel -> WordOff -> CmmLit
cmmLabelOffW lbl wd_off = cmmLabelOffB lbl (wORD_SIZE * wd_off)

cmmLoadIndexW :: CmmExpr -> Int -> CmmType -> CmmExpr
cmmLoadIndexW base off ty = CmmLoad (cmmOffsetW base off) ty

-----------------------
cmmULtWord, cmmUGeWord, cmmUGtWord, cmmSubWord,
  cmmNeWord, cmmEqWord, cmmOrWord, cmmAndWord,
  cmmUShrWord, cmmAddWord, cmmMulWord
  :: CmmExpr -> CmmExpr -> CmmExpr
cmmOrWord  e1 e2 = CmmMachOp mo_wordOr  [e1, e2]
cmmAndWord e1 e2 = CmmMachOp mo_wordAnd [e1, e2]
cmmNeWord  e1 e2 = CmmMachOp mo_wordNe  [e1, e2]
cmmEqWord  e1 e2 = CmmMachOp mo_wordEq  [e1, e2]
cmmULtWord e1 e2 = CmmMachOp mo_wordULt [e1, e2]
cmmUGeWord e1 e2 = CmmMachOp mo_wordUGe [e1, e2]
cmmUGtWord e1 e2 = CmmMachOp mo_wordUGt [e1, e2]
--cmmShlWord e1 e2 = CmmMachOp mo_wordShl [e1, e2]
cmmUShrWord e1 e2 = CmmMachOp mo_wordUShr [e1, e2]
cmmAddWord e1 e2 = CmmMachOp mo_wordAdd [e1, e2]
cmmSubWord e1 e2 = CmmMachOp mo_wordSub [e1, e2]
cmmMulWord e1 e2 = CmmMachOp mo_wordMul [e1, e2]

cmmNegate :: CmmExpr -> CmmExpr
cmmNegate (CmmLit (CmmInt n rep)) = CmmLit (CmmInt (-n) rep)
cmmNegate e			  = CmmMachOp (MO_S_Neg (cmmExprWidth e)) [e]

blankWord :: CmmStatic
blankWord = CmmUninitialised wORD_SIZE

---------------------------------------------------
--
--	CmmExpr predicates
--
---------------------------------------------------

isTrivialCmmExpr :: CmmExpr -> Bool
isTrivialCmmExpr (CmmLoad _ _)   = False
isTrivialCmmExpr (CmmMachOp _ _) = False
isTrivialCmmExpr (CmmLit _)      = True
isTrivialCmmExpr (CmmReg _)      = True
isTrivialCmmExpr (CmmRegOff _ _) = True
isTrivialCmmExpr (CmmStackSlot _ _) = panic "isTrivialCmmExpr CmmStackSlot"

hasNoGlobalRegs :: CmmExpr -> Bool
hasNoGlobalRegs (CmmLoad e _)   	   = hasNoGlobalRegs e
hasNoGlobalRegs (CmmMachOp _ es) 	   = all hasNoGlobalRegs es
hasNoGlobalRegs (CmmLit _)      	   = True
hasNoGlobalRegs (CmmReg (CmmLocal _))      = True
hasNoGlobalRegs (CmmRegOff (CmmLocal _) _) = True
hasNoGlobalRegs _ = False

---------------------------------------------------
--
--	Tagging
--
---------------------------------------------------

-- Tag bits mask
--cmmTagBits = CmmLit (mkIntCLit tAG_BITS)
cmmTagMask, cmmPointerMask :: CmmExpr
cmmTagMask = CmmLit (mkIntCLit tAG_MASK)
cmmPointerMask = CmmLit (mkIntCLit (complement tAG_MASK))

-- Used to untag a possibly tagged pointer
-- A static label need not be untagged
cmmUntag, cmmGetTag :: CmmExpr -> CmmExpr
cmmUntag e@(CmmLit (CmmLabel _)) = e
-- Default case
cmmUntag e = (e `cmmAndWord` cmmPointerMask)

cmmGetTag e = (e `cmmAndWord` cmmTagMask)

-- Test if a closure pointer is untagged
cmmIsTagged :: CmmExpr -> CmmExpr
cmmIsTagged e = (e `cmmAndWord` cmmTagMask)
                 `cmmNeWord` CmmLit zeroCLit

cmmConstrTag, cmmConstrTag1 :: CmmExpr -> CmmExpr
cmmConstrTag e = (e `cmmAndWord` cmmTagMask) `cmmSubWord` (CmmLit (mkIntCLit 1))
-- Get constructor tag, but one based.
cmmConstrTag1 e = e `cmmAndWord` cmmTagMask


--------------------------------------------
--
--        mkLiveness
--
---------------------------------------------

mkLiveness :: [Maybe LocalReg] -> Liveness
mkLiveness [] = []
mkLiveness (reg:regs) 
  = take sizeW bits ++ mkLiveness regs 
  where
    sizeW = case reg of
              Nothing -> 1
              Just r -> (widthInBytes (typeWidth (localRegType r)) + wORD_SIZE - 1)
                        `quot` wORD_SIZE
                        -- number of words, rounded up
    bits = repeat $ is_non_ptr reg -- True <=> Non Ptr

    is_non_ptr Nothing    = True
    is_non_ptr (Just reg) = not $ isGcPtrType (localRegType reg)


-- ============================================== -
-- ============================================== -
-- ============================================== -

---------------------------------------------------
--
--      Manipulating CmmGraphs
--
---------------------------------------------------

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

mapGraphNodes1 :: (forall e x. CmmNode e x -> CmmNode e x) -> CmmGraph -> CmmGraph
mapGraphNodes1 f g = modifyGraph (graphMapBlocks (blockMapNodes f)) g


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
