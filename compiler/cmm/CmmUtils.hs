{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
-- Warnings from deprecated blockToNodeList


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
        mkStgWordCLit,

        -- CmmExpr
        mkIntExpr, zeroExpr,
        mkLblExpr,
        cmmRegOff,  cmmOffset,  cmmLabelOff,  cmmOffsetLit,  cmmOffsetExpr,
        cmmRegOffB, cmmOffsetB, cmmLabelOffB, cmmOffsetLitB, cmmOffsetExprB,
        cmmRegOffW, cmmOffsetW, cmmLabelOffW, cmmOffsetLitW, cmmOffsetExprW,
        cmmIndex, cmmIndexExpr, cmmLoadIndex, cmmLoadIndexW,
        cmmNegate,
        cmmULtWord, cmmUGeWord, cmmUGtWord, cmmSubWord,
        cmmNeWord, cmmEqWord, cmmOrWord, cmmAndWord,
        cmmUShrWord, cmmAddWord, cmmMulWord, cmmQuotWord,

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

        ofBlockMap, toBlockMap, insertBlock,
        ofBlockList, toBlockList, bodyToBlockList, toBlockListEntryFirst,
        foldGraphBlocks, mapGraphNodes, postorderDfs, mapGraphNodes1,

        analFwd, analBwd, analRewFwd, analRewBwd,
        dataflowPassFwd, dataflowPassBwd, dataflowAnalFwd, dataflowAnalBwd,
        dataflowAnalFwdBlocks
  ) where

#include "HsVersions.h"

import TyCon    ( PrimRep(..), PrimElemRep(..) )
import Type     ( UnaryType, typePrimRep )

import SMRep
import Cmm
import BlockId
import CLabel
import Outputable
import Unique
import UniqSupply
import DynFlags
import Util

import Data.Word
import Data.Maybe
import Data.Bits
import Hoopl

---------------------------------------------------
--
--      CmmTypes
--
---------------------------------------------------

primRepCmmType :: DynFlags -> PrimRep -> CmmType
primRepCmmType _      VoidRep          = panic "primRepCmmType:VoidRep"
primRepCmmType dflags PtrRep           = gcWord dflags
primRepCmmType dflags IntRep           = bWord dflags
primRepCmmType dflags WordRep          = bWord dflags
primRepCmmType _      Int64Rep         = b64
primRepCmmType _      Word64Rep        = b64
primRepCmmType dflags AddrRep          = bWord dflags
primRepCmmType _      FloatRep         = f32
primRepCmmType _      DoubleRep        = f64
primRepCmmType _      (VecRep len rep) = vec len (primElemRepCmmType rep)

primElemRepCmmType :: PrimElemRep -> CmmType
primElemRepCmmType Int8ElemRep   = b8
primElemRepCmmType Int16ElemRep  = b16
primElemRepCmmType Int32ElemRep  = b32
primElemRepCmmType Int64ElemRep  = b64
primElemRepCmmType Word8ElemRep  = b8
primElemRepCmmType Word16ElemRep = b16
primElemRepCmmType Word32ElemRep = b32
primElemRepCmmType Word64ElemRep = b64
primElemRepCmmType FloatElemRep  = f32
primElemRepCmmType DoubleElemRep = f64

typeCmmType :: DynFlags -> UnaryType -> CmmType
typeCmmType dflags ty = primRepCmmType dflags (typePrimRep ty)

primRepForeignHint :: PrimRep -> ForeignHint
primRepForeignHint VoidRep      = panic "primRepForeignHint:VoidRep"
primRepForeignHint PtrRep       = AddrHint
primRepForeignHint IntRep       = SignedHint
primRepForeignHint WordRep      = NoHint
primRepForeignHint Int64Rep     = SignedHint
primRepForeignHint Word64Rep    = NoHint
primRepForeignHint AddrRep      = AddrHint -- NB! AddrHint, but NonPtrArg
primRepForeignHint FloatRep     = NoHint
primRepForeignHint DoubleRep    = NoHint
primRepForeignHint (VecRep {})  = NoHint

typeForeignHint :: UnaryType -> ForeignHint
typeForeignHint = primRepForeignHint . typePrimRep

---------------------------------------------------
--
--      CmmLit
--
---------------------------------------------------

-- XXX: should really be Integer, since Int doesn't necessarily cover
-- the full range of target Ints.
mkIntCLit :: DynFlags -> Int -> CmmLit
mkIntCLit dflags i = CmmInt (toInteger i) (wordWidth dflags)

mkIntExpr :: DynFlags -> Int -> CmmExpr
mkIntExpr dflags i = CmmLit $! mkIntCLit dflags i

zeroCLit :: DynFlags -> CmmLit
zeroCLit dflags = CmmInt 0 (wordWidth dflags)

zeroExpr :: DynFlags -> CmmExpr
zeroExpr dflags = CmmLit (zeroCLit dflags)

mkWordCLit :: DynFlags -> Integer -> CmmLit
mkWordCLit dflags wd = CmmInt wd (wordWidth dflags)

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

mkStgWordCLit :: DynFlags -> StgWord -> CmmLit
mkStgWordCLit dflags wd = CmmInt (fromStgWord wd) (wordWidth dflags)

packHalfWordsCLit :: DynFlags -> StgHalfWord -> StgHalfWord -> CmmLit
-- Make a single word literal in which the lower_half_word is
-- at the lower address, and the upper_half_word is at the
-- higher address
-- ToDo: consider using half-word lits instead
--       but be careful: that's vulnerable when reversed
packHalfWordsCLit dflags lower_half_word upper_half_word
   = if wORDS_BIGENDIAN dflags
     then mkWordCLit dflags ((l `shiftL` hALF_WORD_SIZE_IN_BITS dflags) .|. u)
     else mkWordCLit dflags (l .|. (u `shiftL` hALF_WORD_SIZE_IN_BITS dflags))
    where l = fromStgHalfWord lower_half_word
          u = fromStgHalfWord upper_half_word

---------------------------------------------------
--
--      CmmExpr
--
---------------------------------------------------

mkLblExpr :: CLabel -> CmmExpr
mkLblExpr lbl = CmmLit (CmmLabel lbl)

cmmOffsetExpr :: DynFlags -> CmmExpr -> CmmExpr -> CmmExpr
-- assumes base and offset have the same CmmType
cmmOffsetExpr dflags e (CmmLit (CmmInt n _)) = cmmOffset dflags e (fromInteger n)
cmmOffsetExpr dflags e byte_off = CmmMachOp (MO_Add (cmmExprWidth dflags e)) [e, byte_off]

-- NB. Do *not* inspect the value of the offset in these smart constructors!!!
-- because the offset is sometimes involved in a loop in the code generator
-- (we don't know the real Hp offset until we've generated code for the entire
-- basic block, for example).  So we cannot eliminate zero offsets at this
-- stage; they're eliminated later instead (either during printing or
-- a later optimisation step on Cmm).
--
cmmOffset :: DynFlags -> CmmExpr -> Int -> CmmExpr
cmmOffset _ e                 0        = e
cmmOffset _ (CmmReg reg)      byte_off = cmmRegOff reg byte_off
cmmOffset _ (CmmRegOff reg m) byte_off = cmmRegOff reg (m+byte_off)
cmmOffset _ (CmmLit lit)      byte_off = CmmLit (cmmOffsetLit lit byte_off)
cmmOffset _ (CmmStackSlot area off) byte_off
  = CmmStackSlot area (off - byte_off)
  -- note stack area offsets increase towards lower addresses
cmmOffset _ (CmmMachOp (MO_Add rep) [expr, CmmLit (CmmInt byte_off1 _rep)]) byte_off2
  = CmmMachOp (MO_Add rep)
              [expr, CmmLit (CmmInt (byte_off1 + toInteger byte_off2) rep)]
cmmOffset dflags expr byte_off
  = CmmMachOp (MO_Add width) [expr, CmmLit (CmmInt (toInteger byte_off) width)]
  where
    width = cmmExprWidth dflags expr

-- Smart constructor for CmmRegOff.  Same caveats as cmmOffset above.
cmmRegOff :: CmmReg -> Int -> CmmExpr
cmmRegOff reg 0        = CmmReg reg
cmmRegOff reg byte_off = CmmRegOff reg byte_off

cmmOffsetLit :: CmmLit -> Int -> CmmLit
cmmOffsetLit (CmmLabel l)      byte_off = cmmLabelOff l byte_off
cmmOffsetLit (CmmLabelOff l m) byte_off = cmmLabelOff l (m+byte_off)
cmmOffsetLit (CmmLabelDiffOff l1 l2 m) byte_off
                                        = CmmLabelDiffOff l1 l2 (m+byte_off)
cmmOffsetLit (CmmInt m rep)    byte_off = CmmInt (m + fromIntegral byte_off) rep
cmmOffsetLit _                 byte_off = pprPanic "cmmOffsetLit" (ppr byte_off)

cmmLabelOff :: CLabel -> Int -> CmmLit
-- Smart constructor for CmmLabelOff
cmmLabelOff lbl 0        = CmmLabel lbl
cmmLabelOff lbl byte_off = CmmLabelOff lbl byte_off

-- | Useful for creating an index into an array, with a staticaly known offset.
-- The type is the element type; used for making the multiplier
cmmIndex :: DynFlags
         -> Width       -- Width w
         -> CmmExpr     -- Address of vector of items of width w
         -> Int         -- Which element of the vector (0 based)
         -> CmmExpr     -- Address of i'th element
cmmIndex dflags width base idx = cmmOffset dflags base (idx * widthInBytes width)

-- | Useful for creating an index into an array, with an unknown offset.
cmmIndexExpr :: DynFlags
             -> Width           -- Width w
             -> CmmExpr         -- Address of vector of items of width w
             -> CmmExpr         -- Which element of the vector (0 based)
             -> CmmExpr         -- Address of i'th element
cmmIndexExpr dflags width base (CmmLit (CmmInt n _)) = cmmIndex dflags width base (fromInteger n)
cmmIndexExpr dflags width base idx =
  cmmOffsetExpr dflags base byte_off
  where
    idx_w = cmmExprWidth dflags idx
    byte_off = CmmMachOp (MO_Shl idx_w) [idx, mkIntExpr dflags (widthInLog width)]

cmmLoadIndex :: DynFlags -> CmmType -> CmmExpr -> Int -> CmmExpr
cmmLoadIndex dflags ty expr ix = CmmLoad (cmmIndex dflags (typeWidth ty) expr ix) ty

-- The "B" variants take byte offsets
cmmRegOffB :: CmmReg -> ByteOff -> CmmExpr
cmmRegOffB = cmmRegOff

cmmOffsetB :: DynFlags -> CmmExpr -> ByteOff -> CmmExpr
cmmOffsetB = cmmOffset

cmmOffsetExprB :: DynFlags -> CmmExpr -> CmmExpr -> CmmExpr
cmmOffsetExprB = cmmOffsetExpr

cmmLabelOffB :: CLabel -> ByteOff -> CmmLit
cmmLabelOffB = cmmLabelOff

cmmOffsetLitB :: CmmLit -> ByteOff -> CmmLit
cmmOffsetLitB = cmmOffsetLit

-----------------------
-- The "W" variants take word offsets
cmmOffsetExprW :: DynFlags -> CmmExpr -> CmmExpr -> CmmExpr
-- The second arg is a *word* offset; need to change it to bytes
cmmOffsetExprW dflags  e (CmmLit (CmmInt n _)) = cmmOffsetW dflags e (fromInteger n)
cmmOffsetExprW dflags e wd_off = cmmIndexExpr dflags (wordWidth dflags) e wd_off

cmmOffsetW :: DynFlags -> CmmExpr -> WordOff -> CmmExpr
cmmOffsetW dflags e n = cmmOffsetB dflags e (wORD_SIZE dflags * n)

cmmRegOffW :: DynFlags -> CmmReg -> WordOff -> CmmExpr
cmmRegOffW dflags reg wd_off = cmmRegOffB reg (wd_off * wORD_SIZE dflags)

cmmOffsetLitW :: DynFlags -> CmmLit -> WordOff -> CmmLit
cmmOffsetLitW dflags lit wd_off = cmmOffsetLitB lit (wORD_SIZE dflags * wd_off)

cmmLabelOffW :: DynFlags -> CLabel -> WordOff -> CmmLit
cmmLabelOffW dflags lbl wd_off = cmmLabelOffB lbl (wORD_SIZE dflags * wd_off)

cmmLoadIndexW :: DynFlags -> CmmExpr -> Int -> CmmType -> CmmExpr
cmmLoadIndexW dflags base off ty = CmmLoad (cmmOffsetW dflags base off) ty

-----------------------
cmmULtWord, cmmUGeWord, cmmUGtWord, cmmSubWord,
  cmmNeWord, cmmEqWord, cmmOrWord, cmmAndWord,
  cmmUShrWord, cmmAddWord, cmmMulWord, cmmQuotWord
  :: DynFlags -> CmmExpr -> CmmExpr -> CmmExpr
cmmOrWord dflags  e1 e2 = CmmMachOp (mo_wordOr dflags)  [e1, e2]
cmmAndWord dflags e1 e2 = CmmMachOp (mo_wordAnd dflags) [e1, e2]
cmmNeWord dflags  e1 e2 = CmmMachOp (mo_wordNe dflags)  [e1, e2]
cmmEqWord dflags  e1 e2 = CmmMachOp (mo_wordEq dflags)  [e1, e2]
cmmULtWord dflags e1 e2 = CmmMachOp (mo_wordULt dflags) [e1, e2]
cmmUGeWord dflags e1 e2 = CmmMachOp (mo_wordUGe dflags) [e1, e2]
cmmUGtWord dflags e1 e2 = CmmMachOp (mo_wordUGt dflags) [e1, e2]
--cmmShlWord dflags e1 e2 = CmmMachOp (mo_wordShl dflags) [e1, e2]
cmmUShrWord dflags e1 e2 = CmmMachOp (mo_wordUShr dflags) [e1, e2]
cmmAddWord dflags e1 e2 = CmmMachOp (mo_wordAdd dflags) [e1, e2]
cmmSubWord dflags e1 e2 = CmmMachOp (mo_wordSub dflags) [e1, e2]
cmmMulWord dflags e1 e2 = CmmMachOp (mo_wordMul dflags) [e1, e2]
cmmQuotWord dflags e1 e2 = CmmMachOp (mo_wordUQuot dflags) [e1, e2]

cmmNegate :: DynFlags -> CmmExpr -> CmmExpr
cmmNegate _      (CmmLit (CmmInt n rep)) = CmmLit (CmmInt (-n) rep)
cmmNegate dflags e                       = CmmMachOp (MO_S_Neg (cmmExprWidth dflags e)) [e]

blankWord :: DynFlags -> CmmStatic
blankWord dflags = CmmUninitialised (wORD_SIZE dflags)

---------------------------------------------------
--
--      CmmExpr predicates
--
---------------------------------------------------

isTrivialCmmExpr :: CmmExpr -> Bool
isTrivialCmmExpr (CmmLoad _ _)      = False
isTrivialCmmExpr (CmmMachOp _ _)    = False
isTrivialCmmExpr (CmmLit _)         = True
isTrivialCmmExpr (CmmReg _)         = True
isTrivialCmmExpr (CmmRegOff _ _)    = True
isTrivialCmmExpr (CmmStackSlot _ _) = panic "isTrivialCmmExpr CmmStackSlot"

hasNoGlobalRegs :: CmmExpr -> Bool
hasNoGlobalRegs (CmmLoad e _)              = hasNoGlobalRegs e
hasNoGlobalRegs (CmmMachOp _ es)           = all hasNoGlobalRegs es
hasNoGlobalRegs (CmmLit _)                 = True
hasNoGlobalRegs (CmmReg (CmmLocal _))      = True
hasNoGlobalRegs (CmmRegOff (CmmLocal _) _) = True
hasNoGlobalRegs _ = False

---------------------------------------------------
--
--      Tagging
--
---------------------------------------------------

-- Tag bits mask
--cmmTagBits = CmmLit (mkIntCLit tAG_BITS)
cmmTagMask, cmmPointerMask :: DynFlags -> CmmExpr
cmmTagMask dflags = mkIntExpr dflags (tAG_MASK dflags)
cmmPointerMask dflags = mkIntExpr dflags (complement (tAG_MASK dflags))

-- Used to untag a possibly tagged pointer
-- A static label need not be untagged
cmmUntag, cmmGetTag :: DynFlags -> CmmExpr -> CmmExpr
cmmUntag _ e@(CmmLit (CmmLabel _)) = e
-- Default case
cmmUntag dflags e = cmmAndWord dflags e (cmmPointerMask dflags)

cmmGetTag dflags e = cmmAndWord dflags e (cmmTagMask dflags)

-- Test if a closure pointer is untagged
cmmIsTagged :: DynFlags -> CmmExpr -> CmmExpr
cmmIsTagged dflags e = cmmNeWord dflags (cmmAndWord dflags e (cmmTagMask dflags)) (zeroExpr dflags)

cmmConstrTag, cmmConstrTag1 :: DynFlags -> CmmExpr -> CmmExpr
cmmConstrTag dflags e = cmmSubWord dflags (cmmAndWord dflags e (cmmTagMask dflags)) (mkIntExpr dflags 1)
-- Get constructor tag, but one based.
cmmConstrTag1 dflags e = cmmAndWord dflags e (cmmTagMask dflags)


--------------------------------------------
--
--        mkLiveness
--
---------------------------------------------

mkLiveness :: DynFlags -> [Maybe LocalReg] -> Liveness
mkLiveness _      [] = []
mkLiveness dflags (reg:regs)
  = take sizeW bits ++ mkLiveness dflags regs
  where
    sizeW = case reg of
              Nothing -> 1
              Just r -> (widthInBytes (typeWidth (localRegType r)) + wORD_SIZE dflags - 1)
                        `quot` wORD_SIZE dflags
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

toBlockMap :: CmmGraph -> BlockEnv CmmBlock
toBlockMap (CmmGraph {g_graph=GMany NothingO body NothingO}) = body

ofBlockMap :: BlockId -> BlockEnv CmmBlock -> CmmGraph
ofBlockMap entry bodyMap = CmmGraph {g_entry=entry, g_graph=GMany NothingO bodyMap NothingO}

insertBlock :: CmmBlock -> BlockEnv CmmBlock -> BlockEnv CmmBlock
insertBlock block map =
  ASSERT (isNothing $ mapLookup id map)
  mapInsert id block map
  where id = entryLabel block

toBlockList :: CmmGraph -> [CmmBlock]
toBlockList g = mapElems $ toBlockMap g

-- | like 'toBlockList', but the entry block always comes first
toBlockListEntryFirst :: CmmGraph -> [CmmBlock]
toBlockListEntryFirst g
  | mapNull m  = []
  | otherwise  = entry_block : others
  where
    m = toBlockMap g
    entry_id = g_entry g
    Just entry_block = mapLookup entry_id m
    others = filter ((/= entry_id) . entryLabel) (mapElems m)

ofBlockList :: BlockId -> [CmmBlock] -> CmmGraph
ofBlockList entry blocks = CmmGraph { g_entry = entry
                                    , g_graph = GMany NothingO body NothingO }
  where body = foldr addBlock emptyBody blocks

bodyToBlockList :: Body CmmNode -> [CmmBlock]
bodyToBlockList body = mapElems body

mapGraphNodes :: ( CmmNode C O -> CmmNode C O
                 , CmmNode O O -> CmmNode O O
                 , CmmNode O C -> CmmNode O C)
              -> CmmGraph -> CmmGraph
mapGraphNodes funs@(mf,_,_) g =
  ofBlockMap (entryLabel $ mf $ CmmEntry $ g_entry g) $ mapMap (mapBlock3' funs) $ toBlockMap g

mapGraphNodes1 :: (forall e x. CmmNode e x -> CmmNode e x) -> CmmGraph -> CmmGraph
mapGraphNodes1 f = modifyGraph (mapGraph f)


foldGraphBlocks :: (CmmBlock -> a -> a) -> a -> CmmGraph -> a
foldGraphBlocks k z g = mapFold k z $ toBlockMap g

postorderDfs :: CmmGraph -> [CmmBlock]
postorderDfs g = {-# SCC "postorderDfs" #-} postorder_dfs_from (toBlockMap g) (g_entry g)

-------------------------------------------------
-- Running dataflow analysis and/or rewrites

-- Constructing forward and backward analysis-only pass
analFwd    :: DataflowLattice f -> FwdTransfer n f -> FwdPass UniqSM n f
analBwd    :: DataflowLattice f -> BwdTransfer n f -> BwdPass UniqSM n f

analFwd lat xfer = analRewFwd lat xfer noFwdRewrite
analBwd lat xfer = analRewBwd lat xfer noBwdRewrite

-- Constructing forward and backward analysis + rewrite pass
analRewFwd :: DataflowLattice f -> FwdTransfer n f
           -> FwdRewrite UniqSM n f
           -> FwdPass UniqSM n f

analRewBwd :: DataflowLattice f
           -> BwdTransfer n f
           -> BwdRewrite UniqSM n f
           -> BwdPass UniqSM n f

analRewFwd lat xfer rew = FwdPass {fp_lattice = lat, fp_transfer = xfer, fp_rewrite = rew}
analRewBwd lat xfer rew = BwdPass {bp_lattice = lat, bp_transfer = xfer, bp_rewrite = rew}

-- Running forward and backward dataflow analysis + optional rewrite
dataflowPassFwd :: NonLocal n =>
                   GenCmmGraph n -> [(BlockId, f)]
                -> FwdPass UniqSM n f
                -> UniqSM (GenCmmGraph n, BlockEnv f)
dataflowPassFwd (CmmGraph {g_entry=entry, g_graph=graph}) facts fwd = do
  (graph, facts, NothingO) <- analyzeAndRewriteFwd fwd (JustC [entry]) graph (mkFactBase (fp_lattice fwd) facts)
  return (CmmGraph {g_entry=entry, g_graph=graph}, facts)

dataflowAnalFwd :: NonLocal n =>
                   GenCmmGraph n -> [(BlockId, f)]
                -> FwdPass UniqSM n f
                -> BlockEnv f
dataflowAnalFwd (CmmGraph {g_entry=entry, g_graph=graph}) facts fwd =
  analyzeFwd fwd (JustC [entry]) graph (mkFactBase (fp_lattice fwd) facts)

dataflowAnalFwdBlocks :: NonLocal n =>
                   GenCmmGraph n -> [(BlockId, f)]
                -> FwdPass UniqSM n f
                -> UniqSM (BlockEnv f)
dataflowAnalFwdBlocks (CmmGraph {g_entry=entry, g_graph=graph}) facts fwd = do
--  (graph, facts, NothingO) <- analyzeAndRewriteFwd fwd (JustC [entry]) graph (mkFactBase (fp_lattice fwd) facts)
--  return facts
  return (analyzeFwdBlocks fwd (JustC [entry]) graph (mkFactBase (fp_lattice fwd) facts))

dataflowAnalBwd :: NonLocal n =>
                   GenCmmGraph n -> [(BlockId, f)]
                -> BwdPass UniqSM n f
                -> BlockEnv f
dataflowAnalBwd (CmmGraph {g_entry=entry, g_graph=graph}) facts bwd =
  analyzeBwd bwd (JustC [entry]) graph (mkFactBase (bp_lattice bwd) facts)

dataflowPassBwd :: NonLocal n =>
                   GenCmmGraph n -> [(BlockId, f)]
                -> BwdPass UniqSM n f
                -> UniqSM (GenCmmGraph n, BlockEnv f)
dataflowPassBwd (CmmGraph {g_entry=entry, g_graph=graph}) facts bwd = do
  (graph, facts, NothingO) <- analyzeAndRewriteBwd bwd (JustC [entry]) graph (mkFactBase (bp_lattice bwd) facts)
  return (CmmGraph {g_entry=entry, g_graph=graph}, facts)
