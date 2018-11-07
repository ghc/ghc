{-# LANGUAGE GADTs, RankNTypes #-}

-----------------------------------------------------------------------------
--
-- Cmm utilities.
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module CmmUtils(
        -- CmmType
        primRepCmmType, slotCmmType, slotForeignHint,
        typeCmmType, typeForeignHint, primRepForeignHint,

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
        cmmULtWord, cmmUGeWord, cmmUGtWord, cmmUShrWord,
        cmmSLtWord,
        cmmNeWord, cmmEqWord,
        cmmOrWord, cmmAndWord,
        cmmSubWord, cmmAddWord, cmmMulWord, cmmQuotWord,
        cmmToWord,

        isTrivialCmmExpr, hasNoGlobalRegs, isLit, isComparisonExpr,

        baseExpr, spExpr, hpExpr, spLimExpr, hpLimExpr,
        currentTSOExpr, currentNurseryExpr, cccsExpr,

        -- Statics
        blankWord,

        -- Tagging
        cmmTagMask, cmmPointerMask, cmmUntag, cmmIsTagged,
        cmmConstrTag1,

        -- Overlap and usage
        regsOverlap, regUsedIn,

        -- Liveness and bitmaps
        mkLiveness,

        -- * Operations that probably don't belong here
        modifyGraph,

        ofBlockMap, toBlockMap,
        ofBlockList, toBlockList, bodyToBlockList,
        toBlockListEntryFirst, toBlockListEntryFirstFalseFallthrough,
        foldlGraphBlocks, mapGraphNodes, revPostorder, mapGraphNodes1,

        -- * Ticks
        blockTicks
  ) where

import GhcPrelude

import TyCon    ( PrimRep(..), PrimElemRep(..) )
import RepType  ( UnaryType, SlotTy (..), typePrimRep1 )

import SMRep
import Cmm
import BlockId
import CLabel
import Outputable
import DynFlags
import CodeGen.Platform

import Data.Word
import Data.Bits
import Hoopl.Graph
import Hoopl.Label
import Hoopl.Block
import Hoopl.Collections

---------------------------------------------------
--
--      CmmTypes
--
---------------------------------------------------

primRepCmmType :: DynFlags -> PrimRep -> CmmType
primRepCmmType _      VoidRep          = panic "primRepCmmType:VoidRep"
primRepCmmType dflags LiftedRep        = gcWord dflags
primRepCmmType dflags UnliftedRep      = gcWord dflags
primRepCmmType dflags IntRep           = bWord dflags
primRepCmmType dflags WordRep          = bWord dflags
primRepCmmType _      Int8Rep          = b8
primRepCmmType _      Word8Rep         = b8
primRepCmmType _      Int64Rep         = b64
primRepCmmType _      Word64Rep        = b64
primRepCmmType dflags AddrRep          = bWord dflags
primRepCmmType _      FloatRep         = f32
primRepCmmType _      DoubleRep        = f64
primRepCmmType _      (VecRep len rep) = vec len (primElemRepCmmType rep)

slotCmmType :: DynFlags -> SlotTy -> CmmType
slotCmmType dflags PtrSlot    = gcWord dflags
slotCmmType dflags WordSlot   = bWord dflags
slotCmmType _      Word64Slot = b64
slotCmmType _      FloatSlot  = f32
slotCmmType _      DoubleSlot = f64

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
typeCmmType dflags ty = primRepCmmType dflags (typePrimRep1 ty)

primRepForeignHint :: PrimRep -> ForeignHint
primRepForeignHint VoidRep      = panic "primRepForeignHint:VoidRep"
primRepForeignHint LiftedRep    = AddrHint
primRepForeignHint UnliftedRep  = AddrHint
primRepForeignHint IntRep       = SignedHint
primRepForeignHint Int8Rep      = SignedHint
primRepForeignHint Int64Rep     = SignedHint
primRepForeignHint WordRep      = NoHint
primRepForeignHint Word8Rep     = NoHint
primRepForeignHint Word64Rep    = NoHint
primRepForeignHint AddrRep      = AddrHint -- NB! AddrHint, but NonPtrArg
primRepForeignHint FloatRep     = NoHint
primRepForeignHint DoubleRep    = NoHint
primRepForeignHint (VecRep {})  = NoHint

slotForeignHint :: SlotTy -> ForeignHint
slotForeignHint PtrSlot       = AddrHint
slotForeignHint WordSlot      = NoHint
slotForeignHint Word64Slot    = NoHint
slotForeignHint FloatSlot     = NoHint
slotForeignHint DoubleSlot    = NoHint

typeForeignHint :: UnaryType -> ForeignHint
typeForeignHint = primRepForeignHint . typePrimRep1

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

mkByteStringCLit
  :: CLabel -> [Word8] -> (CmmLit, GenCmmDecl CmmStatics info stmt)
-- We have to make a top-level decl for the string,
-- and return a literal pointing to it
mkByteStringCLit lbl bytes
  = (CmmLabel lbl, CmmData (Section sec lbl) $ Statics lbl [CmmString bytes])
  where
    -- This can not happen for String literals (as there \NUL is replaced by
    -- C0 80). However, it can happen with Addr# literals.
    sec = if 0 `elem` bytes then ReadOnlyData else CString

mkDataLits :: Section -> CLabel -> [CmmLit] -> GenCmmDecl CmmStatics info stmt
-- Build a data-segment data block
mkDataLits section lbl lits
  = CmmData section (Statics lbl $ map CmmStaticLit lits)

mkRODataLits :: CLabel -> [CmmLit] -> GenCmmDecl CmmStatics info stmt
-- Build a read-only data block
mkRODataLits lbl lits
  = mkDataLits section lbl lits
  where
    section | any needsRelocation lits = Section RelocatableReadOnlyData lbl
            | otherwise                = Section ReadOnlyData lbl
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
cmmOffsetLit (CmmLabelDiffOff l1 l2 m w) byte_off
                                        = CmmLabelDiffOff l1 l2 (m+byte_off) w
cmmOffsetLit (CmmInt m rep)    byte_off = CmmInt (m + fromIntegral byte_off) rep
cmmOffsetLit _                 byte_off = pprPanic "cmmOffsetLit" (ppr byte_off)

cmmLabelOff :: CLabel -> Int -> CmmLit
-- Smart constructor for CmmLabelOff
cmmLabelOff lbl 0        = CmmLabel lbl
cmmLabelOff lbl byte_off = CmmLabelOff lbl byte_off

-- | Useful for creating an index into an array, with a statically known offset.
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
cmmOffsetW dflags e n = cmmOffsetB dflags e (wordsToBytes dflags n)

cmmRegOffW :: DynFlags -> CmmReg -> WordOff -> CmmExpr
cmmRegOffW dflags reg wd_off = cmmRegOffB reg (wordsToBytes dflags wd_off)

cmmOffsetLitW :: DynFlags -> CmmLit -> WordOff -> CmmLit
cmmOffsetLitW dflags lit wd_off = cmmOffsetLitB lit (wordsToBytes dflags wd_off)

cmmLabelOffW :: DynFlags -> CLabel -> WordOff -> CmmLit
cmmLabelOffW dflags lbl wd_off = cmmLabelOffB lbl (wordsToBytes dflags wd_off)

cmmLoadIndexW :: DynFlags -> CmmExpr -> Int -> CmmType -> CmmExpr
cmmLoadIndexW dflags base off ty = CmmLoad (cmmOffsetW dflags base off) ty

-----------------------
cmmULtWord, cmmUGeWord, cmmUGtWord, cmmUShrWord,
  cmmSLtWord,
  cmmNeWord, cmmEqWord,
  cmmOrWord, cmmAndWord,
  cmmSubWord, cmmAddWord, cmmMulWord, cmmQuotWord
  :: DynFlags -> CmmExpr -> CmmExpr -> CmmExpr
cmmOrWord dflags  e1 e2 = CmmMachOp (mo_wordOr dflags)  [e1, e2]
cmmAndWord dflags e1 e2 = CmmMachOp (mo_wordAnd dflags) [e1, e2]
cmmNeWord dflags  e1 e2 = CmmMachOp (mo_wordNe dflags)  [e1, e2]
cmmEqWord dflags  e1 e2 = CmmMachOp (mo_wordEq dflags)  [e1, e2]
cmmULtWord dflags e1 e2 = CmmMachOp (mo_wordULt dflags) [e1, e2]
cmmUGeWord dflags e1 e2 = CmmMachOp (mo_wordUGe dflags) [e1, e2]
cmmUGtWord dflags e1 e2 = CmmMachOp (mo_wordUGt dflags) [e1, e2]
cmmSLtWord dflags e1 e2 = CmmMachOp (mo_wordSLt dflags) [e1, e2]
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

cmmToWord :: DynFlags -> CmmExpr -> CmmExpr
cmmToWord dflags e
  | w == word  = e
  | otherwise  = CmmMachOp (MO_UU_Conv w word) [e]
  where
    w = cmmExprWidth dflags e
    word = wordWidth dflags

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

isLit :: CmmExpr -> Bool
isLit (CmmLit _) = True
isLit _          = False

isComparisonExpr :: CmmExpr -> Bool
isComparisonExpr (CmmMachOp op _) = isComparisonMachOp op
isComparisonExpr _                  = False

---------------------------------------------------
--
--      Tagging
--
---------------------------------------------------

-- Tag bits mask
cmmTagMask, cmmPointerMask :: DynFlags -> CmmExpr
cmmTagMask dflags = mkIntExpr dflags (tAG_MASK dflags)
cmmPointerMask dflags = mkIntExpr dflags (complement (tAG_MASK dflags))

-- Used to untag a possibly tagged pointer
-- A static label need not be untagged
cmmUntag, cmmIsTagged, cmmConstrTag1 :: DynFlags -> CmmExpr -> CmmExpr
cmmUntag _ e@(CmmLit (CmmLabel _)) = e
-- Default case
cmmUntag dflags e = cmmAndWord dflags e (cmmPointerMask dflags)

-- Test if a closure pointer is untagged
cmmIsTagged dflags e = cmmNeWord dflags (cmmAndWord dflags e (cmmTagMask dflags)) (zeroExpr dflags)

-- Get constructor tag, but one based.
cmmConstrTag1 dflags e = cmmAndWord dflags e (cmmTagMask dflags)


-----------------------------------------------------------------------------
-- Overlap and usage

-- | Returns True if the two STG registers overlap on the specified
-- platform, in the sense that writing to one will clobber the
-- other. This includes the case that the two registers are the same
-- STG register. See Note [Overlapping global registers] for details.
regsOverlap :: DynFlags -> CmmReg -> CmmReg -> Bool
regsOverlap dflags (CmmGlobal g) (CmmGlobal g')
  | Just real  <- globalRegMaybe (targetPlatform dflags) g,
    Just real' <- globalRegMaybe (targetPlatform dflags) g',
    real == real'
    = True
regsOverlap _ reg reg' = reg == reg'

-- | Returns True if the STG register is used by the expression, in
-- the sense that a store to the register might affect the value of
-- the expression.
--
-- We must check for overlapping registers and not just equal
-- registers here, otherwise CmmSink may incorrectly reorder
-- assignments that conflict due to overlap. See Trac #10521 and Note
-- [Overlapping global registers].
regUsedIn :: DynFlags -> CmmReg -> CmmExpr -> Bool
regUsedIn dflags = regUsedIn_ where
  _   `regUsedIn_` CmmLit _         = False
  reg `regUsedIn_` CmmLoad e  _     = reg `regUsedIn_` e
  reg `regUsedIn_` CmmReg reg'      = regsOverlap dflags reg reg'
  reg `regUsedIn_` CmmRegOff reg' _ = regsOverlap dflags reg reg'
  reg `regUsedIn_` CmmMachOp _ es   = any (reg `regUsedIn_`) es
  _   `regUsedIn_` CmmStackSlot _ _ = False

--------------------------------------------
--
--        mkLiveness
--
---------------------------------------------

mkLiveness :: DynFlags -> [LocalReg] -> Liveness
mkLiveness _      [] = []
mkLiveness dflags (reg:regs)
  = bits ++ mkLiveness dflags regs
  where
    sizeW = (widthInBytes (typeWidth (localRegType reg)) + wORD_SIZE dflags - 1)
            `quot` wORD_SIZE dflags
            -- number of words, rounded up
    bits = replicate sizeW is_non_ptr -- True <=> Non Ptr

    is_non_ptr = not $ isGcPtrType (localRegType reg)


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

-- | Like 'toBlockListEntryFirst', but we strive to ensure that we order blocks
-- so that the false case of a conditional jumps to the next block in the output
-- list of blocks. This matches the way OldCmm blocks were output since in
-- OldCmm the false case was a fallthrough, whereas in Cmm conditional branches
-- have both true and false successors. Block ordering can make a big difference
-- in performance in the LLVM backend. Note that we rely crucially on the order
-- of successors returned for CmmCondBranch by the NonLocal instance for CmmNode
-- defined in cmm/CmmNode.hs. -GBM
toBlockListEntryFirstFalseFallthrough :: CmmGraph -> [CmmBlock]
toBlockListEntryFirstFalseFallthrough g
  | mapNull m  = []
  | otherwise  = dfs setEmpty [entry_block]
  where
    m = toBlockMap g
    entry_id = g_entry g
    Just entry_block = mapLookup entry_id m

    dfs :: LabelSet -> [CmmBlock] -> [CmmBlock]
    dfs _ [] = []
    dfs visited (block:bs)
      | id `setMember` visited = dfs visited bs
      | otherwise              = block : dfs (setInsert id visited) bs'
      where id = entryLabel block
            bs' = foldr add_id bs (successors block)
            add_id id bs = case mapLookup id m of
                              Just b  -> b : bs
                              Nothing -> bs

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
  ofBlockMap (entryLabel $ mf $ CmmEntry (g_entry g) GlobalScope) $
  mapMap (mapBlock3' funs) $ toBlockMap g

mapGraphNodes1 :: (forall e x. CmmNode e x -> CmmNode e x) -> CmmGraph -> CmmGraph
mapGraphNodes1 f = modifyGraph (mapGraph f)


foldlGraphBlocks :: (a -> CmmBlock -> a) -> a -> CmmGraph -> a
foldlGraphBlocks k z g = mapFoldl k z $ toBlockMap g

revPostorder :: CmmGraph -> [CmmBlock]
revPostorder g = {-# SCC "revPostorder" #-}
    revPostorderFrom (toBlockMap g) (g_entry g)

-------------------------------------------------
-- Tick utilities

-- | Extract all tick annotations from the given block
blockTicks :: Block CmmNode C C -> [CmmTickish]
blockTicks b = reverse $ foldBlockNodesF goStmt b []
  where goStmt :: CmmNode e x -> [CmmTickish] -> [CmmTickish]
        goStmt  (CmmTick t) ts = t:ts
        goStmt  _other      ts = ts


-- -----------------------------------------------------------------------------
-- Access to common global registers

baseExpr, spExpr, hpExpr, currentTSOExpr, currentNurseryExpr,
  spLimExpr, hpLimExpr, cccsExpr :: CmmExpr
baseExpr = CmmReg baseReg
spExpr = CmmReg spReg
spLimExpr = CmmReg spLimReg
hpExpr = CmmReg hpReg
hpLimExpr = CmmReg hpLimReg
currentTSOExpr = CmmReg currentTSOReg
currentNurseryExpr = CmmReg currentNurseryReg
cccsExpr = CmmReg cccsReg
