{-# LANGUAGE GADTs, RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-----------------------------------------------------------------------------
--
-- Cmm utilities.
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module GHC.Cmm.Utils(
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

        cmmMkAssign,

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

import GHC.Core.TyCon     ( PrimRep(..), PrimElemRep(..) )
import GHC.Types.RepType  ( UnaryType, SlotTy (..), typePrimRep1 )

import GHC.Platform
import GHC.Runtime.Heap.Layout
import GHC.Cmm
import GHC.Cmm.BlockId
import GHC.Cmm.CLabel
import Outputable
import GHC.Driver.Session
import GHC.Types.Unique
import GHC.Platform.Regs

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Bits
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Collections

---------------------------------------------------
--
--      CmmTypes
--
---------------------------------------------------

primRepCmmType :: Platform -> PrimRep -> CmmType
primRepCmmType platform = \case
   VoidRep          -> panic "primRepCmmType:VoidRep"
   LiftedRep        -> gcWord platform
   UnliftedRep      -> gcWord platform
   IntRep           -> bWord platform
   WordRep          -> bWord platform
   Int8Rep          -> b8
   Word8Rep         -> b8
   Int16Rep         -> b16
   Word16Rep        -> b16
   Int32Rep         -> b32
   Word32Rep        -> b32
   Int64Rep         -> b64
   Word64Rep        -> b64
   AddrRep          -> bWord platform
   FloatRep         -> f32
   DoubleRep        -> f64
   (VecRep len rep) -> vec len (primElemRepCmmType rep)

slotCmmType :: Platform -> SlotTy -> CmmType
slotCmmType platform = \case
   PtrSlot    -> gcWord platform
   WordSlot   -> bWord platform
   Word64Slot -> b64
   FloatSlot  -> f32
   DoubleSlot -> f64

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

typeCmmType :: Platform -> UnaryType -> CmmType
typeCmmType platform ty = primRepCmmType platform (typePrimRep1 ty)

primRepForeignHint :: PrimRep -> ForeignHint
primRepForeignHint VoidRep      = panic "primRepForeignHint:VoidRep"
primRepForeignHint LiftedRep    = AddrHint
primRepForeignHint UnliftedRep  = AddrHint
primRepForeignHint IntRep       = SignedHint
primRepForeignHint Int8Rep      = SignedHint
primRepForeignHint Int16Rep     = SignedHint
primRepForeignHint Int32Rep     = SignedHint
primRepForeignHint Int64Rep     = SignedHint
primRepForeignHint WordRep      = NoHint
primRepForeignHint Word8Rep     = NoHint
primRepForeignHint Word16Rep    = NoHint
primRepForeignHint Word32Rep    = NoHint
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
mkIntCLit :: Platform -> Int -> CmmLit
mkIntCLit platform i = CmmInt (toInteger i) (wordWidth platform)

mkIntExpr :: Platform -> Int -> CmmExpr
mkIntExpr platform i = CmmLit $! mkIntCLit platform i

zeroCLit :: Platform -> CmmLit
zeroCLit platform = CmmInt 0 (wordWidth platform)

zeroExpr :: Platform -> CmmExpr
zeroExpr platform = CmmLit (zeroCLit platform)

mkWordCLit :: Platform -> Integer -> CmmLit
mkWordCLit platform wd = CmmInt wd (wordWidth platform)

-- | We make a top-level decl for the string, and return a label pointing to it
mkByteStringCLit
  :: CLabel -> ByteString -> (CmmLit, GenCmmDecl RawCmmStatics info stmt)
mkByteStringCLit lbl bytes
  = (CmmLabel lbl, CmmData (Section sec lbl) $ RawCmmStatics lbl [CmmString bytes])
  where
    -- This can not happen for String literals (as there \NUL is replaced by
    -- C0 80). However, it can happen with Addr# literals.
    sec = if 0 `BS.elem` bytes then ReadOnlyData else CString

mkDataLits :: Section -> CLabel -> [CmmLit] -> GenCmmDecl RawCmmStatics info stmt
-- Build a data-segment data block
mkDataLits section lbl lits
  = CmmData section (RawCmmStatics lbl $ map CmmStaticLit lits)

mkRODataLits :: CLabel -> [CmmLit] -> GenCmmDecl RawCmmStatics info stmt
-- Build a read-only data block
mkRODataLits lbl lits
  = mkDataLits section lbl lits
  where
    section | any needsRelocation lits = Section RelocatableReadOnlyData lbl
            | otherwise                = Section ReadOnlyData lbl
    needsRelocation (CmmLabel _)      = True
    needsRelocation (CmmLabelOff _ _) = True
    needsRelocation _                 = False

mkStgWordCLit :: Platform -> StgWord -> CmmLit
mkStgWordCLit platform wd = CmmInt (fromStgWord wd) (wordWidth platform)

packHalfWordsCLit :: Platform -> StgHalfWord -> StgHalfWord -> CmmLit
-- Make a single word literal in which the lower_half_word is
-- at the lower address, and the upper_half_word is at the
-- higher address
-- ToDo: consider using half-word lits instead
--       but be careful: that's vulnerable when reversed
packHalfWordsCLit platform lower_half_word upper_half_word
   = case platformByteOrder platform of
       BigEndian    -> mkWordCLit platform ((l `shiftL` halfWordSizeInBits platform) .|. u)
       LittleEndian -> mkWordCLit platform (l .|. (u `shiftL` halfWordSizeInBits platform))
    where l = fromStgHalfWord lower_half_word
          u = fromStgHalfWord upper_half_word

---------------------------------------------------
--
--      CmmExpr
--
---------------------------------------------------

mkLblExpr :: CLabel -> CmmExpr
mkLblExpr lbl = CmmLit (CmmLabel lbl)

cmmOffsetExpr :: Platform -> CmmExpr -> CmmExpr -> CmmExpr
-- assumes base and offset have the same CmmType
cmmOffsetExpr platform e (CmmLit (CmmInt n _)) = cmmOffset platform e (fromInteger n)
cmmOffsetExpr platform e byte_off = CmmMachOp (MO_Add (cmmExprWidth platform e)) [e, byte_off]

cmmOffset :: Platform -> CmmExpr -> Int -> CmmExpr
cmmOffset _platform e 0        = e
cmmOffset platform  e byte_off = case e of
   CmmReg reg            -> cmmRegOff reg byte_off
   CmmRegOff reg m       -> cmmRegOff reg (m+byte_off)
   CmmLit lit            -> CmmLit (cmmOffsetLit lit byte_off)
   CmmStackSlot area off -> CmmStackSlot area (off - byte_off)
  -- note stack area offsets increase towards lower addresses
   CmmMachOp (MO_Add rep) [expr, CmmLit (CmmInt byte_off1 _rep)]
      -> CmmMachOp (MO_Add rep) [expr, CmmLit (CmmInt (byte_off1 + toInteger byte_off) rep)]
   _ -> CmmMachOp (MO_Add width) [e, CmmLit (CmmInt (toInteger byte_off) width)]
         where width = cmmExprWidth platform e

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
cmmIndex :: Platform
         -> Width       -- Width w
         -> CmmExpr     -- Address of vector of items of width w
         -> Int         -- Which element of the vector (0 based)
         -> CmmExpr     -- Address of i'th element
cmmIndex platform width base idx = cmmOffset platform base (idx * widthInBytes width)

-- | Useful for creating an index into an array, with an unknown offset.
cmmIndexExpr :: Platform
             -> Width           -- Width w
             -> CmmExpr         -- Address of vector of items of width w
             -> CmmExpr         -- Which element of the vector (0 based)
             -> CmmExpr         -- Address of i'th element
cmmIndexExpr platform width base (CmmLit (CmmInt n _)) = cmmIndex platform width base (fromInteger n)
cmmIndexExpr platform width base idx =
  cmmOffsetExpr platform base byte_off
  where
    idx_w = cmmExprWidth platform idx
    byte_off = CmmMachOp (MO_Shl idx_w) [idx, mkIntExpr platform (widthInLog width)]

cmmLoadIndex :: Platform -> CmmType -> CmmExpr -> Int -> CmmExpr
cmmLoadIndex platform ty expr ix = CmmLoad (cmmIndex platform (typeWidth ty) expr ix) ty

-- The "B" variants take byte offsets
cmmRegOffB :: CmmReg -> ByteOff -> CmmExpr
cmmRegOffB = cmmRegOff

cmmOffsetB :: Platform -> CmmExpr -> ByteOff -> CmmExpr
cmmOffsetB = cmmOffset

cmmOffsetExprB :: Platform -> CmmExpr -> CmmExpr -> CmmExpr
cmmOffsetExprB = cmmOffsetExpr

cmmLabelOffB :: CLabel -> ByteOff -> CmmLit
cmmLabelOffB = cmmLabelOff

cmmOffsetLitB :: CmmLit -> ByteOff -> CmmLit
cmmOffsetLitB = cmmOffsetLit

-----------------------
-- The "W" variants take word offsets

cmmOffsetExprW :: Platform -> CmmExpr -> CmmExpr -> CmmExpr
-- The second arg is a *word* offset; need to change it to bytes
cmmOffsetExprW platform  e (CmmLit (CmmInt n _)) = cmmOffsetW platform e (fromInteger n)
cmmOffsetExprW platform e wd_off = cmmIndexExpr platform (wordWidth platform) e wd_off

cmmOffsetW :: Platform -> CmmExpr -> WordOff -> CmmExpr
cmmOffsetW platform e n = cmmOffsetB platform e (wordsToBytes platform n)

cmmRegOffW :: Platform -> CmmReg -> WordOff -> CmmExpr
cmmRegOffW platform reg wd_off = cmmRegOffB reg (wordsToBytes platform wd_off)

cmmOffsetLitW :: Platform -> CmmLit -> WordOff -> CmmLit
cmmOffsetLitW platform lit wd_off = cmmOffsetLitB lit (wordsToBytes platform wd_off)

cmmLabelOffW :: Platform -> CLabel -> WordOff -> CmmLit
cmmLabelOffW platform lbl wd_off = cmmLabelOffB lbl (wordsToBytes platform wd_off)

cmmLoadIndexW :: Platform -> CmmExpr -> Int -> CmmType -> CmmExpr
cmmLoadIndexW platform base off ty = CmmLoad (cmmOffsetW platform base off) ty

-----------------------
cmmULtWord, cmmUGeWord, cmmUGtWord, cmmUShrWord,
  cmmSLtWord,
  cmmNeWord, cmmEqWord,
  cmmOrWord, cmmAndWord,
  cmmSubWord, cmmAddWord, cmmMulWord, cmmQuotWord
  :: Platform -> CmmExpr -> CmmExpr -> CmmExpr
cmmOrWord platform  e1 e2 = CmmMachOp (mo_wordOr platform)  [e1, e2]
cmmAndWord platform e1 e2 = CmmMachOp (mo_wordAnd platform) [e1, e2]
cmmNeWord platform  e1 e2 = CmmMachOp (mo_wordNe platform)  [e1, e2]
cmmEqWord platform  e1 e2 = CmmMachOp (mo_wordEq platform)  [e1, e2]
cmmULtWord platform e1 e2 = CmmMachOp (mo_wordULt platform) [e1, e2]
cmmUGeWord platform e1 e2 = CmmMachOp (mo_wordUGe platform) [e1, e2]
cmmUGtWord platform e1 e2 = CmmMachOp (mo_wordUGt platform) [e1, e2]
cmmSLtWord platform e1 e2 = CmmMachOp (mo_wordSLt platform) [e1, e2]
cmmUShrWord platform e1 e2 = CmmMachOp (mo_wordUShr platform) [e1, e2]
cmmAddWord platform e1 e2 = CmmMachOp (mo_wordAdd platform) [e1, e2]
cmmSubWord platform e1 e2 = CmmMachOp (mo_wordSub platform) [e1, e2]
cmmMulWord platform e1 e2 = CmmMachOp (mo_wordMul platform) [e1, e2]
cmmQuotWord platform e1 e2 = CmmMachOp (mo_wordUQuot platform) [e1, e2]

cmmNegate :: Platform -> CmmExpr -> CmmExpr
cmmNegate platform = \case
   (CmmLit (CmmInt n rep))
     -> CmmLit (CmmInt (-n) rep)
   e -> CmmMachOp (MO_S_Neg (cmmExprWidth platform e)) [e]

blankWord :: Platform -> CmmStatic
blankWord platform = CmmUninitialised (platformWordSizeInBytes platform)

cmmToWord :: Platform -> CmmExpr -> CmmExpr
cmmToWord platform e
  | w == word  = e
  | otherwise  = CmmMachOp (MO_UU_Conv w word) [e]
  where
    w = cmmExprWidth platform e
    word = wordWidth platform

cmmMkAssign :: Platform -> CmmExpr -> Unique -> (CmmNode O O, CmmExpr)
cmmMkAssign platform expr uq =
  let !ty = cmmExprType platform expr
      reg = (CmmLocal (LocalReg uq ty))
  in  (CmmAssign reg expr, CmmReg reg)


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
cmmTagMask dflags = mkIntExpr (targetPlatform dflags) (tAG_MASK dflags)
cmmPointerMask dflags = mkIntExpr (targetPlatform dflags) (complement (tAG_MASK dflags))

-- Used to untag a possibly tagged pointer
-- A static label need not be untagged
cmmUntag, cmmIsTagged, cmmConstrTag1 :: DynFlags -> CmmExpr -> CmmExpr
cmmUntag _ e@(CmmLit (CmmLabel _)) = e
-- Default case
cmmUntag dflags e = cmmAndWord platform e (cmmPointerMask dflags)
   where platform = targetPlatform dflags

-- Test if a closure pointer is untagged
cmmIsTagged dflags e = cmmNeWord platform (cmmAndWord platform e (cmmTagMask dflags)) (zeroExpr platform)
   where platform = targetPlatform dflags

-- Get constructor tag, but one based.
cmmConstrTag1 dflags e = cmmAndWord platform e (cmmTagMask dflags)
   where platform = targetPlatform dflags


-----------------------------------------------------------------------------
-- Overlap and usage

-- | Returns True if the two STG registers overlap on the specified
-- platform, in the sense that writing to one will clobber the
-- other. This includes the case that the two registers are the same
-- STG register. See Note [Overlapping global registers] for details.
regsOverlap :: Platform -> CmmReg -> CmmReg -> Bool
regsOverlap platform (CmmGlobal g) (CmmGlobal g')
  | Just real  <- globalRegMaybe platform g,
    Just real' <- globalRegMaybe platform g',
    real == real'
    = True
regsOverlap _ reg reg' = reg == reg'

-- | Returns True if the STG register is used by the expression, in
-- the sense that a store to the register might affect the value of
-- the expression.
--
-- We must check for overlapping registers and not just equal
-- registers here, otherwise CmmSink may incorrectly reorder
-- assignments that conflict due to overlap. See #10521 and Note
-- [Overlapping global registers].
regUsedIn :: Platform -> CmmReg -> CmmExpr -> Bool
regUsedIn platform = regUsedIn_ where
  _   `regUsedIn_` CmmLit _         = False
  reg `regUsedIn_` CmmLoad e  _     = reg `regUsedIn_` e
  reg `regUsedIn_` CmmReg reg'      = regsOverlap platform reg reg'
  reg `regUsedIn_` CmmRegOff reg' _ = regsOverlap platform reg reg'
  reg `regUsedIn_` CmmMachOp _ es   = any (reg `regUsedIn_`) es
  _   `regUsedIn_` CmmStackSlot _ _ = False

--------------------------------------------
--
--        mkLiveness
--
---------------------------------------------

mkLiveness :: Platform -> [LocalReg] -> Liveness
mkLiveness _      [] = []
mkLiveness platform (reg:regs)
  = bits ++ mkLiveness platform regs
  where
    word_size = platformWordSizeInBytes platform
    sizeW = (widthInBytes (typeWidth (localRegType reg)) + word_size - 1)
            `quot` word_size
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
