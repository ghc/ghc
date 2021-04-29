{-# language CPP #-}
{-# language GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals, NumericUnderscores #-}
module GHC.CmmToAsm.AArch64.CodeGen (
      cmmTopCodeGen
    , generateJumpTableForInstr
)

where

#include "HsVersions.h"

-- NCG stuff:
import GHC.Prelude hiding (EQ)

import GHC.Platform.Regs
import GHC.CmmToAsm.AArch64.Instr
import GHC.CmmToAsm.AArch64.Regs
import GHC.CmmToAsm.AArch64.Cond

import GHC.CmmToAsm.CPrim
import GHC.Cmm.DebugBlock
import GHC.CmmToAsm.Monad
   ( NatM, getNewRegNat
   , getPicBaseMaybeNat, getPlatform, getConfig
   , getDebugBlock, getFileId
   )
-- import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.PIC
import GHC.CmmToAsm.Format
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Types
import GHC.Platform.Reg
import GHC.Platform

-- Our intermediate code:
import GHC.Cmm.BlockId
import GHC.Cmm
import GHC.Cmm.Utils
import GHC.Cmm.Switch
import GHC.Cmm.CLabel
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.Types.Tickish ( GenTickish(..) )
import GHC.Types.SrcLoc  ( srcSpanFile, srcSpanStartLine, srcSpanStartCol )

-- The rest:
import GHC.Data.OrdList
import GHC.Utils.Outputable

import Control.Monad    ( mapAndUnzipM, when, foldM )
import Data.Bits
import Data.Word
import Data.Maybe
import GHC.Float

import GHC.Types.Basic
import GHC.Types.ForeignCall
import GHC.Data.FastString
import GHC.Utils.Misc
import GHC.Utils.Panic

-- @cmmTopCodeGen@ will be our main entry point to code gen.  Here we'll get
-- @RawCmmDecl@; see GHC.Cmm
--
--   RawCmmDecl = GenCmmDecl RawCmmStatics (LabelMap RawCmmStatics) CmmGraph
--
--   GenCmmDecl d h g = CmmProc h CLabel [GlobalReg] g
--                    | CmmData Section d
--
-- As a result we want to transform this to a list of @NatCmmDecl@, which is
-- defined @GHC.CmmToAsm.Instr@ as
--
--   type NatCmmDecl statics instr
--        = GenCmmDecl statics (LabelMap RawCmmStatics) (ListGraph instr)
--
-- Thus well' turn
--   GenCmmDecl RawCmmStatics (LabelMap RawCmmStatics) CmmGraph
-- into
--   [GenCmmDecl RawCmmStatics (LabelMap RawCmmStatics) (ListGraph Instr)]
--
-- where @CmmGraph@ is
--
--   type CmmGraph = GenCmmGraph CmmNode
--   data GenCmmGraph n = CmmGraph { g_entry :: BlockId, g_graph :: Graph n C C }
--   type CmmBlock = Block CmmNode C C
--
-- and @ListGraph Instr@ is
--
--   newtype ListGraph i = ListGraph [GenBasicBlock i]
--   data GenBasicBlock i = BasicBlock BlockId [i]

cmmTopCodeGen
    :: RawCmmDecl
    -> NatM [NatCmmDecl RawCmmStatics Instr]

-- Thus we'll have to deal with either CmmProc ...
cmmTopCodeGen _cmm@(CmmProc info lab live graph) = do
  -- config <- getConfig
  -- do
  --   traceM $ "-- -------------------------- cmmTopGen (CmmProc) -------------------------- --\n"
  --         ++ showSDocUnsafe (ppr cmm)

  let blocks = toBlockListEntryFirst graph
  (nat_blocks,statics) <- mapAndUnzipM basicBlockCodeGen blocks
  picBaseMb <- getPicBaseMaybeNat

  let proc = CmmProc info lab live (ListGraph $ concat nat_blocks)
      tops = proc : concat statics

  case picBaseMb of
      Just _picBase -> panic "AArch64.cmmTopCodeGen: picBase not implemented"
      Nothing -> return tops

-- ... or CmmData. Do we want to align this?
cmmTopCodeGen _cmm@(CmmData sec dat) = do
  -- config <- getConfig
  -- do
  --   traceM $ "-- -------------------------- cmmTopGen (CmmData) -------------------------- --\n"
  --         ++ showSDocUnsafe (ppr cmm)
  return [CmmData sec dat] -- no translation, we just use CmmStatic

-- So we need BasicBlockCodeGen
basicBlockCodeGen
        :: Block CmmNode C C
        -> NatM ( [NatBasicBlock Instr]
                , [NatCmmDecl RawCmmStatics Instr])

basicBlockCodeGen block = do
  config <- getConfig
  -- do
  --   traceM $ "-- --------------------------- basicBlockCodeGen --------------------------- --\n"
  --         ++ showSDocUnsafe (ppr block)
  let (_, nodes, tail)  = blockSplit block
      id = entryLabel block
      stmts = blockToList nodes

      header_comment_instr = unitOL $ MULTILINE_COMMENT (
          text "-- --------------------------- basicBlockCodeGen --------------------------- --\n"
          $+$ pdoc (ncgPlatform config) block
          )
  -- Generate location directive
  dbg <- getDebugBlock (entryLabel block)
  loc_instrs <- case dblSourceTick =<< dbg of
    Just (SourceNote span name)
      -> do fileId <- getFileId (srcSpanFile span)
            let line = srcSpanStartLine span; col = srcSpanStartCol span
            return $ unitOL $ LOCATION fileId line col name
    _ -> return nilOL
  (mid_instrs,mid_bid) <- stmtsToInstrs id stmts
  (!tail_instrs,_) <- stmtToInstrs mid_bid tail
  let instrs = header_comment_instr `appOL` loc_instrs `appOL` mid_instrs `appOL` tail_instrs
  -- XXX: Then x86 backend run @verifyBasicBlock@ here and inserts
  --      unwinding info.
  -- code generation may introduce new basic block boundaries, which
  -- are indicated by the NEWBLOCK instruction.  We must split up the
  -- instruction stream into basic blocks again.  Also, we extract
  -- LDATAs here too.
  let
        (top,other_blocks,statics) = foldrOL mkBlocks ([],[],[]) instrs

        mkBlocks (NEWBLOCK id) (instrs,blocks,statics)
          = ([], BasicBlock id instrs : blocks, statics)
        mkBlocks (LDATA sec dat) (instrs,blocks,statics)
          = (instrs, blocks, CmmData sec dat:statics)
        mkBlocks instr (instrs,blocks,statics)
          = (instr:instrs, blocks, statics)
  return (BasicBlock id top : other_blocks, statics)


-- -----------------------------------------------------------------------------
-- Generating a table-branch

-- XXX jump tables would be a lot faster, but we'll use bare bones for now.
-- this is usually done by sticking the jump table ids into an instruction
-- and then have the @generateJumpTableForInstr@ callback produce the jump
-- table as a static.
--
-- data SwitchTargets =
--    SwitchTargets
--        Bool                       -- Signed values
--        (Integer, Integer)         -- Range
--        (Maybe Label)              -- Default value
--        (M.Map Integer Label)      -- The branches
--
-- Non Jumptable plan:
-- xE <- expr
--
genSwitch :: CmmExpr -> SwitchTargets -> NatM InstrBlock
genSwitch expr targets = do -- pprPanic "genSwitch" (ppr expr)
  (reg, format, code) <- getSomeReg expr
  let w = formatToWidth format
  let mkbranch acc (key, bid) = do
        (keyReg, _format, code) <- getSomeReg (CmmLit (CmmInt key w))
        return $ code `appOL`
                 toOL [ CMP (OpReg w reg) (OpReg w keyReg)
                      , BCOND EQ (TBlock bid)
                      ] `appOL` acc
      def_code = case switchTargetsDefault targets of
        Just bid -> unitOL (B (TBlock bid))
        Nothing  -> nilOL

  switch_code <- foldM mkbranch nilOL (switchTargetsCases targets)
  return $ code `appOL` switch_code `appOL` def_code

-- We don't do jump tables for now
generateJumpTableForInstr :: NCGConfig -> Instr
  -> Maybe (NatCmmDecl RawCmmStatics Instr)
generateJumpTableForInstr _ _ = Nothing

-- -----------------------------------------------------------------------------
-- Top-level of the instruction selector

-- | 'InstrBlock's are the insn sequences generated by the insn selectors.
-- They are really trees of insns to facilitate fast appending, where a
-- left-to-right traversal (pre-order?) yields the insns in the correct
-- order.

-- See Note [Keeping track of the current block] for why
-- we pass the BlockId.
stmtsToInstrs :: BlockId -- ^ Basic block these statement will start to be placed in.
              -> [CmmNode O O] -- ^ Cmm Statement
              -> NatM (InstrBlock, BlockId) -- ^ Resulting instruction
stmtsToInstrs bid stmts =
    go bid stmts nilOL
  where
    go bid  []        instrs = return (instrs,bid)
    go bid (s:stmts)  instrs = do
      (instrs',bid') <- stmtToInstrs bid s
      -- If the statement introduced a new block, we use that one
      let !newBid = fromMaybe bid bid'
      go newBid stmts (instrs `appOL` instrs')

-- | `bid` refers to the current block and is used to update the CFG
--   if new blocks are inserted in the control flow.
-- See Note [Keeping track of the current block] for more details.
stmtToInstrs :: BlockId -- ^ Basic block this statement will start to be placed in.
             -> CmmNode e x
             -> NatM (InstrBlock, Maybe BlockId)
             -- ^ Instructions, and bid of new block if successive
             -- statements are placed in a different basic block.
stmtToInstrs bid stmt = do
  -- traceM $ "-- -------------------------- stmtToInstrs -------------------------- --\n"
  --     ++ showSDocUnsafe (ppr stmt)
  platform <- getPlatform
  case stmt of
    CmmUnsafeForeignCall target result_regs args
       -> genCCall target result_regs args bid

    _ -> (,Nothing) <$> case stmt of
      CmmComment s   -> return (unitOL (COMMENT (ftext s)))
      CmmTick {}     -> return nilOL

      CmmAssign reg src
        | isFloatType ty         -> assignReg_FltCode format reg src
        | otherwise              -> assignReg_IntCode format reg src
          where ty = cmmRegType platform reg
                format = cmmTypeFormat ty

      CmmStore addr src
        | isFloatType ty         -> assignMem_FltCode format addr src
        | otherwise              -> assignMem_IntCode format addr src
          where ty = cmmExprType platform src
                format = cmmTypeFormat ty

      CmmBranch id          -> genBranch id

      --We try to arrange blocks such that the likely branch is the fallthrough
      --in GHC.Cmm.ContFlowOpt. So we can assume the condition is likely false here.
      CmmCondBranch arg true false _prediction ->
          genCondBranch bid true false arg

      CmmSwitch arg ids -> genSwitch arg ids

      CmmCall { cml_target = arg
              , cml_args_regs = gregs } -> genJump arg (jumpRegs platform gregs)

      CmmUnwind _regs -> return nilOL

      _ -> pprPanic "stmtToInstrs: statement should have been cps'd away" (pdoc platform stmt)

jumpRegs :: Platform -> [GlobalReg] -> [Reg]
jumpRegs = undefined

--------------------------------------------------------------------------------
-- | 'InstrBlock's are the insn sequences generated by the insn selectors.
--      They are really trees of insns to facilitate fast appending, where a
--      left-to-right traversal yields the insns in the correct order.
--
type InstrBlock
        = OrdList Instr

-- | Register's passed up the tree.  If the stix code forces the register
--      to live in a pre-decided machine register, it comes out as @Fixed@;
--      otherwise, it comes out as @Any@, and the parent can decide which
--      register to put it in.
--
data Register
        = Fixed Format Reg InstrBlock
        | Any   Format (Reg -> InstrBlock)

-- | Sometimes we need to change the Format of a register. Primarily during
-- conversion.
swizzleRegisterRep :: Format -> Register -> Register
swizzleRegisterRep format (Fixed _ reg code) = Fixed format reg code
swizzleRegisterRep format (Any _ codefn)     = Any   format codefn

-- | Grab the Reg for a CmmReg
getRegisterReg :: Platform -> CmmReg -> Reg

getRegisterReg _ (CmmLocal (LocalReg u pk))
  = RegVirtual $ mkVirtualReg u (cmmTypeFormat pk)

-- XXX: this crashes for PicBaseReg
getRegisterReg platform (CmmGlobal mid)
  = case globalRegMaybe platform mid of
        Just reg -> RegReal reg
        Nothing  -> pprPanic "getRegisterReg-memory" (ppr $ CmmGlobal mid)
        -- By this stage, the only MagicIds remaining should be the
        -- ones which map to a real machine register on this
        -- platform.  Hence if it's not mapped to a registers something
        -- went wrong earlier in the pipeline.
-- | Convert a BlockId to some CmmStatic data
-- XXX: Add JumpTable Logic
-- jumpTableEntry :: NCGConfig -> Maybe BlockId -> CmmStatic
-- jumpTableEntry config Nothing   = CmmStaticLit (CmmInt 0 (ncgWordWidth config))
-- jumpTableEntry _ (Just blockid) = CmmStaticLit (CmmLabel blockLabel)
--     where blockLabel = blockLbl blockid

-- -----------------------------------------------------------------------------
-- Utility
isIntFormat :: Format -> Bool
isIntFormat = not . isFloatFormat

-- -----------------------------------------------------------------------------
-- General things for putting together code sequences

-- -- Expand CmmRegOff.  ToDo: should we do it this way around, or convert
-- -- CmmExprs into CmmRegOff?
-- mangleIndexTree :: Platform -> CmmExpr -> CmmExpr
-- mangleIndexTree platform (CmmRegOff reg off)
--   = CmmMachOp (MO_Add width) [CmmReg reg, CmmLit (CmmInt (fromIntegral off) width)]
--   where width = typeWidth (cmmRegType platform reg)

-- mangleIndexTree _ _
--         = panic "AArch64.CodeGen.mangleIndexTree: no match"

-- | The dual to getAnyReg: compute an expression into a register, but
--      we don't mind which one it is.
getSomeReg :: CmmExpr -> NatM (Reg, Format, InstrBlock)
getSomeReg expr = do
  r <- getRegister expr
  case r of
    Any rep code -> do
        tmp <- getNewRegNat rep
        return (tmp, rep, code tmp)
    Fixed rep reg code ->
        return (reg, rep, code)

-- XXX OPT: we might be able give getRegister
--          a hint, what kind of register we want.
getFloatReg :: HasCallStack => CmmExpr -> NatM (Reg, Format, InstrBlock)
getFloatReg expr = do
  r <- getRegister expr
  case r of
    Any rep code | isFloatFormat rep -> do
      tmp <- getNewRegNat rep
      return (tmp, rep, code tmp)
    Any II32 code -> do
      tmp <- getNewRegNat FF32
      return (tmp, FF32, code tmp)
    Any II64 code -> do
      tmp <- getNewRegNat FF64
      return (tmp, FF64, code tmp)
    Any _w _code -> do
      config <- getConfig
      pprPanic "can't do getFloatReg on" (pdoc (ncgPlatform config) expr)
    -- can't do much for fixed.
    Fixed rep reg code ->
      return (reg, rep, code)


-- XXX: TODO, bounds. We can't put any immediate
-- value in. They are constrained.
litToImm' :: CmmLit -> NatM (Operand, InstrBlock)
litToImm' lit = return (OpImm (litToImm lit), nilOL)


getRegister :: CmmExpr -> NatM Register
getRegister e = do
  config <- getConfig
  getRegister' config (ncgPlatform config) e

-- Handling PIC on AArch64.  AArch64 does not have a special PIC register, the
-- general approach is to simply go through the GOT, and there is assembly
-- support for this:
--
--   // Load the address of 'sym' from the GOT using ADRP and LDR (used for
--   // position-independent code on AArch64):
--   adrp x0, #:got:sym
--   ldr x0, [x0, #:got_lo12:sym]
--
-- See also: https://developer.arm.com/documentation/dui0774/i/armclang-integrated-assembler-directives/assembly-expressions
--
-- CmmGlobal @PicBaseReg@'s are generated in @GHC.CmmToAsm.PIC@ in the
-- @cmmMakePicReference@.  This is in turn called from @cmmMakeDynamicReference@
-- also in @Cmm.CmmToAsm.PIC@ from where it is also exported.  There are two
-- callsites for this. One is in this module to produce the @target@ in @genCCall@
-- the other is in @GHC.CmmToAsm@ in @cmmExprNative@.
--
-- Conceptually we do not want any special PicBaseReg to be used on AArch64. If
-- we want to distinguish between symbol loading, we need to address this through
-- the way we load it, not through a register.
--
getRegister' :: NCGConfig -> Platform -> CmmExpr -> NatM Register
-- OPTIMIZATION WARNING: CmmExpr rewrites
-- 1. Rewrite: Reg + (-n) => Reg - n
--    XXX: this expression souldn't even be generated to begin with.
getRegister' config plat (CmmMachOp (MO_Add w0) [x, CmmLit (CmmInt i w1)]) | i < 0
  = getRegister' config plat (CmmMachOp (MO_Sub w0) [x, CmmLit (CmmInt (-i) w1)])

getRegister' config plat (CmmMachOp (MO_Sub w0) [x, CmmLit (CmmInt i w1)]) | i < 0
  = getRegister' config plat (CmmMachOp (MO_Add w0) [x, CmmLit (CmmInt (-i) w1)])


-- Generic case.
getRegister' config plat expr
  = case expr of
    CmmReg (CmmGlobal PicBaseReg)
      -> pprPanic "getRegisterReg-memory" (ppr $ PicBaseReg)
    CmmLit lit
      -> case lit of

        -- XXX handle CmmInt 0 specially, use wzr or xzr.

        CmmInt i W8  -> do
          return (Any (intFormat W8) (\dst -> unitOL $ ANN (text $ show expr) (MOV (OpReg W8 dst) (OpImm (ImmInteger (narrowS W8 i))))))
        CmmInt i W16 -> do
          return (Any (intFormat W16) (\dst -> unitOL $ ANN (text $ show expr) (MOV (OpReg W16 dst) (OpImm (ImmInteger (narrowS W16 i))))))

        -- We need to be careful to not shorten this for negative literals.
        -- Those need the upper bits set. We'd either have to explicitly sign
        -- or figure out something smarter. Lowered to
        -- `MOV dst XZR`
        CmmInt i w | is16bit i, i >= 0 -> do
          return (Any (intFormat w) (\dst -> unitOL $ ANN (text $ show expr) (MOV (OpReg W16 dst) (OpImm (ImmInteger i)))))
        CmmInt i w | is32bit i, i >= 0 -> do
          let  half0 = fromIntegral (fromIntegral i :: Word16)
               half1 = fromIntegral (fromIntegral (i `shiftR` 16) :: Word16)
          return (Any (intFormat w) (\dst -> toOL [ ANN (text $ show expr)
                                                  $ MOV (OpReg W32 dst) (OpImm (ImmInt half0))
                                                  , MOVK (OpReg W32 dst) (OpImmShift (ImmInt half1) SLSL 16)
                                                  ]))
        -- fallback for W32
        CmmInt i W32 -> do
          let  half0 = fromIntegral (fromIntegral i :: Word16)
               half1 = fromIntegral (fromIntegral (i `shiftR` 16) :: Word16)
          return (Any (intFormat W32) (\dst -> toOL [ ANN (text $ show expr)
                                                    $ MOV (OpReg W32 dst) (OpImm (ImmInt half0))
                                                    , MOVK (OpReg W32 dst) (OpImmShift (ImmInt half1) SLSL 16)
                                                    ]))
        -- anything else
        CmmInt i W64 -> do
          let  half0 = fromIntegral (fromIntegral i :: Word16)
               half1 = fromIntegral (fromIntegral (i `shiftR` 16) :: Word16)
               half2 = fromIntegral (fromIntegral (i `shiftR` 32) :: Word16)
               half3 = fromIntegral (fromIntegral (i `shiftR` 48) :: Word16)
          return (Any (intFormat W64) (\dst -> toOL [ ANN (text $ show expr)
                                                    $ MOV (OpReg W64 dst) (OpImm (ImmInt half0))
                                                    , MOVK (OpReg W64 dst) (OpImmShift (ImmInt half1) SLSL 16)
                                                    , MOVK (OpReg W64 dst) (OpImmShift (ImmInt half2) SLSL 32)
                                                    , MOVK (OpReg W64 dst) (OpImmShift (ImmInt half3) SLSL 48)
                                                    ]))
        CmmInt _i rep -> do
          (op, imm_code) <- litToImm' lit
          return (Any (intFormat rep) (\dst -> imm_code `snocOL` ANN (text $ show expr) (MOV (OpReg rep dst) op)))

        -- floatToBytes (fromRational f)
        CmmFloat 0 w   -> do
          (op, imm_code) <- litToImm' lit
          return (Any (floatFormat w) (\dst -> imm_code `snocOL` ANN (text $ show expr) (MOV (OpReg w dst) op)))

        CmmFloat _f W8  -> pprPanic "getRegister' (CmmLit:CmmFloat), no support for bytes" (pdoc plat expr)
        CmmFloat _f W16 -> pprPanic "getRegister' (CmmLit:CmmFloat), no support for halfs" (pdoc plat expr)
        CmmFloat f W32 -> do
          let word = castFloatToWord32 (fromRational f) :: Word32
              half0 = fromIntegral (fromIntegral word :: Word16)
              half1 = fromIntegral (fromIntegral (word `shiftR` 16) :: Word16)
          tmp <- getNewRegNat (intFormat W32)
          return (Any (floatFormat W32) (\dst -> toOL [ ANN (text $ show expr)
                                                      $ MOV (OpReg W32 tmp) (OpImm (ImmInt half0))
                                                      , MOVK (OpReg W32 tmp) (OpImmShift (ImmInt half1) SLSL 16)
                                                      , MOV (OpReg W32 dst) (OpReg W32 tmp)
                                                      ]))
        CmmFloat f W64 -> do
          let word = castDoubleToWord64 (fromRational f) :: Word64
              half0 = fromIntegral (fromIntegral word :: Word16)
              half1 = fromIntegral (fromIntegral (word `shiftR` 16) :: Word16)
              half2 = fromIntegral (fromIntegral (word `shiftR` 32) :: Word16)
              half3 = fromIntegral (fromIntegral (word `shiftR` 48) :: Word16)
          tmp <- getNewRegNat (intFormat W64)
          return (Any (floatFormat W64) (\dst -> toOL [ ANN (text $ show expr)
                                                      $ MOV (OpReg W64 tmp) (OpImm (ImmInt half0))
                                                      , MOVK (OpReg W64 tmp) (OpImmShift (ImmInt half1) SLSL 16)
                                                      , MOVK (OpReg W64 tmp) (OpImmShift (ImmInt half2) SLSL 32)
                                                      , MOVK (OpReg W64 tmp) (OpImmShift (ImmInt half3) SLSL 48)
                                                      , MOV (OpReg W64 dst) (OpReg W64 tmp)
                                                      ]))
        CmmFloat _f _w -> pprPanic "getRegister' (CmmLit:CmmFloat), unsupported float lit" (pdoc plat expr)
        CmmVec _ -> pprPanic "getRegister' (CmmLit:CmmVec): " (pdoc plat expr)
        CmmLabel _lbl -> do
          (op, imm_code) <- litToImm' lit
          let rep = cmmLitType plat lit
              format = cmmTypeFormat rep
          return (Any format (\dst -> imm_code `snocOL` (ANN (text $ show expr) $ LDR format (OpReg (formatToWidth format) dst) op)))

        CmmLabelOff _lbl off | is12bit (fromIntegral off) -> do
          (op, imm_code) <- litToImm' lit
          let rep = cmmLitType plat lit
              format = cmmTypeFormat rep
              -- width = typeWidth rep
          return (Any format (\dst -> imm_code `snocOL` LDR format (OpReg (formatToWidth format) dst) op))

        CmmLabelOff lbl off -> do
          (op, imm_code) <- litToImm' (CmmLabel lbl)
          let rep = cmmLitType plat lit
              format = cmmTypeFormat rep
              width = typeWidth rep
          (off_r, _off_format, off_code) <- getSomeReg $ CmmLit (CmmInt (fromIntegral off) width)
          return (Any format (\dst -> imm_code `appOL` off_code `snocOL` LDR format (OpReg (formatToWidth format) dst) op `snocOL` ADD (OpReg width dst) (OpReg width dst) (OpReg width off_r)))

        CmmLabelDiffOff _ _ _ _ -> pprPanic "getRegister' (CmmLit:CmmLabelOff): " (pdoc plat expr)
        CmmBlock _ -> pprPanic "getRegister' (CmmLit:CmmLabelOff): " (pdoc plat expr)
        CmmHighStackMark -> pprPanic "getRegister' (CmmLit:CmmLabelOff): " (pdoc plat expr)
    CmmLoad mem rep -> do
      Amode addr addr_code <- getAmode plat mem
      let format = cmmTypeFormat rep
      return (Any format (\dst -> addr_code `snocOL` LDR format (OpReg (formatToWidth format) dst) (OpAddr addr)))
    CmmStackSlot _ _
      -> pprPanic "getRegister' (CmmStackSlot): " (pdoc plat expr)
    CmmReg reg
      -> return (Fixed (cmmTypeFormat (cmmRegType plat reg))
                       (getRegisterReg plat reg)
                       nilOL)
    CmmRegOff reg off | is12bit (fromIntegral off) -> do
      getRegister' config plat $
            CmmMachOp (MO_Add width) [CmmReg reg, CmmLit (CmmInt (fromIntegral off) width)]
          where width = typeWidth (cmmRegType plat reg)

    CmmRegOff reg off -> do
      (off_r, _off_format, off_code) <- getSomeReg $ CmmLit (CmmInt (fromIntegral off) width)
      (reg, _format, code) <- getSomeReg $ CmmReg reg
      return $ Any (intFormat width) (\dst -> off_code `appOL` code `snocOL` ADD (OpReg width dst) (OpReg width reg) (OpReg width off_r))
          where width = typeWidth (cmmRegType plat reg)



    -- for MachOps, see GHC.Cmm.MachOp
    -- For CmmMachOp, see GHC.Cmm.Expr
    CmmMachOp op [e] -> do
      (reg, _format, code) <- getSomeReg e
      case op of
        MO_Not w -> return $ Any (intFormat w) (\dst -> code `snocOL` MVN (OpReg w dst) (OpReg w reg))

        MO_S_Neg w -> return $ Any (intFormat w) (\dst -> code `snocOL` NEG (OpReg w dst) (OpReg w reg))
        MO_F_Neg w -> return $ Any (floatFormat w) (\dst -> code `snocOL` NEG (OpReg w dst) (OpReg w reg))

        MO_SF_Conv from to -> return $ Any (floatFormat to) (\dst -> code `snocOL` SCVTF (OpReg to dst) (OpReg from reg))  -- (Signed ConVerT Float)
        MO_FS_Conv from to -> return $ Any (intFormat to) (\dst -> code `snocOL` FCVTZS (OpReg to dst) (OpReg from reg)) -- (float convert (-> zero) signed)

        -- XXX this is very hacky
        -- Note, UBFM and SBFM expect source and target register to be of the same size, so we'll use @max from to@
        -- UBFM will set the high bits to 0. SBFM will copy the sign (sign extend).
        MO_UU_Conv from to -> return $ Any (intFormat to) (\dst -> code `snocOL` UBFM (OpReg (max from to) dst) (OpReg (max from to) reg) (OpImm (ImmInt 0)) (toImm (min from to)))
        MO_SS_Conv from to -> return $ Any (intFormat to) (\dst -> code `snocOL` SBFM (OpReg (max from to) dst) (OpReg (max from to) reg) (OpImm (ImmInt 0)) (toImm (min from to)))
        MO_FF_Conv from to -> return $ Any (floatFormat to) (\dst -> code `snocOL` FCVT (OpReg to dst) (OpReg from reg))

        -- Conversions
        MO_XX_Conv _from to -> swizzleRegisterRep (intFormat to) <$> getRegister e

        _ -> pprPanic "getRegister' (monadic CmmMachOp):" (pdoc plat expr)
      where toImm W8 =  (OpImm (ImmInt 7))
            toImm W16 = (OpImm (ImmInt 15))
            toImm W32 = (OpImm (ImmInt 31))
            toImm W64 = (OpImm (ImmInt 63))
            toImm W128 = (OpImm (ImmInt 127))
            toImm W256 = (OpImm (ImmInt 255))
            toImm W512 = (OpImm (ImmInt 511))
    -- Dyadic machops:
    --
    -- The general idea is:
    -- compute x<i> <- x
    -- compute x<j> <- y
    -- OP x<r>, x<i>, x<j>
    --
    -- XXX: for now we'll only implement the 64bit versions. And rely on the
    --      fallthrough to alert us if things go wrong!
    -- OPTIMIZATION WARNING: Dyadic CmmMachOp destructuring
    -- 0. XXX This should not exist! Rewrite: Reg +- 0 -> Reg
    CmmMachOp (MO_Add _) [expr'@(CmmReg (CmmGlobal _r)), CmmLit (CmmInt 0 _)] -> getRegister' config plat expr'
    CmmMachOp (MO_Sub _) [expr'@(CmmReg (CmmGlobal _r)), CmmLit (CmmInt 0 _)] -> getRegister' config plat expr'
    -- 1. Compute Reg +/- n directly.
    --    For Add/Sub we can directly encode 12bits, or 12bits lsl #12.
    CmmMachOp (MO_Add w) [(CmmReg reg), CmmLit (CmmInt n _)]
      | n > 0 && n < 4096 -> return $ Any (intFormat w) (\d -> unitOL $ ANN (text $ show expr) (ADD (OpReg w d) (OpReg w' r') (OpImm (ImmInteger n))))
      -- XXX: 12bits lsl #12; e.g. lower 12 bits of n are 0; shift n >> 12, and set lsl to #12.
      where w' = formatToWidth (cmmTypeFormat (cmmRegType plat reg))
            r' = getRegisterReg plat reg
    CmmMachOp (MO_Sub w) [(CmmReg reg), CmmLit (CmmInt n _)]
      | n > 0 && n < 4096 -> return $ Any (intFormat w) (\d -> unitOL $ ANN (text $ show expr) (SUB (OpReg w d) (OpReg w' r') (OpImm (ImmInteger n))))
      -- XXX: 12bits lsl #12; e.g. lower 12 bits of n are 0; shift n >> 12, and set lsl to #12.
      where w' = formatToWidth (cmmTypeFormat (cmmRegType plat reg))
            r' = getRegisterReg plat reg

    -- 2. Shifts. x << n, x >> n.
    CmmMachOp (MO_Shl w) [x, (CmmLit (CmmInt n _))] | w == W32, 0 <= n, n < 32 -> do
      (reg_x, _format_x, code_x) <- getSomeReg x
      return $ Any (intFormat w) (\dst -> code_x `snocOL` ANN (text $ show expr) (LSL (OpReg w dst) (OpReg w reg_x) (OpImm (ImmInteger n))))
    CmmMachOp (MO_Shl w) [x, (CmmLit (CmmInt n _))] | w == W64, 0 <= n, n < 64 -> do
      (reg_x, _format_x, code_x) <- getSomeReg x
      return $ Any (intFormat w) (\dst -> code_x `snocOL` ANN (text $ show expr) (LSL (OpReg w dst) (OpReg w reg_x) (OpImm (ImmInteger n))))

    CmmMachOp (MO_U_Shr w) [x, (CmmLit (CmmInt n _))] | w == W32, 0 <= n, n < 32 -> do
      (reg_x, _format_x, code_x) <- getSomeReg x
      return $ Any (intFormat w) (\dst -> code_x `snocOL` ANN (text $ show expr) (LSR (OpReg w dst) (OpReg w reg_x) (OpImm (ImmInteger n))))
    CmmMachOp (MO_U_Shr w) [x, (CmmLit (CmmInt n _))] | w == W64, 0 <= n, n < 64 -> do
      (reg_x, _format_x, code_x) <- getSomeReg x
      return $ Any (intFormat w) (\dst -> code_x `snocOL` ANN (text $ show expr) (LSR (OpReg w dst) (OpReg w reg_x) (OpImm (ImmInteger n))))

    -- 3. Logic &&, ||
    CmmMachOp (MO_And w) [(CmmReg reg), CmmLit (CmmInt n _)] | isBitMaskImmediate (fromIntegral n) ->
      return $ Any (intFormat w) (\d -> unitOL $ ANN (text $ show expr) (AND (OpReg w d) (OpReg w' r') (OpImm (ImmInteger n))))
      where w' = formatToWidth (cmmTypeFormat (cmmRegType plat reg))
            r' = getRegisterReg plat reg

    CmmMachOp (MO_Or w) [(CmmReg reg), CmmLit (CmmInt n _)] | isBitMaskImmediate (fromIntegral n) ->
      return $ Any (intFormat w) (\d -> unitOL $ ANN (text $ show expr) (ORR (OpReg w d) (OpReg w' r') (OpImm (ImmInteger n))))
      where w' = formatToWidth (cmmTypeFormat (cmmRegType plat reg))
            r' = getRegisterReg plat reg

    -- Generic case.
    CmmMachOp op [x, y] -> do
      -- alright, so we have an operation, and two expressions. And we want to essentially do
      -- ensure we get float regs
      let genOp w op = do
            (reg_x, format_x, code_x) <- getSomeReg x
            (reg_y, format_y, code_y) <- getSomeReg y
            when ((isFloatFormat format_x && isIntFormat format_y) || (isIntFormat format_x && isFloatFormat format_y)) $ pprPanic "getRegister:genOp" (text "formats don't match:" <+> text (show format_x) <+> text "/=" <+> text (show format_y))
            return $ Any format_x (\dst -> code_x `appOL` code_y `appOL` op (OpReg w dst) (OpReg w reg_x) (OpReg w reg_y))

          withTempIntReg w op = OpReg w <$> getNewRegNat (intFormat w) >>= op
          -- withTempFloatReg w op = OpReg w <$> getNewRegNat (floatFormat w) >>= op

          intOp w op = do
            -- compute x<m> <- x
            -- compute x<o> <- y
            -- <OP> x<n>, x<m>, x<o>
            (reg_x, _format_x, code_x) <- getSomeReg x
            (reg_y, _format_y, code_y) <- getSomeReg y
            return $ Any (intFormat w) (\dst -> code_x `appOL` code_y `appOL` op (OpReg w dst) (OpReg w reg_x) (OpReg w reg_y))
          floatOp w op = do
            (reg_fx, _format_x, code_fx) <- getFloatReg x
            (reg_fy, _format_y, code_fy) <- getFloatReg y
            return $ Any (floatFormat w) (\dst -> code_fx `appOL` code_fy `appOL` op (OpReg w dst) (OpReg w reg_fx) (OpReg w reg_fy))
          -- need a special one for conditionals, as they return ints
          floatCond w op = do
            (reg_fx, _format_x, code_fx) <- getFloatReg x
            (reg_fy, _format_y, code_fy) <- getFloatReg y
            return $ Any (intFormat w) (\dst -> code_fx `appOL` code_fy `appOL` op (OpReg w dst) (OpReg w reg_fx) (OpReg w reg_fy))

      case op of
        -- Integer operations
        -- Add/Sub should only be Interger Options.
        -- But our Cmm parser doesn't care about types
        -- and thus we end up with <float> + <float> => MO_Add <float> <float>
        MO_Add w -> genOp w (\d x y -> unitOL $ ANN (text $ show expr) (ADD d x y))
        MO_Sub w -> genOp w (\d x y -> unitOL $ ANN (text $ show expr) (SUB d x y))
        --    31  30  29  28
        --  .---+---+---+---+-- - -
        --  | N | Z | C | V |
        --  '---+---+---+---+-- - -
        --  Negative
        --  Zero
        --  Carry
        --  oVerflow
        --
        --  .------+-------------------------------------+-----------------+----------.
        --  | Code | Meaning                             | Flags           | Encoding |
        --  |------+-------------------------------------+-----------------+----------|
        --  |  EQ  | Equal                               | Z = 1           | 0000     |
        --  |  NE  | Not Equal                           | Z = 0           | 0001     |
        --  |  HI  | Unsigned Higher                     | C = 1 && Z = 0  | 1000     |
        --  |  HS  | Unsigned Higher or Same             | C = 1           | 0010     |
        --  |  LS  | Unsigned Lower or Same              | C = 0 || Z = 1  | 1001     |
        --  |  LO  | Unsigned Lower                      | C = 0           | 0011     |
        --  |  GT  | Signed Greater Than                 | Z = 0 && N = V  | 1100     |
        --  |  GE  | Signed Greater Than or Equal        | N = V           | 1010     |
        --  |  LE  | Signed Less Than or Equal           | Z = 1 || N /= V | 1101     |
        --  |  LT  | Signed Less Than                    | N /= V          | 1011     |
        --  |  CS  | Carry Set (Unsigned Overflow)       | C = 1           | 0010     |
        --  |  CC  | Carry Clear (No Unsigned Overflow)  | C = 0           | 0011     |
        --  |  VS  | Signed Overflow                     | V = 1           | 0110     |
        --  |  VC  | No Signed Overflow                  | V = 0           | 0111     |
        --  |  MI  | Minus, Negative                     | N = 1           | 0100     |
        --  |  PL  | Plus, Positive or Zero (!)          | N = 0           | 0101     |
        --  |  AL  | Always                              | Any             | 1110     |
        --  |  NV  | Never                               | Any             | 1111     |
        --- '-------------------------------------------------------------------------'

        MO_Eq w -> intOp w (\d x y -> toOL [ CMP x y, CSET d EQ ])
        MO_Ne w -> intOp w (\d x y -> toOL [ CMP x y, CSET d NE ])
        MO_Mul w -> intOp w (\d x y -> unitOL $ MUL d x y)

        -- Signed multiply/divide
        MO_S_MulMayOflo w -> intOp w (\d x y -> toOL [ MUL d x y, CSET d VS ])
        MO_S_Quot w -> intOp w (\d x y -> unitOL $ SDIV d x y)

        -- No native rem instruction. So we'll compute the following
        -- Rd  <- Rx / Ry             | 2 <- 7 / 3      -- SDIV Rd Rx Ry
        -- Rd' <- Rx - Rd * Ry        | 1 <- 7 - 2 * 3  -- MSUB Rd' Rd Ry Rx
        --        |     '---|----------------|---'   |
        --        |         '----------------|-------'
        --        '--------------------------'
        -- Note the swap in Rx and Ry.
        MO_S_Rem w -> withTempIntReg w $ \t ->
          intOp w (\d x y -> toOL [ SDIV t x y, MSUB d t y x ])

        -- Unsigned multiply/divide
        MO_U_MulMayOflo _w -> unsupportedP plat expr
        MO_U_Quot w -> intOp w (\d x y -> unitOL $ UDIV d x y)
        MO_U_Rem w  -> withTempIntReg w $ \t ->
          intOp w (\d x y -> toOL [ UDIV t x y, MSUB d t y x ])

        -- Signed comparisons -- see above for the CSET discussion
        MO_S_Ge w -> intOp w (\d x y -> toOL [ CMP x y, CSET d SGE ])
        MO_S_Le w -> intOp w (\d x y -> toOL [ CMP x y, CSET d SLE ])
        MO_S_Gt w -> intOp w (\d x y -> toOL [ CMP x y, CSET d SGT ])
        MO_S_Lt w -> intOp w (\d x y -> toOL [ CMP x y, CSET d SLT ])

        -- Unsigned comparisons
        MO_U_Ge w -> intOp w (\d x y -> toOL [ CMP x y, CSET d UGE ])
        MO_U_Le w -> intOp w (\d x y -> toOL [ CMP x y, CSET d ULE ])
        MO_U_Gt w -> intOp w (\d x y -> toOL [ CMP x y, CSET d UGT ])
        MO_U_Lt w -> intOp w (\d x y -> toOL [ CMP x y, CSET d ULT ])

        -- Floating point arithmetic
        MO_F_Add w   -> floatOp w (\d x y -> unitOL $ ADD d x y)
        MO_F_Sub w   -> floatOp w (\d x y -> unitOL $ SUB d x y)
        MO_F_Mul w   -> floatOp w (\d x y -> unitOL $ MUL d x y)
        MO_F_Quot w  -> floatOp w (\d x y -> unitOL $ SDIV d x y)

        -- Floating point comparison
        MO_F_Eq w    -> floatCond w (\d x y -> toOL [ CMP x y, CSET d EQ ])
        MO_F_Ne w    -> floatCond w (\d x y -> toOL [ CMP x y, CSET d NE ])

        -- careful with the floating point operations.
        -- SLE is effectively LE or unordered (NaN)
        -- SLT is the same. ULE, and ULT will not return true for NaN.
        -- This is a bit counter intutive. Don't let yourself be fooled by
        -- the S/U prefix for floats, it's only meaningful for integers.
        MO_F_Ge w    -> floatCond w (\d x y -> toOL [ CMP x y, CSET d OGE ])
        MO_F_Le w    -> floatCond w (\d x y -> toOL [ CMP x y, CSET d OLE ]) -- x <= y <=> y > x
        MO_F_Gt w    -> floatCond w (\d x y -> toOL [ CMP x y, CSET d OGT ])
        MO_F_Lt w    -> floatCond w (\d x y -> toOL [ CMP x y, CSET d OLT ]) -- x < y <=> y >= x

        -- Bitwise operations
        MO_And   w -> intOp w (\d x y -> unitOL $ AND d x y)
        MO_Or    w -> intOp w (\d x y -> unitOL $ ORR d x y)
        MO_Xor   w -> intOp w (\d x y -> unitOL $ EOR d x y)
        -- MO_Not   W64 ->
        MO_Shl   w -> intOp w (\d x y -> unitOL $ LSL d x y)
        MO_U_Shr w -> intOp w (\d x y -> unitOL $ LSR d x y)
        MO_S_Shr w -> intOp w (\d x y -> unitOL $ ASR d x y)

        -- XXX

        op -> pprPanic "getRegister' (unhandled dyadic CmmMachOp): " $ (pprMachOp op) <+> text "in" <+> (pdoc plat expr)
    CmmMachOp _op _xs
      -> pprPanic "getRegister' (variadic CmmMachOp): " (pdoc plat expr)

  where
    unsupportedP :: OutputableP env a => env -> a -> b
    unsupportedP platform op = pprPanic "Unsupported op:" (pdoc platform op)

    is12bit :: Integer -> Bool
    is12bit i = (-1 `shiftL` 11) <= i && i < (1 `shiftL` 11)
    is16bit :: Integer -> Bool
    is16bit i = (-1 `shiftL` 15) <= i && i < (1 `shiftL` 15)
    is32bit :: Integer -> Bool
    is32bit i = (-1 `shiftL` 31) <= i && i < (1 `shiftL` 31)
    -- This needs to check if n can be encoded as a bitmask immediate:
    --
    -- See https://stackoverflow.com/questions/30904718/range-of-immediate-values-in-armv8-a64-assembly
    --
    isBitMaskImmediate :: Integer -> Bool
    isBitMaskImmediate i = i `elem` [0b0000_0001, 0b0000_0010, 0b0000_0100, 0b0000_1000, 0b0001_0000, 0b0010_0000, 0b0100_0000, 0b1000_0000
                                    ,0b0000_0011, 0b0000_0110, 0b0000_1100, 0b0001_1000, 0b0011_0000, 0b0110_0000, 0b1100_0000
                                    ,0b0000_0111, 0b0000_1110, 0b0001_1100, 0b0011_1000, 0b0111_0000, 0b1110_0000
                                    ,0b0000_1111, 0b0001_1110, 0b0011_1100, 0b0111_1000, 0b1111_0000
                                    ,0b0001_1111, 0b0011_1110, 0b0111_1100, 0b1111_1000
                                    ,0b0011_1111, 0b0111_1110, 0b1111_1100
                                    ,0b0111_1111, 0b1111_1110
                                    ,0b1111_1111]


-- -----------------------------------------------------------------------------
--  The 'Amode' type: Memory addressing modes passed up the tree.
data Amode = Amode AddrMode InstrBlock

getAmode :: Platform -> CmmExpr -> NatM Amode
-- XXX: Specialize stuff we can destructure here.

-- OPTIMIZATION WARNING: Addressing modes.
-- Addressing options:
-- LDUR/STUR: imm9: -256 - 255
getAmode platform (CmmRegOff reg off) | -256 <= off, off <= 255
  = return $ Amode (AddrRegImm reg' off') nilOL
    where reg' = getRegisterReg platform reg
          off' = ImmInt off
-- LDR/STR: imm12: if reg is 32bit: 0 -- 16380 in multiples of 4
getAmode platform (CmmRegOff reg off)
  | typeWidth (cmmRegType platform reg) == W32, 0 <= off, off <= 16380, off `mod` 4 == 0
  = return $ Amode (AddrRegImm reg' off') nilOL
    where reg' = getRegisterReg platform reg
          off' = ImmInt off
-- LDR/STR: imm12: if reg is 64bit: 0 -- 32760 in multiples of 8
getAmode platform (CmmRegOff reg off)
  | typeWidth (cmmRegType platform reg) == W64, 0 <= off, off <= 32760, off `mod` 8 == 0
  = return $ Amode (AddrRegImm reg' off') nilOL
    where reg' = getRegisterReg platform reg
          off' = ImmInt off

-- For Stores we often see something like this:
-- CmmStore (CmmMachOp (MO_Add w) [CmmLoad expr, CmmLit (CmmInt n w')]) (expr2)
-- E.g. a CmmStoreOff really. This can be translated to `str $expr2, [$expr, #n ]
-- for `n` in range.
getAmode _platform (CmmMachOp (MO_Add _w) [expr, CmmLit (CmmInt off _w')])
  | -256 <= off, off <= 255
  = do (reg, _format, code) <- getSomeReg expr
       return $ Amode (AddrRegImm reg (ImmInteger off)) code

getAmode _platform (CmmMachOp (MO_Sub _w) [expr, CmmLit (CmmInt off _w')])
  | -256 <= -off, -off <= 255
  = do (reg, _format, code) <- getSomeReg expr
       return $ Amode (AddrRegImm reg (ImmInteger (-off))) code

-- Generic case
getAmode _platform expr
  = do (reg, _format, code) <- getSomeReg expr
       return $ Amode (AddrReg reg) code

-- -----------------------------------------------------------------------------
-- Generating assignments

-- Assignments are really at the heart of the whole code generation
-- business.  Almost all top-level nodes of any real importance are
-- assignments, which correspond to loads, stores, or register
-- transfers.  If we're really lucky, some of the register transfers
-- will go away, because we can use the destination register to
-- complete the code generation for the right hand side.  This only
-- fails when the right hand side is forced into a fixed register
-- (e.g. the result of a call).

assignMem_IntCode :: Format -> CmmExpr -> CmmExpr -> NatM InstrBlock
assignReg_IntCode :: Format -> CmmReg  -> CmmExpr -> NatM InstrBlock

assignMem_FltCode :: Format -> CmmExpr -> CmmExpr -> NatM InstrBlock
assignReg_FltCode :: Format -> CmmReg  -> CmmExpr -> NatM InstrBlock

assignMem_IntCode rep addrE srcE
  = do
    (src_reg, _format, code) <- getSomeReg srcE
    platform <- getPlatform
    Amode addr addr_code <- getAmode platform addrE
    return $ COMMENT (text "CmmStore" <+> parens (text (show addrE)) <+> parens (text (show srcE)))
            `consOL` (code
            `appOL` addr_code
            `snocOL` STR rep (OpReg (formatToWidth rep) src_reg) (OpAddr addr))

assignReg_IntCode _ reg src
  = do
    platform <- getPlatform
    let dst = getRegisterReg platform reg
    r <- getRegister src
    return $ case r of
      Any _ code              -> COMMENT (text "CmmAssign" <+> parens (text (show reg)) <+> parens (text (show src))) `consOL` code dst
      Fixed format freg fcode -> COMMENT (text "CmmAssign" <+> parens (text (show reg)) <+> parens (text (show src))) `consOL` (fcode `snocOL` MOV (OpReg (formatToWidth format) dst) (OpReg (formatToWidth format) freg))

-- Let's treat Floating point stuff
-- as integer code for now. Opaque.
assignMem_FltCode = assignMem_IntCode
assignReg_FltCode = assignReg_IntCode

-- -----------------------------------------------------------------------------
-- Jumps
genJump :: CmmExpr{-the branch target-} -> [Reg] -> NatM InstrBlock
genJump expr@(CmmLit (CmmLabel lbl)) _regs
  = return $ unitOL (ANN (text $ show expr) (J (TLabel lbl)))
  -- = return (toOL [ PUSH_STACK_FRAME
  --               , DELTA (-16)
  --               , B (TLabel lbl)
  --               , POP_STACK_FRAME
  --               , DELTA 0] )

genJump expr _regs = do
    (target, _format, code) <- getSomeReg expr
    return (code `appOL` unitOL (ANN (text $ show expr) (J (TReg target)))
                        --  toOL [ PUSH_STACK_FRAME
                        --       , DELTA (-16)
                        --       , B (TReg target)
                        --       , POP_STACK_FRAME
                        --       , DELTA 0
                        --       ]
                              )

-- -----------------------------------------------------------------------------
--  Unconditional branches
genBranch :: BlockId -> NatM InstrBlock
genBranch = return . toOL . mkJumpInstr

-- -----------------------------------------------------------------------------
-- Conditional branches
genCondJump
    :: BlockId
    -> CmmExpr
    -> NatM InstrBlock
genCondJump bid expr = do
    case expr of
      -- Optimized == 0 case.
      CmmMachOp (MO_Eq w) [x, CmmLit (CmmInt 0 _)] -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        return $ code_x `snocOL` (ANN (text $ show expr) (CBZ (OpReg w reg_x) (TBlock bid)))

      -- Optimized /= 0 case.
      CmmMachOp (MO_Ne w) [x, CmmLit (CmmInt 0 _)] -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        return $ code_x `snocOL`  (ANN (text $ show expr) (CBNZ (OpReg w reg_x) (TBlock bid)))

      -- Generic case.
      CmmMachOp mop [x, y] -> do

        let bcond w cmp = do
              -- compute both sides.
              (reg_x, _format_x, code_x) <- getSomeReg x
              (reg_y, _format_y, code_y) <- getSomeReg y
              return $ code_x `appOL` code_y `snocOL` CMP (OpReg w reg_x) (OpReg w reg_y) `snocOL` (ANN (text $ show expr) (BCOND cmp (TBlock bid)))
            fbcond w cmp = do
              -- ensure we get float regs
              (reg_fx, _format_fx, code_fx) <- getFloatReg x
              (reg_fy, _format_fy, code_fy) <- getFloatReg y
              return $ code_fx `appOL` code_fy `snocOL` CMP (OpReg w reg_fx) (OpReg w reg_fy) `snocOL` (ANN (text $ show expr) (BCOND cmp (TBlock bid)))

        case mop of
          MO_F_Eq w -> fbcond w EQ
          MO_F_Ne w -> fbcond w NE

          MO_F_Gt w -> fbcond w OGT
          MO_F_Ge w -> fbcond w OGE
          MO_F_Lt w -> fbcond w OLT
          MO_F_Le w -> fbcond w OLE

          MO_Eq w   -> bcond w EQ
          MO_Ne w   -> bcond w NE

          MO_S_Gt w -> bcond w SGT
          MO_S_Ge w -> bcond w SGE
          MO_S_Lt w -> bcond w SLT
          MO_S_Le w -> bcond w SLE
          MO_U_Gt w -> bcond w UGT
          MO_U_Ge w -> bcond w UGE
          MO_U_Lt w -> bcond w ULT
          MO_U_Le w -> bcond w ULE
          _ -> pprPanic "AArch64.genCondJump:case mop: " (text $ show expr)
      _ -> pprPanic "AArch64.genCondJump: " (text $ show expr)


genCondBranch
    :: BlockId      -- the source of the jump
    -> BlockId      -- the true branch target
    -> BlockId      -- the false branch target
    -> CmmExpr      -- the condition on which to branch
    -> NatM InstrBlock -- Instructions

genCondBranch _ true false expr = do
  b1 <- genCondJump true expr
  b2 <- genBranch false
  return (b1 `appOL` b2)

-- -----------------------------------------------------------------------------
--  Generating C calls

-- Now the biggest nightmare---calls.  Most of the nastiness is buried in
-- @get_arg@, which moves the arguments to the correct registers/stack
-- locations.  Apart from that, the code is easy.
--
-- As per *convention*:
-- x0-x7:   (volatile) argument registers
-- x8:      (volatile) indirect result register / Linux syscall no
-- x9-x15:  (volatile) caller saved regs
-- x16,x17: (volatile) intra-procedure-call registers
-- x18:     (volatile) platform register. don't use for portability
-- x19-x28: (non-volatile) callee save regs
-- x29:     (non-volatile) frame pointer
-- x30:                    link register
-- x31:                    stack pointer / zero reg
--
-- Thus, this is what a c function will expect. Find the arguments in x0-x7,
-- anything above that on the stack.  We'll ignore c functions with more than
-- 8 arguments for now.  Sorry.
--
-- We need to make sure we preserve x9-x15, don't want to touch x16, x17.

-- Note [PLT vs GOT relocations]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- When linking objects together, we may need to lookup foreign references. That
-- is symbolic references to functions or values in other objects. When
-- compiling the object, we can not know where those elements will end up in
-- memory (relative to the current location). Thus the use of symbols. There
-- are two types of items we are interested, code segments we want to jump to
-- and continue execution there (functions, ...), and data items we want to look
-- up (strings, numbers, ...). For functions we can use the fact that we can use
-- an intermediate jump without visibility to the programs execution.  If we
-- want to jump to a function that is simply too far away to reach for the B/BL
-- instruction, we can create a small piece of code that loads the full target
-- address and jumps to that on demand. Say f wants to call g, however g is out
-- of range for a direct jump, we can create a function h in range for f, that
-- will load the address of g, and jump there. The area where we construct h
-- is called the Procedure Linking Table (PLT), we have essentially replaced
-- f -> g with f -> h -> g.  This is fine for function calls.  However if we
-- want to lookup values, this trick doesn't work, so we need something else.
-- We will instead reserve a slot in memory, and have a symbol pointing to that
-- slot. Now what we essentially do is, we reference that slot, and expect that
-- slot to hold the final resting address of the data we are interested in.
-- Thus what that symbol really points to is the location of the final data.
-- The block of memory where we hold all those slots is the Global Offset Table
-- (GOT).  Instead of x <- $foo, we now do y <- $fooPtr, and x <- [$y].
--
-- For JUMP/CALLs we have 26bits (+/- 128MB), for conditional branches we only
-- have 19bits (+/- 1MB).  Symbol lookups are also within +/- 1MB, thus for most
-- of the LOAD/STOREs we'd want to use adrp, and add to compute a value within
-- 4GB of the PC, and load that.  For anything outside of that range, we'd have
-- to go through the GOT.
--
--  adrp x0, <symbol>
--  add x0, :lo:<symbol>
--
-- will compute the address of <symbol> int x0 if <symbol> is within 4GB of the
-- PC.
--
-- If we want to get the slot in the global offset table (GOT), we can do this:
--
--   adrp x0, #:got:<symbol>
--   ldr x0, [x0, #:got_lo12:<symbol>]
--
-- this will compute the address anywhere in the addressable 64bit space into
-- x0, by loading the address from the GOT slot.
--
-- To actually get the value of <symbol>, we'd need to ldr x0, x0 still, which
-- for the first case can be optimized to use ldr x0, [x0, #:lo12:<symbol>]
-- instaed of the add instruction.
--
-- As the memory model for AArch64 for PIC is considered to be +/- 4GB, we do
-- not need to go through the GOT, unless we want to address the full address
-- range within 64bit.

genCCall
    :: ForeignTarget      -- function to call
    -> [CmmFormal]        -- where to put the result
    -> [CmmActual]        -- arguments (of mixed type)
    -> BlockId            -- The block we are in
    -> NatM (InstrBlock, Maybe BlockId)
-- XXX: Specialize where we can.
-- Generic impl
genCCall target dest_regs arg_regs bid = do
  -- we want to pass arg_regs into allArgRegs
  -- pprTraceM "genCCall target" (ppr target)
  -- pprTraceM "genCCall formal" (ppr dest_regs)
  -- pprTraceM "genCCall actual" (ppr arg_regs)

  case target of
    -- The target :: ForeignTarget call can either
    -- be a foreign procedure with an address expr
    -- and a calling convention.
    ForeignTarget expr _cconv -> do
      (call_target, call_target_code) <- case expr of
        -- if this is a label, let's just directly to it.  This will produce the
        -- correct CALL relocation for BL...
        (CmmLit (CmmLabel lbl)) -> pure (TLabel lbl, nilOL)
        -- ... if it's not a label--well--let's compute the expression into a
        -- register and jump to that. See Note [PLT vs GOT relocations]
        _ -> do (reg, _format, reg_code) <- getSomeReg expr
                pure (TReg reg, reg_code)
      -- compute the code and register logic for all arg_regs.
      -- this will give us the format information to match on.
      arg_regs' <- mapM getSomeReg arg_regs

      -- Now this is stupid.  Our Cmm expressions doesn't carry the proper sizes
      -- so while in Cmm we might get W64 incorrectly for an int, that is W32 in
      -- STG; this thenn breaks packing of stack arguments, if we need to pack
      -- for the pcs, e.g. darwinpcs.  Option one would be to fix the Int type
      -- in Cmm proper. Option two, which we choose here is to use extended Hint
      -- information to contain the size information and use that when packing
      -- arguments, spilled onto the stack.
      let (_res_hints, arg_hints) = foreignTargetHints target
          arg_regs'' = zipWith (\(r, f, c) h -> (r,f,h,c)) arg_regs' arg_hints

      platform <- getPlatform
      let packStack = platformOS platform == OSDarwin

      (stackSpace', passRegs, passArgumentsCode) <- passArguments packStack allGpArgRegs allFpArgRegs arg_regs'' 0 [] nilOL

      -- if we pack the stack, we may need to adjust to multiple of 8byte.
      -- if we don't pack the stack, it will always be multiple of 8.
      let stackSpace = if stackSpace' `mod` 8 /= 0
                       then 8 * (stackSpace' `div` 8 + 1)
                       else stackSpace'

      (returnRegs, readResultsCode)   <- readResults allGpArgRegs allFpArgRegs dest_regs [] nilOL

      let moveStackDown 0 = toOL [ PUSH_STACK_FRAME
                                 , DELTA (-16) ]
          moveStackDown i | odd i = moveStackDown (i + 1)
          moveStackDown i = toOL [ PUSH_STACK_FRAME
                                 , SUB (OpReg W64 (regSingle 31)) (OpReg W64 (regSingle 31)) (OpImm (ImmInt (8 * i)))
                                 , DELTA (-8 * i - 16) ]
          moveStackUp 0 = toOL [ POP_STACK_FRAME
                               , DELTA 0 ]
          moveStackUp i | odd i = moveStackUp (i + 1)
          moveStackUp i = toOL [ ADD (OpReg W64 (regSingle 31)) (OpReg W64 (regSingle 31)) (OpImm (ImmInt (8 * i)))
                               , POP_STACK_FRAME
                               , DELTA 0 ]

      let code =    call_target_code          -- compute the label (possibly into a register)
            `appOL` moveStackDown (stackSpace `div` 8)
            `appOL` passArgumentsCode         -- put the arguments into x0, ...
            `appOL` (unitOL $ BL call_target passRegs returnRegs) -- branch and link.
            `appOL` readResultsCode           -- parse the results into registers
            `appOL` moveStackUp (stackSpace `div` 8)
      return (code, Nothing)

    -- or a possibly side-effecting machine operation
    -- mop :: CallishMachOp (see GHC.Cmm.MachOp)
    PrimTarget mop -> do
      -- We'll need config to construct forien targets
      case mop of
        -- 64 bit float ops
        MO_F64_Pwr   -> mkCCall "pow"

        MO_F64_Sin   -> mkCCall "sin"
        MO_F64_Cos   -> mkCCall "cos"
        MO_F64_Tan   -> mkCCall "tan"

        MO_F64_Sinh  -> mkCCall "sinh"
        MO_F64_Cosh  -> mkCCall "cosh"
        MO_F64_Tanh  -> mkCCall "tanh"

        MO_F64_Asin  -> mkCCall "asin"
        MO_F64_Acos  -> mkCCall "acos"
        MO_F64_Atan  -> mkCCall "atan"

        MO_F64_Asinh -> mkCCall "asinh"
        MO_F64_Acosh -> mkCCall "acosh"
        MO_F64_Atanh -> mkCCall "atanh"

        MO_F64_Log   -> mkCCall "log"
        MO_F64_Log1P -> mkCCall "log1p"
        MO_F64_Exp   -> mkCCall "exp"
        MO_F64_ExpM1 -> mkCCall "expm1"
        MO_F64_Fabs  -> mkCCall "fabs"
        MO_F64_Sqrt  -> mkCCall "sqrt"

        -- 32 bit float ops
        MO_F32_Pwr   -> mkCCall "powf"

        MO_F32_Sin   -> mkCCall "sinf"
        MO_F32_Cos   -> mkCCall "cosf"
        MO_F32_Tan   -> mkCCall "tanf"
        MO_F32_Sinh  -> mkCCall "sinhf"
        MO_F32_Cosh  -> mkCCall "coshf"
        MO_F32_Tanh  -> mkCCall "tanhf"
        MO_F32_Asin  -> mkCCall "asinf"
        MO_F32_Acos  -> mkCCall "acosf"
        MO_F32_Atan  -> mkCCall "atanf"
        MO_F32_Asinh -> mkCCall "asinhf"
        MO_F32_Acosh -> mkCCall "acoshf"
        MO_F32_Atanh -> mkCCall "atanhf"
        MO_F32_Log   -> mkCCall "logf"
        MO_F32_Log1P -> mkCCall "log1pf"
        MO_F32_Exp   -> mkCCall "expf"
        MO_F32_ExpM1 -> mkCCall "expm1f"
        MO_F32_Fabs  -> mkCCall "fasbf"
        MO_F32_Sqrt  -> mkCCall "sqrtf"

        -- Conversion
        MO_UF_Conv w        -> mkCCall (word2FloatLabel w)

        -- Arithmatic
        -- These are not supported on X86, so I doubt they are used much.
        MO_S_Mul2     _w -> unsupported mop
        MO_S_QuotRem  _w -> unsupported mop
        MO_U_QuotRem  _w -> unsupported mop
        MO_U_QuotRem2 _w -> unsupported mop
        MO_Add2       _w -> unsupported mop
        MO_AddWordC   _w -> unsupported mop
        MO_SubWordC   _w -> unsupported mop
        MO_AddIntC    _w -> unsupported mop
        MO_SubIntC    _w -> unsupported mop
        MO_U_Mul2     _w -> unsupported mop

        -- Memory Ordering
        -- XXX DMBSY is probably *way* too much!
        MO_ReadBarrier      ->  return (unitOL DMBSY, Nothing)
        MO_WriteBarrier     ->  return (unitOL DMBSY, Nothing)
        MO_Touch            ->  return (nilOL, Nothing) -- Keep variables live (when using interior pointers)
        -- Prefetch
        MO_Prefetch_Data _n -> return (nilOL, Nothing) -- Prefetch hint.

        -- Memory copy/set/move/cmp, with alignment for optimization

        -- XXX Optimize and use e.g. quad registers to move memory around instead
        -- of offloading this to memcpy. For small memcpys we can utilize
        -- the 128bit quad registers in NEON to move block of bytes around.
        -- Might also make sense of small memsets? Use xzr? What's the function
        -- call overhead?
        MO_Memcpy  _align   -> mkCCall "memcpy"
        MO_Memset  _align   -> mkCCall "memset"
        MO_Memmove _align   -> mkCCall "memmove"
        MO_Memcmp  _align   -> mkCCall "memcmp"

        MO_PopCnt w         -> mkCCall (popCntLabel w)
        MO_Pdep w           -> mkCCall (pdepLabel w)
        MO_Pext w           -> mkCCall (pextLabel w)
        MO_Clz w            -> mkCCall (clzLabel w)
        MO_Ctz w            -> mkCCall (ctzLabel w)
        MO_BSwap w          -> mkCCall (bSwapLabel w)
        MO_BRev w           -> mkCCall (bRevLabel w)

        -- -- Atomic read-modify-write.
        MO_AtomicRMW w amop -> mkCCall (atomicRMWLabel w amop)
        MO_AtomicRead w     -> mkCCall (atomicReadLabel w)
        MO_AtomicWrite w    -> mkCCall (atomicWriteLabel w)
        MO_Cmpxchg w        -> mkCCall (cmpxchgLabel w)
        -- -- Should be an AtomicRMW variant eventually.
        -- -- Sequential consistent.
        -- XXX: this should be implemented properly!
        MO_Xchg w           -> mkCCall (xchgLabel w)

  where
    unsupported :: Show a => a -> b
    unsupported mop = panic ("outOfLineCmmOp: " ++ show mop
                          ++ " not supported here")
    mkCCall :: String -> NatM (InstrBlock, Maybe BlockId)
    mkCCall name = do
      config <- getConfig
      target <- cmmMakeDynamicReference config CallReference $
          mkForeignLabel (fsLit name) Nothing ForeignLabelInThisPackage IsFunction
      let cconv = ForeignConvention CCallConv [NoHint] [NoHint] CmmMayReturn
      genCCall (ForeignTarget target cconv) dest_regs arg_regs bid

    -- XXX: Optimize using paired load LDP
    passArguments :: Bool -> [Reg] -> [Reg] -> [(Reg, Format, ForeignHint, InstrBlock)] -> Int -> [Reg] -> InstrBlock -> NatM (Int, [Reg], InstrBlock)
    passArguments _packStack _ _ [] stackSpace accumRegs accumCode = return (stackSpace, accumRegs, accumCode)
    -- passArguments _ _ [] accumCode stackSpace | isEven stackSpace = return $ SUM (OpReg W64 x31) (OpReg W64 x31) OpImm (ImmInt (-8 * stackSpace))
    -- passArguments _ _ [] accumCode stackSpace = return $ SUM (OpReg W64 x31) (OpReg W64 x31) OpImm (ImmInt (-8 * (stackSpace + 1)))
    -- passArguments [] fpRegs (arg0:arg1:args) stack accumCode = do
    --   -- allocate this on the stack
    --   (r0, format0, code_r0) <- getSomeReg arg0
    --   (r1, format1, code_r1) <- getSomeReg arg1
    --   let w0 = formatToWidth format0
    --       w1 = formatToWidth format1
    --       stackCode = unitOL $ STP (OpReg w0 r0) (OpReg w1 R1), (OpAddr (AddrRegImm x31 (ImmInt (stackSpace * 8)))
    --   passArguments gpRegs (fpReg:fpRegs) args (stackCode `appOL` accumCode)

      -- float promotion.
      -- According to
      --  ISO/IEC 9899:2018
      --  Information technology — Programming languages — C
      --
      -- e.g.
      -- http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1124.pdf
      -- http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf
      --
      -- GHC would need to know the prototype.
      --
      -- > If the expression that denotes the called function has a type that does not include a
      -- > prototype, the integer promotions are performed on each argument, and arguments that
      -- > have type float are promoted to double.
      --
      -- As we have no way to get prototypes for C yet, we'll *not* promote this
      -- which is in line with the x86_64 backend :(
      --
      -- See the encode_values.cmm test.
      --
      -- We would essentially need to insert an FCVT (OpReg W64 fpReg) (OpReg W32 fpReg)
      -- if w == W32.  But *only* if we don't have a prototype m(
      --
      -- For AArch64 specificies see: https://developer.arm.com/docs/ihi0055/latest/procedure-call-standard-for-the-arm-64-bit-architecture
      --
    -- Still have GP regs, and we want to pass an GP argument.
    passArguments pack (gpReg:gpRegs) fpRegs ((r, format, _hint, code_r):args) stackSpace accumRegs accumCode | isIntFormat format = do
      let w = formatToWidth format
      passArguments pack gpRegs fpRegs args stackSpace (gpReg:accumRegs) (accumCode `appOL` code_r `snocOL` (ANN (text $ "Pass gp argument: " ++ show r) $ MOV (OpReg w gpReg) (OpReg w r)))

    -- Still have FP regs, and we want to pass an FP argument.
    passArguments pack gpRegs (fpReg:fpRegs) ((r, format, _hint, code_r):args) stackSpace accumRegs accumCode | isFloatFormat format = do
      let w = formatToWidth format
      passArguments pack gpRegs fpRegs args stackSpace (fpReg:accumRegs) (accumCode `appOL` code_r `snocOL` (ANN (text $ "Pass fp argument: " ++ show r) $ MOV (OpReg w fpReg) (OpReg w r)))

    -- No mor regs left to pass. Must pass on stack.
    passArguments pack [] [] ((r, format, _hint, code_r):args) stackSpace accumRegs accumCode = do
      let w = formatToWidth format
          bytes = widthInBits w `div` 8
          space = if pack then bytes else 8
          stackCode = code_r `snocOL` (ANN (text $ "Pass argument (size " ++ show w ++ ") on the stack: " ++ show r) $ STR format (OpReg w r) (OpAddr (AddrRegImm (regSingle 31) (ImmInt stackSpace))))
      passArguments pack [] [] args (stackSpace+space) accumRegs (stackCode `appOL` accumCode)

    -- Still have fpRegs left, but want to pass a GP argument. Must be passed on the stack then.
    passArguments pack [] fpRegs ((r, format, _hint, code_r):args) stackSpace accumRegs accumCode | isIntFormat format = do
      let w = formatToWidth format
          bytes = widthInBits w `div` 8
          space = if pack then bytes else 8
          stackCode = code_r `snocOL` (ANN (text $ "Pass argument (size " ++ show w ++ ") on the stack: " ++ show r) $ STR format (OpReg w r) (OpAddr (AddrRegImm (regSingle 31) (ImmInt stackSpace))))
      passArguments pack [] fpRegs args (stackSpace+space) accumRegs (stackCode `appOL` accumCode)

    -- Still have gpRegs left, but want to pass a FP argument. Must be passed on the stack then.
    passArguments pack gpRegs [] ((r, format, _hint, code_r):args) stackSpace accumRegs accumCode | isFloatFormat format = do
      let w = formatToWidth format
          bytes = widthInBits w `div` 8
          space = if pack then bytes else 8
          stackCode = code_r `snocOL` (ANN (text $ "Pass argument (size " ++ show w ++ ") on the stack: " ++ show r) $ STR format (OpReg w r) (OpAddr (AddrRegImm (regSingle 31) (ImmInt stackSpace))))
      passArguments pack gpRegs [] args (stackSpace+space) accumRegs (stackCode `appOL` accumCode)

    passArguments _ _ _ _ _ _ _ = pprPanic "passArguments" (text "invalid state")

    readResults :: [Reg] -> [Reg] -> [LocalReg] -> [Reg]-> InstrBlock -> NatM ([Reg], InstrBlock)
    readResults _ _ [] accumRegs accumCode = return (accumRegs, accumCode)
    readResults [] _ _ _ _ = do
      platform <- getPlatform
      pprPanic "genCCall, out of gp registers when reading results" (pdoc platform target)
    readResults _ [] _ _ _ = do
      platform <- getPlatform
      pprPanic "genCCall, out of fp registers when reading results" (pdoc platform target)
    readResults (gpReg:gpRegs) (fpReg:fpRegs) (dst:dsts) accumRegs accumCode = do
      -- gp/fp reg -> dst
      platform <- getPlatform
      let rep = cmmRegType platform (CmmLocal dst)
          format = cmmTypeFormat rep
          w   = cmmRegWidth platform (CmmLocal dst)
          r_dst = getRegisterReg platform (CmmLocal dst)
      if isFloatFormat format
        then readResults (gpReg:gpRegs) fpRegs dsts (fpReg:accumRegs) (accumCode `snocOL` MOV (OpReg w r_dst) (OpReg w fpReg))
        else readResults gpRegs (fpReg:fpRegs) dsts (gpReg:accumRegs) (accumCode `snocOL` MOV (OpReg w r_dst) (OpReg w gpReg))



    -- XXX: This is automomatically done by the allocator for us.
    -- XXX However it's not optimal, as we'd rather want to have control over
    --     all spill/load registers, so we can optimize with instructions like
    --       STP xA, xB, [sp, #-16]!
    --     and
    --       LDP xA, xB, sp, #16
    --
    -- -- Thus we need to save r0-r18, v0-v7, v16-v31 over a C call.
    -- -- Therefore we need (19/2 = 10 + 8/2 = 4 + 16/2 = 8) * 16 = 22 * 16 = 352 bytes of stack to save them all.
    -- -- If we knew which of our registers were actually active, we could cut this down dramatically.
    -- --
    -- -- XXX: this could be done with half the instructions using stp (store pair)
    -- saveRegsCode :: InstrBlock
    -- saveRegsCode = toOL $
    --   [ SUB (OpReg W64 sp) (OpReg W64 sp) (OpImm (ImmInt 352)) ]
    --   ++ map (\i -> STR II64 (_x i) (OpAddr (AddrRegImm sp (ImmInt (i * 8))))) [0..18]
    --   ++ map (\i -> STR II64 (_d (32+i)) (OpAddr (AddrRegImm sp (ImmInt ((20+i)*8))))) [0..7]
    --   ++ map (\i -> STR II64 (_d (32+i)) (OpAddr (AddrRegImm sp (ImmInt ((28+i-8)*8))))) [16..31]
    -- -- Restore saved registers
    -- restoreRegsCode :: InstrBlock
    -- restoreRegsCode = toOL $
    --   map (\i -> LDR FF64 (_d (32+i)) (OpAddr (AddrRegImm sp (ImmInt ((28+i-8)*8))))) (reverse [16..31])
    --   ++ map (\i -> LDR FF64 (_d (32+i)) (OpAddr (AddrRegImm sp (ImmInt ((20+i)*8))))) (reverse [0..7])
    --   ++ map (\i -> LDR FF64 (_x i) (OpAddr (AddrRegImm sp (ImmInt (i * 8))))) (reverse [0..18])
    --   ++ [ ADD (OpReg W64 sp) (OpReg W64 sp) (OpImm (ImmInt 352)) ]
