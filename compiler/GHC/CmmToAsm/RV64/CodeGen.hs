{-# language GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
module GHC.CmmToAsm.RV64.CodeGen (
      cmmTopCodeGen
    , generateJumpTableForInstr
)

where

-- NCG stuff:
import GHC.Prelude hiding (EQ)

import Data.Word

import GHC.Platform.Regs
import GHC.CmmToAsm.RV64.Instr
import GHC.CmmToAsm.RV64.Regs
import GHC.CmmToAsm.RV64.Cond

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

import Control.Monad    ( mapAndUnzipM, foldM )
import Data.Maybe
import GHC.Float

import GHC.Types.Basic
import GHC.Types.ForeignCall
import GHC.Data.FastString
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Constants (debugIsOn)

-- Note [General layout of an NCG]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  -- do
  --   traceM $ "-- -------------------------- cmmTopGen (CmmProc) -------------------------- --\n"
  --         ++ showSDocUnsafe (ppr cmm)

  let blocks = toBlockListEntryFirst graph
  (nat_blocks,statics) <- mapAndUnzipM basicBlockCodeGen blocks
  picBaseMb <- getPicBaseMaybeNat

  let proc = CmmProc info lab live (ListGraph $ concat nat_blocks)
      tops = proc : concat statics

  case picBaseMb of
      Just _picBase -> panic "RV64.cmmTopCodeGen: picBase not implemented"
      Nothing -> return tops

-- ... or CmmData.
cmmTopCodeGen _cmm@(CmmData sec dat) = do
  -- do
  --   traceM $ "-- -------------------------- cmmTopGen (CmmData) -------------------------- --\n"
  --         ++ showSDocUnsafe (ppr cmm)
  return [CmmData sec dat] -- no translation, we just use CmmStatic

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

      header_comment_instr | debugIsOn = unitOL $ MULTILINE_COMMENT (
          text "-- --------------------------- basicBlockCodeGen --------------------------- --\n"
          $+$ withPprStyle defaultDumpStyle (pdoc (ncgPlatform config) block)
          )
                           | otherwise = nilOL
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
  -- TODO: Then x86 backend run @verifyBasicBlock@ here and inserts
  --      unwinding info. See Ticket 19913
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
-- | Utilities
ann :: SDoc -> Instr -> Instr
ann doc instr {- debugIsOn -} = ANN doc instr
-- ann _ instr = instr
{-# INLINE ann #-}

-- Using pprExpr will hide the AST, @ANN@ will end up in the assembly with
-- -dppr-debug.  The idea is that we can trivially see how a cmm expression
-- ended up producing the assembly we see.  By having the verbatim AST printed
-- we can simply check the patterns that were matched to arrive at the assembly
-- we generated.
--
-- pprExpr will hide a lot of noise of the underlying data structure and print
-- the expression into something that can be easily read by a human. However
-- going back to the exact CmmExpr representation can be laborious and adds
-- indirections to find the matches that lead to the assembly.
--
-- An improvement oculd be to have
--
--    (pprExpr genericPlatform e) <> parens (text. show e)
--
-- to have the best of both worlds.
--
-- Note: debugIsOn is too restrictive, it only works for debug compilers.
-- However, we do not only want to inspect this for debug compilers. Ideally
-- we'd have a check for -dppr-debug here already, such that we don't even
-- generate the ANN expressions. However, as they are lazy, they shouldn't be
-- forced until we actually force them, and without -dppr-debug they should
-- never end up being forced.
annExpr :: CmmExpr -> Instr -> Instr
annExpr e {- debugIsOn -} = ANN (text . show $ e)
-- annExpr e instr {- debugIsOn -} = ANN (pprExpr genericPlatform e) instr
-- annExpr _ instr = instr
{-# INLINE annExpr #-}

-- -----------------------------------------------------------------------------
-- Generating a table-branch

-- TODO jump tables would be a lot faster, but we'll use bare bones for now.
-- this is usually done by sticking the jump table ids into an instruction
-- and then have the @generateJumpTableForInstr@ callback produce the jump
-- table as a static.
--
-- See Ticket 19912
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
                 toOL [ BCOND EQ (OpReg w reg) (OpReg w keyReg) (TBlock bid)
                      ] `appOL` acc
      def_code = case switchTargetsDefault targets of
        Just bid -> unitOL (B (TBlock bid))
        Nothing  -> nilOL

  switch_code <- foldM mkbranch nilOL (switchTargetsCases targets)
  return $ code `appOL` switch_code `appOL` def_code

-- We don't do jump tables for now, see Ticket 19912
generateJumpTableForInstr :: NCGConfig -> Instr
  -> Maybe (NatCmmDecl RawCmmStatics Instr)
generateJumpTableForInstr _ _ = Nothing

-- -----------------------------------------------------------------------------
-- Top-level of the instruction selector

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

      CmmStore addr src _alignment
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

      CmmCall { cml_target = arg } -> genJump arg

      CmmUnwind _regs -> return nilOL

      _ -> pprPanic "stmtToInstrs: statement should have been cps'd away" (pdoc platform stmt)

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

getRegisterReg platform (CmmGlobal mid)
  = case globalRegMaybe platform mid of
        Just reg -> RegReal reg
        Nothing  -> pprPanic "getRegisterReg-memory" (ppr $ CmmGlobal mid)
        -- By this stage, the only MagicIds remaining should be the
        -- ones which map to a real machine register on this
        -- platform.  Hence if it's not mapped to a registers something
        -- went wrong earlier in the pipeline.
-- | Convert a BlockId to some CmmStatic data
-- TODO: Add JumpTable Logic, see Ticket 19912
-- jumpTableEntry :: NCGConfig -> Maybe BlockId -> CmmStatic
-- jumpTableEntry config Nothing   = CmmStaticLit (CmmInt 0 (ncgWordWidth config))
-- jumpTableEntry _ (Just blockid) = CmmStaticLit (CmmLabel blockLabel)
--     where blockLabel = blockLbl blockid

-- -----------------------------------------------------------------------------
-- General things for putting together code sequences

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

-- TODO OPT: we might be able give getRegister
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

-- TODO: TODO, bounds. We can't put any immediate
-- value in. They are constrained.
-- See Ticket 19911
litToImm' :: CmmLit -> NatM (Operand, InstrBlock)
litToImm' lit = return (OpImm (litToImm lit), nilOL)

getRegister :: CmmExpr -> NatM Register
getRegister e = do
  config <- getConfig
  getRegister' config (ncgPlatform config) e

-- | The register width to be used for an operation on the given width
-- operand.
opRegWidth :: Width -> Width
opRegWidth W64 = W64  -- x
opRegWidth W32 = W32  -- w
opRegWidth W16 = W32  -- w
opRegWidth W8  = W32  -- w
opRegWidth w   = pprPanic "opRegWidth" (text "Unsupported width" <+> ppr w)

-- Note [Signed arithmetic on RISCV64]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Handling signed arithmetic on sub-word-size values on RISCV64 is a bit
-- tricky as Cmm's type system does not capture signedness. While 32-bit values
-- are fairly easy to handle due to RISCV64's 32-bit instruction variants
-- (denoted by use of %wN registers), 16- and 8-bit values require quite some
-- care.
--
-- We handle 16-and 8-bit values by using the 32-bit operations and
-- sign-/zero-extending operands and truncate results as necessary. For
-- simplicity we maintain the invariant that a register containing a
-- sub-word-size value always contains the zero-extended form of that value
-- in between operations.
--
-- For instance, consider the program,
--
--    test(bits64 buffer)
--      bits8 a = bits8[buffer];
--      bits8 b = %mul(a, 42);
--      bits8 c = %not(b);
--      bits8 d = %shrl(c, 4::bits8);
--      return (d);
--    }
--
-- This program begins by loading `a` from memory, for which we use a
-- zero-extended byte-size load.  We next sign-extend `a` to 32-bits, and use a
-- 32-bit multiplication to compute `b`, and truncate the result back down to
-- 8-bits.
--
-- Next we compute `c`: The `%not` requires no extension of its operands, but
-- we must still truncate the result back down to 8-bits. Finally the `%shrl`
-- requires no extension and no truncate since we can assume that
-- `c` is zero-extended.
--
-- The "RISC-V Sign Extension Optimizations" LLVM tech talk presentation by
-- Craig Topper covers possible future improvements
-- (https://llvm.org/devmtg/2022-11/slides/TechTalk21-RISC-VSignExtensionOptimizations.pdf)
--
-- TODO:
--   Don't use Width in Operands
--   Instructions should rather carry a RegWidth
--
-- Note [Handling PIC on RV64]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- RV64 does not have a special PIC register, the general approach is to
-- simply go through the GOT, and there is assembly support for this:
--
-- rv64 assembly has a `la` (load address) pseudo-instruction, that allows
-- loading a label, ... into a register.  The instruction is desugared into
--
-- 1: lui  rd1, %pcrel_hi(label)
--    addi rd1, %pcrel_lo(1b)
--
-- See https://sourceware.org/binutils/docs/as/RISC_002dV_002dModifiers.html,
-- PIC can be enabled/disabled through
--
--  .option pic
--
-- See https://sourceware.org/binutils/docs/as/RISC_002dV_002dDirectives.html#RISC_002dV_002dDirectives
--
-- CmmGlobal @PicBaseReg@'s are generated in @GHC.CmmToAsm.PIC@ in the
-- @cmmMakePicReference@.  This is in turn called from @cmmMakeDynamicReference@
-- also in @Cmm.CmmToAsm.PIC@ from where it is also exported.  There are two
-- callsites for this. One is in this module to produce the @target@ in @genCCall@
-- the other is in @GHC.CmmToAsm@ in @cmmExprNative@.
--
-- Conceptually we do not want any special PicBaseReg to be used on RV64. If
-- we want to distinguish between symbol loading, we need to address this through
-- the way we load it, not through a register.
--

getRegister' :: NCGConfig -> Platform -> CmmExpr -> NatM Register
-- OPTIMIZATION WARNING: CmmExpr rewrites
-- 1. Rewrite: Reg + (-n) => Reg - n
--    TODO: this expression shouldn't even be generated to begin with.
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

        CmmInt 0 w -> pure $ Fixed (intFormat w) zero_reg nilOL

        CmmInt i W8 | i >= 0 -> do
          return (Any (intFormat W8) (\dst -> unitOL $ annExpr expr (MOV (OpReg W8 dst) (OpImm (ImmInteger (narrowU W8 i))))))
        CmmInt i W16 | i >= 0 -> do
          return (Any (intFormat W16) (\dst -> unitOL $ annExpr expr (MOV (OpReg W16 dst) (OpImm (ImmInteger (narrowU W16 i))))))

        CmmInt i W8  -> do
          return (Any (intFormat W8) (\dst -> unitOL $ annExpr expr (MOV (OpReg W8 dst) (OpImm (ImmInteger (narrowU W8 i))))))
        CmmInt i W16 -> do
          return (Any (intFormat W16) (\dst -> unitOL $ annExpr expr (MOV (OpReg W16 dst) (OpImm (ImmInteger (narrowU W16 i))))))

        -- We need to be careful to not shorten this for negative literals.
        -- Those need the upper bits set. We'd either have to explicitly sign
        -- or figure out something smarter. Lowered to
        -- `MOV dst XZR`
        CmmInt i w -> do
          return (Any (intFormat w) (\dst -> unitOL $ annExpr expr (MOV (OpReg w dst) (OpImm (ImmInteger i)))))

        CmmInt _i rep -> do
          (op, imm_code) <- litToImm' lit
          return (Any (intFormat rep) (\dst -> imm_code `snocOL` annExpr expr (MOV (OpReg rep dst) op)))

        -- floatToBytes (fromRational f)
        CmmFloat 0 w   -> do
          (op, imm_code) <- litToImm' lit
          return (Any (floatFormat w) (\dst -> imm_code `snocOL` annExpr expr (MOV (OpReg w dst) op)))

        CmmFloat _f W8  -> pprPanic "getRegister' (CmmLit:CmmFloat), no support for bytes" (pdoc plat expr)
        CmmFloat _f W16 -> pprPanic "getRegister' (CmmLit:CmmFloat), no support for halfs" (pdoc plat expr)
        CmmFloat f W32 -> do
          let word = castFloatToWord32 (fromRational f) :: Word32
          tmp <- getNewRegNat (intFormat W32)
          return (Any (floatFormat W32) (\dst -> toOL [ annExpr expr
                                                      $ MOV (OpReg W32 tmp) (OpImm (ImmInteger (fromIntegral word)))
                                                      , MOV (OpReg W32 dst) (OpReg W32 tmp)
                                                      ]))
        CmmFloat f W64 -> do
          let word = castDoubleToWord64 (fromRational f) :: Word64
          tmp <- getNewRegNat (intFormat W64)
          return (Any (floatFormat W64) (\dst -> toOL [ annExpr expr
                                                      $ MOV (OpReg W64 tmp) (OpImm (ImmInteger (fromIntegral word)))
                                                      , MOV (OpReg W64 dst) (OpReg W64 tmp)
                                                      ]))
        CmmFloat _f _w -> pprPanic "getRegister' (CmmLit:CmmFloat), unsupported float lit" (pdoc plat expr)
        CmmVec _ -> pprPanic "getRegister' (CmmLit:CmmVec): " (pdoc plat expr)
        CmmLabel _lbl -> do
          (op, imm_code) <- litToImm' lit
          let rep = cmmLitType plat lit
              format = cmmTypeFormat rep
          return (Any format (\dst -> imm_code `snocOL` (annExpr expr $ LDR format (OpReg (formatToWidth format) dst) op)))

        CmmLabelOff _lbl off | isNbitEncodeable 12 (fromIntegral off) -> do
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
    CmmLoad mem rep _ -> do
      Amode addr addr_code <- getAmode plat (typeWidth rep) mem
      let format = cmmTypeFormat rep
      return (Any format (\dst -> addr_code `snocOL` LDR format (OpReg (formatToWidth format) dst) (OpAddr addr)))
    CmmStackSlot _ _
      -> pprPanic "getRegister' (CmmStackSlot): " (pdoc plat expr)
    CmmReg reg
      -> return (Fixed (cmmTypeFormat (cmmRegType plat reg))
                       (getRegisterReg plat reg)
                       nilOL)
    CmmRegOff reg off | isNbitEncodeable 12 (fromIntegral off) -> do
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
        MO_Not w -> return $ Any (intFormat w) $ \dst ->
            let w' = opRegWidth w
             in code `snocOL`
                -- pseudo instruction `not` is `xori rd, rs, -1`
                ann (text "not") (XORI (OpReg w' dst) (OpReg w' reg) (OpImm (ImmInt (-1)))) `appOL`
                truncateReg w' w dst -- See Note [Signed arithmetic on RISCV64]

        MO_S_Neg w -> negate code w reg
        MO_F_Neg w -> return $ Any (floatFormat w) (\dst -> code `snocOL` NEG (OpReg w dst) (OpReg w reg))

        MO_SF_Conv from to -> return $ Any (floatFormat to) (\dst -> code `snocOL` SCVTF (OpReg to dst) (OpReg from reg))  -- (Signed ConVerT Float)
        MO_FS_Conv from to -> return $ Any (intFormat to) (\dst -> code `snocOL` FCVTZS (OpReg to dst) (OpReg from reg)) -- (float convert (-> zero) signed)

        -- TODO this is very slow. We effectively use store + load (byte, half, word, double)
        --      for this in memory.
        MO_UU_Conv from to -> return $ Any (intFormat to) (\dst ->
          code `appOL` toOL [ SUB sp sp (OpImm (ImmInt 8))
                             , STR (intFormat from) (OpReg from reg) (OpAddr (AddrRegImm sp_reg (ImmInt 0)))
                             , LDR (intFormat to)   (OpReg to dst)   (OpAddr (AddrRegImm sp_reg (ImmInt 0)))
                             , ADD sp sp (OpImm (ImmInt 8))
                             ])
        -- MO_UU_Conv from to -> return $ Any (intFormat to) (\dst -> code `snocOL` UBFM (OpReg (max from to) dst) (OpReg (max from to) reg) (OpImm (ImmInt 0)) (toImm (min from to)))
        MO_SS_Conv from to -> ss_conv from to reg code
        MO_FF_Conv from to -> return $ Any (floatFormat to) (\dst -> code `snocOL` FCVT (OpReg to dst) (OpReg from reg))

        -- Conversions
        MO_XX_Conv _from to -> swizzleRegisterRep (intFormat to) <$> getRegister e

        _ -> pprPanic "getRegister' (monadic CmmMachOp):" (pdoc plat expr)
      where
        toImm W8 =  (OpImm (ImmInt 7))
        toImm W16 = (OpImm (ImmInt 15))
        toImm W32 = (OpImm (ImmInt 31))
        toImm W64 = (OpImm (ImmInt 63))
        toImm W128 = (OpImm (ImmInt 127))
        toImm W256 = (OpImm (ImmInt 255))
        toImm W512 = (OpImm (ImmInt 511))

        -- In the case of 16- or 8-bit values we need to sign-extend to 32-bits
        -- See Note [Signed arithmetic on AArch64].
        negate code w reg = do
            let w' = opRegWidth w
            (reg', code_sx) <- signExtendReg w w' reg
            return $ Any (intFormat w) $ \dst ->
                code `appOL`
                code_sx `snocOL`
                NEG (OpReg w' dst) (OpReg w' reg') `appOL`
                truncateReg w' w dst

        ss_conv from to reg code | from == to =
          pure $ Any (intFormat from) $ \dst ->
            code `snocOL` (MOV (OpReg from dst) (OpReg from reg))
        ss_conv from to reg code | from < to = do
          pure $ Any (intFormat to) $ \dst ->
            code
            `appOL` signExtend from to reg dst
            `appOL` truncateReg from to dst
        ss_conv from to reg code | from > to =
          pure $ Any (intFormat to) $ \dst ->
            code
              `appOL` toOL
                [ ann
                    (text "narrow register signed" <+> ppr reg <+> ppr from <> text "->" <> ppr to)
                    (LSL (OpReg to dst) (OpReg from reg) (OpImm (ImmInt shift))),
                  -- signed right shift
                  ASR (OpReg to dst) (OpReg to dst) (OpImm (ImmInt shift))
                ]
              `appOL` truncateReg from to dst
                  where
                    -- Why -1? We need to shift out one more bit for the sign.
                    shift = 64 - (widthInBits from - widthInBits to - 1)

    -- Dyadic machops:
    --
    -- The general idea is:
    -- compute x<i> <- x
    -- compute x<j> <- y
    -- OP x<r>, x<i>, x<j>
    --
    -- TODO: for now we'll only implement the 64bit versions. And rely on the
    --      fallthrough to alert us if things go wrong!
    -- OPTIMIZATION WARNING: Dyadic CmmMachOp destructuring
    -- 0. TODO This should not exist! Rewrite: Reg +- 0 -> Reg
    CmmMachOp (MO_Add _) [expr'@(CmmReg (CmmGlobal _r)), CmmLit (CmmInt 0 _)] -> getRegister' config plat expr'
    CmmMachOp (MO_Sub _) [expr'@(CmmReg (CmmGlobal _r)), CmmLit (CmmInt 0 _)] -> getRegister' config plat expr'
    -- 1. Compute Reg +/- n directly.
    --    For Add/Sub we can directly encode 12bits, or 12bits lsl #12.
    CmmMachOp (MO_Add w) [(CmmReg reg), CmmLit (CmmInt n _)]
      | n > 0 && n < 4096 -> return $ Any (intFormat w) (\d -> unitOL $ annExpr expr (ADD (OpReg w d) (OpReg w' r') (OpImm (ImmInteger n))))
      -- TODO: 12bits lsl #12; e.g. lower 12 bits of n are 0; shift n >> 12, and set lsl to #12.
      where w' = formatToWidth (cmmTypeFormat (cmmRegType plat reg))
            r' = getRegisterReg plat reg
    CmmMachOp (MO_Sub w) [(CmmReg reg), CmmLit (CmmInt n _)]
      | n > 0 && n < 4096 -> return $ Any (intFormat w) (\d -> unitOL $ annExpr expr (SUB (OpReg w d) (OpReg w' r') (OpImm (ImmInteger n))))
      -- TODO: 12bits lsl #12; e.g. lower 12 bits of n are 0; shift n >> 12, and set lsl to #12.
      where w' = formatToWidth (cmmTypeFormat (cmmRegType plat reg))
            r' = getRegisterReg plat reg

    CmmMachOp (MO_U_Quot w) [x, y] | w == W8 || w == W16  -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      (reg_y, format_y, code_y) <- getSomeReg y
      return $ Any (intFormat w) (\dst -> code_x `appOL`
                                          truncateReg (formatToWidth format_x) w reg_x `appOL`
                                          code_y `appOL`
                                          truncateReg (formatToWidth format_y) w reg_y `snocOL`
                                          annExpr expr (DIVU (OpReg w dst) (OpReg w reg_x) (OpReg w reg_y)))

    -- 2. Shifts. x << n, x >> n.
    CmmMachOp (MO_Shl w) [x, (CmmLit (CmmInt n _))] | w == W32, 0 <= n, n < 32 -> do
      (reg_x, _format_x, code_x) <- getSomeReg x
      return $ Any (intFormat w) (\dst -> code_x `snocOL` annExpr expr (LSL (OpReg w dst) (OpReg w reg_x) (OpImm (ImmInteger n))))
    CmmMachOp (MO_Shl w) [x, (CmmLit (CmmInt n _))] | w == W64, 0 <= n, n < 64 -> do
      (reg_x, _format_x, code_x) <- getSomeReg x
      return $ Any (intFormat w) (\dst -> code_x `snocOL` annExpr expr (LSL (OpReg w dst) (OpReg w reg_x) (OpImm (ImmInteger n))))

    CmmMachOp (MO_S_Shr w) [x, (CmmLit (CmmInt n _))] | fitsIn12bitImm n -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      (reg_x', code_x') <- signExtendReg (formatToWidth format_x) w reg_x
      return $ Any (intFormat w) (
        \dst ->
          code_x `appOL` code_x' `snocOL` annExpr expr (ASR (OpReg w dst) (OpReg w reg_x') (OpImm (ImmInteger n)))
        )
    CmmMachOp (MO_S_Shr w) [x, y] -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      (reg_y, _format_y, code_y) <- getSomeReg y
      (reg_x', code_x') <- signExtendReg (formatToWidth format_x) w reg_x
      return $ Any (intFormat w) (
        \dst ->
          code_x `appOL` code_x' `appOL` code_y `snocOL` annExpr expr (ASR (OpReg w dst) (OpReg w reg_x') (OpReg w reg_y))
        )

    CmmMachOp (MO_U_Shr w) [x, (CmmLit (CmmInt n _))] | w == W8, 0 <= n, n < 8 -> do
      (reg_x, _format_x, code_x) <- getSomeReg x
      return $ Any (intFormat w) (\dst -> code_x `snocOL` annExpr expr (UBFX (OpReg w dst) (OpReg w reg_x) (OpImm (ImmInteger n)) (OpImm (ImmInteger (8-n)))))
    CmmMachOp (MO_U_Shr w) [x, y] | w == W8 || w == W16 -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      (reg_y, _format_y, code_y) <- getSomeReg y
      return $ Any (intFormat w) (\dst -> code_x `appOL` code_y `appOL` truncateReg (formatToWidth format_x) w reg_x `snocOL` annExpr expr (ASR (OpReg w dst) (OpReg w reg_x) (OpReg w reg_y)))

    CmmMachOp (MO_U_Shr w) [x, (CmmLit (CmmInt n _))] | w == W16, 0 <= n, n < 16 -> do
      (reg_x, _format_x, code_x) <- getSomeReg x
      return $ Any (intFormat w) (\dst -> code_x `snocOL` annExpr expr (UBFX (OpReg w dst) (OpReg w reg_x) (OpImm (ImmInteger n)) (OpImm (ImmInteger (16-n)))))

    CmmMachOp (MO_U_Shr w) [x, (CmmLit (CmmInt n _))] | w == W32, 0 <= n, n < 32 -> do
      (reg_x, _format_x, code_x) <- getSomeReg x
      return $ Any (intFormat w) (\dst -> code_x `snocOL` annExpr expr (LSR (OpReg w dst) (OpReg w reg_x) (OpImm (ImmInteger n))))

    CmmMachOp (MO_U_Shr w) [x, (CmmLit (CmmInt n _))] | w == W64, 0 <= n, n < 64 -> do
      (reg_x, _format_x, code_x) <- getSomeReg x
      return $ Any (intFormat w) (\dst -> code_x `snocOL` annExpr expr (LSR (OpReg w dst) (OpReg w reg_x) (OpImm (ImmInteger n))))

    -- 3. Logic &&, ||
    CmmMachOp (MO_And w) [(CmmReg reg), CmmLit (CmmInt n _)] | fitsIn12bitImm n ->
      return $ Any (intFormat w) (\d -> unitOL $ annExpr expr (AND (OpReg w d) (OpReg w' r') (OpImm (ImmInteger n))))
      where w' = formatToWidth (cmmTypeFormat (cmmRegType plat reg))
            r' = getRegisterReg plat reg

    CmmMachOp (MO_Or w) [(CmmReg reg), CmmLit (CmmInt n _)] | fitsIn12bitImm n ->
      return $ Any (intFormat w) (\d -> unitOL $ annExpr expr (ORI (OpReg w d) (OpReg w' r') (OpImm (ImmInteger n))))
      where w' = formatToWidth (cmmTypeFormat (cmmRegType plat reg))
            r' = getRegisterReg plat reg

    -- Generic case.
    CmmMachOp op [x, y] -> do
      -- alright, so we have an operation, and two expressions. And we want to essentially do
      -- ensure we get float regs (TODO(Ben): What?)
      let withTempIntReg w op = OpReg w <$> getNewRegNat (intFormat w) >>= op
          -- withTempFloatReg w op = OpReg w <$> getNewRegNat (floatFormat w) >>= op

          -- A "plain" operation.
          bitOp w op = do
            -- compute x<m> <- x
            -- compute x<o> <- y
            -- <OP> x<n>, x<m>, x<o>
            (reg_x, format_x, code_x) <- getSomeReg x
            (reg_y, format_y, code_y) <- getSomeReg y
            massertPpr (isIntFormat format_x == isIntFormat format_y) $ text "bitOp: incompatible"
            return $ Any (intFormat w) (\dst ->
                code_x `appOL`
                code_y `appOL`
                op (OpReg w dst) (OpReg w reg_x) (OpReg w reg_y))

          -- A (potentially signed) integer operation.
          -- In the case of 8- and 16-bit signed arithmetic we must first
          -- sign-extend both arguments to 32-bits.
          -- See Note [Signed arithmetic on AArch64].
          intOp is_signed w op = do
              -- compute x<m> <- x
              -- compute x<o> <- y
              -- <OP> x<n>, x<m>, x<o>
              (reg_x, format_x, code_x) <- getSomeReg x
              (reg_y, format_y, code_y) <- getSomeReg y
              massertPpr (isIntFormat format_x && isIntFormat format_y) $ text "intOp: non-int"
              -- This is the width of the registers on which the operation
              -- should be performed.
              let w' = opRegWidth w
                  signExt r
                    | not is_signed  = return (r, nilOL)
                    | otherwise      = signExtendReg w w' r
              (reg_x_sx, code_x_sx) <- signExt reg_x
              (reg_y_sx, code_y_sx) <- signExt reg_y
              return $ Any (intFormat w) $ \dst ->
                  code_x `appOL`
                  code_y `appOL`
                  -- sign-extend both operands
                  code_x_sx `appOL`
                  code_y_sx `appOL`
                  op (OpReg w' dst) (OpReg w' reg_x_sx) (OpReg w' reg_y_sx) `appOL`
                  truncateReg w' w dst -- truncate back to the operand's original width

          floatOp w op = do
            (reg_fx, format_x, code_fx) <- getFloatReg x
            (reg_fy, format_y, code_fy) <- getFloatReg y
            massertPpr (isFloatFormat format_x && isFloatFormat format_y) $ text "floatOp: non-float"
            return $ Any (floatFormat w) (\dst -> code_fx `appOL` code_fy `appOL` op (OpReg w dst) (OpReg w reg_fx) (OpReg w reg_fy))

          -- need a special one for conditionals, as they return ints
          floatCond w op = do
            (reg_fx, format_x, code_fx) <- getFloatReg x
            (reg_fy, format_y, code_fy) <- getFloatReg y
            massertPpr (isFloatFormat format_x && isFloatFormat format_y) $ text "floatCond: non-float"
            return $ Any (intFormat w) (\dst -> code_fx `appOL` code_fy `appOL` op (OpReg w dst) (OpReg w reg_fx) (OpReg w reg_fy))

      case op of
        -- Integer operations
        -- Add/Sub should only be Integer Options.
        MO_Add w -> intOp False w (\d x y -> unitOL $ annExpr expr (ADD d x y))
        -- TODO: Handle sub-word case
        MO_Sub w -> intOp False w (\d x y -> unitOL $ annExpr expr (SUB d x y))

        -- Note [CSET]
        -- ~~~~~~~~~~~
        -- Setting conditional flags: the architecture internally knows the
        -- following flag bits.  And based on thsoe comparisons as in the
        -- table below.
        --
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

        -- N.B. We needn't sign-extend sub-word size (in)equality comparisons
        -- since we don't care about ordering.
        MO_Eq w     -> bitOp w (\d x y -> toOL [ CSET d x y EQ ])
        MO_Ne w     -> bitOp w (\d x y -> toOL [ CSET d x y NE ])

        -- Signed multiply/divide
        MO_Mul w          -> intOp True w (\d x y -> unitOL $ MUL d x y)
        MO_S_MulMayOflo w -> do_mul_may_oflo w x y
        MO_S_Quot w       -> intOp True w (\d x y -> unitOL $ DIV d x y)

        -- Note the swap in Rx and Ry.
        MO_S_Rem w -> intOp True w (\d x y -> unitOL $ REM d x y)

        -- Unsigned multiply/divide
        MO_U_Quot w -> intOp False w (\d x y -> unitOL $ DIVU d x y)
        MO_U_Rem w  -> intOp False w (\d x y -> unitOL $ REM d x y)

        -- Signed comparisons -- see Note [CSET]
        MO_S_Ge w     -> intOp True  w (\d x y -> toOL [ CSET d x y SGE ])
        MO_S_Le w     -> intOp True  w (\d x y -> toOL [ CSET d x y SLE ])
        MO_S_Gt w     -> intOp True  w (\d x y -> toOL [ CSET d x y SGT ])
        MO_S_Lt w     -> intOp True  w (\d x y -> toOL [ CSET d x y SLT ])

        -- Unsigned comparisons
        MO_U_Ge w     -> intOp False w (\d x y -> toOL [ CSET d x y UGE ])
        MO_U_Le w     -> intOp False w (\d x y -> toOL [ CSET d x y ULE ])
        MO_U_Gt w     -> intOp False w (\d x y -> toOL [ CSET d x y UGT ])
        MO_U_Lt w     -> intOp False w (\d x y -> toOL [ CSET d x y ULT ])

        -- Floating point arithmetic
        MO_F_Add w   -> floatOp w (\d x y -> unitOL $ ADD d x y)
        MO_F_Sub w   -> floatOp w (\d x y -> unitOL $ SUB d x y)
        MO_F_Mul w   -> floatOp w (\d x y -> unitOL $ MUL d x y)
        MO_F_Quot w  -> floatOp w (\d x y -> unitOL $ DIV d x y)

        -- Floating point comparison
        MO_F_Eq w    -> floatCond w (\d x y -> toOL [ CSET d x y EQ ])
        MO_F_Ne w    -> floatCond w (\d x y -> toOL [ CSET d x y NE ])

        -- careful with the floating point operations.
        -- SLE is effectively LE or unordered (NaN)
        -- SLT is the same. ULE, and ULT will not return true for NaN.
        -- This is a bit counter-intuitive. Don't let yourself be fooled by
        -- the S/U prefix for floats, it's only meaningful for integers.
        MO_F_Ge w    -> floatCond w (\d x y -> toOL [ CSET d x y OGE ])
        MO_F_Le w    -> floatCond w (\d x y -> toOL [ CSET d x y OLE ]) -- x <= y <=> y > x
        MO_F_Gt w    -> floatCond w (\d x y -> toOL [ CSET d x y OGT ])
        MO_F_Lt w    -> floatCond w (\d x y -> toOL [ CSET d x y OLT ]) -- x < y <=> y >= x

        -- Bitwise operations
        MO_And   w -> bitOp w (\d x y -> unitOL $ AND d x y)
        MO_Or    w -> bitOp w (\d x y -> unitOL $ OR d x y)
        MO_Xor   w -> bitOp w (\d x y -> unitOL $ XOR d x y)
        MO_Shl   w -> intOp False w (\d x y -> unitOL $ LSL d x y)
        MO_U_Shr w -> intOp False w (\d x y -> unitOL $ LSR d x y)
        MO_S_Shr w -> intOp True  w (\d x y -> unitOL $ ASR d x y)

        -- TODO

        op -> pprPanic "getRegister' (unhandled dyadic CmmMachOp): " $ (pprMachOp op) <+> text "in" <+> (pdoc plat expr)
    CmmMachOp _op _xs
      -> pprPanic "getRegister' (variadic CmmMachOp): " (pdoc plat expr)

  where
    isNbitEncodeable :: Int -> Integer -> Bool
    isNbitEncodeable n i = let shift = n - 1 in (-1 `shiftL` shift) <= i && i < (1 `shiftL` shift)
    -- N.B. MUL does not set the overflow flag.
    -- Return 0 when the operation cannot overflow, /= 0 otherwise
    do_mul_may_oflo :: Width -> CmmExpr -> CmmExpr -> NatM Register
    do_mul_may_oflo w _x _y | w > W64 = pprPanic "Cannot multiply larger than 64bit" (ppr w)
    do_mul_may_oflo w@W64 x y = do
      (reg_x, format_x, code_x) <- getSomeReg x
      (reg_y, format_y, code_y) <- getSomeReg y
      -- TODO: Can't we clobber reg_x and reg_y to save registers?
      lo <- getNewRegNat II64
      hi <- getNewRegNat II64
      -- TODO: Overhaul CSET: 3rd operand isn't needed for SNEZ
      let nonSense = OpImm (ImmInt 0)
      pure $
        Any
          (intFormat w)
          ( \dst ->
              code_x
                `appOL` signExtend (formatToWidth format_x) W64 reg_x reg_x
                `appOL` code_y
                `appOL` signExtend (formatToWidth format_y) W64 reg_x reg_y
                `appOL` toOL
                  [ annExpr expr (SMULH (OpReg w hi) (OpReg w reg_x) (OpReg w reg_y)),
                    MUL (OpReg w lo) (OpReg w reg_x) (OpReg w reg_y),
                    ASR (OpReg w lo) (OpReg w reg_x) (OpImm (ImmInt (widthInBits W64 - 1))),
                    ann
                      (text "Set flag if result of MULH contains more than sign bits.")
                      (SUB (OpReg w hi) (OpReg w hi) (OpReg w lo)),
                    CSET (OpReg w dst) (OpReg w hi) nonSense NE
                  ]
          )
    do_mul_may_oflo w x y = do
      (reg_x, format_x, code_x) <- getSomeReg x
      (reg_y, format_y, code_y) <- getSomeReg y
      let width_x = formatToWidth format_x
          width_y = formatToWidth format_y
      if w > width_x && w > width_y
        then
          pure $
            Any
              (intFormat w)
              ( \dst ->
                  -- 8bit * 8bit cannot overflow 16bit
                  -- 16bit * 16bit cannot overflow 32bit
                  -- 32bit * 32bit cannot overflow 64bit
                  unitOL $ annExpr expr (ADD (OpReg w dst) zero (OpImm (ImmInt 0)))
              )
        else do
          let use32BitMul = width_x <= W32 && width_y <= W32
              nonSense = OpImm (ImmInt 0)
          if use32BitMul
            then do
              narrowedReg <- getNewRegNat II64
              pure $
                Any
                  (intFormat w)
                  ( \dst ->
                      code_x
                        `appOL` signExtend (formatToWidth format_x) W32 reg_x reg_x
                        `appOL` code_y
                        `appOL` signExtend (formatToWidth format_y) W32 reg_y reg_y
                        `snocOL` annExpr expr (MUL (OpReg W32 dst) (OpReg W32 reg_x) (OpReg W32 reg_y))
                        `appOL` signExtend W32 w dst narrowedReg
                        `appOL` toOL
                          [ ann
                              (text "Check if the multiplied value fits in the narrowed register")
                              (SUB (OpReg w dst) (OpReg w dst) (OpReg w narrowedReg)),
                            CSET (OpReg w dst) (OpReg w dst) nonSense NE
                          ]
                  )
            else do
              -- TODO: Can't we clobber reg_x and reg_y to save registers?
              lo <- getNewRegNat II64
              hi <- getNewRegNat II64
              narrowedLo <- getNewRegNat II64

              -- TODO: Overhaul CSET: 3rd operand isn't needed for SNEZ
              let nonSense = OpImm (ImmInt 0)
              pure $
                Any
                  (intFormat w)
                  ( \dst ->
                      code_x
                        `appOL` signExtend (formatToWidth format_x) W64 reg_x reg_x
                        `appOL` code_y
                        `appOL` signExtend (formatToWidth format_y) W64 reg_x reg_y
                        `appOL` toOL
                          [ annExpr expr (SMULH (OpReg w hi) (OpReg w reg_x) (OpReg w reg_y)),
                            MUL (OpReg w lo) (OpReg w reg_x) (OpReg w reg_y),
                            ASR (OpReg w lo) (OpReg w reg_x) (OpImm (ImmInt (widthInBits W64 - 1))),
                            ann
                              (text "Set flag if result of MULH contains more than sign bits.")
                              (SUB (OpReg w hi) (OpReg w hi) (OpReg w lo)),
                            CSET (OpReg w hi) (OpReg w hi) nonSense NE
                          ]
                        `appOL` signExtend W64 w lo narrowedLo
                        `appOL` toOL
                          [ ann
                              (text "Check if the multiplied value fits in the narrowed register")
                              (SUB (OpReg w narrowedLo) (OpReg w lo) (OpReg w narrowedLo)),
                            CSET (OpReg w narrowedLo) (OpReg w narrowedLo) nonSense NE,
                            ann
                              (text "Combine both overflow flags")
                              (OR (OpReg w dst) (OpReg w narrowedLo) (OpReg w hi))
                          ]
                )

-- | Instructions to sign-extend the value in the given register from width @w@
-- up to width @w'@.
signExtendReg :: Width -> Width -> Reg -> NatM (Reg, OrdList Instr)
signExtendReg w _w' r | w == W64 = pure (r, nilOL)
signExtendReg w w' r = do
  r' <- getNewRegNat (intFormat w')
  let instrs = signExtend w w' r r'
  pure (r', instrs)

-- | Sign extends to 64bit, if needed
--
-- Source `Reg` @r@ stays untouched, while the conversion happens on destination
-- `Reg` @r'@.
signExtend :: Width -> Width -> Reg -> Reg -> OrdList Instr
signExtend w w' _r _r' | w > w' = pprPanic "This is not a sign extension, but a truncation." $ ppr w <> text "->" <+> ppr w'
signExtend w w' _r _r' | w > W64 || w' > W64 = pprPanic "Unexpected width (max is 64bit):" $ ppr w <> text "->" <+> ppr w'
signExtend w w' r r' | w == W64 && w' == W64 && r == r' = nilOL
signExtend w w' r r' | w == W64 && w' == W64 = unitOL $ MOV (OpReg w' r') (OpReg w r)
signExtend w w' r r'
  | w == W32 && w' == W64 =
      unitOL $
        ann
          (text "sign-extend register (SEXT.W)" <+> ppr r <+> ppr w <> text "->" <> ppr w')
          -- `ADDIW r r 0` is the pseudo-op SEXT.W
          (ADD (OpReg w' r') (OpReg w r) (OpImm (ImmInt 0)))
signExtend w w' r r' =
  toOL
    [ ann
        (text "narrow register signed" <+> ppr r <> char ':' <> ppr w <> text "->" <> ppr r <> char ':' <> ppr w')
        (LSL (OpReg w' r') (OpReg w r) (OpImm (ImmInt shift))),
      -- signed (arithmetic) right shift
      ASR (OpReg w' r') (OpReg w' r') (OpImm (ImmInt shift))
    ]
  where
    shift = 64 - widthInBits w

-- | Instructions to truncate the value in the given register from width @w@
-- to width @w'@.
--
-- In other words, it just cuts the width out of the register. N.B.: This
-- ignores signedness (no sign extension takes place)!
truncateReg :: Width -> Width -> Reg -> OrdList Instr
truncateReg _w w' _r | w' == W64 = nilOL
truncateReg _w w' r | w' > W64 = pprPanic "Cannot truncate to width bigger than register size (max is 64bit):" $ text (show r) <> char ':' <+> ppr w'
truncateReg w _w' r | w > W64 = pprPanic "Unexpected register size (max is 64bit):" $ text (show r) <> char ':' <+> ppr w
truncateReg w w' r =
  toOL
    [ ann
        (text "truncate register" <+> ppr r <+> ppr w <> text "->" <> ppr w')
        (LSL (OpReg w' r) (OpReg w r) (OpImm (ImmInt shift))),
      -- SHL ignores signedness!
      LSR (OpReg w' r) (OpReg w r) (OpImm (ImmInt shift))
    ]
  where
    shift = 64 - widthInBits w'

-- -----------------------------------------------------------------------------
--  The 'Amode' type: Memory addressing modes passed up the tree.
data Amode = Amode AddrMode InstrBlock

-- | Provide the value of a `CmmExpr` with an `Amode`
--
-- N.B. this function should be used to provide operands to load and store
-- instructions with signed 12bit wide immediates (S & I types). For other
-- immediate sizes and formats (e.g. B type uses multiples of 2) this function
-- would need to be adjusted.
getAmode :: Platform
         -> Width     -- ^ width of loaded value
         -> CmmExpr
         -> NatM Amode
-- TODO: Specialize stuff we can destructure here.

-- LDR/STR: Immediate can be represented with 12bits
getAmode platform w (CmmRegOff reg off)
  | w <= W64, fitsIn12bitImm off
  = return $ Amode (AddrRegImm reg' off') nilOL
    where reg' = getRegisterReg platform reg
          off' = ImmInt off

-- For Stores we often see something like this:
-- CmmStore (CmmMachOp (MO_Add w) [CmmLoad expr, CmmLit (CmmInt n w')]) (expr2)
-- E.g. a CmmStoreOff really. This can be translated to `str $expr2, [$expr, #n ]
-- for `n` in range.
getAmode _platform _ (CmmMachOp (MO_Add _w) [expr, CmmLit (CmmInt off _w')])
  | fitsIn12bitImm off
  = do (reg, _format, code) <- getSomeReg expr
       return $ Amode (AddrRegImm reg (ImmInteger off)) code

getAmode _platform _ (CmmMachOp (MO_Sub _w) [expr, CmmLit (CmmInt off _w')])
  | fitsIn12bitImm (-off)
  = do (reg, _format, code) <- getSomeReg expr
       return $ Amode (AddrRegImm reg (ImmInteger (-off))) code

-- Generic case
getAmode _platform _ expr
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
    let w = formatToWidth rep
    Amode addr addr_code <- getAmode platform w addrE
    return $ COMMENT (text "CmmStore" <+> parens (text (show addrE)) <+> parens (text (show srcE)))
            `consOL` (code
            `appOL` addr_code
            `snocOL` STR rep (OpReg w src_reg) (OpAddr addr))

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
-- AArch64 has 26bits for targets, whereas RiscV only has 20.
-- Thus we need to distinguish between far (outside of the)
-- current compilation unit. And regular branches.
-- RiscV has ±2MB of displacement, whereas AArch64 has ±128MB.
-- Thus for most branches we can get away with encoding it
-- directly in the instruction rather than always loading the
-- address into a register and then using that to jump.
-- Under the assumption that our linked build product is less than
-- ~2*128MB of TEXT, and there are no jump that span the whole
-- TEXT segment.
-- Something where riscv's compressed instruction might come in
-- handy.
genJump :: CmmExpr{-the branch target-} -> NatM InstrBlock
genJump expr = do
    (target, _format, code) <- getSomeReg expr
    return (code `appOL` unitOL (annExpr expr (J (TReg target))))

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
        return $ code_x `snocOL` (annExpr expr (CBZ (OpReg w reg_x) (TBlock bid)))

      -- Optimized /= 0 case.
      CmmMachOp (MO_Ne w) [x, CmmLit (CmmInt 0 _)] -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        return $ code_x `snocOL`  (annExpr expr (CBNZ (OpReg w reg_x) (TBlock bid)))

      -- Generic case.
      CmmMachOp mop [x, y] -> do

        let ubcond w cmp = do
                -- compute both sides.
                (reg_x, format_x, code_x) <- getSomeReg x
                (reg_y, format_y, code_y) <- getSomeReg y
                let x' = OpReg w reg_x
                    y' = OpReg w reg_y
                return $ case w of
                  w | w == W8 || w == W16 -> code_x `appOL`
                      truncateReg (formatToWidth format_x) w reg_x  `appOL`
                      code_y `appOL`
                      truncateReg (formatToWidth format_y) w reg_y  `appOL`
                      code_y `snocOL`
                      annExpr expr (BCOND cmp x' y' (TBlock bid))
                  _   -> code_x `appOL` code_y `snocOL` annExpr expr (BCOND cmp x' y' (TBlock bid))

            sbcond w cmp = do
              -- compute both sides.
              (reg_x, format_x, code_x) <- getSomeReg x
              (reg_y, format_y, code_y) <- getSomeReg y
              let x' = OpReg w reg_x
                  y' = OpReg w reg_y
              return $ case w of
                W8 ->
                  code_x
                    `appOL` signExtend (formatToWidth format_x) W64 reg_x reg_x
                    `appOL` code_y
                    `appOL` signExtend (formatToWidth format_y) W64 reg_y reg_y
                    `appOL` unitOL (annExpr expr (BCOND cmp x' y' (TBlock bid)))
                W16 ->
                  code_x
                    `appOL` signExtend (formatToWidth format_x) W64 reg_x reg_x
                    `appOL` code_y
                    `appOL` signExtend (formatToWidth format_y) W64 reg_y reg_y
                    `appOL` unitOL (annExpr expr (BCOND cmp x' y' (TBlock bid)))
                _ -> code_x `appOL` code_y `appOL` unitOL (annExpr expr (BCOND cmp x' y' (TBlock bid)))

            fbcond w cmp = do
              -- ensure we get float regs
              (reg_fx, _format_fx, code_fx) <- getFloatReg x
              (reg_fy, _format_fy, code_fy) <- getFloatReg y
              return $ code_fx `appOL` code_fy `snocOL` (annExpr expr (BCOND cmp (OpReg w reg_fx) (OpReg w reg_fy) (TBlock bid)))

        case mop of
          MO_F_Eq w -> fbcond w EQ
          MO_F_Ne w -> fbcond w NE

          MO_F_Gt w -> fbcond w OGT
          MO_F_Ge w -> fbcond w OGE
          MO_F_Lt w -> fbcond w OLT
          MO_F_Le w -> fbcond w OLE

          MO_Eq w   -> sbcond w EQ
          MO_Ne w   -> sbcond w NE

          MO_S_Gt w -> sbcond w SGT
          MO_S_Ge w -> sbcond w SGE
          MO_S_Lt w -> sbcond w SLT
          MO_S_Le w -> sbcond w SLE
          MO_U_Gt w -> ubcond w UGT
          MO_U_Ge w -> ubcond w UGE
          MO_U_Lt w -> ubcond w ULT
          MO_U_Le w -> ubcond w ULE
          _ -> pprPanic "RV64.genCondJump:case mop: " (text $ show expr)
      _ -> pprPanic "RV64.genCondJump: " (text $ show expr)


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
-- FIXME: Update for RISCV, the below is still AArch64.
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
-- instead of the add instruction.
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
-- TODO: Specialize where we can.
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
        -- While this works on aarch64, for _most_ labels, it will fall short
        -- where label branching only works for shoter distances (e.g. riscv)
        -- (CmmLit (CmmLabel lbl)) -> pure (TLabel lbl, nilOL)
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
                                 , SUB (OpReg W64 (regSingle 2)) (OpReg W64 (regSingle 2)) (OpImm (ImmInt (8 * i)))
                                 , DELTA (-8 * i - 16) ]
          moveStackUp 0 = toOL [ POP_STACK_FRAME
                               , DELTA 0 ]
          moveStackUp i | odd i = moveStackUp (i + 1)
          moveStackUp i = toOL [ ADD (OpReg W64 (regSingle 2)) (OpReg W64 (regSingle 2)) (OpImm (ImmInt (8 * i)))
                               , POP_STACK_FRAME
                               , DELTA 0 ]

      let code =    call_target_code          -- compute the label (possibly into a register)
            `appOL` moveStackDown (stackSpace `div` 8)
            `appOL` passArgumentsCode         -- put the arguments into x0, ...
            `appOL` (unitOL $ BL call_target passRegs returnRegs) -- branch and link.
            `appOL` readResultsCode           -- parse the results into registers
            `appOL` moveStackUp (stackSpace `div` 8)
      return (code, Nothing)

    PrimTarget MO_F32_Fabs
      | [arg_reg] <- arg_regs, [dest_reg] <- dest_regs ->
        unaryFloatOp W32 (\d x -> unitOL $ FABS d x) arg_reg dest_reg
    PrimTarget MO_F64_Fabs
      | [arg_reg] <- arg_regs, [dest_reg] <- dest_regs ->
        unaryFloatOp W64 (\d x -> unitOL $ FABS d x) arg_reg dest_reg

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
        MO_F32_Fabs  -> mkCCall "fabsf"
        MO_F32_Sqrt  -> mkCCall "sqrtf"

        -- 64-bit primops
        MO_I64_ToI   -> mkCCall "hs_int64ToInt"
        MO_I64_FromI -> mkCCall "hs_intToInt64"
        MO_W64_ToW   -> mkCCall "hs_word64ToWord"
        MO_W64_FromW -> mkCCall "hs_wordToWord64"
        MO_x64_Neg   -> mkCCall "hs_neg64"
        MO_x64_Add   -> mkCCall "hs_add64"
        MO_x64_Sub   -> mkCCall "hs_sub64"
        MO_x64_Mul   -> mkCCall "hs_mul64"
        MO_I64_Quot  -> mkCCall "hs_quotInt64"
        MO_I64_Rem   -> mkCCall "hs_remInt64"
        MO_W64_Quot  -> mkCCall "hs_quotWord64"
        MO_W64_Rem   -> mkCCall "hs_remWord64"
        MO_x64_And   -> mkCCall "hs_and64"
        MO_x64_Or    -> mkCCall "hs_or64"
        MO_x64_Xor   -> mkCCall "hs_xor64"
        MO_x64_Not   -> mkCCall "hs_not64"
        MO_x64_Shl   -> mkCCall "hs_uncheckedShiftL64"
        MO_I64_Shr   -> mkCCall "hs_uncheckedIShiftRA64"
        MO_W64_Shr   -> mkCCall "hs_uncheckedShiftRL64"
        MO_x64_Eq    -> mkCCall "hs_eq64"
        MO_x64_Ne    -> mkCCall "hs_ne64"
        MO_I64_Ge    -> mkCCall "hs_geInt64"
        MO_I64_Gt    -> mkCCall "hs_gtInt64"
        MO_I64_Le    -> mkCCall "hs_leInt64"
        MO_I64_Lt    -> mkCCall "hs_ltInt64"
        MO_W64_Ge    -> mkCCall "hs_geWord64"
        MO_W64_Gt    -> mkCCall "hs_gtWord64"
        MO_W64_Le    -> mkCCall "hs_leWord64"
        MO_W64_Lt    -> mkCCall "hs_ltWord64"

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
        -- TODO DMBSY is probably *way* too much!
        MO_ReadBarrier      ->  return (unitOL DMBSY, Nothing)
        MO_WriteBarrier     ->  return (unitOL DMBSY, Nothing)
        MO_Touch            ->  return (nilOL, Nothing) -- Keep variables live (when using interior pointers)
        -- Prefetch
        MO_Prefetch_Data _n -> return (nilOL, Nothing) -- Prefetch hint.

        -- Memory copy/set/move/cmp, with alignment for optimization

        -- TODO Optimize and use e.g. quad registers to move memory around instead
        -- of offloading this to memcpy. For small memcpys we can utilize
        -- the 128bit quad registers in NEON to move block of bytes around.
        -- Might also make sense of small memsets? Use xzr? What's the function
        -- call overhead?
        MO_Memcpy  _align   -> mkCCall "memcpy"
        MO_Memset  _align   -> mkCCall "memset"
        MO_Memmove _align   -> mkCCall "memmove"
        MO_Memcmp  _align   -> mkCCall "memcmp"

        MO_SuspendThread    -> mkCCall "suspendThread"
        MO_ResumeThread     -> mkCCall "resumeThread"

        MO_PopCnt w         -> mkCCall (popCntLabel w)
        MO_Pdep w           -> mkCCall (pdepLabel w)
        MO_Pext w           -> mkCCall (pextLabel w)
        MO_Clz w            -> mkCCall (clzLabel w)
        MO_Ctz w            -> mkCCall (ctzLabel w)
        MO_BSwap w          -> mkCCall (bSwapLabel w)
        MO_BRev w           -> mkCCall (bRevLabel w)

        -- -- Atomic read-modify-write.
        MO_AtomicRead w ord
          | [p_reg] <- arg_regs
          , [dst_reg] <- dest_regs -> do
              (p, _fmt_p, code_p) <- getSomeReg p_reg
              platform <- getPlatform
              let instr = case ord of
                      MemOrderRelaxed -> LDR
                      _               -> panic "no proper atomic write support" -- LDAR
                  dst = getRegisterReg platform (CmmLocal dst_reg)
                  code =
                    code_p `snocOL`
                    instr (intFormat w) (OpReg w dst) (OpAddr $ AddrReg p)
              return (code, Nothing)
          | otherwise -> panic "mal-formed AtomicRead"
        MO_AtomicWrite w ord
          | [p_reg, val_reg] <- arg_regs -> do
              (p, _fmt_p, code_p) <- getSomeReg p_reg
              (val, fmt_val, code_val) <- getSomeReg val_reg
              let instr = case ord of
                      MemOrderRelaxed -> STR
                      _               -> panic "no proper atomic write support" -- STLR
                  code =
                    code_p `appOL`
                    code_val `snocOL`
                    instr fmt_val (OpReg w val) (OpAddr $ AddrReg p)
              return (code, Nothing)
          | otherwise -> panic "mal-formed AtomicWrite"
        MO_AtomicRMW w amop -> mkCCall (atomicRMWLabel w amop)
        MO_Cmpxchg w        -> mkCCall (cmpxchgLabel w)
        -- -- Should be an AtomicRMW variant eventually.
        -- -- Sequential consistent.
        -- TODO: this should be implemented properly!
        MO_Xchg w           -> mkCCall (xchgLabel w)

  where
    unsupported :: Show a => a -> b
    unsupported mop = panic ("outOfLineCmmOp: " ++ show mop
                          ++ " not supported here")
    mkCCall :: FastString -> NatM (InstrBlock, Maybe BlockId)
    mkCCall name = do
      config <- getConfig
      target <- cmmMakeDynamicReference config CallReference $
          mkForeignLabel name Nothing ForeignLabelInThisPackage IsFunction
      let cconv = ForeignConvention CCallConv [NoHint] [NoHint] CmmMayReturn
      genCCall (ForeignTarget target cconv) dest_regs arg_regs bid

    -- TODO: Optimize using paired stores and loads (STP, LDP). It is
    -- automatically done by the allocator for us. However it's not optimal,
    -- as we'd rather want to have control over
    --     all spill/load registers, so we can optimize with instructions like
    --       STP xA, xB, [sp, #-16]!
    --     and
    --       LDP xA, xB, sp, #16
    --
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

    passArguments pack (gpReg:gpRegs) fpRegs ((r, format, hint, code_r):args) stackSpace accumRegs accumCode | isIntFormat format = do
      platform <- getPlatform
    -- RISCV64 Integer Calling Convention: "When passed in registers or on the
    -- stack, integer scalars narrower than XLEN bits are widened according to
    -- the sign of their type up to 32 bits, then sign-extended to XLEN bits."
      let w = formatToWidth format
          accumCode' = accumCode `appOL`
                       code_r `appOL`
                       signExtend w W64 r gpReg
      passArguments pack gpRegs fpRegs args stackSpace (gpReg:accumRegs) accumCode'

    -- Still have FP regs, and we want to pass an FP argument.
    passArguments pack gpRegs (fpReg:fpRegs) ((r, format, _hint, code_r):args) stackSpace accumRegs accumCode | isFloatFormat format = do
      let w = formatToWidth format
          mov = MOV (OpReg w fpReg) (OpReg w r)
          accumCode' = accumCode `appOL`
                       code_r `snocOL`
                       ann (text "Pass fp argument: " <> ppr r) mov
      passArguments pack gpRegs fpRegs args stackSpace (fpReg:accumRegs) accumCode'

    -- No mor regs left to pass. Must pass on stack.
    passArguments pack [] [] ((r, format, _hint, code_r):args) stackSpace accumRegs accumCode = do
      let w = formatToWidth format
          bytes = widthInBits w `div` 8
          space = if pack then bytes else 8
          stackSpace' | pack && stackSpace `mod` space /= 0 = stackSpace + space - (stackSpace `mod` space)
                      | otherwise                           = stackSpace
          str = STR format (OpReg w r) (OpAddr (AddrRegImm (regSingle 2) (ImmInt stackSpace')))
          stackCode = code_r `snocOL`
                      ann (text "Pass argument (size " <> ppr w <> text ") on the stack: " <> ppr r) str
      passArguments pack [] [] args (stackSpace'+space) accumRegs (stackCode `appOL` accumCode)

    -- Still have fpRegs left, but want to pass a GP argument. Must be passed on the stack then.
    passArguments pack [] fpRegs ((r, format, _hint, code_r):args) stackSpace accumRegs accumCode | isIntFormat format = do
      let w = formatToWidth format
          bytes = widthInBits w `div` 8
          space = if pack then bytes else 8
          stackSpace' | pack && stackSpace `mod` space /= 0 = stackSpace + space - (stackSpace `mod` space)
                      | otherwise                           = stackSpace
          str = STR format (OpReg w r) (OpAddr (AddrRegImm (regSingle 2) (ImmInt stackSpace')))
          stackCode = code_r `snocOL`
                      ann (text "Pass argument (size " <> ppr w <> text ") on the stack: " <> ppr r) str
      passArguments pack [] fpRegs args (stackSpace'+space) accumRegs (stackCode `appOL` accumCode)

    -- Still have gpRegs left, but want to pass a FP argument. Must be passed on the stack then.
    passArguments pack gpRegs [] ((r, format, _hint, code_r):args) stackSpace accumRegs accumCode | isFloatFormat format = do
      let w = formatToWidth format
          bytes = widthInBits w `div` 8
          space = if pack then bytes else 8
          stackSpace' | pack && stackSpace `mod` space /= 0 = stackSpace + space - (stackSpace `mod` space)
                      | otherwise                           = stackSpace
          str = STR format (OpReg w r) (OpAddr (AddrRegImm (regSingle 2) (ImmInt stackSpace')))
          stackCode = code_r `snocOL`
                      ann (text "Pass argument (size " <> ppr w <> text ") on the stack: " <> ppr r) str
      passArguments pack gpRegs [] args (stackSpace'+space) accumRegs (stackCode `appOL` accumCode)

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

    unaryFloatOp w op arg_reg dest_reg = do
      platform <- getPlatform
      (reg_fx, _format_x, code_fx) <- getFloatReg arg_reg
      let dst = getRegisterReg platform (CmmLocal dest_reg)
      let code = code_fx `appOL` op (OpReg w dst) (OpReg w reg_fx)
      return (code, Nothing)
