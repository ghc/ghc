{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.CmmToAsm.RV64.CodeGen
  ( cmmTopCodeGen,
    generateJumpTableForInstr,
    makeFarBranches,
  )
where

import Control.Monad
import Data.Maybe
import Data.Word
import GHC.Cmm
import GHC.Cmm.BlockId
import GHC.Cmm.CLabel
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.DebugBlock
import GHC.Cmm.Switch
import GHC.Cmm.Utils
import GHC.CmmToAsm.CPrim
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Format
import GHC.CmmToAsm.Monad
  ( NatM,
    getBlockIdNat,
    getConfig,
    getDebugBlock,
    getFileId,
    getNewLabelNat,
    getNewRegNat,
    getPicBaseMaybeNat,
    getPlatform,
  )
import GHC.CmmToAsm.PIC
import GHC.CmmToAsm.RV64.Cond
import GHC.CmmToAsm.RV64.Instr
import GHC.CmmToAsm.RV64.Regs
import GHC.CmmToAsm.Types
import GHC.Data.FastString
import GHC.Data.OrdList
import GHC.Float
import GHC.Platform
import GHC.Platform.Reg
import GHC.Platform.Regs
import GHC.Prelude hiding (EQ)
import GHC.Types.Basic
import GHC.Types.ForeignCall
import GHC.Types.SrcLoc (srcSpanFile, srcSpanStartCol, srcSpanStartLine)
import GHC.Types.Tickish (GenTickish (..))
import GHC.Types.Unique.Supply
import GHC.Utils.Constants (debugIsOn)
import GHC.Utils.Misc
import GHC.Utils.Monad
import GHC.Utils.Outputable
import GHC.Utils.Panic

-- For an overview of an NCG's structure, see Note [General layout of an NCG]

cmmTopCodeGen ::
  RawCmmDecl ->
  NatM [NatCmmDecl RawCmmStatics Instr]
-- Thus we'll have to deal with either CmmProc ...
cmmTopCodeGen _cmm@(CmmProc info lab live graph) = do
  picBaseMb <- getPicBaseMaybeNat
  when (isJust picBaseMb) $ panic "RV64.cmmTopCodeGen: Unexpected PIC base register (RISCV ISA does not define one)"

  let blocks = toBlockListEntryFirst graph
  (nat_blocks, statics) <- mapAndUnzipM basicBlockCodeGen blocks

  let proc = CmmProc info lab live (ListGraph $ concat nat_blocks)
      tops = proc : concat statics

  pure tops

-- ... or CmmData.
cmmTopCodeGen (CmmData sec dat) = pure [CmmData sec dat] -- no translation, we just use CmmStatic

basicBlockCodeGen ::
  Block CmmNode C C ->
  NatM
    ( [NatBasicBlock Instr],
      [NatCmmDecl RawCmmStatics Instr]
    )
basicBlockCodeGen block = do
  config <- getConfig
  let (_, nodes, tail) = blockSplit block
      id = entryLabel block
      stmts = blockToList nodes

      header_comment_instr
        | debugIsOn =
            unitOL
              $ MULTILINE_COMMENT
                ( text "-- --------------------------- basicBlockCodeGen --------------------------- --\n"
                    $+$ withPprStyle defaultDumpStyle (pdoc (ncgPlatform config) block)
                )
        | otherwise = nilOL

  -- Generate location directive `.loc` (DWARF debug location info)
  loc_instrs <- genLocInstrs

  -- Generate other instructions
  mid_instrs <- stmtsToInstrs stmts
  (!tail_instrs) <- stmtToInstrs tail

  let instrs = header_comment_instr `appOL` loc_instrs `appOL` mid_instrs `appOL` tail_instrs

      -- TODO: Then x86 backend runs @verifyBasicBlock@ here. How important it is to
      -- have a valid CFG is an open question: This and the AArch64 and PPC NCGs
      -- work fine without it.

      -- Code generation may introduce new basic block boundaries, which are
      -- indicated by the NEWBLOCK instruction. We must split up the instruction
      -- stream into basic blocks again. Also, we extract LDATAs here too.
      (top, other_blocks, statics) = foldrOL mkBlocks ([], [], []) instrs

  return (BasicBlock id top : other_blocks, statics)
  where
    genLocInstrs :: NatM (OrdList Instr)
    genLocInstrs = do
      dbg <- getDebugBlock (entryLabel block)
      case dblSourceTick =<< dbg of
        Just (SourceNote span name) ->
          do
            fileId <- getFileId (srcSpanFile span)
            let line = srcSpanStartLine span; col = srcSpanStartCol span
            pure $ unitOL $ LOCATION fileId line col name
        _ -> pure nilOL

mkBlocks ::
  Instr ->
  ([Instr], [GenBasicBlock Instr], [GenCmmDecl RawCmmStatics h g]) ->
  ([Instr], [GenBasicBlock Instr], [GenCmmDecl RawCmmStatics h g])
mkBlocks (NEWBLOCK id) (instrs, blocks, statics) =
  ([], BasicBlock id instrs : blocks, statics)
mkBlocks (LDATA sec dat) (instrs, blocks, statics) =
  (instrs, blocks, CmmData sec dat : statics)
mkBlocks instr (instrs, blocks, statics) =
  (instr : instrs, blocks, statics)

-- -----------------------------------------------------------------------------

-- | Utilities

-- | Annotate an `Instr` with a `SDoc` comment
ann :: SDoc -> Instr -> Instr
ann doc instr {- debugIsOn -} = ANN doc instr
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
-- An improvement could be to have
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

-- Note [RISCV64 Jump Tables]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Jump tables are implemented by generating a table of relative addresses,
-- where each entry is the relative offset to the target block from the first
-- entry / table label (`generateJumpTableForInstr`). Using the jump table means
-- loading the entry's value and jumping to the calculated absolute address
-- (`genSwitch`).
--
-- For example, this Cmm switch
--
--   switch [1 .. 10] _s2wn::I64 {
--       case 1 : goto c347;
--       case 2 : goto c348;
--       case 3 : goto c349;
--       case 4 : goto c34a;
--       case 5 : goto c34b;
--       case 6 : goto c34c;
--       case 7 : goto c34d;
--       case 8 : goto c34e;
--       case 9 : goto c34f;
--       case 10 : goto c34g;
--   }   // CmmSwitch
--
-- leads to this jump table in Assembly
--
--   .section .rodata
--           .balign 8
--   .Ln34G:
--           .quad   0
--           .quad   .Lc347-(.Ln34G)+0
--           .quad   .Lc348-(.Ln34G)+0
--           .quad   .Lc349-(.Ln34G)+0
--           .quad   .Lc34a-(.Ln34G)+0
--           .quad   .Lc34b-(.Ln34G)+0
--           .quad   .Lc34c-(.Ln34G)+0
--           .quad   .Lc34d-(.Ln34G)+0
--           .quad   .Lc34e-(.Ln34G)+0
--           .quad   .Lc34f-(.Ln34G)+0
--           .quad   .Lc34g-(.Ln34G)+0
--
-- and this indexing code where the jump should be done (register t0 contains
-- the index)
--
--           addi t0, t0, 0 // silly move (ignore it)
--           la t1, .Ln34G // load the table's address
--           sll t0, t0, 3 // index * 8 -> offset in bytes
--           add t0, t0, t1 // address of the table's entry
--           ld t0, 0(t0) // load entry
--           add t0, t0, t1 // relative to absolute address
--           jalr zero, t0, 0 // jump to the block
--
-- In object code (disassembled) the table looks like
--
--   0000000000000000 <.Ln34G>:
--        ...
--        8: R_RISCV_ADD64        .Lc347
--        8: R_RISCV_SUB64        .Ln34G
--        10: R_RISCV_ADD64       .Lc348
--        10: R_RISCV_SUB64       .Ln34G
--        18: R_RISCV_ADD64       .Lc349
--        18: R_RISCV_SUB64       .Ln34G
--        20: R_RISCV_ADD64       .Lc34a
--        20: R_RISCV_SUB64       .Ln34G
--        28: R_RISCV_ADD64       .Lc34b
--        28: R_RISCV_SUB64       .Ln34G
--        30: R_RISCV_ADD64       .Lc34c
--        30: R_RISCV_SUB64       .Ln34G
--        38: R_RISCV_ADD64       .Lc34d
--        38: R_RISCV_SUB64       .Ln34G
--        40: R_RISCV_ADD64       .Lc34e
--        40: R_RISCV_SUB64       .Ln34G
--        48: R_RISCV_ADD64       .Lc34f
--        48: R_RISCV_SUB64       .Ln34G
--        50: R_RISCV_ADD64       .Lc34g
--        50: R_RISCV_SUB64       .Ln34G
--
-- I.e. the relative offset calculations are done by the linker via relocations.
-- This seems to be PIC compatible; at least `scanelf` (pax-utils) does not
-- complain.


-- | Generate jump to jump table target
--
-- The index into the jump table is calulated by evaluating @expr@. The
-- corresponding table entry contains the relative address to jump to (relative
-- to the jump table's first entry / the table's own label).
genSwitch :: NCGConfig -> CmmExpr -> SwitchTargets -> NatM InstrBlock
genSwitch config expr targets = do
  (reg, fmt1, e_code) <- getSomeReg indexExpr
  let fmt = II64
  tmp <- getNewRegNat fmt
  lbl <- getNewLabelNat
  dynRef <- cmmMakeDynamicReference config DataReference lbl
  (tableReg, fmt2, t_code) <- getSomeReg dynRef
  let code =
        toOL
          [ COMMENT (text "indexExpr" <+> (text . show) indexExpr),
            COMMENT (text "dynRef" <+> (text . show) dynRef)
          ]
          `appOL` e_code
          `appOL` t_code
          `appOL` toOL
            [ COMMENT (ftext "Jump table for switch"),
              -- index to offset into the table (relative to tableReg)
              annExpr expr (SLL (OpReg (formatToWidth fmt1) reg) (OpReg (formatToWidth fmt1) reg) (OpImm (ImmInt 3))),
              -- calculate table entry address
              ADD (OpReg W64 tmp) (OpReg (formatToWidth fmt1) reg) (OpReg (formatToWidth fmt2) tableReg),
              -- load table entry (relative offset from tableReg (first entry) to target label)
              LDRU II64 (OpReg W64 tmp) (OpAddr (AddrRegImm tmp (ImmInt 0))),
              -- calculate absolute address of the target label
              ADD (OpReg W64 tmp) (OpReg W64 tmp) (OpReg W64 tableReg),
              -- prepare jump to target label
              J_TBL ids (Just lbl) tmp
            ]
  return code
  where
    -- See Note [Sub-word subtlety during jump-table indexing] in
    -- GHC.CmmToAsm.X86.CodeGen for why we must first offset, then widen.
    indexExpr0 = cmmOffset platform expr offset
    -- We widen to a native-width register to sanitize the high bits
    indexExpr =
      CmmMachOp
        (MO_UU_Conv expr_w (platformWordWidth platform))
        [indexExpr0]
    expr_w = cmmExprWidth platform expr
    (offset, ids) = switchTargetsToTable targets
    platform = ncgPlatform config

-- | Generate jump table data (if required)
--
-- The idea is to emit one table entry per case. The entry is the relative
-- address of the block to jump to (relative to the table's first entry /
-- table's own label.) The calculation itself is done by the linker.
generateJumpTableForInstr ::
  NCGConfig ->
  Instr ->
  Maybe (NatCmmDecl RawCmmStatics Instr)
generateJumpTableForInstr config (J_TBL ids (Just lbl) _) =
  let jumpTable =
        map jumpTableEntryRel ids
        where
          jumpTableEntryRel Nothing =
            CmmStaticLit (CmmInt 0 (ncgWordWidth config))
          jumpTableEntryRel (Just blockid) =
            CmmStaticLit
              ( CmmLabelDiffOff
                  blockLabel
                  lbl
                  0
                  (ncgWordWidth config)
              )
            where
              blockLabel = blockLbl blockid
   in Just (CmmData (Section ReadOnlyData lbl) (CmmStaticsRaw lbl jumpTable))
generateJumpTableForInstr _ _ = Nothing

-- -----------------------------------------------------------------------------
-- Top-level of the instruction selector

stmtsToInstrs ::
  -- | Cmm Statements
  [CmmNode O O] ->
  -- | Resulting instruction
  NatM InstrBlock
stmtsToInstrs stmts = concatOL <$> mapM stmtToInstrs stmts

stmtToInstrs ::
  CmmNode e x ->
  -- | Resulting instructions
  NatM InstrBlock
stmtToInstrs stmt = do
  config <- getConfig
  platform <- getPlatform
  case stmt of
    CmmUnsafeForeignCall target result_regs args ->
      genCCall target result_regs args
    CmmComment s -> pure (unitOL (COMMENT (ftext s)))
    CmmTick {} -> pure nilOL
    CmmAssign reg src
      | isFloatType ty -> assignReg_FltCode format reg src
      | otherwise -> assignReg_IntCode format reg src
      where
        ty = cmmRegType reg
        format = cmmTypeFormat ty
    CmmStore addr src _alignment
      | isFloatType ty -> assignMem_FltCode format addr src
      | otherwise -> assignMem_IntCode format addr src
      where
        ty = cmmExprType platform src
        format = cmmTypeFormat ty
    CmmBranch id -> genBranch id
    -- We try to arrange blocks such that the likely branch is the fallthrough
    -- in GHC.Cmm.ContFlowOpt. So we can assume the condition is likely false here.
    CmmCondBranch arg true false _prediction ->
      genCondBranch true false arg
    CmmSwitch arg ids -> genSwitch config arg ids
    CmmCall {cml_target = arg} -> genJump arg
    CmmUnwind _regs -> pure nilOL
    -- Intentionally not have a default case here: If anybody adds a
    -- constructor, the compiler should force them to think about this here.
    CmmForeignCall {} -> pprPanic "stmtToInstrs: statement should have been cps'd away" (pdoc platform stmt)
    CmmEntry {} -> pprPanic "stmtToInstrs: statement should have been cps'd away" (pdoc platform stmt)

--------------------------------------------------------------------------------

-- | 'InstrBlock's are the insn sequences generated by the insn selectors.
--
-- They are really trees of insns to facilitate fast appending, where a
-- left-to-right traversal yields the insns in the correct order.
type InstrBlock =
  OrdList Instr

-- | Register's passed up the tree.
--
-- If the stix code forces the register to live in a pre-decided machine
-- register, it comes out as @Fixed@; otherwise, it comes out as @Any@, and the
-- parent can decide which register to put it in.
data Register
  = Fixed Format Reg InstrBlock
  | Any Format (Reg -> InstrBlock)

-- | Sometimes we need to change the Format of a register. Primarily during
-- conversion.
swizzleRegisterRep :: Format -> Register -> Register
swizzleRegisterRep format' (Fixed _format reg code) = Fixed format' reg code
swizzleRegisterRep format' (Any _format codefn) = Any format' codefn

-- | Grab a `Reg` for a `CmmReg`
--
-- `LocalReg`s are assigned virtual registers (`RegVirtual`), `GlobalReg`s are
-- assigned real registers (`RegReal`). It is an error if a `GlobalReg` is not a
-- STG register.
getRegisterReg :: Platform -> CmmReg -> Reg
getRegisterReg _ (CmmLocal (LocalReg u pk)) =
  RegVirtual $ mkVirtualReg u (cmmTypeFormat pk)
getRegisterReg platform (CmmGlobal mid) =
  case globalRegMaybe platform (globalRegUseGlobalReg mid) of
    Just reg -> RegReal reg
    Nothing -> pprPanic "getRegisterReg-memory" (ppr $ CmmGlobal mid)

-- -----------------------------------------------------------------------------
-- General things for putting together code sequences

-- | Compute an expression into any register
getSomeReg :: CmmExpr -> NatM (Reg, Format, InstrBlock)
getSomeReg expr = do
  r <- getRegister expr
  case r of
    Any rep code -> do
      tmp <- getNewRegNat rep
      return (tmp, rep, code tmp)
    Fixed rep reg code ->
      return (reg, rep, code)

-- | Compute an expression into any floating-point register
--
-- If the initial expression is not a floating-point expression, finally move
-- the result into a floating-point register.
getFloatReg :: (HasCallStack) => CmmExpr -> NatM (Reg, Format, InstrBlock)
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

-- | Map `CmmLit` to `OpImm`
--
-- N.B. this is a partial function, because not all `CmmLit`s have an immediate
-- representation.
litToImm' :: CmmLit -> Operand
litToImm' = OpImm . litToImm

-- | Compute a `CmmExpr` into a `Register`
getRegister :: CmmExpr -> NatM Register
getRegister e = do
  config <- getConfig
  getRegister' config (ncgPlatform config) e

-- | The register width to be used for an operation on the given width
-- operand.
opRegWidth :: Width -> Width
opRegWidth W64 = W64
opRegWidth W32 = W32
opRegWidth W16 = W32
opRegWidth W8 = W32
opRegWidth w = pprPanic "opRegWidth" (text "Unsupported width" <+> ppr w)

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
--
-- Note [Handling PIC on RV64]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- RV64 does not have a special PIC register, the general approach is to simply
-- do PC-relative addressing or go through the GOT. There is assembly support
-- for both.
--
-- rv64 assembly has a `la` (load address) pseudo-instruction, that allows
-- loading a label's address into a register. The instruction is desugared into
-- different addressing modes, e.g. PC-relative addressing:
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
getRegister' config plat (CmmMachOp (MO_Add w0) [x, CmmLit (CmmInt i w1)])
  | i < 0 =
      getRegister' config plat (CmmMachOp (MO_Sub w0) [x, CmmLit (CmmInt (-i) w1)])
getRegister' config plat (CmmMachOp (MO_Sub w0) [x, CmmLit (CmmInt i w1)])
  | i < 0 =
      getRegister' config plat (CmmMachOp (MO_Add w0) [x, CmmLit (CmmInt (-i) w1)])
-- Generic case.
getRegister' config plat expr =
  case expr of
    CmmReg (CmmGlobal (GlobalRegUse PicBaseReg _)) ->
      -- See Note [Handling PIC on RV64]
      pprPanic "getRegister': There's no PIC base register on RISCV" (ppr PicBaseReg)
    CmmLit lit ->
      case lit of
        CmmInt 0 w -> pure $ Fixed (intFormat w) zeroReg nilOL
        CmmInt i w ->
          -- narrowU is important: Negative immediates may be
          -- sign-extended on load!
          let imm = OpImm . ImmInteger $ narrowU w i
           in pure (Any (intFormat w) (\dst -> unitOL $ annExpr expr (MOV (OpReg w dst) imm)))
        CmmFloat 0 w -> do
          let op = litToImm' lit
          pure (Any (floatFormat w) (\dst -> unitOL $ annExpr expr (MOV (OpReg w dst) op)))
        CmmFloat _f W8 -> pprPanic "getRegister' (CmmLit:CmmFloat), no support for bytes" (pdoc plat expr)
        CmmFloat _f W16 -> pprPanic "getRegister' (CmmLit:CmmFloat), no support for halfs" (pdoc plat expr)
        CmmFloat f W32 -> do
          let word = castFloatToWord32 (fromRational f) :: Word32
          tmp <- getNewRegNat (intFormat W32)
          return
            ( Any
                (floatFormat W32)
                ( \dst ->
                    toOL
                      [ annExpr expr
                          $ MOV (OpReg W32 tmp) (OpImm (ImmInteger (fromIntegral word))),
                        MOV (OpReg W32 dst) (OpReg W32 tmp)
                      ]
                )
            )
        CmmFloat f W64 -> do
          let word = castDoubleToWord64 (fromRational f) :: Word64
          tmp <- getNewRegNat (intFormat W64)
          return
            ( Any
                (floatFormat W64)
                ( \dst ->
                    toOL
                      [ annExpr expr
                          $ MOV (OpReg W64 tmp) (OpImm (ImmInteger (fromIntegral word))),
                        MOV (OpReg W64 dst) (OpReg W64 tmp)
                      ]
                )
            )
        CmmFloat _f _w -> pprPanic "getRegister' (CmmLit:CmmFloat), unsupported float lit" (pdoc plat expr)
        CmmVec _lits -> pprPanic "getRegister' (CmmLit:CmmVec): " (pdoc plat expr)
        CmmLabel lbl -> do
          let op = OpImm (ImmCLbl lbl)
              rep = cmmLitType plat lit
              format = cmmTypeFormat rep
          return (Any format (\dst -> unitOL $ annExpr expr (LDR format (OpReg (formatToWidth format) dst) op)))
        CmmLabelOff lbl off | isNbitEncodeable 12 (fromIntegral off) -> do
          let op = OpImm (ImmIndex lbl off)
              rep = cmmLitType plat lit
              format = cmmTypeFormat rep
          return (Any format (\dst -> unitOL $ LDR format (OpReg (formatToWidth format) dst) op))
        CmmLabelOff lbl off -> do
          let op = litToImm' (CmmLabel lbl)
              rep = cmmLitType plat lit
              format = cmmTypeFormat rep
              width = typeWidth rep
          (off_r, _off_format, off_code) <- getSomeReg $ CmmLit (CmmInt (fromIntegral off) width)
          return
            ( Any
                format
                ( \dst ->
                    off_code
                      `snocOL` LDR format (OpReg (formatToWidth format) dst) op
                      `snocOL` ADD (OpReg width dst) (OpReg width dst) (OpReg width off_r)
                )
            )
        CmmLabelDiffOff {} -> pprPanic "getRegister' (CmmLit:CmmLabelOff): " (pdoc plat expr)
        CmmBlock _ -> pprPanic "getRegister' (CmmLit:CmmLabelOff): " (pdoc plat expr)
        CmmHighStackMark -> pprPanic "getRegister' (CmmLit:CmmLabelOff): " (pdoc plat expr)
    CmmLoad mem rep _ -> do
      let format = cmmTypeFormat rep
          width = typeWidth rep
      Amode addr addr_code <- getAmode plat width mem
      case width of
        w
          | w <= W64 ->
              -- Load without sign-extension. See Note [Signed arithmetic on RISCV64]
              pure
                ( Any
                    format
                    ( \dst ->
                        addr_code
                          `snocOL` LDRU format (OpReg width dst) (OpAddr addr)
                    )
                )
        _ ->
          pprPanic ("Width too big! Cannot load: " ++ show width) (pdoc plat expr)
    CmmStackSlot _ _ ->
      pprPanic "getRegister' (CmmStackSlot): " (pdoc plat expr)
    CmmReg reg ->
      return
        ( Fixed
            (cmmTypeFormat (cmmRegType reg))
            (getRegisterReg plat reg)
            nilOL
        )
    CmmRegOff reg off | isNbitEncodeable 12 (fromIntegral off) -> do
      getRegister' config plat
        $ CmmMachOp (MO_Add width) [CmmReg reg, CmmLit (CmmInt (fromIntegral off) width)]
      where
        width = typeWidth (cmmRegType reg)
    CmmRegOff reg off -> do
      (off_r, _off_format, off_code) <- getSomeReg $ CmmLit (CmmInt (fromIntegral off) width)
      (reg, _format, code) <- getSomeReg $ CmmReg reg
      return
        $ Any
          (intFormat width)
          ( \dst ->
              off_code
                `appOL` code
                `snocOL` ADD (OpReg width dst) (OpReg width reg) (OpReg width off_r)
          )
      where
        width = typeWidth (cmmRegType reg)

    -- Handle MO_RelaxedRead as a normal CmmLoad, to allow
    -- non-trivial addressing modes to be used.
    CmmMachOp (MO_RelaxedRead w) [e] ->
      getRegister (CmmLoad e (cmmBits w) NaturallyAligned)
    -- for MachOps, see GHC.Cmm.MachOp
    -- For CmmMachOp, see GHC.Cmm.Expr
    CmmMachOp op [e] -> do
      (reg, _format, code) <- getSomeReg e
      case op of
        MO_Not w -> return $ Any (intFormat w) $ \dst ->
          let w' = opRegWidth w
           in code
                `snocOL`
                -- pseudo instruction `not` is `xori rd, rs, -1`
                ann (text "not") (XORI (OpReg w' dst) (OpReg w' reg) (OpImm (ImmInt (-1))))
                `appOL` truncateReg w' w dst -- See Note [Signed arithmetic on RISCV64]
        MO_S_Neg w -> negate code w reg
        MO_F_Neg w ->
          return
            $ Any
              (floatFormat w)
              ( \dst ->
                  code
                    `snocOL` NEG (OpReg w dst) (OpReg w reg)
              )
        -- TODO: Can this case happen?
        MO_SF_Round from to | from < W32 -> do
          -- extend to the smallest available representation
          (reg_x, code_x) <- signExtendReg from W32 reg
          pure
            $ Any
              (floatFormat to)
              ( \dst ->
                  code
                    `appOL` code_x
                    `snocOL` annExpr expr (FCVT IntToFloat (OpReg to dst) (OpReg from reg_x)) -- (Signed ConVerT Float)
              )
        MO_SF_Round from to ->
          pure
            $ Any
              (floatFormat to)
              ( \dst ->
                  code
                    `snocOL` annExpr expr (FCVT IntToFloat (OpReg to dst) (OpReg from reg)) -- (Signed ConVerT Float)
              )
        -- TODO: Can this case happen?
        MO_FS_Truncate from to
          | to < W32 ->
              pure
                $ Any
                  (intFormat to)
                  ( \dst ->
                      code
                        `snocOL`
                        -- W32 is the smallest width to convert to. Decrease width afterwards.
                        annExpr expr (FCVT FloatToInt (OpReg W32 dst) (OpReg from reg))
                        `appOL` signExtendAdjustPrecission W32 to dst dst -- (float convert (-> zero) signed)
                  )
        MO_FS_Truncate from to ->
          pure
            $ Any
              (intFormat to)
              ( \dst ->
                  code
                    `snocOL` annExpr expr (FCVT FloatToInt (OpReg to dst) (OpReg from reg))
                    `appOL` truncateReg from to dst -- (float convert (-> zero) signed)
              )
        MO_UU_Conv from to
          | from <= to ->
              pure
                $ Any
                  (intFormat to)
                  ( \dst ->
                      code
                        `snocOL` annExpr e (MOV (OpReg to dst) (OpReg from reg))
                  )
        MO_UU_Conv from to ->
          pure
            $ Any
              (intFormat to)
              ( \dst ->
                  code
                    `snocOL` annExpr e (MOV (OpReg from dst) (OpReg from reg))
                    `appOL` truncateReg from to dst
              )
        MO_SS_Conv from to -> ss_conv from to reg code
        MO_FF_Conv from to -> return $ Any (floatFormat to) (\dst -> code `snocOL` annExpr e (FCVT FloatToFloat (OpReg to dst) (OpReg from reg)))
        MO_WF_Bitcast w    -> return $ Any (floatFormat w)  (\dst -> code `snocOL` MOV (OpReg w dst) (OpReg w reg))
        MO_FW_Bitcast w    -> return $ Any (intFormat w)    (\dst -> code `snocOL` MOV (OpReg w dst) (OpReg w reg))

        -- Conversions
        -- TODO: Duplication with MO_UU_Conv
        MO_XX_Conv from to
          | to < from ->
              pure
                $ Any
                  (intFormat to)
                  ( \dst ->
                      code
                        `snocOL` annExpr e (MOV (OpReg from dst) (OpReg from reg))
                        `appOL` truncateReg from to dst
                  )
        MO_XX_Conv _from to -> swizzleRegisterRep (intFormat to) <$> getRegister e
        MO_AlignmentCheck align wordWidth -> do
          reg <- getRegister' config plat e
          addAlignmentCheck align wordWidth reg
        x -> pprPanic ("getRegister' (monadic CmmMachOp): " ++ show x) (pdoc plat expr)
      where
        -- In the case of 16- or 8-bit values we need to sign-extend to 32-bits
        -- See Note [Signed arithmetic on RISCV64].
        negate code w reg = do
          let w' = opRegWidth w
          (reg', code_sx) <- signExtendReg w w' reg
          return $ Any (intFormat w) $ \dst ->
            code
              `appOL` code_sx
              `snocOL` NEG (OpReg w' dst) (OpReg w' reg')
              `appOL` truncateReg w' w dst

        ss_conv from to reg code
          | from < to = do
              pure $ Any (intFormat to) $ \dst ->
                code
                  `appOL` signExtend from to reg dst
                  `appOL` truncateReg from to dst
          | from > to =
              pure $ Any (intFormat to) $ \dst ->
                code
                  `appOL` toOL
                    [ ann
                        (text "MO_SS_Conv: narrow register signed" <+> ppr reg <+> ppr from <> text "->" <> ppr to)
                        (SLL (OpReg to dst) (OpReg from reg) (OpImm (ImmInt shift))),
                      -- signed right shift
                      SRA (OpReg to dst) (OpReg to dst) (OpImm (ImmInt shift))
                    ]
                  `appOL` truncateReg from to dst
          | otherwise =
              -- No conversion necessary: Just copy.
              pure $ Any (intFormat from) $ \dst ->
                code `snocOL` MOV (OpReg from dst) (OpReg from reg)
          where
            shift = 64 - (widthInBits from - widthInBits to)

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
    CmmMachOp (MO_Add w) [CmmReg reg, CmmLit (CmmInt n _)]
      | fitsIn12bitImm n -> return $ Any (intFormat w) (\d -> unitOL $ annExpr expr (ADD (OpReg w d) (OpReg w' r') (OpImm (ImmInteger n))))
      where
        -- TODO: 12bits lsl #12; e.g. lower 12 bits of n are 0; shift n >> 12, and set lsl to #12.
        w' = formatToWidth (cmmTypeFormat (cmmRegType reg))
        r' = getRegisterReg plat reg
    CmmMachOp (MO_Sub w) [CmmReg reg, CmmLit (CmmInt n _)]
      | fitsIn12bitImm n -> return $ Any (intFormat w) (\d -> unitOL $ annExpr expr (SUB (OpReg w d) (OpReg w' r') (OpImm (ImmInteger n))))
      where
        -- TODO: 12bits lsl #12; e.g. lower 12 bits of n are 0; shift n >> 12, and set lsl to #12.
        w' = formatToWidth (cmmTypeFormat (cmmRegType reg))
        r' = getRegisterReg plat reg
    CmmMachOp (MO_U_Quot w) [x, y] | w == W8 || w == W16 -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      (reg_y, format_y, code_y) <- getSomeReg y
      return
        $ Any
          (intFormat w)
          ( \dst ->
              code_x
                `appOL` truncateReg (formatToWidth format_x) w reg_x
                `appOL` code_y
                `appOL` truncateReg (formatToWidth format_y) w reg_y
                `snocOL` annExpr expr (DIVU (OpReg w dst) (OpReg w reg_x) (OpReg w reg_y))
          )

    -- 2. Shifts. x << n, x >> n.
    CmmMachOp (MO_Shl w) [x, CmmLit (CmmInt n _)]
      | w == W32,
        0 <= n,
        n < 32 -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          return
            $ Any
              (intFormat w)
              ( \dst ->
                  code_x
                    `snocOL` annExpr expr (SLL (OpReg w dst) (OpReg w reg_x) (OpImm (ImmInteger n)))
                    `appOL` truncateReg w w dst
              )
    CmmMachOp (MO_Shl w) [x, CmmLit (CmmInt n _)]
      | w == W64,
        0 <= n,
        n < 64 -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          return
            $ Any
              (intFormat w)
              ( \dst ->
                  code_x
                    `snocOL` annExpr expr (SLL (OpReg w dst) (OpReg w reg_x) (OpImm (ImmInteger n)))
                    `appOL` truncateReg w w dst
              )
    CmmMachOp (MO_S_Shr w) [x, CmmLit (CmmInt n _)] | fitsIn12bitImm n -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      (reg_x', code_x') <- signExtendReg (formatToWidth format_x) w reg_x
      return
        $ Any
          (intFormat w)
          ( \dst ->
              code_x
                `appOL` code_x'
                `snocOL` annExpr expr (SRA (OpReg w dst) (OpReg w reg_x') (OpImm (ImmInteger n)))
          )
    CmmMachOp (MO_S_Shr w) [x, y] -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      (reg_y, _format_y, code_y) <- getSomeReg y
      (reg_x', code_x') <- signExtendReg (formatToWidth format_x) w reg_x
      return
        $ Any
          (intFormat w)
          ( \dst ->
              code_x
                `appOL` code_x'
                `appOL` code_y
                `snocOL` annExpr expr (SRA (OpReg w dst) (OpReg w reg_x') (OpReg w reg_y))
          )
    CmmMachOp (MO_U_Shr w) [x, CmmLit (CmmInt n _)]
      | w == W8,
        0 <= n,
        n < 8 -> do
          (reg_x, format_x, code_x) <- getSomeReg x
          return
            $ Any
              (intFormat w)
              ( \dst ->
                  code_x
                    `appOL` truncateReg (formatToWidth format_x) w reg_x
                    `snocOL` annExpr expr (SRL (OpReg w dst) (OpReg w reg_x) (OpImm (ImmInteger n)))
              )
    CmmMachOp (MO_U_Shr w) [x, CmmLit (CmmInt n _)]
      | w == W16,
        0 <= n,
        n < 16 -> do
          (reg_x, format_x, code_x) <- getSomeReg x
          return
            $ Any
              (intFormat w)
              ( \dst ->
                  code_x
                    `appOL` truncateReg (formatToWidth format_x) w reg_x
                    `snocOL` annExpr expr (SRL (OpReg w dst) (OpReg w reg_x) (OpImm (ImmInteger n)))
              )
    CmmMachOp (MO_U_Shr w) [x, y] | w == W8 || w == W16 -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      (reg_y, _format_y, code_y) <- getSomeReg y
      return
        $ Any
          (intFormat w)
          ( \dst ->
              code_x
                `appOL` code_y
                `appOL` truncateReg (formatToWidth format_x) w reg_x
                `snocOL` annExpr expr (SRL (OpReg w dst) (OpReg w reg_x) (OpReg w reg_y))
          )
    CmmMachOp (MO_U_Shr w) [x, CmmLit (CmmInt n _)]
      | w == W32,
        0 <= n,
        n < 32 -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          return
            $ Any
              (intFormat w)
              ( \dst ->
                  code_x
                    `snocOL` annExpr expr (SRL (OpReg w dst) (OpReg w reg_x) (OpImm (ImmInteger n)))
              )
    CmmMachOp (MO_U_Shr w) [x, CmmLit (CmmInt n _)]
      | w == W64,
        0 <= n,
        n < 64 -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          return
            $ Any
              (intFormat w)
              ( \dst ->
                  code_x
                    `snocOL` annExpr expr (SRL (OpReg w dst) (OpReg w reg_x) (OpImm (ImmInteger n)))
              )

    -- 3. Logic &&, ||
    CmmMachOp (MO_And w) [CmmReg reg, CmmLit (CmmInt n _)]
      | fitsIn12bitImm n ->
          return $ Any (intFormat w) (\d -> unitOL $ annExpr expr (AND (OpReg w d) (OpReg w' r') (OpImm (ImmInteger n))))
      where
        w' = formatToWidth (cmmTypeFormat (cmmRegType reg))
        r' = getRegisterReg plat reg
    CmmMachOp (MO_Or w) [CmmReg reg, CmmLit (CmmInt n _)]
      | fitsIn12bitImm n ->
          return $ Any (intFormat w) (\d -> unitOL $ annExpr expr (ORI (OpReg w d) (OpReg w' r') (OpImm (ImmInteger n))))
      where
        w' = formatToWidth (cmmTypeFormat (cmmRegType reg))
        r' = getRegisterReg plat reg

    -- Generic binary case.
    CmmMachOp op [x, y] -> do
      let -- A "plain" operation.
          bitOp w op = do
            -- compute x<m> <- x
            -- compute x<o> <- y
            -- <OP> x<n>, x<m>, x<o>
            (reg_x, format_x, code_x) <- getSomeReg x
            (reg_y, format_y, code_y) <- getSomeReg y
            massertPpr (isIntFormat format_x == isIntFormat format_y) $ text "bitOp: incompatible"
            return
              $ Any
                (intFormat w)
                ( \dst ->
                    code_x
                      `appOL` code_y
                      `appOL` op (OpReg w dst) (OpReg w reg_x) (OpReg w reg_y)
                )

          -- A (potentially signed) integer operation.
          -- In the case of 8- and 16-bit signed arithmetic we must first
          -- sign-extend both arguments to 32-bits.
          -- See Note [Signed arithmetic on RISCV64].
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
                  | not is_signed = return (r, nilOL)
                  | otherwise = signExtendReg w w' r
            (reg_x_sx, code_x_sx) <- signExt reg_x
            (reg_y_sx, code_y_sx) <- signExt reg_y
            return $ Any (intFormat w) $ \dst ->
              code_x
                `appOL` code_y
                `appOL`
                -- sign-extend both operands
                code_x_sx
                `appOL` code_y_sx
                `appOL` op (OpReg w' dst) (OpReg w' reg_x_sx) (OpReg w' reg_y_sx)
                `appOL` truncateReg w' w dst -- truncate back to the operand's original width
          floatOp w op = do
            (reg_fx, format_x, code_fx) <- getFloatReg x
            (reg_fy, format_y, code_fy) <- getFloatReg y
            massertPpr (isFloatFormat format_x && isFloatFormat format_y) $ text "floatOp: non-float"
            return
              $ Any
                (floatFormat w)
                ( \dst ->
                    code_fx
                      `appOL` code_fy
                      `appOL` op (OpReg w dst) (OpReg w reg_fx) (OpReg w reg_fy)
                )

          -- need a special one for conditionals, as they return ints
          floatCond w op = do
            (reg_fx, format_x, code_fx) <- getFloatReg x
            (reg_fy, format_y, code_fy) <- getFloatReg y
            massertPpr (isFloatFormat format_x && isFloatFormat format_y) $ text "floatCond: non-float"
            return
              $ Any
                (intFormat w)
                ( \dst ->
                    code_fx
                      `appOL` code_fy
                      `appOL` op (OpReg w dst) (OpReg w reg_fx) (OpReg w reg_fy)
                )

      case op of
        -- Integer operations
        -- Add/Sub should only be Integer Options.
        MO_Add w -> intOp False w (\d x y -> unitOL $ annExpr expr (ADD d x y))
        -- TODO: Handle sub-word case
        MO_Sub w -> intOp False w (\d x y -> unitOL $ annExpr expr (SUB d x y))
        -- N.B. We needn't sign-extend sub-word size (in)equality comparisons
        -- since we don't care about ordering.
        MO_Eq w -> bitOp w (\d x y -> unitOL $ annExpr expr (CSET d x y EQ))
        MO_Ne w -> bitOp w (\d x y -> unitOL $ annExpr expr (CSET d x y NE))
        -- Signed multiply/divide
        MO_Mul w -> intOp True w (\d x y -> unitOL $ annExpr expr (MUL d x y))
        MO_S_MulMayOflo w -> do_mul_may_oflo w x y
        MO_S_Quot w -> intOp True w (\d x y -> unitOL $ annExpr expr (DIV d x y))
        MO_S_Rem w -> intOp True w (\d x y -> unitOL $ annExpr expr (REM d x y))
        -- Unsigned multiply/divide
        MO_U_Quot w -> intOp False w (\d x y -> unitOL $ annExpr expr (DIVU d x y))
        MO_U_Rem w -> intOp False w (\d x y -> unitOL $ annExpr expr (REMU d x y))
        -- Signed comparisons
        MO_S_Ge w -> intOp True w (\d x y -> unitOL $ annExpr expr (CSET d x y SGE))
        MO_S_Le w -> intOp True w (\d x y -> unitOL $ annExpr expr (CSET d x y SLE))
        MO_S_Gt w -> intOp True w (\d x y -> unitOL $ annExpr expr (CSET d x y SGT))
        MO_S_Lt w -> intOp True w (\d x y -> unitOL $ annExpr expr (CSET d x y SLT))
        -- Unsigned comparisons
        MO_U_Ge w -> intOp False w (\d x y -> unitOL $ annExpr expr (CSET d x y UGE))
        MO_U_Le w -> intOp False w (\d x y -> unitOL $ annExpr expr (CSET d x y ULE))
        MO_U_Gt w -> intOp False w (\d x y -> unitOL $ annExpr expr (CSET d x y UGT))
        MO_U_Lt w -> intOp False w (\d x y -> unitOL $ annExpr expr (CSET d x y ULT))
        -- Floating point arithmetic
        MO_F_Add w -> floatOp w (\d x y -> unitOL $ annExpr expr (ADD d x y))
        MO_F_Sub w -> floatOp w (\d x y -> unitOL $ annExpr expr (SUB d x y))
        MO_F_Mul w -> floatOp w (\d x y -> unitOL $ annExpr expr (MUL d x y))
        MO_F_Quot w -> floatOp w (\d x y -> unitOL $ annExpr expr (DIV d x y))
        -- Floating point comparison
        MO_F_Eq w -> floatCond w (\d x y -> unitOL $ annExpr expr (CSET d x y EQ))
        MO_F_Ne w -> floatCond w (\d x y -> unitOL $ annExpr expr (CSET d x y NE))
        MO_F_Ge w -> floatCond w (\d x y -> unitOL $ annExpr expr (CSET d x y FGE))
        MO_F_Le w -> floatCond w (\d x y -> unitOL $ annExpr expr (CSET d x y FLE)) -- x <= y <=> y > x
        MO_F_Gt w -> floatCond w (\d x y -> unitOL $ annExpr expr (CSET d x y FGT))
        MO_F_Lt w -> floatCond w (\d x y -> unitOL $ annExpr expr (CSET d x y FLT)) -- x < y <=> y >= x

        -- Bitwise operations
        MO_And w -> bitOp w (\d x y -> unitOL $ annExpr expr (AND d x y))
        MO_Or w -> bitOp w (\d x y -> unitOL $ annExpr expr (OR d x y))
        MO_Xor w -> bitOp w (\d x y -> unitOL $ annExpr expr (XOR d x y))
        MO_Shl w -> intOp False w (\d x y -> unitOL $ annExpr expr (SLL d x y))
        MO_U_Shr w -> intOp False w (\d x y -> unitOL $ annExpr expr (SRL d x y))
        MO_S_Shr w -> intOp True w (\d x y -> unitOL $ annExpr expr (SRA d x y))
        op -> pprPanic "getRegister' (unhandled dyadic CmmMachOp): " $ pprMachOp op <+> text "in" <+> pdoc plat expr

    -- Generic ternary case.
    CmmMachOp op [x, y, z] ->
      case op of
        -- Floating-point fused multiply-add operations
        --
        -- x86 fmadd    x * y + z <=> RISCV64 fmadd : d =   r1 * r2 + r3
        -- x86 fmsub    x * y - z <=> RISCV64 fnmsub: d =   r1 * r2 - r3
        -- x86 fnmadd - x * y + z <=> RISCV64 fmsub : d = - r1 * r2 + r3
        -- x86 fnmsub - x * y - z <=> RISCV64 fnmadd: d = - r1 * r2 - r3
        MO_FMA var w -> case var of
          FMAdd -> float3Op w (\d n m a -> unitOL $ FMA FMAdd d n m a)
          FMSub -> float3Op w (\d n m a -> unitOL $ FMA FMSub d n m a)
          FNMAdd -> float3Op w (\d n m a -> unitOL $ FMA FNMSub d n m a)
          FNMSub -> float3Op w (\d n m a -> unitOL $ FMA FNMAdd d n m a)
        _ ->
          pprPanic "getRegister' (unhandled ternary CmmMachOp): "
            $ pprMachOp op
            <+> text "in"
            <+> pdoc plat expr
      where
        float3Op w op = do
          (reg_fx, format_x, code_fx) <- getFloatReg x
          (reg_fy, format_y, code_fy) <- getFloatReg y
          (reg_fz, format_z, code_fz) <- getFloatReg z
          massertPpr (isFloatFormat format_x && isFloatFormat format_y && isFloatFormat format_z)
            $ text "float3Op: non-float"
          pure
            $ Any (floatFormat w)
            $ \dst ->
              code_fx
                `appOL` code_fy
                `appOL` code_fz
                `appOL` op (OpReg w dst) (OpReg w reg_fx) (OpReg w reg_fy) (OpReg w reg_fz)
    CmmMachOp _op _xs ->
      pprPanic "getRegister' (variadic CmmMachOp): " (pdoc plat expr)
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
      pure
        $ Any
          (intFormat w)
          ( \dst ->
              code_x
                `appOL` signExtend (formatToWidth format_x) W64 reg_x reg_x
                `appOL` code_y
                `appOL` signExtend (formatToWidth format_y) W64 reg_y reg_y
                `appOL` toOL
                  [ annExpr expr (MULH (OpReg w hi) (OpReg w reg_x) (OpReg w reg_y)),
                    MUL (OpReg w lo) (OpReg w reg_x) (OpReg w reg_y),
                    SRA (OpReg w lo) (OpReg w lo) (OpImm (ImmInt (widthInBits W64 - 1))),
                    ann
                      (text "Set flag if result of MULH contains more than sign bits.")
                      (XOR (OpReg w hi) (OpReg w hi) (OpReg w lo)),
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
          pure
            $ Any
              (intFormat w)
              ( \dst ->
                  -- 8bit * 8bit cannot overflow 16bit
                  -- 16bit * 16bit cannot overflow 32bit
                  -- 32bit * 32bit cannot overflow 64bit
                  unitOL $ annExpr expr (ADD (OpReg w dst) zero (OpImm (ImmInt 0)))
              )
        else do
          let use32BitMul = w <= W32 && width_x <= W32 && width_y <= W32
              nonSense = OpImm (ImmInt 0)
          if use32BitMul
            then do
              narrowedReg <- getNewRegNat II64
              pure
                $ Any
                  (intFormat w)
                  ( \dst ->
                      code_x
                        `appOL` signExtend (formatToWidth format_x) W32 reg_x reg_x
                        `appOL` code_y
                        `appOL` signExtend (formatToWidth format_y) W32 reg_y reg_y
                        `snocOL` annExpr expr (MUL (OpReg W32 dst) (OpReg W32 reg_x) (OpReg W32 reg_y))
                        `appOL` signExtendAdjustPrecission W32 w dst narrowedReg
                        `appOL` toOL
                          [ ann
                              (text "Check if the multiplied value fits in the narrowed register")
                              (SUB (OpReg w dst) (OpReg w dst) (OpReg w narrowedReg)),
                            CSET (OpReg w dst) (OpReg w dst) nonSense NE
                          ]
                  )
            else
              pure
                $ Any
                  (intFormat w)
                  ( \dst ->
                      -- Do not handle this unlikely case. Just tell that it may overflow.
                      unitOL $ annExpr expr (ADD (OpReg w dst) zero (OpImm (ImmInt 1)))
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
      unitOL
        $ ann
          (text "sign-extend register (SEXT.W)" <+> ppr r <+> ppr w <> text "->" <> ppr w')
          -- `ADDIW r r 0` is the pseudo-op SEXT.W
          (ADD (OpReg w' r') (OpReg w r) (OpImm (ImmInt 0)))
signExtend w w' r r' =
  toOL
    [ ann
        (text "narrow register signed" <+> ppr r <> char ':' <> ppr w <> text "->" <> ppr r <> char ':' <> ppr w')
        (SLL (OpReg w' r') (OpReg w r) (OpImm (ImmInt shift))),
      -- signed (arithmetic) right shift
      SRA (OpReg w' r') (OpReg w' r') (OpImm (ImmInt shift))
    ]
  where
    shift = 64 - widthInBits w

-- | Sign extends to 64bit, if needed and reduces the precission to the target `Width` (@w'@)
--
-- Source `Reg` @r@ stays untouched, while the conversion happens on destination
-- `Reg` @r'@.
signExtendAdjustPrecission :: Width -> Width -> Reg -> Reg -> OrdList Instr
signExtendAdjustPrecission w w' _r _r' | w > W64 || w' > W64 = pprPanic "Unexpected width (max is 64bit):" $ ppr w <> text "->" <+> ppr w'
signExtendAdjustPrecission w w' r r' | w == W64 && w' == W64 && r == r' = nilOL
signExtendAdjustPrecission w w' r r' | w == W64 && w' == W64 = unitOL $ MOV (OpReg w' r') (OpReg w r)
signExtendAdjustPrecission w w' r r'
  | w == W32 && w' == W64 =
      unitOL
        $ ann
          (text "sign-extend register (SEXT.W)" <+> ppr r <+> ppr w <> text "->" <> ppr w')
          -- `ADDIW r r 0` is the pseudo-op SEXT.W
          (ADD (OpReg w' r') (OpReg w r) (OpImm (ImmInt 0)))
signExtendAdjustPrecission w w' r r'
  | w > w' =
      toOL
        [ ann
            (text "narrow register signed" <+> ppr r <> char ':' <> ppr w <> text "->" <> ppr r <> char ':' <> ppr w')
            (SLL (OpReg w' r') (OpReg w r) (OpImm (ImmInt shift))),
          -- signed (arithmetic) right shift
          SRA (OpReg w' r') (OpReg w' r') (OpImm (ImmInt shift))
        ]
  where
    shift = 64 - widthInBits w'
signExtendAdjustPrecission w w' r r' =
  toOL
    [ ann
        (text "sign extend register" <+> ppr r <> char ':' <> ppr w <> text "->" <> ppr r <> char ':' <> ppr w')
        (SLL (OpReg w' r') (OpReg w r) (OpImm (ImmInt shift))),
      -- signed (arithmetic) right shift
      SRA (OpReg w' r') (OpReg w' r') (OpImm (ImmInt shift))
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
        (SLL (OpReg w' r) (OpReg w r) (OpImm (ImmInt shift))),
      -- SHL ignores signedness!
      SRL (OpReg w' r) (OpReg w r) (OpImm (ImmInt shift))
    ]
  where
    shift = 64 - widthInBits w'

-- | Given a 'Register', produce a new 'Register' with an instruction block
-- which will check the value for alignment. Used for @-falignment-sanitisation@.
addAlignmentCheck :: Int -> Width -> Register -> NatM Register
addAlignmentCheck align wordWidth reg = do
  jumpReg <- getNewRegNat II64
  cmpReg <- getNewRegNat II64
  okayLblId <- getBlockIdNat

  pure $ case reg of
    Fixed fmt reg code -> Fixed fmt reg (code `appOL` check fmt jumpReg cmpReg okayLblId reg)
    Any fmt f -> Any fmt (\reg -> f reg `appOL` check fmt jumpReg cmpReg okayLblId reg)
  where
    check :: Format -> Reg -> Reg -> BlockId -> Reg -> InstrBlock
    check fmt jumpReg cmpReg okayLblId reg =
      let width = formatToWidth fmt
       in assert (not $ isFloatFormat fmt)
            $ toOL
              [ ann
                  (text "Alignment check - alignment: " <> int align <> text ", word width: " <> text (show wordWidth))
                  (AND (OpReg width cmpReg) (OpReg width reg) (OpImm $ ImmInt $ align - 1)),
                BCOND EQ (OpReg width cmpReg) zero (TBlock okayLblId),
                COMMENT (text "Alignment check failed"),
                LDR II64 (OpReg W64 jumpReg) (OpImm $ ImmCLbl mkBadAlignmentLabel),
                B (TReg jumpReg),
                NEWBLOCK okayLblId
              ]

-- -----------------------------------------------------------------------------
--  The 'Amode' type: Memory addressing modes passed up the tree.
data Amode = Amode AddrMode InstrBlock

-- | Provide the value of a `CmmExpr` with an `Amode`
--
-- N.B. this function should be used to provide operands to load and store
-- instructions with signed 12bit wide immediates (S & I types). For other
-- immediate sizes and formats (e.g. B type uses multiples of 2) this function
-- would need to be adjusted.
getAmode ::
  Platform ->
  -- | width of loaded value
  Width ->
  CmmExpr ->
  NatM Amode
-- TODO: Specialize stuff we can destructure here.

-- LDR/STR: Immediate can be represented with 12bits
getAmode platform w (CmmRegOff reg off)
  | w <= W64,
    fitsIn12bitImm off =
      return $ Amode (AddrRegImm reg' off') nilOL
  where
    reg' = getRegisterReg platform reg
    off' = ImmInt off

-- For Stores we often see something like this:
-- CmmStore (CmmMachOp (MO_Add w) [CmmLoad expr, CmmLit (CmmInt n w')]) (expr2)
-- E.g. a CmmStoreOff really. This can be translated to `str $expr2, [$expr, #n ]
-- for `n` in range.
getAmode _platform _ (CmmMachOp (MO_Add _w) [expr, CmmLit (CmmInt off _w')])
  | fitsIn12bitImm off =
      do
        (reg, _format, code) <- getSomeReg expr
        return $ Amode (AddrRegImm reg (ImmInteger off)) code
getAmode _platform _ (CmmMachOp (MO_Sub _w) [expr, CmmLit (CmmInt off _w')])
  | fitsIn12bitImm (-off) =
      do
        (reg, _format, code) <- getSomeReg expr
        return $ Amode (AddrRegImm reg (ImmInteger (-off))) code

-- Generic case
getAmode _platform _ expr =
  do
    (reg, _format, code) <- getSomeReg expr
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
assignReg_IntCode :: Format -> CmmReg -> CmmExpr -> NatM InstrBlock
assignMem_FltCode :: Format -> CmmExpr -> CmmExpr -> NatM InstrBlock
assignReg_FltCode :: Format -> CmmReg -> CmmExpr -> NatM InstrBlock
assignMem_IntCode rep addrE srcE =
  do
    (src_reg, _format, code) <- getSomeReg srcE
    platform <- getPlatform
    let w = formatToWidth rep
    Amode addr addr_code <- getAmode platform w addrE
    return $ COMMENT (text "CmmStore" <+> parens (text (show addrE)) <+> parens (text (show srcE)))
      `consOL` ( code
                   `appOL` addr_code
                   `snocOL` STR rep (OpReg w src_reg) (OpAddr addr)
               )

assignReg_IntCode _ reg src =
  do
    platform <- getPlatform
    let dst = getRegisterReg platform reg
    r <- getRegister src
    return $ case r of
      Any _ code ->
        COMMENT (text "CmmAssign" <+> parens (text (show reg)) <+> parens (text (show src)))
          `consOL` code dst
      Fixed format freg fcode ->
        COMMENT (text "CmmAssign" <+> parens (text (show reg)) <+> parens (text (show src)))
          `consOL` ( fcode
                       `snocOL` MOV (OpReg (formatToWidth format) dst) (OpReg (formatToWidth format) freg)
                   )

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
genJump :: CmmExpr {-the branch target-} -> NatM InstrBlock
genJump expr = do
  (target, _format, code) <- getSomeReg expr
  return (code `appOL` unitOL (annExpr expr (B (TReg target))))

-- -----------------------------------------------------------------------------
--  Unconditional branches
genBranch :: BlockId -> NatM InstrBlock
genBranch = return . toOL . mkJumpInstr

-- -----------------------------------------------------------------------------
-- Conditional branches
genCondJump ::
  BlockId ->
  CmmExpr ->
  NatM InstrBlock
genCondJump bid expr = do
  case expr of
    -- Optimized == 0 case.
    CmmMachOp (MO_Eq w) [x, CmmLit (CmmInt 0 _)] -> do
      (reg_x, _format_x, code_x) <- getSomeReg x
      return $ code_x `snocOL` annExpr expr (BCOND EQ zero (OpReg w reg_x) (TBlock bid))

    -- Optimized /= 0 case.
    CmmMachOp (MO_Ne w) [x, CmmLit (CmmInt 0 _)] -> do
      (reg_x, _format_x, code_x) <- getSomeReg x
      return $ code_x `snocOL` annExpr expr (BCOND NE zero (OpReg w reg_x) (TBlock bid))

    -- Generic case.
    CmmMachOp mop [x, y] -> do
      let ubcond w cmp = do
            -- compute both sides.
            (reg_x, format_x, code_x) <- getSomeReg x
            (reg_y, format_y, code_y) <- getSomeReg y
            let x' = OpReg w reg_x
                y' = OpReg w reg_y
            return $ case w of
              w
                | w == W8 || w == W16 ->
                    code_x
                      `appOL` truncateReg (formatToWidth format_x) w reg_x
                      `appOL` code_y
                      `appOL` truncateReg (formatToWidth format_y) w reg_y
                      `appOL` code_y
                      `snocOL` annExpr expr (BCOND cmp x' y' (TBlock bid))
              _ ->
                code_x
                  `appOL` code_y
                  `snocOL` annExpr expr (BCOND cmp x' y' (TBlock bid))

          sbcond w cmp = do
            -- compute both sides.
            (reg_x, format_x, code_x) <- getSomeReg x
            (reg_y, format_y, code_y) <- getSomeReg y
            let x' = OpReg w reg_x
                y' = OpReg w reg_y
            return $ case w of
              w
                | w `elem` [W8, W16, W32] ->
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
            oneReg <- getNewRegNat II64
            return $ code_fx
              `appOL` code_fy
              `snocOL` annExpr expr (CSET ip (OpReg w reg_fx) (OpReg w reg_fy) cmp)
              `snocOL` MOV (OpReg W64 oneReg) (OpImm (ImmInt 1))
              `snocOL` BCOND EQ ip (OpReg w oneReg) (TBlock bid)

      case mop of
        MO_F_Eq w -> fbcond w EQ
        MO_F_Ne w -> fbcond w NE
        MO_F_Gt w -> fbcond w FGT
        MO_F_Ge w -> fbcond w FGE
        MO_F_Lt w -> fbcond w FLT
        MO_F_Le w -> fbcond w FLE
        MO_Eq w -> sbcond w EQ
        MO_Ne w -> sbcond w NE
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

-- | Generate conditional branching instructions
--
-- This is basically an "if with else" statement.
genCondBranch ::
  -- | the true branch target
  BlockId ->
  -- | the false branch target
  BlockId ->
  -- | the condition on which to branch
  CmmExpr ->
  -- | Instructions
  NatM InstrBlock
genCondBranch true false expr =
  appOL
    <$> genCondJump true expr
    <*> genBranch false

-- -----------------------------------------------------------------------------
--  Generating C calls

-- | Generate a call to a C function.
--
-- - Integer values are passed in GP registers a0-a7.
-- - Floating point values are passed in FP registers fa0-fa7.
-- - If there are no free floating point registers, the FP values are passed in GP registers.
-- - If all GP registers are taken, the values are spilled as whole words (!) onto the stack.
-- - For integers/words, the return value is in a0.
-- - The return value is in fa0 if the return type is a floating point value.
genCCall ::
  ForeignTarget -> -- function to call
  [CmmFormal] -> -- where to put the result
  [CmmActual] -> -- arguments (of mixed type)
  NatM InstrBlock
-- TODO: Specialize where we can.
-- Generic impl
genCCall target@(ForeignTarget expr _cconv) dest_regs arg_regs = do
  -- we want to pass arg_regs into allArgRegs
  -- The target :: ForeignTarget call can either
  -- be a foreign procedure with an address expr
  -- and a calling convention.
  (call_target_reg, call_target_code) <-
    -- Compute the address of the call target into a register. This
    -- addressing enables us to jump through the whole address space
    -- without further ado. PC-relative addressing would involve
    -- instructions to do similar, though.
    do
      (reg, _format, reg_code) <- getSomeReg expr
      pure (reg, reg_code)
  -- compute the code and register logic for all arg_regs.
  -- this will give us the format information to match on.
  arg_regs' <- mapM getSomeReg arg_regs

  -- Now this is stupid.  Our Cmm expressions doesn't carry the proper sizes
  -- so while in Cmm we might get W64 incorrectly for an int, that is W32 in
  -- STG; this then breaks packing of stack arguments, if we need to pack
  -- for the pcs, e.g. darwinpcs.  Option one would be to fix the Int type
  -- in Cmm proper. Option two, which we choose here is to use extended Hint
  -- information to contain the size information and use that when packing
  -- arguments, spilled onto the stack.
  let (_res_hints, arg_hints) = foreignTargetHints target
      arg_regs'' = zipWith (\(r, f, c) h -> (r, f, h, c)) arg_regs' arg_hints

  (stackSpaceWords, passRegs, passArgumentsCode) <- passArguments allGpArgRegs allFpArgRegs arg_regs'' 0 [] nilOL

  readResultsCode <- readResults allGpArgRegs allFpArgRegs dest_regs [] nilOL

  let moveStackDown 0 =
        toOL
          [ PUSH_STACK_FRAME,
            DELTA (-16)
          ]
      moveStackDown i | odd i = moveStackDown (i + 1)
      moveStackDown i =
        toOL
          [ PUSH_STACK_FRAME,
            SUB (OpReg W64 spMachReg) (OpReg W64 spMachReg) (OpImm (ImmInt (8 * i))),
            DELTA (-8 * i - 16)
          ]
      moveStackUp 0 =
        toOL
          [ POP_STACK_FRAME,
            DELTA 0
          ]
      moveStackUp i | odd i = moveStackUp (i + 1)
      moveStackUp i =
        toOL
          [ ADD (OpReg W64 (spMachReg)) (OpReg W64 spMachReg) (OpImm (ImmInt (8 * i))),
            POP_STACK_FRAME,
            DELTA 0
          ]

  let code =
        call_target_code -- compute the label (possibly into a register)
          `appOL` moveStackDown stackSpaceWords
          `appOL` passArgumentsCode -- put the arguments into x0, ...
          `snocOL` BL call_target_reg passRegs -- branch and link (C calls aren't tail calls, but return)
          `appOL` readResultsCode -- parse the results into registers
          `appOL` moveStackUp stackSpaceWords
  return code
  where
    -- Implementiation of the RISCV ABI calling convention.
    -- https://github.com/riscv-non-isa/riscv-elf-psabi-doc/blob/948463cd5dbebea7c1869e20146b17a2cc8fda2f/riscv-cc.adoc#integer-calling-convention
    passArguments :: [Reg] -> [Reg] -> [(Reg, Format, ForeignHint, InstrBlock)] -> Int -> [Reg] -> InstrBlock -> NatM (Int, [Reg], InstrBlock)
    -- Base case: no more arguments to pass (left)
    passArguments _ _ [] stackSpaceWords accumRegs accumCode = return (stackSpaceWords, accumRegs, accumCode)
    -- Still have GP regs, and we want to pass an GP argument.
    passArguments (gpReg : gpRegs) fpRegs ((r, format, hint, code_r) : args) stackSpaceWords accumRegs accumCode | isIntFormat format = do
      -- RISCV64 Integer Calling Convention: "When passed in registers or on the
      -- stack, integer scalars narrower than XLEN bits are widened according to
      -- the sign of their type up to 32 bits, then sign-extended to XLEN bits."
      let w = formatToWidth format
          assignArg =
            if hint == SignedHint
              then
                COMMENT (text "Pass gp argument sign-extended (SignedHint): " <> ppr r)
                  `consOL` signExtend w W64 r gpReg
              else
                toOL
                  [ COMMENT (text "Pass gp argument sign-extended (SignedHint): " <> ppr r),
                    MOV (OpReg w gpReg) (OpReg w r)
                  ]
          accumCode' =
            accumCode
              `appOL` code_r
              `appOL` assignArg
      passArguments gpRegs fpRegs args stackSpaceWords (gpReg : accumRegs) accumCode'

    -- Still have FP regs, and we want to pass an FP argument.
    passArguments gpRegs (fpReg : fpRegs) ((r, format, _hint, code_r) : args) stackSpaceWords accumRegs accumCode | isFloatFormat format = do
      let w = formatToWidth format
          mov = MOV (OpReg w fpReg) (OpReg w r)
          accumCode' =
            accumCode
              `appOL` code_r
              `snocOL` ann (text "Pass fp argument: " <> ppr r) mov
      passArguments gpRegs fpRegs args stackSpaceWords (fpReg : accumRegs) accumCode'

    -- No mor regs left to pass. Must pass on stack.
    passArguments [] [] ((r, format, hint, code_r) : args) stackSpaceWords accumRegs accumCode = do
      let w = formatToWidth format
          spOffet = 8 * stackSpaceWords
          str = STR format (OpReg w r) (OpAddr (AddrRegImm spMachReg (ImmInt spOffet)))
          stackCode =
            if hint == SignedHint
              then
                code_r
                  `appOL` signExtend w W64 r ipReg
                  `snocOL` ann (text "Pass signed argument (size " <> ppr w <> text ") on the stack: " <> ppr ipReg) str
              else
                code_r
                  `snocOL` ann (text "Pass unsigned argument (size " <> ppr w <> text ") on the stack: " <> ppr r) str
      passArguments [] [] args (stackSpaceWords + 1) accumRegs (stackCode `appOL` accumCode)

    -- Still have fpRegs left, but want to pass a GP argument. Must be passed on the stack then.
    passArguments [] fpRegs ((r, format, _hint, code_r) : args) stackSpaceWords accumRegs accumCode | isIntFormat format = do
      let w = formatToWidth format
          spOffet = 8 * stackSpaceWords
          str = STR format (OpReg w r) (OpAddr (AddrRegImm spMachReg (ImmInt spOffet)))
          stackCode =
            code_r
              `snocOL` ann (text "Pass argument (size " <> ppr w <> text ") on the stack: " <> ppr r) str
      passArguments [] fpRegs args (stackSpaceWords + 1) accumRegs (stackCode `appOL` accumCode)

    -- Still have gpRegs left, but want to pass a FP argument. Must be passed in gpReg then.
    passArguments (gpReg : gpRegs) [] ((r, format, _hint, code_r) : args) stackSpaceWords accumRegs accumCode | isFloatFormat format = do
      let w = formatToWidth format
          mov = MOV (OpReg w gpReg) (OpReg w r)
          accumCode' =
            accumCode
              `appOL` code_r
              `snocOL` ann (text "Pass fp argument in gpReg: " <> ppr r) mov
      passArguments gpRegs [] args stackSpaceWords (gpReg : accumRegs) accumCode'
    passArguments _ _ _ _ _ _ = pprPanic "passArguments" (text "invalid state")

    readResults :: [Reg] -> [Reg] -> [LocalReg] -> [Reg] -> InstrBlock -> NatM InstrBlock
    readResults _ _ [] _ accumCode = return accumCode
    readResults [] _ _ _ _ = do
      platform <- getPlatform
      pprPanic "genCCall, out of gp registers when reading results" (pdoc platform target)
    readResults _ [] _ _ _ = do
      platform <- getPlatform
      pprPanic "genCCall, out of fp registers when reading results" (pdoc platform target)
    readResults (gpReg : gpRegs) (fpReg : fpRegs) (dst : dsts) accumRegs accumCode = do
      -- gp/fp reg -> dst
      platform <- getPlatform
      let rep = cmmRegType (CmmLocal dst)
          format = cmmTypeFormat rep
          w = cmmRegWidth (CmmLocal dst)
          r_dst = getRegisterReg platform (CmmLocal dst)
      if isFloatFormat format
        then readResults (gpReg : gpRegs) fpRegs dsts (fpReg : accumRegs) (accumCode `snocOL` MOV (OpReg w r_dst) (OpReg w fpReg))
        else
          readResults gpRegs (fpReg : fpRegs) dsts (gpReg : accumRegs)
            $ accumCode
            `snocOL` MOV (OpReg w r_dst) (OpReg w gpReg)
            `appOL`
            -- truncate, otherwise an unexpectedly big value might be used in upfollowing calculations
            truncateReg W64 w r_dst
genCCall (PrimTarget mop) dest_regs arg_regs = do
  case mop of
    MO_F32_Fabs
      | [arg_reg] <- arg_regs,
        [dest_reg] <- dest_regs ->
          unaryFloatOp W32 (\d x -> unitOL $ FABS d x) arg_reg dest_reg
    MO_F64_Fabs
      | [arg_reg] <- arg_regs,
        [dest_reg] <- dest_regs ->
          unaryFloatOp W64 (\d x -> unitOL $ FABS d x) arg_reg dest_reg
    -- 64 bit float ops
    MO_F64_Pwr -> mkCCall "pow"
    MO_F64_Sin -> mkCCall "sin"
    MO_F64_Cos -> mkCCall "cos"
    MO_F64_Tan -> mkCCall "tan"
    MO_F64_Sinh -> mkCCall "sinh"
    MO_F64_Cosh -> mkCCall "cosh"
    MO_F64_Tanh -> mkCCall "tanh"
    MO_F64_Asin -> mkCCall "asin"
    MO_F64_Acos -> mkCCall "acos"
    MO_F64_Atan -> mkCCall "atan"
    MO_F64_Asinh -> mkCCall "asinh"
    MO_F64_Acosh -> mkCCall "acosh"
    MO_F64_Atanh -> mkCCall "atanh"
    MO_F64_Log -> mkCCall "log"
    MO_F64_Log1P -> mkCCall "log1p"
    MO_F64_Exp -> mkCCall "exp"
    MO_F64_ExpM1 -> mkCCall "expm1"
    MO_F64_Fabs -> mkCCall "fabs"
    MO_F64_Sqrt -> mkCCall "sqrt"
    -- 32 bit float ops
    MO_F32_Pwr -> mkCCall "powf"
    MO_F32_Sin -> mkCCall "sinf"
    MO_F32_Cos -> mkCCall "cosf"
    MO_F32_Tan -> mkCCall "tanf"
    MO_F32_Sinh -> mkCCall "sinhf"
    MO_F32_Cosh -> mkCCall "coshf"
    MO_F32_Tanh -> mkCCall "tanhf"
    MO_F32_Asin -> mkCCall "asinf"
    MO_F32_Acos -> mkCCall "acosf"
    MO_F32_Atan -> mkCCall "atanf"
    MO_F32_Asinh -> mkCCall "asinhf"
    MO_F32_Acosh -> mkCCall "acoshf"
    MO_F32_Atanh -> mkCCall "atanhf"
    MO_F32_Log -> mkCCall "logf"
    MO_F32_Log1P -> mkCCall "log1pf"
    MO_F32_Exp -> mkCCall "expf"
    MO_F32_ExpM1 -> mkCCall "expm1f"
    MO_F32_Fabs -> mkCCall "fabsf"
    MO_F32_Sqrt -> mkCCall "sqrtf"
    -- 64-bit primops
    MO_I64_ToI -> mkCCall "hs_int64ToInt"
    MO_I64_FromI -> mkCCall "hs_intToInt64"
    MO_W64_ToW -> mkCCall "hs_word64ToWord"
    MO_W64_FromW -> mkCCall "hs_wordToWord64"
    MO_x64_Neg -> mkCCall "hs_neg64"
    MO_x64_Add -> mkCCall "hs_add64"
    MO_x64_Sub -> mkCCall "hs_sub64"
    MO_x64_Mul -> mkCCall "hs_mul64"
    MO_I64_Quot -> mkCCall "hs_quotInt64"
    MO_I64_Rem -> mkCCall "hs_remInt64"
    MO_W64_Quot -> mkCCall "hs_quotWord64"
    MO_W64_Rem -> mkCCall "hs_remWord64"
    MO_x64_And -> mkCCall "hs_and64"
    MO_x64_Or -> mkCCall "hs_or64"
    MO_x64_Xor -> mkCCall "hs_xor64"
    MO_x64_Not -> mkCCall "hs_not64"
    MO_x64_Shl -> mkCCall "hs_uncheckedShiftL64"
    MO_I64_Shr -> mkCCall "hs_uncheckedIShiftRA64"
    MO_W64_Shr -> mkCCall "hs_uncheckedShiftRL64"
    MO_x64_Eq -> mkCCall "hs_eq64"
    MO_x64_Ne -> mkCCall "hs_ne64"
    MO_I64_Ge -> mkCCall "hs_geInt64"
    MO_I64_Gt -> mkCCall "hs_gtInt64"
    MO_I64_Le -> mkCCall "hs_leInt64"
    MO_I64_Lt -> mkCCall "hs_ltInt64"
    MO_W64_Ge -> mkCCall "hs_geWord64"
    MO_W64_Gt -> mkCCall "hs_gtWord64"
    MO_W64_Le -> mkCCall "hs_leWord64"
    MO_W64_Lt -> mkCCall "hs_ltWord64"
    -- Conversion
    MO_UF_Conv w -> mkCCall (word2FloatLabel w)
    -- Optional MachOps
    -- These are enabled/disabled by backend flags: GHC.StgToCmm.Config
    MO_S_Mul2 _w -> unsupported mop
    MO_S_QuotRem _w -> unsupported mop
    MO_U_QuotRem _w -> unsupported mop
    MO_U_QuotRem2 _w -> unsupported mop
    MO_Add2 _w -> unsupported mop
    MO_AddWordC _w -> unsupported mop
    MO_SubWordC _w -> unsupported mop
    MO_AddIntC _w -> unsupported mop
    MO_SubIntC _w -> unsupported mop
    MO_U_Mul2 _w -> unsupported mop
    -- Memory Ordering
    -- The related C functions are:
    -- #include <stdatomic.h>
    -- atomic_thread_fence(memory_order_acquire);
    -- atomic_thread_fence(memory_order_release);
    -- atomic_thread_fence(memory_order_seq_cst);
    MO_AcquireFence -> pure (unitOL (FENCE FenceRead FenceReadWrite))
    MO_ReleaseFence -> pure (unitOL (FENCE FenceReadWrite FenceWrite))
    MO_SeqCstFence -> pure (unitOL (FENCE FenceReadWrite FenceReadWrite))
    MO_Touch -> pure nilOL -- Keep variables live (when using interior pointers)
    -- Prefetch
    MO_Prefetch_Data _n -> pure nilOL -- Prefetch hint.

    -- Memory copy/set/move/cmp, with alignment for optimization
    MO_Memcpy _align -> mkCCall "memcpy"
    MO_Memset _align -> mkCCall "memset"
    MO_Memmove _align -> mkCCall "memmove"
    MO_Memcmp _align -> mkCCall "memcmp"
    MO_SuspendThread -> mkCCall "suspendThread"
    MO_ResumeThread -> mkCCall "resumeThread"
    MO_PopCnt w -> mkCCall (popCntLabel w)
    MO_Pdep w -> mkCCall (pdepLabel w)
    MO_Pext w -> mkCCall (pextLabel w)
    MO_Clz w -> mkCCall (clzLabel w)
    MO_Ctz w -> mkCCall (ctzLabel w)
    MO_BSwap w -> mkCCall (bSwapLabel w)
    MO_BRev w -> mkCCall (bRevLabel w)
    -- Atomic read-modify-write.
    mo@(MO_AtomicRead w ord)
      | [p_reg] <- arg_regs,
        [dst_reg] <- dest_regs -> do
          (p, _fmt_p, code_p) <- getSomeReg p_reg
          platform <- getPlatform
          -- Analog to the related MachOps (above)
          -- The related C functions are:
          -- #include <stdatomic.h>
          -- __atomic_load_n(&a, __ATOMIC_ACQUIRE);
          -- __atomic_load_n(&a, __ATOMIC_SEQ_CST);
          let instrs = case ord of
                MemOrderRelaxed -> unitOL $ ann moDescr (LDR (intFormat w) (OpReg w dst) (OpAddr $ AddrReg p))
                MemOrderAcquire ->
                  toOL
                    [ ann moDescr (LDR (intFormat w) (OpReg w dst) (OpAddr $ AddrReg p)),
                      FENCE FenceRead FenceReadWrite
                    ]
                MemOrderSeqCst ->
                  toOL
                    [ ann moDescr (FENCE FenceReadWrite FenceReadWrite),
                      LDR (intFormat w) (OpReg w dst) (OpAddr $ AddrReg p),
                      FENCE FenceRead FenceReadWrite
                    ]
                MemOrderRelease -> panic $ "Unexpected MemOrderRelease on an AtomicRead: " ++ show mo
              dst = getRegisterReg platform (CmmLocal dst_reg)
              moDescr = (text . show) mo
              code = code_p `appOL` instrs
          return code
      | otherwise -> panic "mal-formed AtomicRead"
    mo@(MO_AtomicWrite w ord)
      | [p_reg, val_reg] <- arg_regs -> do
          (p, _fmt_p, code_p) <- getSomeReg p_reg
          (val, fmt_val, code_val) <- getSomeReg val_reg
          -- Analog to the related MachOps (above)
          -- The related C functions are:
          -- #include <stdatomic.h>
          -- __atomic_store_n(&a, 23, __ATOMIC_SEQ_CST);
          -- __atomic_store_n(&a, 23, __ATOMIC_RELEASE);
          let instrs = case ord of
                MemOrderRelaxed -> unitOL $ ann moDescr (STR fmt_val (OpReg w val) (OpAddr $ AddrReg p))
                MemOrderSeqCst ->
                  toOL
                    [ ann moDescr (FENCE FenceReadWrite FenceWrite),
                      STR fmt_val (OpReg w val) (OpAddr $ AddrReg p),
                      FENCE FenceReadWrite FenceReadWrite
                    ]
                MemOrderRelease ->
                  toOL
                    [ ann moDescr (FENCE FenceReadWrite FenceWrite),
                      STR fmt_val (OpReg w val) (OpAddr $ AddrReg p)
                    ]
                MemOrderAcquire -> panic $ "Unexpected MemOrderAcquire on an AtomicWrite" ++ show mo
              moDescr = (text . show) mo
              code =
                code_p
                  `appOL` code_val
                  `appOL` instrs
          pure code
      | otherwise -> panic "mal-formed AtomicWrite"
    MO_AtomicRMW w amop -> mkCCall (atomicRMWLabel w amop)
    MO_Cmpxchg w -> mkCCall (cmpxchgLabel w)
    -- -- Should be an AtomicRMW variant eventually.
    -- -- Sequential consistent.
    -- TODO: this should be implemented properly!
    MO_Xchg w -> mkCCall (xchgLabel w)
  where
    unsupported :: (Show a) => a -> b
    unsupported mop =
      panic
        ( "outOfLineCmmOp: "
            ++ show mop
            ++ " not supported here"
        )
    mkCCall :: FastString -> NatM InstrBlock
    mkCCall name = do
      config <- getConfig
      target <-
        cmmMakeDynamicReference config CallReference
          $ mkForeignLabel name ForeignLabelInThisPackage IsFunction
      let cconv = ForeignConvention CCallConv [NoHint] [NoHint] CmmMayReturn
      genCCall (ForeignTarget target cconv) dest_regs arg_regs

    unaryFloatOp w op arg_reg dest_reg = do
      platform <- getPlatform
      (reg_fx, _format_x, code_fx) <- getFloatReg arg_reg
      let dst = getRegisterReg platform (CmmLocal dest_reg)
      let code = code_fx `appOL` op (OpReg w dst) (OpReg w reg_fx)
      pure code

{- Note [RISCV64 far jumps]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

RISCV64 conditional jump instructions can only encode an offset of +/-4KiB
(12bits) which is usually enough but can be exceeded in edge cases. In these
cases we will replace:

  b.cond <cond> foo

with the sequence:

  b.cond <cond> <lbl_true>
  b <lbl_false>
  <lbl_true>:
  la reg foo
  b reg
  <lbl_false>:

and

  b foo

with the sequence:

  la reg foo
  b reg

Compared to AArch64 the target label is loaded to a register, because
unconditional jump instructions can only address +/-1MiB. The LA
pseudo-instruction will be replaced by up to two real instructions, ensuring
correct addressing.

One could surely find more efficient replacements, taking PC-relative addressing
into account. This could be a future improvement. (As far branches are pretty
rare, one might question and measure the value of such improvement.)

RISCV has many pseudo-instructions which emit more than one real instructions.
Thus, we count the real instructions after the Assembler has seen them.

We make some simplifications in the name of performance which can result in
overestimating jump <-> label offsets:

\* To avoid having to recalculate the label offsets once we replaced a jump we simply
  assume all label jumps will be expanded to a three instruction far jump sequence.
\* For labels associated with a info table we assume the info table is 64byte large.
  Most info tables are smaller than that but it means we don't have to distinguish
  between multiple types of info tables.

In terms of implementation we walk the instruction stream at least once calculating
label offsets, and if we determine during this that the functions body is big enough
to potentially contain out of range jumps we walk the instructions a second time, replacing
out of range jumps with the sequence of instructions described above.

-}

-- | A conditional jump to a far target
--
-- By loading the far target into a register for the jump, we can address the
-- whole memory range.
genCondFarJump :: (MonadUnique m) => Cond -> Operand -> Operand -> BlockId -> m InstrBlock
genCondFarJump cond op1 op2 far_target = do
  skip_lbl_id <- newBlockId
  jmp_lbl_id <- newBlockId

  -- TODO: We can improve this by inverting the condition
  -- but it's not quite trivial since we don't know if we
  -- need to consider float orderings.
  -- So we take the hit of the additional jump in the false
  -- case for now.
  return
    $ toOL
      [ ann (text "Conditional far jump to: " <> ppr far_target)
          $ BCOND cond op1 op2 (TBlock jmp_lbl_id),
        B (TBlock skip_lbl_id),
        NEWBLOCK jmp_lbl_id,
        LDR II64 (OpReg W64 ipReg) (OpImm (ImmCLbl (blockLbl far_target))),
        B (TReg ipReg),
        NEWBLOCK skip_lbl_id
      ]

-- | An unconditional jump to a far target
--
-- By loading the far target into a register for the jump, we can address the
-- whole memory range.
genFarJump :: (MonadUnique m) => BlockId -> m InstrBlock
genFarJump far_target =
  return
    $ toOL
      [ ann (text "Unconditional far jump to: " <> ppr far_target)
          $ LDR II64 (OpReg W64 ipReg) (OpImm (ImmCLbl (blockLbl far_target))),
        B (TReg ipReg)
      ]

-- See Note [RISCV64 far jumps]
data BlockInRange = InRange | NotInRange BlockId

-- See Note [RISCV64 far jumps]
makeFarBranches ::
  Platform ->
  LabelMap RawCmmStatics ->
  [NatBasicBlock Instr] ->
  UniqSM [NatBasicBlock Instr]
makeFarBranches {- only used when debugging -} _platform statics basic_blocks = do
  -- All offsets/positions are counted in multiples of 4 bytes (the size of RISCV64 instructions)
  -- That is an offset of 1 represents a 4-byte/one instruction offset.
  let (func_size, lblMap) = foldl' calc_lbl_positions (0, mapEmpty) basic_blocks
  if func_size < max_jump_dist
    then pure basic_blocks
    else do
      (_, blocks) <- mapAccumLM (replace_blk lblMap) 0 basic_blocks
      pure $ concat blocks
  where
    -- pprTrace "lblMap" (ppr lblMap) $ basic_blocks

    -- 2^11, 12 bit immediate with one bit is reserved for the sign
    max_jump_dist = 2 ^ (11 :: Int) - 1 :: Int
    -- Currently all inline info tables fit into 64 bytes.
    max_info_size = 16 :: Int
    long_bc_jump_size = 5 :: Int
    long_b_jump_size = 2 :: Int

    -- Replace out of range conditional jumps with unconditional jumps.
    replace_blk :: LabelMap Int -> Int -> GenBasicBlock Instr -> UniqSM (Int, [GenBasicBlock Instr])
    replace_blk !m !pos (BasicBlock lbl instrs) = do
      -- Account for a potential info table before the label.
      let !block_pos = pos + infoTblSize_maybe lbl
      (!pos', instrs') <- mapAccumLM (replace_jump m) block_pos instrs
      let instrs'' = concat instrs'
      -- We might have introduced new labels, so split the instructions into basic blocks again if neccesary.
      let (top, split_blocks, no_data) = foldr mkBlocks ([], [], []) instrs''
      -- There should be no data in the instruction stream at this point
      massert (null no_data)

      let final_blocks = BasicBlock lbl top : split_blocks
      pure (pos', final_blocks)

    replace_jump :: LabelMap Int -> Int -> Instr -> UniqSM (Int, [Instr])
    replace_jump !m !pos instr = do
      case instr of
        ANN ann instr -> do
          (idx, instr' : instrs') <- replace_jump m pos instr
          pure (idx, ANN ann instr' : instrs')
        BCOND cond op1 op2 t ->
          case target_in_range m t pos of
            InRange -> pure (pos + instr_size instr, [instr])
            NotInRange far_target -> do
              jmp_code <- genCondFarJump cond op1 op2 far_target
              pure (pos + instr_size instr, fromOL jmp_code)
        B t ->
          case target_in_range m t pos of
            InRange -> pure (pos + instr_size instr, [instr])
            NotInRange far_target -> do
              jmp_code <- genFarJump far_target
              pure (pos + instr_size instr, fromOL jmp_code)
        _ -> pure (pos + instr_size instr, [instr])

    target_in_range :: LabelMap Int -> Target -> Int -> BlockInRange
    target_in_range m target src =
      case target of
        (TReg {}) -> InRange
        (TBlock bid) -> block_in_range m src bid

    block_in_range :: LabelMap Int -> Int -> BlockId -> BlockInRange
    block_in_range m src_pos dest_lbl =
      case mapLookup dest_lbl m of
        Nothing ->
          pprTrace "not in range" (ppr dest_lbl)
            $ NotInRange dest_lbl
        Just dest_pos ->
          if abs (dest_pos - src_pos) < max_jump_dist
            then InRange
            else NotInRange dest_lbl

    calc_lbl_positions :: (Int, LabelMap Int) -> GenBasicBlock Instr -> (Int, LabelMap Int)
    calc_lbl_positions (pos, m) (BasicBlock lbl instrs) =
      let !pos' = pos + infoTblSize_maybe lbl
       in foldl' instr_pos (pos', mapInsert lbl pos' m) instrs

    instr_pos :: (Int, LabelMap Int) -> Instr -> (Int, LabelMap Int)
    instr_pos (pos, m) instr = (pos + instr_size instr, m)

    infoTblSize_maybe bid =
      case mapLookup bid statics of
        Nothing -> 0 :: Int
        Just _info_static -> max_info_size

    instr_size :: Instr -> Int
    instr_size i = case i of
      COMMENT {} -> 0
      MULTILINE_COMMENT {} -> 0
      ANN _ instr -> instr_size instr
      LOCATION {} -> 0
      DELTA {} -> 0
      -- At this point there should be no NEWBLOCK in the instruction stream (pos, mapInsert bid pos m)
      NEWBLOCK {} -> panic "mkFarBranched - Unexpected"
      LDATA {} -> panic "mkFarBranched - Unexpected"
      PUSH_STACK_FRAME -> 4
      POP_STACK_FRAME -> 4
      ADD {} -> 1
      MUL {} -> 1
      MULH {} -> 1
      NEG {} -> 1
      DIV {} -> 1
      REM {} -> 1
      REMU {} -> 1
      SUB {} -> 1
      DIVU {} -> 1
      AND {} -> 1
      OR {} -> 1
      SRA {} -> 1
      XOR {} -> 1
      SLL {} -> 1
      SRL {} -> 1
      MOV {} -> 2
      ORI {} -> 1
      XORI {} -> 1
      CSET {} -> 2
      STR {} -> 1
      LDR {} -> 3
      LDRU {} -> 1
      FENCE {} -> 1
      FCVT {} -> 1
      FABS {} -> 1
      FMA {} -> 1
      -- estimate the subsituted size for jumps to lables
      -- jumps to registers have size 1
      BCOND {} -> long_bc_jump_size
      B (TBlock _) -> long_b_jump_size
      B (TReg _) -> 1
      BL _ _ -> 1
      J_TBL {} -> 1
