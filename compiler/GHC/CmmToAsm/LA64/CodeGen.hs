{-# language GADTs #-}
{-# language LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
module GHC.CmmToAsm.LA64.CodeGen (
      cmmTopCodeGen
    , generateJumpTableForInstr
)

where

import Data.Maybe
import Data.Word
import GHC.Cmm
import GHC.Cmm.BlockId
import GHC.Cmm.CLabel
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.DebugBlock
import GHC.Cmm.Switch
import GHC.Cmm.Utils
import GHC.CmmToAsm.CPrim
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Format
import GHC.CmmToAsm.Monad
  ( NatM,
    getConfig,
    getDebugBlock,
    getFileId,
    getNewLabelNat,
    getNewRegNat,
    getPicBaseMaybeNat,
    getPlatform,
  )
import GHC.CmmToAsm.PIC
import GHC.CmmToAsm.LA64.Cond
import GHC.CmmToAsm.LA64.Instr
import GHC.CmmToAsm.LA64.Regs
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
import GHC.Utils.Constants (debugIsOn)
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Cmm.Dataflow.Label()
import GHC.Utils.Monad
import Control.Monad
import GHC.Types.Unique.DSM()

-- [General layout of an NCG]
cmmTopCodeGen ::
  RawCmmDecl ->
  NatM [NatCmmDecl RawCmmStatics Instr]
-- Thus we'll have to deal with either CmmProc ...
cmmTopCodeGen _cmm@(CmmProc info lab live graph) = do
  picBaseMb <- getPicBaseMaybeNat
  when (isJust picBaseMb) $ panic "LA64.cmmTopCodeGen: Unexpected PIC base register"

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
-- The index into the jump table is calulated by evaluating @expr@. The
-- corresponding table entry contains the address to jump to.
genSwitch :: NCGConfig -> CmmExpr -> SwitchTargets -> NatM InstrBlock
genSwitch config expr targets = do
  (reg, fmt1, e_code) <- getSomeReg indexExpr
  targetReg <- getNewRegNat II64
  lbl <- getNewLabelNat
  dynRef <- cmmMakeDynamicReference config DataReference lbl
  (tableReg, fmt2, t_code) <- getSomeReg $ dynRef
  let code =
        toOL [ COMMENT (text "indexExpr" <+> (text . show) indexExpr)
             , COMMENT (text "dynRef" <+> (text . show) dynRef)
             ]
          `appOL` e_code
          `appOL` t_code
          `appOL` toOL
            [
              COMMENT (ftext "Jump table for switch"),
              -- index to offset into the table (relative to tableReg)
              annExpr expr (SLL (OpReg W64 reg) (OpReg (formatToWidth fmt1) reg) (OpImm (ImmInt 3))),
              -- calculate table entry address
              ADD (OpReg W64 targetReg) (OpReg W64 reg) (OpReg (formatToWidth fmt2) tableReg),
              -- load table entry (relative offset from tableReg (first entry) to target label)
              LDU II64 (OpReg W64 targetReg) (OpAddr (AddrRegImm targetReg (ImmInt 0))),
              -- calculate absolute address of the target label
              ADD (OpReg W64 targetReg) (OpReg W64 targetReg) (OpReg W64 tableReg),
              -- prepare jump to target label
              J_TBL bids (Just lbl) targetReg
            ]
  return code
  where
    platform = ncgPlatform config
    expr_w = cmmExprWidth platform expr
    indexExpr0 = cmmOffset platform expr offset
    -- Widen to a native-width register(addressing modes)
    indexExpr = CmmMachOp
        (MO_UU_Conv expr_w (platformWordWidth platform))
        [indexExpr0]
    (offset, bids) = switchTargetsToTable targets


-- Generate jump table data (if required)
--
-- Relies on PIC relocations. The idea is to emit one table entry per case. The
-- entry is the label of the block to jump to. This will be relocated to be the
-- address of the jump target.
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
    CmmUnsafeForeignCall target result_regs args
      -> genCCall target result_regs args

    CmmComment s   -> return (unitOL (COMMENT (ftext s)))
    CmmTick {}     -> return nilOL

    CmmAssign reg src
      | isFloatType ty         -> assignReg_FltCode format reg src
      | otherwise              -> assignReg_IntCode format reg src
        where ty = cmmRegType reg
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
        genCondBranch true false arg

    CmmSwitch arg ids -> genSwitch config arg ids

    CmmCall { cml_target = arg } -> genJump arg

    CmmUnwind _regs -> pure nilOL

    _ ->  pprPanic "stmtToInstrs: statement should have been cps'd away" (pdoc platform stmt)

-- | 'InstrBlock's are the insn sequences generated by the insn selectors.
--  They are really trees of insns to facilitate fast appending, where a
--  left-to-right traversal yields the insns in the correct order.
type InstrBlock =
  OrdList Instr

-- | Register's passed up the tree.
--  If the stix code forces the register to live in a pre-decided machine
--  register, it comes out as @Fixed@; otherwise, it comes out as @Any@, and the
--  parent can decide which register to put it in.
data Register
  = Fixed Format Reg InstrBlock
  | Any Format (Reg -> InstrBlock)

-- | Sometimes we need to change the Format of a register. Primarily during
--  conversion.
swizzleRegisterRep :: Format -> Register -> Register
swizzleRegisterRep format' (Fixed _ reg code) = Fixed format' reg code
swizzleRegisterRep format' (Any _ codefn) = Any format' codefn

-- | Grab a `Reg` for a `CmmReg`
getRegisterReg :: Platform -> CmmReg -> Reg

getRegisterReg _ (CmmLocal (LocalReg u pk))
  = RegVirtual $ mkVirtualReg u (cmmTypeFormat pk)

getRegisterReg platform (CmmGlobal mid)
  = case globalRegMaybe platform (globalRegUse_reg mid) of
        Just reg -> RegReal reg
        Nothing  -> pprPanic "getRegisterReg-memory" (ppr $ CmmGlobal mid)

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

-- | Compute an expression into floating point register
--  If the initial expression is not a floating-point expression, finally move
--  the result into a floating-point register.
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

-- | Map `CmmLit` to `OpImm`
litToImm' :: CmmLit -> Operand
litToImm' = OpImm . litToImm

-- Handling PIC on LA64
-- Commonly, `PIC` means of `position independent code`, that to say, the execution
-- of code does not be influenced by Load_address. Through PC-Relative addressing
-- or GOT addressing, both can be used to implement `PIC`.
--
-- For LoongArch's common compiler(GCC, Clang), they generate PIC code by default
-- without condition. The command option `-fPIC` dicates to generate code for
-- shared-library. If not just specified for shared-library, another option `-fPIE`
-- was be created.
--
-- Like RV64, LA64 does not have a special PIC register, the general approach is to
-- simply do PC-relative addressing or go through the GOT. There is assembly support
-- for both.
--
-- LA64 assembly has many `la*` (load address) pseudo-instructions, that allows
-- loading a symbols's address into a register. These instructions is desugared into
-- different addressing modes. See following:
--
-- la        rd, label + addend  -> Load global symbol
-- la.global rd, label + addend  -> Same as `la`
-- la.local  rd, label + addend  -> Load local symbol
-- la.pcrel  rd, label + addend
-- la.got    rd, label
-- la.abs    rd, label + addend
--
-- `la` is alias of `la.global`. Commonly recommended use `la.local` and `la.global`.
--
-- PC-relative addressing:
--   pcalau12i $a0, %pc_hi20(a)
--   addi.d    $a0, $a0, %pc_lo12(a)
--
-- GOT addressing:
--   pcalau12i $a0, %got_pc_hi20(global_a)
--   ld.d      $a0, $a0, %got_pc_lo12(global_a)
--
-- PIC can be enabled/disabled through:
--  .option pic
--
-- CmmGlobal @PicBaseReg@'s are generated in @GHC.CmmToAsm.PIC@ in the
-- @cmmMakePicReference@.  This is in turn called from @cmmMakeDynamicReference@
-- also in @Cmm.CmmToAsm.PIC@ from where it is also exported.  There are two
-- callsites for this. One is in this module to produce the @target@ in @genCCall@
-- the other is in @GHC.CmmToAsm@ in @cmmExprNative@.
--
-- Conceptually we do not want any special PicBaseReg to be used on LA64. If
-- we want to distinguish between symbol loading, we need to address this through
-- the way we load it, not through a register.

-- Compute a `CmmExpr` into a `Register`
getRegister :: CmmExpr -> NatM Register
getRegister e = do
  config <- getConfig
  getRegister' config (ncgPlatform config) e

-- Signed arithmetic on LoongArch64
--
-- Handling signed arithmetic on sub-word-size values on LA64 is a bit tricky
-- as Cmm's type system does not capture signedness. While 32- and 64-bit
-- values are fairly easy to handle due to LA64's 32- and 64-bit instructions
-- with responding register, 8- and 16-bit values require quite some care.
--
-- For LoongArch64, EXT.W.[B/H] will sign-extend 8- and 16-bit to 64-bit.
-- However, it is best to use EXT instruction only if the input and
-- output data widths are fully determined.
--
-- We handle 16-and 8-bit values by using the following two steps:
--  1. Sign- or Zero-extending operands.
--  2. Truncate results as necessary.
--
-- For simplicity we maintain the invariant that a register containing a
-- sub-word-size value always contains the zero-extended form of that value
-- in between operations.

getRegister' :: NCGConfig -> Platform -> CmmExpr -> NatM Register

-- OPTIMIZATION WARNING: CmmExpr rewrites
-- Maybe we can do more?
-- 1. Rewrite: Reg + (-i) => Reg - i
getRegister' config plat (CmmMachOp (MO_Add w0) [x, CmmLit (CmmInt i w1)]) | i < 0
  = getRegister' config plat (CmmMachOp (MO_Sub w0) [x, CmmLit (CmmInt (-i) w1)])

-- 2. Rewrite: Reg - (-i) => Reg + i
getRegister' config plat (CmmMachOp (MO_Sub w0) [x, CmmLit (CmmInt i w1)]) | i < 0
  = getRegister' config plat (CmmMachOp (MO_Add w0) [x, CmmLit (CmmInt (-i) w1)])

-- Generic case.
getRegister' config plat expr =
  case expr of
    CmmReg (CmmGlobal (GlobalRegUse PicBaseReg _)) ->
      pprPanic "getRegisterReg-memory" (ppr PicBaseReg)

    CmmLit lit ->
      case lit of
        CmmInt 0 w -> pure $ Fixed (intFormat w) zeroReg nilOL
        CmmInt i w -> do
          -- narrowU is important: Negative immediates may be
          -- sign-extended on load!
          let imm = OpImm . ImmInteger $ narrowU w i
          return (Any (intFormat w) (\dst -> unitOL $ annExpr expr (MOV (OpReg w dst) imm)))

        CmmFloat 0 w -> do
          let op = litToImm' lit
          pure (Any (floatFormat w) (\dst -> unitOL $ annExpr expr (MOV (OpReg w dst) op)))

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
        CmmVec _lits -> pprPanic "getRegister' (CmmLit:CmmVec): " (pdoc plat expr)

        CmmLabel lbl -> do
          let op = OpImm (ImmCLbl lbl)
              rep = cmmLitType plat lit
              format = cmmTypeFormat rep
          return (Any format (\dst -> unitOL $ annExpr expr (LD format (OpReg (formatToWidth format) dst) op)))

        CmmLabelOff lbl off | isNbitEncodeable 12 (fromIntegral off) -> do
          let op = OpImm (ImmIndex lbl off)
              rep = cmmLitType plat lit
              format = cmmTypeFormat rep
          return (Any format (\dst -> unitOL $ LD format (OpReg (formatToWidth format) dst) op))

        CmmLabelOff lbl off -> do
          let op = litToImm' (CmmLabel lbl)
              rep = cmmLitType plat lit
              format = cmmTypeFormat rep
              width = typeWidth rep
          (off_r, _off_format, off_code) <- getSomeReg $ CmmLit (CmmInt (fromIntegral off) width)
          return (Any format (\dst -> off_code `snocOL`
                                                LD format (OpReg (formatToWidth format) dst) op `snocOL`
                                                ADD (OpReg W64 dst) (OpReg width dst) (OpReg width off_r)
                             ))

        CmmLabelDiffOff {} -> pprPanic "getRegister' (CmmLit:CmmLabelOff): " (pdoc plat expr)
        CmmBlock _ -> pprPanic "getRegister' (CmmLit:CmmLabelOff): " (pdoc plat expr)
        CmmHighStackMark -> pprPanic "getRegister' (CmmLit:CmmLabelOff): " (pdoc plat expr)

    CmmLoad mem rep _ -> do
      let format = cmmTypeFormat rep
          width = typeWidth rep
      Amode addr addr_code <- getAmode plat width mem
      case width of
        w | w `elem` [W8, W16, W32, W64] ->
            -- Load without sign-extension.
            pure (Any format (\dst ->
              addr_code `snocOL`
              LDU format (OpReg width dst) (OpAddr addr))
                              )
        _ -> pprPanic ("Unknown width to load: " ++ show width) (pdoc plat expr)

    CmmStackSlot _ _  -> pprPanic "getRegister' (CmmStackSlot): " (pdoc plat expr)

    CmmReg reg -> return (Fixed (cmmTypeFormat (cmmRegType reg))
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
      return $ Any (intFormat width) ( \dst ->
                                        off_code `appOL`
                                        code `snocOL`
                                        ADD (OpReg W64 dst) (OpReg width reg) (OpReg width off_r)
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
      (reg, format, code) <- getSomeReg e
      case op of
        MO_Not w -> return $ Any (intFormat w) $ \dst ->
          code `appOL`
          -- pseudo instruction `not dst rd` is `nor dst, r0, rd`
          truncateReg (formatToWidth format) W64 reg `snocOL`
          -- At this point an 8- or 16-bit value would be zero-extended
          -- to 64-bits. Truncate back down the final width.
          ann (text "not") (NOR (OpReg W64 dst) (OpReg W64 reg) zero) `appOL`
          truncateReg W64 w dst

        MO_S_Neg w -> negate code w reg
        MO_F_Neg w -> return $ Any (floatFormat w) (\dst -> code `snocOL` FNEG (OpReg w dst) (OpReg w reg))

        -- Floating convertion oprations
        -- Float -> Float
        MO_FF_Conv from to -> return $ Any (floatFormat to) (\dst -> code `snocOL` FCVT (OpReg to dst) (OpReg from reg))

        -- Signed int -> Float
        MO_SF_Round from to -> return $ Any (floatFormat to) (\dst -> code `snocOL` SCVTF (OpReg to dst) (OpReg from reg))

        -- Float -> Signed int
        MO_FS_Truncate from to | from == W32 -> do
            tmp <- getNewRegNat FF32
            return $ Any (intFormat to) (\dst -> code `snocOL` FCVTZS (OpReg to dst) (OpReg from tmp) (OpReg from reg))

        MO_FS_Truncate from to | from == W64-> do
            tmp <- getNewRegNat FF64
            return $ Any (intFormat to) (\dst -> code `snocOL` FCVTZS (OpReg to dst) (OpReg from tmp) (OpReg from reg))

        -- unsigned int -> unsigned int
        MO_UU_Conv from to -> return $ Any (intFormat to) (\dst ->
          code `snocOL` BSTRPICK II64 (OpReg W64 dst) (OpReg W64 reg) (OpImm (ImmInt (widthToInt (min from to) - 1))) (OpImm (ImmInt 0))
                                                          )

        -- Signed int -> Signed int
        MO_SS_Conv from to -> ss_conv from to reg code

        -- int -> int
        MO_XX_Conv _from to -> swizzleRegisterRep (intFormat to) <$> getRegister e

        MO_WF_Bitcast w    -> return $ Any (floatFormat w)  (\dst -> code `snocOL` MOV (OpReg w dst) (OpReg w reg))
        MO_FW_Bitcast w    -> return $ Any (intFormat w)    (\dst -> code `snocOL` MOV (OpReg w dst) (OpReg w reg))

        x -> pprPanic ("getRegister' (monadic CmmMachOp): " ++ show x) (pdoc plat expr)
      where
        -- In the case of 32- or 16- or 8-bit values we need to sign-extend to 64-bits
        negate code w reg = do
            return $ Any (intFormat w) $ \dst ->
                code `appOL`
                signExtend w W64 reg reg `snocOL`
                NEG (OpReg W64 dst) (OpReg W64 reg) `appOL`
                truncateReg W64 w dst

        ss_conv from to reg code =
            return $ Any (intFormat to) $ \dst ->
                code `appOL`
                signExtend from W64 reg dst `appOL`
                -- At this point an 8- or 16-bit value would be sign-extended
                -- to 64-bits. Truncate back down the final width.
                truncateReg W64 to dst


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

    CmmMachOp (MO_Add w) [x, CmmLit (CmmInt n _)]
      | w `elem` [W8, W16, W32]
      , fitsInNbits 12 (fromIntegral n) -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      return $ Any (intFormat w) ( \dst ->
                                    code_x `appOL`
                                    signExtend (formatToWidth format_x) W64 reg_x reg_x `snocOL`
                                    annExpr expr (ADD (OpReg W64 dst) (OpReg W64 reg_x) (OpImm (ImmInt (fromIntegral n) ))) `appOL`
                                    truncateReg W64 w dst
                                 )

    CmmMachOp (MO_Sub w) [x, CmmLit (CmmInt n _)]
      | w `elem` [W8, W16, W32]
      , fitsInNbits 12 (fromIntegral n) -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      return $ Any (intFormat w) ( \dst ->
                                    code_x `appOL`
                                    signExtend (formatToWidth format_x) W64 reg_x reg_x `snocOL`
                                    annExpr expr (SUB (OpReg W64 dst) (OpReg W64 reg_x) (OpImm (ImmInt (fromIntegral n) ))) `appOL`
                                    truncateReg W64 w dst
                                 )

    CmmMachOp (MO_Add w) [CmmReg reg, CmmLit (CmmInt n _)]
      | w `elem` [W8, W16, W32]
      , fitsInNbits 12 (fromIntegral n) -> do
      let w' = formatToWidth (cmmTypeFormat (cmmRegType reg))
          r' = getRegisterReg plat reg
      return $ Any (intFormat w) ( \dst ->
                                    signExtend w' W64 r' r' `snocOL`
                                    annExpr expr (ADD (OpReg W64 dst) (OpReg w' r') (OpImm (ImmInt (fromIntegral n) ))) `appOL`
                                    truncateReg W64 w dst
                                 )

    CmmMachOp (MO_Sub w) [CmmReg reg, CmmLit (CmmInt n _)]
      | w `elem` [W8, W16, W32]
      , fitsInNbits 12 (fromIntegral n) -> do
      let w' = formatToWidth (cmmTypeFormat (cmmRegType reg))
          r' = getRegisterReg plat reg
      return $ Any (intFormat w) ( \dst ->
                                    signExtend w' W64 r' r' `snocOL`
                                    annExpr expr (SUB (OpReg W64 dst) (OpReg w' r') (OpImm (ImmInt (fromIntegral n) ))) `appOL`
                                    truncateReg W64 w dst
                                 )

    CmmMachOp (MO_U_Quot w) [x, y]
      | w `elem` [W8, W16, W32] -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      (reg_y, format_y, code_y) <- getSomeReg y
      return $ Any (intFormat w) ( \dst ->
                                    code_x `appOL`
                                    truncateReg (formatToWidth format_x) W64 reg_x `appOL`
                                    code_y `appOL`
                                    truncateReg (formatToWidth format_y) W64 reg_y `snocOL`
                                    annExpr expr (DIVU (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y)) `appOL`
                                    truncateReg W64 w dst
                                 )

    -- 2. Shifts.
    CmmMachOp (MO_Shl w) [x, y]
      | w `elem` [W8, W16, W32] -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      (reg_y, format_y, code_y) <- getSomeReg y
      return $ Any (intFormat w) ( \dst ->
                                    code_x `appOL`
                                    signExtend (formatToWidth format_x) W64 reg_x reg_x `appOL`
                                    code_y `appOL`
                                    signExtend (formatToWidth format_y) W64 reg_y reg_y `snocOL`
                                    annExpr expr (SLL (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y)) `appOL`
                                    truncateReg W64 w dst
                                 )

    CmmMachOp (MO_Shl w) [x, CmmLit (CmmInt n _)]
      | w `elem` [W8, W16, W32]
      , 0 <= n, n < fromIntegral (widthInBits w) -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      return $ Any (intFormat w) ( \dst ->
                                    code_x `appOL`
                                    signExtend (formatToWidth format_x) W64 reg_x reg_x `snocOL`
                                    annExpr expr (SLL (OpReg W64 dst) (OpReg w reg_x) (OpImm (ImmInt (fromIntegral n) ))) `appOL`
                                    truncateReg W64 w dst
                                 )

    -- MO_S_Shr: signed-shift-right
    CmmMachOp (MO_S_Shr w) [x, y]
      | w `elem` [W8, W16, W32] -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      (reg_y, format_y, code_y) <- getSomeReg y
      return $ Any (intFormat w) ( \dst ->
                                    code_x `appOL`
                                    signExtend (formatToWidth format_x) W64 reg_x reg_x `appOL`
                                    code_y `appOL`
                                    signExtend (formatToWidth format_y) W64 reg_y reg_y `snocOL`
                                    annExpr expr (SRA (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y)) `appOL`
                                    truncateReg W64 w dst
                                 )
    CmmMachOp (MO_S_Shr w) [x, CmmLit (CmmInt n _)]
      | w `elem` [W8, W16, W32]
      , fitsInNbits 12 (fromIntegral n) -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      return $ Any (intFormat w)  (\dst ->
                                    code_x `appOL`
                                    signExtend (formatToWidth format_x) W64 reg_x reg_x `snocOL`
                                    annExpr expr (SRA (OpReg W64 dst) (OpReg W64 reg_x) (OpImm (ImmInt (fromIntegral n) ))) `appOL`
                                    truncateReg W64 w dst
                                  )

    -- MO_U_Shr: unsigned-shift-right
    CmmMachOp (MO_U_Shr w) [x, y]
      | w `elem` [W8, W16, W32] -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      (reg_y, format_y, code_y) <- getSomeReg y
      return $ Any (intFormat w) ( \dst ->
                                    code_x `appOL`
                                    truncateReg (formatToWidth format_x) W64 reg_x `appOL`
                                    code_y `appOL`
                                    truncateReg (formatToWidth format_y) W64 reg_y `snocOL`
                                    annExpr expr (SRL (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y)) `appOL`
                                    truncateReg W64 w dst
                                 )
    CmmMachOp (MO_U_Shr w) [x, CmmLit (CmmInt n _)]
      | w `elem` [W8, W16, W32]
      , 0 <= n, n < fromIntegral (widthInBits w) -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      return $ Any (intFormat w) ( \dst ->
                                    code_x `appOL`
                                    truncateReg (formatToWidth format_x) W64 reg_x `snocOL`
                                    annExpr expr (SRL (OpReg W64 dst) (OpReg W64 reg_x) (OpImm (ImmInt (fromIntegral n) ))) `appOL`
                                    truncateReg W64 w dst
                                 )

    -- 3. Logic &&, ||
    -- andi Instr's Imm-operand is zero-extended.
    CmmMachOp (MO_And w) [x, y]
      | w `elem` [W8, W16, W32] -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      (reg_y, format_y, code_y) <- getSomeReg y
      return $ Any (intFormat w) ( \dst ->
                                    code_x `appOL`
                                    truncateReg (formatToWidth format_x) W64 reg_x `appOL`
                                    code_y `appOL`
                                    truncateReg (formatToWidth format_y) W64 reg_y `snocOL`
                                    annExpr expr (AND (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y)) `appOL`
                                    truncateReg W64 w dst
                                 )

    CmmMachOp (MO_And w) [x, CmmLit (CmmInt n _)]
      | w `elem` [W8, W16, W32] -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      return $ Any (intFormat w) ( \dst ->
                                    code_x `appOL`
                                    truncateReg (formatToWidth format_x) W64 reg_x `snocOL`
                                    annExpr expr (AND (OpReg W64 dst) (OpReg W64 reg_x) (OpImm (ImmInt (fromIntegral n) ))) `appOL`
                                    truncateReg W64 w dst
                                 )

    CmmMachOp (MO_Or w) [x, y]
      | w `elem` [W8, W16, W32] -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      (reg_y, format_y, code_y) <- getSomeReg y
      return $ Any (intFormat w) ( \dst ->
                                    code_x `appOL`
                                    truncateReg (formatToWidth format_x) W64 reg_x `appOL`
                                    code_y `appOL`
                                    truncateReg (formatToWidth format_y) W64 reg_y `snocOL`
                                    annExpr expr (OR (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y)) `appOL`
                                    truncateReg W64 w dst
                                 )

    CmmMachOp (MO_Or w) [x, CmmLit (CmmInt n _)]
      | w `elem` [W8, W16, W32] -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      return $ Any (intFormat w) ( \dst ->
                                    code_x `appOL`
                                    truncateReg (formatToWidth format_x) W64 reg_x `snocOL`
                                    annExpr expr (OR (OpReg W64 dst) (OpReg W64 reg_x) (OpImm (ImmInt (fromIntegral n) ))) `appOL`
                                    truncateReg W64 w dst
                                 )

    CmmMachOp (MO_Xor w) [x, y]
      | w `elem` [W8, W16, W32] -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      (reg_y, format_y, code_y) <- getSomeReg y
      return $ Any (intFormat w) ( \dst ->
                                    code_x `appOL`
                                    truncateReg (formatToWidth format_x) W64 reg_x `appOL`
                                    code_y `appOL`
                                    truncateReg (formatToWidth format_y) W64 reg_y `snocOL`
                                    annExpr expr (XOR (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y)) `appOL`
                                    truncateReg W64 w dst
                                 )

    CmmMachOp (MO_Xor w) [x, CmmLit (CmmInt n _)]
      | w `elem` [W8, W16, W32] -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      return $ Any (intFormat w) ( \dst ->
                                    code_x `appOL`
                                    truncateReg (formatToWidth format_x) W64 reg_x `snocOL`
                                    annExpr expr (XOR (OpReg W64 dst) (OpReg W64 reg_x) (OpImm (ImmInt (fromIntegral n) ))) `appOL`
                                    truncateReg W64 w dst
                                 )

    -- CSET commands register operand being W64.
    CmmMachOp (MO_Eq w) [x, y]
      | w `elem` [W8, W16, W32] -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      (reg_y, format_y, code_y) <- getSomeReg y
      return $ Any (intFormat w) ( \dst ->
                                    code_x `appOL`
                                    signExtend (formatToWidth format_x) W64 reg_x reg_x `appOL`
                                    code_y `appOL`
                                    signExtend (formatToWidth format_y) W64 reg_y reg_y `snocOL`
                                    annExpr expr (CSET EQ (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y)) `appOL`
                                    truncateReg W64 w dst
                                 )

    CmmMachOp (MO_Ne w) [x, y]
      | w `elem` [W8, W16, W32] -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      (reg_y, format_y, code_y) <- getSomeReg y
      return $ Any (intFormat w) ( \dst ->
                                    code_x `appOL`
                                    signExtend (formatToWidth format_x) W64 reg_x reg_x `appOL`
                                    code_y `appOL`
                                    signExtend (formatToWidth format_y) W64 reg_y reg_y `snocOL`
                                    annExpr expr (CSET NE (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y)) `appOL`
                                    truncateReg W64 w dst
                                 )

    CmmMachOp (MO_S_Lt w) [x, y]
      | w `elem` [W8, W16, W32] -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      (reg_y, format_y, code_y) <- getSomeReg y
      return $ Any (intFormat w) ( \dst ->
                                    code_x `appOL`
                                    signExtend (formatToWidth format_x) W64 reg_x reg_x `appOL`
                                    code_y `appOL`
                                    signExtend (formatToWidth format_y) W64 reg_y reg_y `snocOL`
                                    annExpr expr (CSET SLT (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y)) `appOL`
                                    truncateReg W64 w dst
                                 )

    CmmMachOp (MO_S_Le w) [x, y]
      | w `elem` [W8, W16, W32] -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      (reg_y, format_y, code_y) <- getSomeReg y
      return $ Any (intFormat w) ( \dst ->
                                    code_x `appOL`
                                    signExtend (formatToWidth format_x) W64 reg_x reg_x `appOL`
                                    code_y `appOL`
                                    signExtend (formatToWidth format_y) W64 reg_y reg_y `snocOL`
                                    annExpr expr (CSET SLE (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y)) `appOL`
                                    truncateReg W64 w dst
                                 )

    CmmMachOp (MO_S_Ge w) [x, y]
      | w `elem` [W8, W16, W32] -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      (reg_y, format_y, code_y) <- getSomeReg y
      return $ Any (intFormat w) ( \dst ->
                                    code_x `appOL`
                                    signExtend (formatToWidth format_x) W64 reg_x reg_x `appOL`
                                    code_y `appOL`
                                    signExtend (formatToWidth format_y) W64 reg_y reg_y `snocOL`
                                    annExpr expr (CSET SGE (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y)) `appOL`
                                    truncateReg W64 w dst
                                 )

    CmmMachOp (MO_S_Gt w) [x, y]
      | w `elem` [W8, W16, W32] -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      (reg_y, format_y, code_y) <- getSomeReg y
      return $ Any (intFormat w) ( \dst ->
                                    code_x `appOL`
                                    signExtend (formatToWidth format_x) W64 reg_x reg_x `appOL`
                                    code_y `appOL`
                                    signExtend (formatToWidth format_y) W64 reg_y reg_y `snocOL`
                                    annExpr expr (CSET SGT (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y)) `appOL`
                                    truncateReg W64 w dst
                                 )

    CmmMachOp (MO_U_Lt w) [x, y]
      | w `elem` [W8, W16, W32] -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      (reg_y, format_y, code_y) <- getSomeReg y
      return $ Any (intFormat w) ( \dst ->
                                    code_x `appOL`
                                    truncateReg (formatToWidth format_x) W64 reg_x `appOL`
                                    code_y `appOL`
                                    truncateReg (formatToWidth format_y) W64 reg_y `snocOL`
                                    annExpr expr (CSET ULT (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y)) `appOL`
                                    truncateReg W64 w dst
                                 )

    CmmMachOp (MO_U_Le w) [x, y]
      | w `elem` [W8, W16, W32] -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      (reg_y, format_y, code_y) <- getSomeReg y
      return $ Any (intFormat w) ( \dst ->
                                    code_x `appOL`
                                    truncateReg (formatToWidth format_x) W64 reg_x `appOL`
                                    code_y `appOL`
                                    truncateReg (formatToWidth format_y) W64 reg_y `snocOL`
                                    annExpr expr (CSET ULE (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y)) `appOL`
                                    truncateReg W64 w dst
                                 )

    CmmMachOp (MO_U_Ge w) [x, y]
      | w `elem` [W8, W16, W32] -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      (reg_y, format_y, code_y) <- getSomeReg y
      return $ Any (intFormat w) ( \dst ->
                                    code_x `appOL`
                                    truncateReg (formatToWidth format_x) W64 reg_x `appOL`
                                    code_y `appOL`
                                    truncateReg (formatToWidth format_y) W64 reg_y `snocOL`
                                    annExpr expr (CSET UGE (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y)) `appOL`
                                    truncateReg W64 w dst
                                 )

    CmmMachOp (MO_U_Gt w) [x, y]
      | w `elem` [W8, W16, W32] -> do
      (reg_x, format_x, code_x) <- getSomeReg x
      (reg_y, format_y, code_y) <- getSomeReg y
      return $ Any (intFormat w) ( \dst ->
                                    code_x `appOL`
                                    truncateReg (formatToWidth format_x) W64 reg_x `appOL`
                                    code_y `appOL`
                                    truncateReg (formatToWidth format_y) W64 reg_y `snocOL`
                                    annExpr expr (CSET UGT (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y)) `appOL`
                                    truncateReg W64 w dst
                                 )


    -- Generic binary case.
    CmmMachOp op [x, y] -> do
      let
          -- A (potentially signed) integer operation.
          -- In the case of 8-, 16- and 32-bit signed arithmetic we must first
          -- sign-extend all arguments to 64-bits.
          -- TODO: can be simplified.
          intOp is_signed w op = do
              -- compute x<m> <- x
              -- compute x<o> <- y
              -- <OP> x<n>, x<m>, x<o>
              (reg_x, format_x, code_x) <- getSomeReg x
              (reg_y, format_y, code_y) <- getSomeReg y
              massertPpr (isIntFormat format_x && isIntFormat format_y) $ text "intOp: non-int"
              let w' = W64
              -- This is the width of the registers on which the operation
              -- should be performed.
              if not is_signed
                then return $ Any (intFormat w) $ \dst ->
                      code_x `appOL`
                      code_y `appOL`
                      -- zero-extend both operands
                      truncateReg (formatToWidth format_x) w' reg_x `appOL`
                      truncateReg (formatToWidth format_y) w' reg_y `snocOL`
                      op (OpReg w' dst) (OpReg w' reg_x) (OpReg w' reg_y) `appOL`
                      truncateReg w' w dst -- truncate back to the operand's original width
                else return $ Any (intFormat w) $ \dst ->
                      code_x `appOL`
                      code_y `appOL`
                      -- sign-extend both operands
                      signExtend (formatToWidth format_x) W64 reg_x reg_x `appOL`
                      signExtend (formatToWidth format_x) W64 reg_y reg_y `snocOL`
                      op (OpReg w' dst) (OpReg w' reg_x) (OpReg w' reg_y) `appOL`
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
        MO_Add w -> intOp False w (\d x y ->  annExpr expr (ADD d x y))
        MO_Sub w -> intOp False w (\d x y ->  annExpr expr (SUB d x y))

        -- Signed multiply/divide/remain
        MO_Mul w          -> intOp True w (\d x y -> annExpr expr (MUL d x y))
        MO_S_MulMayOflo w -> do_mul_may_oflo w x y

        MO_S_Quot w  -> intOp True w (\d x y -> annExpr expr (DIV d x y))
        MO_S_Rem w   -> intOp True w (\d x y -> annExpr expr (MOD d x y))

        -- Unsigned divide/remain
        MO_U_Quot w  -> intOp False w (\d x y -> annExpr expr (DIVU d x y))
        MO_U_Rem w   -> intOp False w (\d x y -> annExpr expr (MODU d x y))

        MO_Eq   w    -> intOp False  w (\d x y -> annExpr expr (CSET EQ d x y))
        MO_Ne   w    -> intOp False  w (\d x y -> annExpr expr (CSET NE d x y))

        -- Signed comparisons
        MO_S_Ge w    -> intOp True  w (\d x y -> annExpr expr (CSET SGE d x y))
        MO_S_Le w    -> intOp True  w (\d x y -> annExpr expr (CSET SLE d x y))
        MO_S_Gt w    -> intOp True  w (\d x y -> annExpr expr (CSET SGT d x y))
        MO_S_Lt w    -> intOp True  w (\d x y -> annExpr expr (CSET SLT d x y))

        -- Unsigned comparisons
        MO_U_Ge w    -> intOp False w (\d x y -> annExpr expr (CSET UGE d x y))
        MO_U_Le w    -> intOp False w (\d x y -> annExpr expr (CSET ULE d x y))
        MO_U_Gt w    -> intOp False w (\d x y -> annExpr expr (CSET UGT d x y))
        MO_U_Lt w    -> intOp False w (\d x y -> annExpr expr (CSET ULT d x y))

        -- Floating point arithmetic
        MO_F_Add w   -> floatOp w (\d x y -> unitOL $ annExpr expr (ADD d x y))
        MO_F_Sub w   -> floatOp w (\d x y -> unitOL $ annExpr expr (SUB d x y))
        MO_F_Mul w   -> floatOp w (\d x y -> unitOL $ annExpr expr (MUL d x y))
        MO_F_Quot w  -> floatOp w (\d x y -> unitOL $ annExpr expr (DIV d x y))
        MO_F_Min w   -> floatOp w (\d x y -> unitOL $ annExpr expr (FMIN d x y))
        MO_F_Max w   -> floatOp w (\d x y -> unitOL $ annExpr expr (FMAX d x y))

        -- Floating point comparison
        MO_F_Eq w    -> floatCond w (\d x y -> unitOL $ annExpr expr (CSET EQ d x y))
        MO_F_Ne w    -> floatCond w (\d x y -> unitOL $ annExpr expr (CSET NE d x y))
        MO_F_Ge w    -> floatCond w (\d x y -> unitOL $ annExpr expr (CSET FGE d x y))
        MO_F_Le w    -> floatCond w (\d x y -> unitOL $ annExpr expr (CSET FLE d x y))
        MO_F_Gt w    -> floatCond w (\d x y -> unitOL $ annExpr expr (CSET FGT d x y))
        MO_F_Lt w    -> floatCond w (\d x y -> unitOL $ annExpr expr (CSET FLT d x y))

        MO_Shl   w   -> intOp False w (\d x y -> annExpr expr (SLL d x y))
        MO_U_Shr w   -> intOp False w (\d x y -> annExpr expr (SRL d x y))
        MO_S_Shr w   -> intOp True  w (\d x y -> annExpr expr (SRA d x y))

        -- Bitwise operations
        MO_And   w   -> intOp False w (\d x y -> annExpr expr (AND d x y))
        MO_Or    w   -> intOp False w (\d x y -> annExpr expr (OR d x y))
        MO_Xor   w   -> intOp False w (\d x y -> annExpr expr (XOR d x y))

        op -> pprPanic "getRegister' (unhandled dyadic CmmMachOp): " $ pprMachOp op <+> text "in" <+> pdoc plat expr

    -- Generic ternary case.
    CmmMachOp op [x, y, z] ->
      case op of

        -- Floating-point fused multiply-add operations
        MO_FMA var l w
          | l == 1
          -> case var of
            FMAdd  -> float3Op w (\d n m a -> unitOL $ FMA FMAdd  d n m a)
            FMSub  -> float3Op w (\d n m a -> unitOL $ FMA FMSub d n m a)
            FNMAdd -> float3Op w (\d n m a -> unitOL $ FMA FNMSub  d n m a)
            FNMSub -> float3Op w (\d n m a -> unitOL $ FMA FNMAdd d n m a)
          | otherwise
          -> sorry "The RISCV64 backend does not (yet) support vectors."

        _ -> pprPanic "getRegister' (unhandled ternary CmmMachOp): " $ (pprMachOp op) <+> text "in" <+> (pdoc plat expr)

      where
          float3Op w op = do
            (reg_fx, format_x, code_fx) <- getFloatReg x
            (reg_fy, format_y, code_fy) <- getFloatReg y
            (reg_fz, format_z, code_fz) <- getFloatReg z
            massertPpr (isFloatFormat format_x && isFloatFormat format_y && isFloatFormat format_z) $
              text "float3Op: non-float"
            pure $
              Any (floatFormat w) $ \ dst ->
                code_fx `appOL`
                code_fy `appOL`
                code_fz `appOL`
                op (OpReg w dst) (OpReg w reg_fx) (OpReg w reg_fy) (OpReg w reg_fz)

    CmmMachOp _op _xs
      -> pprPanic "getRegister' (variadic CmmMachOp): " (pdoc plat expr)

  where
    -- N.B. MUL does not set the overflow flag.
    -- Return 0 when the operation cannot overflow, /= 0 otherwise
    do_mul_may_oflo :: Width -> CmmExpr -> CmmExpr -> NatM Register
    do_mul_may_oflo W64 x y = do
      (reg_x, _format_x, code_x) <- getSomeReg x
      (reg_y, _format_y, code_y) <- getSomeReg y
      lo <- getNewRegNat II64
      hi <- getNewRegNat II64
      return $ Any (intFormat W64) (\dst ->
        code_x `appOL`
        code_y `snocOL`
        MULH (OpReg W64 hi) (OpReg W64 reg_x)  (OpReg W64 reg_y) `snocOL`
        MUL  (OpReg W64 lo) (OpReg W64 reg_x)  (OpReg W64 reg_y) `snocOL`
        SRA  (OpReg W64 lo) (OpReg W64 lo)     (OpImm (ImmInt 63)) `snocOL`
        CSET NE (OpReg W64 dst) (OpReg W64 hi)  (OpReg W64 lo)
                                 )

    do_mul_may_oflo W32 x y = do
        (reg_x, _format_x, code_x) <- getSomeReg x
        (reg_y, _format_y, code_y) <- getSomeReg y
        tmp1 <- getNewRegNat II64
        tmp2 <- getNewRegNat II64
        return $ Any (intFormat W32) (\dst ->
            code_x `appOL`
            code_y `snocOL`
            MULW (OpReg W64 tmp1) (OpReg W64 reg_x) (OpReg W64 reg_y) `snocOL`
            ADD (OpReg W64 tmp2) (OpReg W32 tmp1) (OpImm (ImmInt 0)) `snocOL`
            CSET NE (OpReg W64 dst) (OpReg W64 tmp1)  (OpReg W64 tmp2) `appOL`
            truncateReg W64 W32 dst
                                     )

    -- General case
    do_mul_may_oflo w x y = do
      -- Assert: 8bit * 8bit cannot overflow 16bit, and so on.
      (reg_x, format_x, code_x) <- getSomeReg x
      (reg_y, format_y, code_y) <- getSomeReg y
      tmp1 <- getNewRegNat II64
      tmp2 <- getNewRegNat II64
      let width_x = formatToWidth format_x
          width_y = formatToWidth format_y
          extend dst src =
            case w of
              W8  -> SLL (OpReg W64 dst) (OpReg W32 src) (OpImm (ImmInt 0))
              W16 -> SLL (OpReg W64 dst) (OpReg W32 src) (OpImm (ImmInt 0))
              _   -> panic "Must be in [W8, W16, W32]!"
          extract width dst src =
            case width of
              W8  -> EXT (OpReg W64 dst) (OpReg W8 src)
              W16 -> EXT (OpReg W64 dst) (OpReg W16 src)
              W32 -> SLL (OpReg W64 dst) (OpReg W32 src) (OpImm (ImmInt 0))
              _   -> panic "Must be in [W8, W16, W32]!"

      case w of
        w | (width_x < w) && (width_y < w) ->
          return $ Any (intFormat w) ( \dst ->
            unitOL $ annExpr expr (MOV (OpReg w dst) (OpImm (ImmInt 0)))
                                     )
        w | w <= W32 && width_x <= W32 && width_y <= W32 ->
            return $ Any (intFormat W32) (\dst ->
                code_x `appOL`
                code_y `appOL`
                -- signExtend [W8, W16] register to W64 and then SLL
                -- nil for W32
                signExtend (formatToWidth format_x) W64 reg_x reg_x `appOL`
                signExtend (formatToWidth format_y) W64 reg_y reg_y `snocOL`
                extend reg_x reg_x `snocOL`
                extend reg_y reg_y `snocOL`
                -- 64-bits MUL
                MUL (OpReg W64 tmp1) (OpReg W64 reg_x) (OpReg W64 reg_y) `snocOL`
                -- extract valid result via result's width
                -- slli.w for W32, otherwise ext.w.[b, h]
                extract w tmp2 tmp1 `snocOL`
                CSET NE (OpReg W64 dst) (OpReg W64 tmp1)  (OpReg W64 tmp2) `appOL`
                truncateReg W64 w dst
                                        )

        -- Should it be happened?
        _ ->
          return $ Any (intFormat w) ( \dst ->
            unitOL $ annExpr expr (MOV (OpReg w dst) (OpImm (ImmInt 1))))

-- Sign-extend the value in the given register from width @w@
-- up to width @w'@.
-- TODO: Is there room for optimization?
signExtend :: Width -> Width -> Reg -> Reg -> OrdList Instr
signExtend w w' r r'
  | w > w' = pprPanic "Sign-extend Error: not a sign extension, but a truncation." $ ppr w <> text "->" <+> ppr w'
  | w > W64 || w' > W64  = pprPanic "Sign-extend Error: from/to register width greater than 64-bit." $ ppr w <> text "->" <+> ppr w'
  | w == W64 && w' == W64 && r == r' = nilOL
  | w == W64 && w' == W64 = unitOL $ MOV (OpReg w' r') (OpReg w r)
  | w == W32 && w' == W64 = unitOL $ SLL (OpReg W64 r') (OpReg w r) (OpImm (ImmInt 0))
  -- Sign-extend W8 and W16 to W64.
  | w `elem` [W8, W16] = unitOL $ EXT (OpReg W64 r') (OpReg w r)
  | w == W32 && w' == W32 = unitOL $ MOV (OpReg w' r') (OpReg w r)
  | otherwise = pprPanic "signExtend: Unexpected width: " $ ppr w  <> text "->" <+> ppr w'

-- | Instructions to truncate the value in the given register from width @w@
-- down to width @w'@.
truncateReg :: Width -> Width -> Reg -> OrdList Instr
truncateReg w w' r
  | w > W64 || w' > W64  = pprPanic "Tructate Error: from/to register width greater than 64-bit." $ ppr w <> text "->" <+> ppr w'
  | w == w' = nilOL
  | w /= w' = toOL
    [
      ann
        (text "truncateReg: " <+> ppr r <+> ppr w <> text "->" <> ppr w')
        (BSTRPICK II64 (OpReg w' r) (OpReg w r) (OpImm (ImmInt shift)) (OpImm (ImmInt 0)))
    ]
  | otherwise = pprPanic "truncateReg: Unexpected width: " $ ppr w  <> text "->" <+> ppr w'
  where
    shift = (min (widthInBits w) (widthInBits w')) - 1

--  The 'Amode' type: Memory addressing modes passed up the tree.
data Amode = Amode AddrMode InstrBlock

-- | Provide the value of a `CmmExpr` with an `Amode`
--  N.B. this function should be used to provide operands to load and store
--  instructions with signed 12bit wide immediates (S & I types). For other
--  immediate sizes and formats (e.g. B type uses multiples of 2) this function
--  would need to be adjusted.
getAmode :: Platform
         -> Width     -- ^ width of loaded value
         -> CmmExpr
         -> NatM Amode

-- LD/ST: Immediate can be represented with 12bits
getAmode platform w (CmmRegOff reg off)
  | w <= W64, fitsInNbits 12 (fromIntegral off)
  = return $ Amode (AddrRegImm reg' off') nilOL
    where reg' = getRegisterReg platform reg
          off' = ImmInt off

-- For Stores we often see something like this:
-- CmmStore (CmmMachOp (MO_Add w) [CmmLoad expr, CmmLit (CmmInt n w')]) (expr2)
-- E.g. a CmmStoreOff really. This can be translated to `str $expr2, [$expr, #n ]
-- for `n` in range.
getAmode _platform _ (CmmMachOp (MO_Add _w) [expr, CmmLit (CmmInt off _w')])
  | fitsInNbits 12 (fromIntegral off)
  = do (reg, _format, code) <- getSomeReg expr
       return $ Amode (AddrRegImm reg (ImmInteger off)) code

getAmode _platform _ (CmmMachOp (MO_Sub _w) [expr, CmmLit (CmmInt off _w')])
  | fitsInNbits 12 (fromIntegral (-off))
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
            `appOL`   addr_code
            `snocOL`  ST rep (OpReg w src_reg) (OpAddr addr)
                     )

assignReg_IntCode _ reg src
  = do
    platform <- getPlatform
    let dst = getRegisterReg platform reg
    r <- getRegister src
    return $ case r of
      Any _ code              -> COMMENT (text "CmmAssign" <+> parens (text (show reg)) <+> parens (text (show src))) `consOL` code dst
      Fixed format freg fcode -> COMMENT (text "CmmAssign" <+> parens (text (show reg)) <+> parens (text (show src))) `consOL`
                                               (fcode `snocOL`
                                                 MOV (OpReg (formatToWidth format) dst) (OpReg (formatToWidth format) freg)
                                               )

-- Let's treat Floating point stuff
-- as integer code for now. Opaque.
assignMem_FltCode = assignMem_IntCode
assignReg_FltCode = assignReg_IntCode

-- Jumps
genJump :: CmmExpr{-the branch target-} -> NatM InstrBlock
-- `b label` may be optimal, but not the right one in some scenarios.
-- genJump expr@(CmmLit (CmmLabel lbl))
--   = return $ unitOL (annExpr expr (J (TLabel lbl)))
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
      CmmMachOp (MO_Eq W64) [x, CmmLit (CmmInt 0 _)] -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        return $
          code_x `snocOL`
          BEQZ (OpReg W64 reg_x) (TBlock bid)
      CmmMachOp (MO_Eq w) [x, CmmLit (CmmInt 0 _)]
        | w `elem` [W8, W16, W32] -> do
        (reg_x, format_x, code_x) <- getSomeReg x
        return $
          code_x `appOL`
          signExtend (formatToWidth format_x) W64 reg_x reg_x `snocOL`
          BEQZ (OpReg W64 reg_x) (TBlock bid)

      -- Optimized /= 0 case.
      CmmMachOp (MO_Ne W64) [x, CmmLit (CmmInt 0 _)] -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        return $ code_x `snocOL` (annExpr expr (BNEZ (OpReg W64 reg_x) (TBlock bid)))
      CmmMachOp (MO_Ne w) [x, CmmLit (CmmInt 0 _)]
        | w `elem` [W8, W16, W32] -> do
        (reg_x, format_x, code_x) <- getSomeReg x
        return $
          code_x `appOL`
          signExtend (formatToWidth format_x) W64 reg_x reg_x `snocOL`
          BNEZ (OpReg W64 reg_x) (TBlock bid)

      -- Generic case.
      CmmMachOp mop [x, y] -> do

        let ubcond w cmp | w `elem` [W8, W16, W32] = do
              (reg_x, format_x, code_x) <- getSomeReg x
              (reg_y, format_y, code_y) <- getSomeReg y
              reg_t <- getNewRegNat (intFormat W64)
              return $
                code_x `appOL`
                truncateReg (formatToWidth format_x) W64 reg_x  `appOL`
                code_y `appOL`
                truncateReg (formatToWidth format_y) W64 reg_y  `snocOL`
                MOV (OpReg W64 reg_t) (OpImm (ImmInt 12)) `snocOL`
                BCOND cmp (OpReg W64 reg_x) (OpReg W64 reg_y) (TBlock bid) (OpReg W64 reg_t)
            ubcond _w cmp = do
              (reg_x, _format_x, code_x) <- getSomeReg x
              (reg_y, _format_y, code_y) <- getSomeReg y
              reg_t <- getNewRegNat (intFormat W64)
              return $
                code_x `appOL`
                code_y `snocOL`
                MOV (OpReg W64 reg_t) (OpImm (ImmInt 12)) `snocOL`
                BCOND cmp (OpReg W64 reg_x) (OpReg W64 reg_y) (TBlock bid) (OpReg W64 reg_t)


            sbcond w cmp | w `elem` [W8, W16, W32] = do
              (reg_x, format_x, code_x) <- getSomeReg x
              (reg_y, format_y, code_y) <- getSomeReg y
              reg_t <- getNewRegNat (intFormat W64)
              return $
                code_x `appOL`
                signExtend (formatToWidth format_x) W64 reg_x reg_x `appOL`
                code_y `appOL`
                signExtend (formatToWidth format_y) W64 reg_y reg_y `snocOL`
                MOV (OpReg W64 reg_t) (OpImm (ImmInt 13)) `snocOL`
                BCOND cmp (OpReg W64 reg_x) (OpReg W64 reg_y) (TBlock bid) (OpReg W64 reg_t)

            sbcond _w cmp = do
              (reg_x, _format_x, code_x) <- getSomeReg x
              (reg_y, _format_y, code_y) <- getSomeReg y
              reg_t <- getNewRegNat (intFormat W64)
              return $
                code_x `appOL`
                code_y `snocOL`
                MOV (OpReg W64 reg_t) (OpImm (ImmInt 13)) `snocOL`
                BCOND cmp (OpReg W64 reg_x) (OpReg W64 reg_y) (TBlock bid) (OpReg W64 reg_t)


            fbcond w cmp = do
              (reg_fx, _format_fx, code_fx) <- getFloatReg x
              (reg_fy, _format_fy, code_fy) <- getFloatReg y
              rst <- OpReg W64 <$> getNewRegNat II64
              oneReg <- OpReg W64 <$> getNewRegNat II64
              reg_t <- getNewRegNat (intFormat W64)
              return $
                code_fx `appOL`
                code_fy `snocOL`
                MOV (OpReg W64 reg_t) (OpImm (ImmInt 14)) `snocOL`
                CSET cmp rst (OpReg w reg_fx) (OpReg w reg_fy) `snocOL`
                MOV oneReg (OpImm (ImmInt 1)) `snocOL`
                BCOND EQ rst oneReg (TBlock bid) (OpReg W64 reg_t)


        case mop of
          MO_F_Eq w -> fbcond w EQ
          MO_F_Ne w -> fbcond w NE
          MO_F_Gt w -> fbcond w FGT
          MO_F_Ge w -> fbcond w FGE
          MO_F_Lt w -> fbcond w FLT
          MO_F_Le w -> fbcond w FLE

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
          _ -> pprPanic "LA64.genCondJump:case mop: " (text $ show expr)

      _ -> pprPanic "LA64.genCondJump: " (text $ show expr)


-- | Generate conditional branching instructions
-- This is basically an "if with else" statement.
genCondBranch ::
  BlockId ->
  BlockId ->
  CmmExpr ->
  NatM InstrBlock
genCondBranch true false expr = do
  b1 <- genCondJump true expr
  b2 <- genBranch false
  return (b1 `appOL` b2)

-- -----------------------------------------------------------------------------
{-
Generating C calls

Generate a call to a C function:

GARs: 8 general-purpose registers $a0 - $a7, where $a0 and $a1 are also used for
integral values.
FARs: 8 floating-point registers $fa0 - $fa7, where $fa0 and $fa1 are also used
for returning values.

An argument is passed using the stack only when no appropriate argument register
is available.

Subroutines should ensure that the initial values of the general-purpose registers
$s0 - $s9 and floating-point registers $fs0 - $fs7 are preserved across the call.

At the entry of a procedure call, the return address of the call site is stored
in $ra. A branch jump to this address should be the last instruction executed in
the called procedure.

The on-stack part of the structure and scalar arguments are aligned to the greater
of the type alignment and GRLEN bits, except when this alignment is larger than
 the 16-byte stack alignment. In this case, the part of the argument should be
16-byte-aligned.

In a procedure call, GARs / FARs are generally only used for passing non-floating
-point / floating-point argument data, respectively. However, the floating-point
member of a structure or union argument, or a vector/floating-point argument
wider than FRLEN may be passed in a GAR.
-}

genCCall
    :: ForeignTarget      -- function to call
    -> [CmmFormal]        -- where to put the result
    -> [CmmActual]        -- arguments (of mixed type)
    -> NatM InstrBlock

-- TODO: Specialize where we can.
-- Generic impl
genCCall target dest_regs arg_regs = do
  case target of
    -- The target :: ForeignTarget call can either
    -- be a foreign procedure with an address expr
    -- and a calling convention.
    ForeignTarget expr _cconv -> do
--      (call_target, call_target_code) <- case expr of
--        -- if this is a label, let's just directly to it.  This will produce the
--        -- correct CALL relocation for BL.
--        (CmmLit (CmmLabel lbl)) -> pure (TLabel lbl, nilOL)
--        -- if it's not a label, let's compute the expression into a
--        -- register and jump to that.
--        _ -> do
      (call_target_reg, call_target_code) <- do
        (reg, _format, reg_code) <- getSomeReg expr
        pure (reg, reg_code)
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

      (stackSpaceWords, passRegs, passArgumentsCode) <- passArguments allGpArgRegs allFpArgRegs arg_regs'' 0 [] nilOL

      readResultsCode <- readResults allGpArgRegs allFpArgRegs dest_regs [] nilOL

      let moveStackDown 0 = toOL [ PUSH_STACK_FRAME
                                 , DELTA (-16)
                                 ]
          moveStackDown i | odd i = moveStackDown (i + 1)
          moveStackDown i = toOL [ PUSH_STACK_FRAME
                                 , SUB (OpReg W64 (spMachReg)) (OpReg W64 (spMachReg)) (OpImm (ImmInt (8 * i)))
                                 , DELTA (-8 * i - 16)
                                 ]
          moveStackUp 0 = toOL [ POP_STACK_FRAME
                               , DELTA 0
                               ]
          moveStackUp i | odd i = moveStackUp (i + 1)
          moveStackUp i = toOL [ ADD (OpReg W64 (spMachReg)) (OpReg W64 (spMachReg)) (OpImm (ImmInt (8 * i)))
                               , POP_STACK_FRAME
                               , DELTA 0
                               ]

      let code =
            call_target_code -- compute the label (possibly into a register)
              `appOL` moveStackDown (stackSpaceWords)
              `appOL` passArgumentsCode -- put the arguments into x0, ...
              -- `snocOL` BL call_target passRegs -- branch and link (C calls aren't tail calls, but return)
              `snocOL` BL (TReg call_target_reg) passRegs -- branch and link (C calls aren't tail calls, but return)
              `appOL` readResultsCode -- parse the results into registers
              `appOL` moveStackUp (stackSpaceWords)
      return code

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

        -- Optional MachOps
        -- These are enabled/disabled by backend flags: GHC.StgToCmm.Config
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

        MO_VS_Quot {} -> unsupported mop
        MO_VS_Rem {}  -> unsupported mop
        MO_VU_Quot {} -> unsupported mop
        MO_VU_Rem {}  -> unsupported mop
        MO_I64X2_Min -> unsupported mop
        MO_I64X2_Max -> unsupported mop
        MO_W64X2_Min -> unsupported mop
        MO_W64X2_Max -> unsupported mop

        -- Memory Ordering
        -- A hint value of 0 is mandatory by default, and it indicates a fully functional synchronization barrier.
        -- Only after all previous load/store access operations are completely executed, the DBAR 0 instruction can be executed;
        -- and only after the execution of DBAR 0 is completed, all subsequent load/store access operations can be executed.

        MO_AcquireFence -> pure (unitOL (DBAR Hint0))
        MO_ReleaseFence -> pure (unitOL (DBAR Hint0))
        MO_SeqCstFence  -> pure (unitOL (DBAR Hint0))

        MO_Touch        -> pure nilOL -- Keep variables live (when using interior pointers)
        -- Prefetch
        MO_Prefetch_Data _n -> pure nilOL -- Prefetch hint.

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

        mo@(MO_AtomicRead w ord)
          | [p_reg] <- arg_regs
          , [dst_reg] <- dest_regs -> do
              (p, _fmt_p, code_p) <- getSomeReg p_reg
              platform <- getPlatform
              let instrs = case ord of
                      MemOrderRelaxed -> unitOL $ ann moDescr (LD (intFormat w) (OpReg w dst) (OpAddr $ AddrReg p))

                      MemOrderAcquire -> toOL [
                                                ann moDescr (LD (intFormat w) (OpReg w dst) (OpAddr $ AddrReg p)),
                                                DBAR Hint0
                                              ]
                      MemOrderSeqCst -> toOL [
                                                ann moDescr (DBAR Hint0),
                                                LD (intFormat w) (OpReg w dst) (OpAddr $ AddrReg p),
                                                DBAR Hint0
                                              ]
                      _ -> panic $ "Unexpected MemOrderRelease on an AtomicRead: " ++ show mo
                  dst = getRegisterReg platform (CmmLocal dst_reg)
                  moDescr = (text . show) mo
                  code = code_p `appOL` instrs
              pure code
          | otherwise -> panic "mal-formed AtomicRead"

        mo@(MO_AtomicWrite w ord)
          | [p_reg, val_reg] <- arg_regs -> do
              (p, _fmt_p, code_p) <- getSomeReg p_reg
              (val, fmt_val, code_val) <- getSomeReg val_reg
              let instrs = case ord of
                      MemOrderRelaxed -> unitOL $ ann moDescr (ST fmt_val (OpReg w val) (OpAddr $ AddrReg p))
                      MemOrderRelease -> toOL [
                                                ann moDescr (DBAR Hint0),
                                                ST fmt_val (OpReg w val) (OpAddr $ AddrReg p)
                                              ]
                      MemOrderSeqCst  -> toOL [
                                                ann moDescr (DBAR Hint0),
                                                ST fmt_val (OpReg w val) (OpAddr $ AddrReg p),
                                                DBAR Hint0
                                              ]
                      _ ->  panic $ "Unexpected MemOrderAcquire on an AtomicWrite" ++ show mo
                  moDescr = (text . show) mo
                  code =
                    code_p `appOL`
                    code_val `appOL`
                    instrs
              pure code
          | otherwise -> panic "mal-formed AtomicWrite"

        MO_AtomicRMW w amop -> mkCCall (atomicRMWLabel w amop)
        MO_Cmpxchg w        -> mkCCall (cmpxchgLabel w)
        MO_Xchg w           -> mkCCall (xchgLabel w)

  where
    unsupported :: Show a => a -> b
    unsupported mop = panic ("outOfLineCmmOp: " ++ show mop
                          ++ " not supported here")

    mkCCall :: FastString -> NatM InstrBlock
    mkCCall name = do
      config <- getConfig
      target <-
        cmmMakeDynamicReference config CallReference
          $ mkForeignLabel name ForeignLabelInThisPackage IsFunction
      let cconv = ForeignConvention CCallConv [NoHint] [NoHint] CmmMayReturn
      genCCall (ForeignTarget target cconv) dest_regs arg_regs

    -- Implementiation of the LoongArch ABI calling convention.
    -- https://github.com/loongson/la-abi-specs/blob/release/lapcs.adoc#passing-arguments
    passArguments :: [Reg] -> [Reg] -> [(Reg, Format, ForeignHint, InstrBlock)] -> Int -> [Reg] -> InstrBlock -> NatM (Int, [Reg], InstrBlock)

    -- 1. Base case: no more arguments to pass (left)
    passArguments _ _ [] stackSpaceWords accumRegs accumCode = return (stackSpaceWords, accumRegs, accumCode)

    -- 2. Still have GP regs, and we want to pass an GP argument.
    passArguments (gpReg : gpRegs) fpRegs ((r, format, _hint, code_r) : args) stackSpaceWords accumRegs accumCode | isIntFormat format = do
      let w = formatToWidth format
          ext
            -- Specifically, LoongArch64's ABI requires that the caller
            -- sign-extend arguments which are smaller than 64-bits.
            | w `elem` [W8, W16, W32]
            = case w of
              W8  -> EXT (OpReg W64 gpReg) (OpReg w r)
              W16 -> EXT (OpReg W64 gpReg) (OpReg w r)
              W32 -> SLL (OpReg W64 gpReg) (OpReg w r) (OpImm (ImmInt 0))
              _ -> panic "Unexpected width(Here w < W64)!"
            | otherwise
            = MOV (OpReg w gpReg) (OpReg w r)
          accumCode' = accumCode `appOL`
                          code_r `snocOL`
                          ann (text "Pass gp argument: " <> ppr r) ext

      passArguments gpRegs fpRegs args stackSpaceWords (gpReg : accumRegs) accumCode'

    -- 3. Still have FP regs, and we want to pass an FP argument.
    passArguments gpRegs (fpReg : fpRegs) ((r, format, _hint, code_r) : args) stackSpaceWords accumRegs accumCode | isFloatFormat format = do
      let w = formatToWidth format
          mov = MOV (OpReg w fpReg) (OpReg w r)
          accumCode' = accumCode `appOL`
                       code_r `snocOL`
                       ann (text "Pass fp argument: " <> ppr r) mov

      passArguments gpRegs fpRegs args stackSpaceWords (fpReg : accumRegs) accumCode'

    -- 4. No mor regs left to pass. Must pass on stack.
    passArguments [] [] ((r, format, _hint, code_r) : args) stackSpaceWords accumRegs accumCode = do
      let w = formatToWidth format
          spOffet = 8 * stackSpaceWords
          str = ST format (OpReg w r) (OpAddr (AddrRegImm spMachReg (ImmInt spOffet)))
          stackCode =
            code_r
              `snocOL` (MOV (OpReg w tmpReg) (OpReg w r))
              `appOL` truncateReg w W64 tmpReg
              `snocOL` ann (text "Pass signed argument (size " <> ppr w <> text ") on the stack: " <> ppr tmpReg) str

      passArguments [] [] args (stackSpaceWords + 1) accumRegs (stackCode `appOL` accumCode)

    -- 5. Still have fpRegs left, but want to pass a GP argument. Must be passed on the stack then.
    passArguments [] fpRegs ((r, format, _hint, code_r) : args) stackSpaceWords accumRegs accumCode | isIntFormat format = do
      let w = formatToWidth format
          spOffet = 8 * stackSpaceWords
          str = ST format (OpReg w r) (OpAddr (AddrRegImm spMachReg (ImmInt spOffet)))
          stackCode =
            code_r
              `snocOL` ann (text "Pass argument (size " <> ppr w <> text ") on the stack: " <> ppr r) str

      passArguments [] fpRegs args (stackSpaceWords + 1) accumRegs (stackCode `appOL` accumCode)

   -- 6. Still have gpRegs left, but want to pass a FP argument. Must be passed in gpReg then.
    passArguments (gpReg : gpRegs) [] ((r, format, _hint, code_r) : args) stackSpaceWords accumRegs accumCode | isFloatFormat format = do
      let w = formatToWidth format
          mov = MOV (OpReg w gpReg) (OpReg w r)
          accumCode' = accumCode `appOL`
                       code_r `snocOL`
                       ann (text "Pass fp argument in gpReg: " <> ppr r) mov

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
    readResults (gpReg:gpRegs) (fpReg:fpRegs) (dst:dsts) accumRegs accumCode = do
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

    unaryFloatOp w op arg_reg dest_reg = do
      platform <- getPlatform
      (reg_fx, _format_x, code_fx) <- getFloatReg arg_reg
      let dst = getRegisterReg platform (CmmLocal dest_reg)
      let code = code_fx `appOL` op (OpReg w dst) (OpReg w reg_fx)
      pure code
