{-# language GADTs #-}
{-# language LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module GHC.CmmToAsm.LA64.CodeGen (
      cmmTopCodeGen
    , generateJumpTableForInstr
    , makeFarBranches
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
    getPlatform
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
import GHC.Utils.Monad
import Control.Monad
import GHC.Cmm.Dataflow.Label
import GHC.Types.Unique.DSM

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
    CmmUnsafeForeignCall target result_regs args ->
      case target of
        PrimTarget primOp       -> genPrim primOp result_regs args
        ForeignTarget addr conv -> genCCall addr conv result_regs args

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
        negate code w reg
          | w `elem` [W8, W16] = do
            return $ Any (intFormat w) $ \dst ->
                code `snocOL`
                EXT (OpReg W64 reg) (OpReg w reg) `snocOL`
                NEG (OpReg W64 dst) (OpReg W64 reg) `appOL`
                truncateReg W64 w dst
          | otherwise = do
            return $ Any (intFormat w) $ \dst ->
                code `snocOL`
                NEG (OpReg W64 dst) (OpReg w reg)

        ss_conv from to reg code
          | from `elem` [W8, W16] || to `elem` [W8, W16] = do
            return $ Any (intFormat to) $ \dst ->
                code `snocOL`
                EXT (OpReg W64 dst) (OpReg (min from to) reg) `appOL`
                -- At this point an 8- or 16-bit value would be sign-extended
                -- to 64-bits. Truncate back down the final width.
                truncateReg W64 to dst
          | from == W32 && to == W64 = do
            return $ Any (intFormat to) $ \dst ->
                code `snocOL`
                SLL (OpReg to dst) (OpReg from reg) (OpImm (ImmInt 0))
          | from == to = do
            return $ Any (intFormat from) $ \dst ->
                 code `snocOL` MOV (OpReg from dst) (OpReg from reg)
          | otherwise = do
            return $ Any (intFormat to) $ \dst ->
                code `appOL`
                signExtend from W64 reg dst `appOL`
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

    CmmMachOp (MO_Add w) [x, CmmLit (CmmInt n _)] | fitsInNbits 12 (fromIntegral n) -> do
      if w `elem` [W8, W16]
        then do
          (reg_x, _format_x, code_x) <- getSomeReg x
          return $ Any (intFormat w) (\dst ->
                                        code_x `snocOL`
                                        annExpr expr (EXT (OpReg W64 reg_x) (OpReg w reg_x)) `snocOL`
                                        ADD (OpReg W64 dst) (OpReg W64 reg_x) (OpImm (ImmInteger n))
                                     )
        else do
          (reg_x, _format_x, code_x) <- getSomeReg x
          return $ Any (intFormat w) (\dst -> code_x `snocOL` annExpr expr (ADD (OpReg W64 dst) (OpReg w reg_x) (OpImm (ImmInteger n))))

    CmmMachOp (MO_Sub w) [x, CmmLit (CmmInt n _)] | fitsInNbits 12 (fromIntegral n) -> do
      if w `elem` [W8, W16]
        then do
          (reg_x, _format_x, code_x) <- getSomeReg x
          return $ Any (intFormat w) (\dst ->
                                        code_x `snocOL`
                                        annExpr expr (EXT (OpReg W64 reg_x) (OpReg w reg_x)) `snocOL`
                                        SUB (OpReg W64 dst) (OpReg W64 reg_x) (OpImm (ImmInteger n))
                                     )
        else do
          (reg_x, _format_x, code_x) <- getSomeReg x
          return $ Any (intFormat w) (\dst -> code_x `snocOL` annExpr expr (SUB (OpReg W64 dst) (OpReg w reg_x) (OpImm (ImmInteger n))))

    CmmMachOp (MO_U_Quot w) [x, y]
      | w `elem` [W8, W16] -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        (reg_y, _format_y, code_y) <- getSomeReg y
        return $ Any (intFormat w) (\dst ->
                                      code_x `appOL`
                                      code_y `appOL`
                                      truncateReg w W64 reg_x `appOL`
                                      truncateReg w W64 reg_y `snocOL`
                                      annExpr expr (DIVU (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y))
                                   )

    -- 2. Shifts.
    CmmMachOp (MO_Shl w) [x, y] ->
      case y of
        CmmLit (CmmInt n _) | w `elem` [W8, W16], 0 <= n, n < fromIntegral (widthInBits w) -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          return $ Any (intFormat w) (\dst ->
                                        code_x `snocOL`
                                        annExpr expr (EXT (OpReg W64 reg_x) (OpReg w reg_x)) `snocOL`
                                        SLL (OpReg W64 dst) (OpReg W64 reg_x) (OpImm (ImmInteger n))
                                     )
        CmmLit (CmmInt n _) | 0 <= n, n < fromIntegral (widthInBits w) -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          return $ Any (intFormat w) (\dst -> code_x `snocOL` annExpr expr (SLL (OpReg W64 dst) (OpReg w reg_x) (OpImm (ImmInteger n))))

        _ | w `elem` [W8, W16] -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          (reg_y, _format_y, code_y) <- getSomeReg y
          return $ Any (intFormat w) (\dst ->
                                        code_x `appOL`
                                        code_y `snocOL`
                                        annExpr expr (EXT (OpReg W64 reg_x) (OpReg w reg_x)) `snocOL`
                                        EXT (OpReg W64 reg_y) (OpReg w reg_y) `snocOL`
                                        SLL (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y)
                                     )
        _ -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          (reg_y, _format_y, code_y) <- getSomeReg y
          return $ Any (intFormat w) (\dst ->
                                        code_x `appOL`
                                        code_y `snocOL`
                                        annExpr expr (SLL (OpReg W64 dst) (OpReg w reg_x) (OpReg w reg_y))
                                     )

    -- MO_S_Shr: signed-shift-right
    CmmMachOp (MO_S_Shr w) [x, y] ->
      case y of
        CmmLit (CmmInt n _) | w `elem` [W8, W16], 0 <= n, n < fromIntegral (widthInBits w) -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          return $ Any (intFormat w)  (\dst ->
                                        code_x `snocOL`
                                        annExpr expr (EXT (OpReg W64 reg_x) (OpReg w reg_x)) `snocOL`
                                        SRA (OpReg W64 dst) (OpReg W64 reg_x) (OpImm (ImmInteger n))
                                      )
        CmmLit (CmmInt n _) | 0 <= n, n < fromIntegral (widthInBits w) -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          return $ Any (intFormat w) (\dst -> code_x `snocOL` annExpr expr (SRA (OpReg W64 dst) (OpReg w reg_x) (OpImm (ImmInteger n))))

        _ | w `elem` [W8, W16] -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          (reg_y, _format_y, code_y) <- getSomeReg y
          return $ Any (intFormat w) (\dst ->
                                        code_x `appOL`
                                        code_y `snocOL`
                                        annExpr expr (EXT (OpReg W64 reg_x) (OpReg w reg_x)) `snocOL`
                                        EXT (OpReg W64 reg_y) (OpReg w reg_y) `snocOL`
                                        SRA (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y)
                                     )
        _ -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          (reg_y, _format_y, code_y) <- getSomeReg y
          return $ Any (intFormat w) (\dst ->
                                        code_x `appOL`
                                        code_y `snocOL`
                                        annExpr expr (SRA (OpReg W64 dst) (OpReg w reg_x) (OpReg w reg_y))
                                     )

    -- MO_U_Shr: unsigned-shift-right
    CmmMachOp (MO_U_Shr w) [x, y] ->
      case y of
        CmmLit (CmmInt n _) | w `elem` [W8, W16], 0 <= n, n < fromIntegral (widthInBits w) -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          return $ Any (intFormat w) (\dst ->
                                        code_x `appOL`
                                        truncateReg w W64 reg_x `snocOL`
                                        annExpr expr (SRL (OpReg W64 dst) (OpReg W64 reg_x) (OpImm (ImmInteger n)))
                                     )
        CmmLit (CmmInt n _) | 0 <= n, n < fromIntegral (widthInBits w) -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          return $ Any (intFormat w) (\dst -> code_x `snocOL` annExpr expr (SRL (OpReg W64 dst) (OpReg w reg_x) (OpImm (ImmInteger n))))

        _ | w `elem` [W8, W16] -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          (reg_y, _format_y, code_y) <- getSomeReg y
          return $ Any (intFormat w) (\dst ->
                                        code_x `appOL`
                                        code_y `appOL`
                                        truncateReg w W64 reg_x `appOL`
                                        truncateReg w W64 reg_y `snocOL`
                                        annExpr expr (SRL (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y))
                                     )
        _ -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          (reg_y, _format_y, code_y) <- getSomeReg y
          return $ Any (intFormat w) (\dst ->
                                        code_x `appOL`
                                        code_y `snocOL`
                                        annExpr expr (SRL (OpReg W64 dst) (OpReg w reg_x) (OpReg w reg_y))
                                     )

    -- 3. Logic &&, ||
    -- andi Instr's Imm-operand is zero-extended.
    CmmMachOp (MO_And w) [x, y] ->
      case y of
        CmmLit (CmmInt n _) | w `elem` [W8, W16, W32], (n :: Integer) >= 0, (n :: Integer) <= 4095 -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          return $ Any (intFormat w) (\dst ->
                                        code_x `appOL`
                                        truncateReg w W64 reg_x `snocOL`
                                        annExpr expr (AND (OpReg W64 dst) (OpReg W64 reg_x) (OpImm (ImmInteger n)))
                                     )

        CmmLit (CmmInt n _) | (n :: Integer) >= 0, (n :: Integer) <= 4095 -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          return $ Any (intFormat w) (\dst -> code_x `snocOL` annExpr expr (AND (OpReg W64 dst) (OpReg w reg_x) (OpImm (ImmInteger n))))

        CmmLit (CmmInt n _) | w `elem` [W8, W16, W32] -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          tmp <- getNewRegNat II64
          return $ Any (intFormat w) (\dst ->
                                       code_x `appOL`
                                       truncateReg w W64 reg_x `snocOL`
                                       annExpr expr (MOV (OpReg W64 tmp) (OpImm (ImmInteger n))) `snocOL`
                                       AND (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 tmp)
                                     )

        CmmLit (CmmInt n _) -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          tmp <- getNewRegNat II64
          return $ Any (intFormat w) (\dst ->
                                        code_x `snocOL`
                                        annExpr expr (MOV (OpReg W64 tmp) (OpImm (ImmInteger  n))) `snocOL`
                                        AND (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 tmp)
                                     )

        _ | w `elem` [W8, W16, W32] -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          (reg_y, _format_y, code_y) <- getSomeReg y
          return $ Any (intFormat w) (\dst ->
                                        code_x `appOL`
                                        code_y `appOL`
                                        truncateReg w W64 reg_x `appOL`
                                        truncateReg w W64 reg_y `snocOL`
                                        annExpr expr (AND (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y))
                                     )

        _ -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          (reg_y, _format_y, code_y) <- getSomeReg y
          return $ Any (intFormat w) (\dst ->
                                        code_x `appOL`
                                        code_y `snocOL`
                                        annExpr expr (AND (OpReg W64 dst) (OpReg w reg_x) (OpReg w reg_y))
                                     )

    -- ori Instr's Imm-operand is zero-extended.
    CmmMachOp (MO_Or w) [x, y] ->
      case y of
        CmmLit (CmmInt n _) | w `elem` [W8, W16, W32], (n :: Integer) >= 0, (n :: Integer) <= 4095 -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          return $ Any (intFormat w) (\dst ->
                                        code_x `appOL`
                                        truncateReg w W64 reg_x `snocOL`
                                        annExpr expr (OR (OpReg W64 dst) (OpReg W64 reg_x) (OpImm (ImmInteger n)))
                                     )

        CmmLit (CmmInt n _) | (n :: Integer) >= 0, (n :: Integer) <= 4095 -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          return $ Any (intFormat w) (\dst -> code_x `snocOL` annExpr expr (OR (OpReg W64 dst) (OpReg w reg_x) (OpImm (ImmInteger n))))

        CmmLit (CmmInt n _) | w `elem` [W8, W16, W32] -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          tmp <- getNewRegNat II64
          return $ Any (intFormat w) (\dst ->
                                       code_x `appOL`
                                       truncateReg w W64 reg_x `snocOL`
                                       annExpr expr (MOV (OpReg W64 tmp) (OpImm (ImmInteger n))) `snocOL`
                                       OR (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 tmp)
                                     )

        CmmLit (CmmInt n _) -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          tmp <- getNewRegNat II64
          return $ Any (intFormat w) (\dst ->
                                        code_x `snocOL`
                                        annExpr expr (MOV (OpReg W64 tmp) (OpImm (ImmInteger  n))) `snocOL`
                                        OR (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 tmp)
                                     )

        _ | w `elem` [W8, W16, W32] -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          (reg_y, _format_y, code_y) <- getSomeReg y
          return $ Any (intFormat w) (\dst ->
                                        code_x `appOL`
                                        code_y `appOL`
                                        truncateReg w W64 reg_x `appOL`
                                        truncateReg w W64 reg_y `snocOL`
                                        annExpr expr (OR (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y))
                                     )

        _ -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          (reg_y, _format_y, code_y) <- getSomeReg y
          return $ Any (intFormat w) (\dst ->
                                        code_x `appOL`
                                        code_y `snocOL`
                                        annExpr expr (OR (OpReg W64 dst) (OpReg w reg_x) (OpReg w reg_y))
                                     )

    -- xori Instr's Imm-operand is zero-extended.
    CmmMachOp (MO_Xor w) [x, y] ->
      case y of
        CmmLit (CmmInt n _) | w `elem` [W8, W16, W32], (n :: Integer) >= 0, (n :: Integer) <= 4095 -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          return $ Any (intFormat w) (\dst ->
                                        code_x `appOL`
                                        truncateReg w W64 reg_x `snocOL`
                                        annExpr expr (XOR (OpReg W64 dst) (OpReg W64 reg_x) (OpImm (ImmInteger n)))
                                     )

        CmmLit (CmmInt n _) | (n :: Integer) >= 0, (n :: Integer) <= 4095 -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          return $ Any (intFormat w) (\dst -> code_x `snocOL` annExpr expr (XOR (OpReg W64 dst) (OpReg w reg_x) (OpImm (ImmInteger n))))

        CmmLit (CmmInt n _) | w `elem` [W8, W16, W32] -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          tmp <- getNewRegNat II64
          return $ Any (intFormat w) (\dst ->
                                       code_x `appOL`
                                       truncateReg w W64 reg_x `snocOL`
                                       annExpr expr (MOV (OpReg W64 tmp) (OpImm (ImmInteger n))) `snocOL`
                                       XOR (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 tmp)
                                     )

        CmmLit (CmmInt n _) -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          tmp <- getNewRegNat II64
          return $ Any (intFormat w) (\dst ->
                                        code_x `snocOL`
                                        annExpr expr (MOV (OpReg W64 tmp) (OpImm (ImmInteger  n))) `snocOL`
                                        XOR (OpReg W64 dst) (OpReg w reg_x) (OpReg W64 tmp)
                                     )

        _ | w `elem` [W8, W16, W32] -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          (reg_y, _format_y, code_y) <- getSomeReg y
          return $ Any (intFormat w) (\dst ->
                                        code_x `appOL`
                                        code_y `appOL`
                                        truncateReg w W64 reg_x `appOL`
                                        truncateReg w W64 reg_y `snocOL`
                                        annExpr expr (XOR (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y))
                                     )

        _ -> do
          (reg_x, _format_x, code_x) <- getSomeReg x
          (reg_y, _format_y, code_y) <- getSomeReg y
          return $ Any (intFormat w) (\dst ->
                                        code_x `appOL`
                                        code_y `snocOL`
                                        annExpr expr (XOR (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y))
                                     )

    -- CSET commands register operand being W64.
    CmmMachOp (MO_Eq w) [x, y]
      | w `elem` [W8, W16, W32] -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        (reg_y, _format_y, code_y) <- getSomeReg y
        return $ Any (intFormat w) ( \dst ->
                                      code_x `appOL`
                                      code_y `appOL`
                                      signExtend w W64 reg_x reg_x `appOL`
                                      signExtend w W64 reg_y reg_y `snocOL`
                                      annExpr expr (CSET EQ (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y))
                                   )
       | otherwise -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        (reg_y, _format_y, code_y) <- getSomeReg y
        return $ Any (intFormat w) ( \dst ->
                                      code_x `appOL`
                                      code_y `snocOL`
                                      annExpr expr (CSET EQ (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y))
                                   )

    CmmMachOp (MO_Ne w) [x, y]
      | w `elem` [W8, W16, W32] -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        (reg_y, _format_y, code_y) <- getSomeReg y
        return $ Any (intFormat w) ( \dst ->
                                      code_x `appOL`
                                      code_y `appOL`
                                      signExtend w W64 reg_x reg_x `appOL`
                                      signExtend w W64 reg_y reg_y `snocOL`
                                      annExpr expr (CSET NE (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y))
                                   )
      | otherwise -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        (reg_y, _format_y, code_y) <- getSomeReg y
        return $ Any (intFormat w) ( \dst ->
                                      code_x `appOL`
                                      code_y `snocOL`
                                      annExpr expr (CSET NE (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y))
                                   )

    CmmMachOp (MO_S_Lt w) [x, CmmLit (CmmInt n _)]
      | w `elem` [W8, W16, W32]
      , fitsInNbits 12 (fromIntegral n) -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        return $ Any (intFormat w) ( \dst ->
                                      code_x `appOL`
                                      signExtend w W64 reg_x reg_x `snocOL`
                                      annExpr expr (SSLT (OpReg W64 dst) (OpReg W64 reg_x) (OpImm (ImmInteger n)))
                                   )
      | fitsInNbits 12 (fromIntegral n) -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        return $ Any (intFormat w) ( \dst -> code_x `snocOL` annExpr expr (SSLT (OpReg W64 dst) (OpReg W64 reg_x) (OpImm (ImmInteger n))))

    CmmMachOp (MO_U_Lt w) [x, CmmLit (CmmInt n _)]
      | w `elem` [W8, W16, W32]
      , fitsInNbits 12 (fromIntegral n) -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        return $ Any (intFormat w) ( \dst ->
                                      code_x `appOL`
                                      truncateReg w W64 reg_x `snocOL`
                                      annExpr expr (SSLTU (OpReg W64 dst) (OpReg W64 reg_x) (OpImm (ImmInteger n)))
                                   )
      | fitsInNbits 12 (fromIntegral n) -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        return $ Any (intFormat w) ( \dst -> code_x `snocOL` annExpr expr (SSLTU (OpReg W64 dst) (OpReg W64 reg_x) (OpImm (ImmInteger  n))))

    CmmMachOp (MO_S_Lt w) [x, y]
      | w `elem` [W8, W16, W32] -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        (reg_y, _format_y, code_y) <- getSomeReg y
        return $ Any (intFormat w) ( \dst ->
                                      code_x `appOL`
                                      code_y `appOL`
                                      signExtend w W64 reg_x reg_x `appOL`
                                      signExtend w W64 reg_y reg_y `snocOL`
                                      annExpr expr (CSET SLT (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y))
                                   )
      | otherwise -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        (reg_y, _format_y, code_y) <- getSomeReg y
        return $ Any (intFormat w) ( \dst ->
                                      code_x `appOL`
                                      code_y `snocOL`
                                      annExpr expr (CSET SLT (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y))
                                   )

    CmmMachOp (MO_S_Le w) [x, y]
      | w `elem` [W8, W16, W32] -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        (reg_y, _format_y, code_y) <- getSomeReg y
        return $ Any (intFormat w) ( \dst ->
                                      code_x `appOL`
                                      code_y `appOL`
                                      signExtend w W64 reg_x reg_x `appOL`
                                      signExtend w W64 reg_y reg_y `snocOL`
                                      annExpr expr (CSET SLE (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y))
                                   )
      | otherwise -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        (reg_y, _format_y, code_y) <- getSomeReg y
        return $ Any (intFormat w) ( \dst ->
                                      code_x `appOL`
                                      code_y `snocOL`
                                      annExpr expr (CSET SLE (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y))
                                   )

    CmmMachOp (MO_S_Ge w) [x, y]
      | w `elem` [W8, W16, W32] -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        (reg_y, _format_y, code_y) <- getSomeReg y
        return $ Any (intFormat w) ( \dst ->
                                      code_x `appOL`
                                      code_y `appOL`
                                      signExtend w W64 reg_x reg_x `appOL`
                                      signExtend w W64 reg_y reg_y `snocOL`
                                      annExpr expr (CSET SGE (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y))
                                   )
      | otherwise -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        (reg_y, _format_y, code_y) <- getSomeReg y
        return $ Any (intFormat w) ( \dst ->
                                      code_x `appOL`
                                      code_y `snocOL`
                                      annExpr expr (CSET SGE (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y))
                                   )

    CmmMachOp (MO_S_Gt w) [x, y]
      | w `elem` [W8, W16, W32] -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        (reg_y, _format_y, code_y) <- getSomeReg y
        return $ Any (intFormat w) ( \dst ->
                                      code_x `appOL`
                                      code_y `appOL`
                                      signExtend w W64 reg_x reg_x `appOL`
                                      signExtend w W64 reg_y reg_y `snocOL`
                                      annExpr expr (CSET SGT (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y))
                                   )
      | otherwise -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        (reg_y, _format_y, code_y) <- getSomeReg y
        return $ Any (intFormat w) ( \dst ->
                                      code_x `appOL`
                                      code_y `snocOL`
                                      annExpr expr (CSET SGT (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y))
                                   )

    CmmMachOp (MO_U_Lt w) [x, y]
      | w `elem` [W8, W16, W32] -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        (reg_y, _format_y, code_y) <- getSomeReg y
        return $ Any (intFormat w) ( \dst ->
                                      code_x `appOL`
                                      code_y `appOL`
                                      truncateReg w W64 reg_x `appOL`
                                      truncateReg w W64 reg_y `snocOL`
                                      annExpr expr (CSET ULT (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y))
                                   )
      | otherwise -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        (reg_y, _format_y, code_y) <- getSomeReg y
        return $ Any (intFormat w) ( \dst ->
                                      code_x `appOL`
                                      code_y `snocOL`
                                      annExpr expr (CSET ULT (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y))
                                   )

    CmmMachOp (MO_U_Le w) [x, y]
      | w `elem` [W8, W16, W32] -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        (reg_y, _format_y, code_y) <- getSomeReg y
        return $ Any (intFormat w) ( \dst ->
                                      code_x `appOL`
                                      code_y `appOL`
                                      truncateReg w W64 reg_x `appOL`
                                      truncateReg w W64 reg_y `snocOL`
                                      annExpr expr (CSET ULE (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y))
                                   )
      | otherwise -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        (reg_y, _format_y, code_y) <- getSomeReg y
        return $ Any (intFormat w) ( \dst ->
                                      code_x `appOL`
                                      code_y `snocOL`
                                      annExpr expr (CSET ULE (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y))
                                   )

    CmmMachOp (MO_U_Ge w) [x, y]
      | w `elem` [W8, W16, W32] -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        (reg_y, _format_y, code_y) <- getSomeReg y
        return $ Any (intFormat w) ( \dst ->
                                      code_x `appOL`
                                      code_y `appOL`
                                      truncateReg w W64 reg_x `appOL`
                                      truncateReg w W64 reg_y `snocOL`
                                      annExpr expr (CSET UGE (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y))
                                   )
      | otherwise -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        (reg_y, _format_y, code_y) <- getSomeReg y
        return $ Any (intFormat w) ( \dst ->
                                      code_x `appOL`
                                      code_y `snocOL`
                                      annExpr expr (CSET UGE (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y))
                                   )

    CmmMachOp (MO_U_Gt w) [x, y]
      | w `elem` [W8, W16, W32] -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        (reg_y, _format_y, code_y) <- getSomeReg y
        return $ Any (intFormat w) ( \dst ->
                                      code_x `appOL`
                                      code_y `appOL`
                                      truncateReg w W64 reg_x `appOL`
                                      truncateReg w W64 reg_y `snocOL`
                                      annExpr expr (CSET UGT (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y))
                                   )
      | otherwise -> do
        (reg_x, _format_x, code_x) <- getSomeReg x
        (reg_y, _format_y, code_y) <- getSomeReg y
        return $ Any (intFormat w) ( \dst ->
                                      code_x `appOL`
                                      code_y `snocOL`
                                      annExpr expr (CSET UGT (OpReg W64 dst) (OpReg W64 reg_x) (OpReg W64 reg_y))
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
            CSET NE (OpReg W64 dst) (OpReg W64 tmp1)  (OpReg W64 tmp2)
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
                CSET NE (OpReg W64 dst) (OpReg W64 tmp1)  (OpReg W64 tmp2)
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
  | w == W32 && w' == W64 = unitOL $ SLL (OpReg W64 r') (OpReg w r) (OpImm (ImmInt 0))
  -- Sign-extend W8 and W16 to W64.
  | w `elem` [W8, W16] = unitOL $ EXT (OpReg W64 r') (OpReg w r)
  | w == w' = unitOL $ MOV (OpReg w' r') (OpReg w r)
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
genJump expr = do
  case expr of
    (CmmLit (CmmLabel lbl)) -> do
      return $ unitOL (annExpr expr (TAIL36 (OpReg W64 tmpReg) (TLabel lbl)))
    (CmmLit (CmmBlock bid)) -> do
      return $ unitOL (annExpr expr (TAIL36 (OpReg W64 tmpReg) (TBlock bid)))
    _ -> do
      (target, _format, code) <- getSomeReg expr
      -- I'd like to do more.
      return $ COMMENT (text "genJump for unknow expr: " <+> (text (show expr))) `consOL`
        (code `appOL`
          unitOL (annExpr expr (J (TReg target)))
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
        let ubcond w cmp = do
              (reg_x, format_x, code_x) <- getSomeReg x
              (reg_y, format_y, code_y) <- getSomeReg y
              return $ case w of
                w | w `elem` [W8, W16, W32] ->
                    code_x `appOL`
                    truncateReg (formatToWidth format_x) W64 reg_x  `appOL`
                    code_y `appOL`
                    truncateReg (formatToWidth format_y) W64 reg_y  `snocOL`
                    BCOND1 cmp (OpReg W64 reg_x) (OpReg W64 reg_y) (TBlock bid)
                _ ->
                    code_x `appOL`
                    code_y `snocOL`
                    BCOND1 cmp (OpReg W64 reg_x) (OpReg W64 reg_y) (TBlock bid)

            sbcond w cmp = do
              (reg_x, format_x, code_x) <- getSomeReg x
              (reg_y, format_y, code_y) <- getSomeReg y
              return $ case w of
                w | w `elem` [W8, W16, W32] ->
                  code_x `appOL`
                  signExtend (formatToWidth format_x) W64 reg_x reg_x `appOL`
                  code_y `appOL`
                  signExtend (formatToWidth format_y) W64 reg_y reg_y `snocOL`
                  BCOND1 cmp (OpReg W64 reg_x) (OpReg W64 reg_y) (TBlock bid)
                _ ->
                  code_x `appOL`
                  code_y `snocOL`
                  BCOND1 cmp (OpReg W64 reg_x) (OpReg W64 reg_y) (TBlock bid)

            fbcond w cmp = do
              (reg_fx, _format_fx, code_fx) <- getFloatReg x
              (reg_fy, _format_fy, code_fy) <- getFloatReg y
              rst <- OpReg W64 <$> getNewRegNat II64
              oneReg <- OpReg W64 <$> getNewRegNat II64
              return $
                code_fx `appOL`
                code_fy `snocOL`
                CSET cmp rst (OpReg w reg_fx) (OpReg w reg_fy) `snocOL`
                MOV oneReg (OpImm (ImmInt 1)) `snocOL`
                BCOND1 EQ rst oneReg (TBlock bid)


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

genPrim
    :: CallishMachOp      -- MachOp
    -> [CmmFormal]        -- where to put the result
    -> [CmmActual]        -- arguments (of mixed type)
    -> NatM InstrBlock

genPrim MO_F32_Fabs          [dst]   [src]          = genFloatAbs W32 dst src
genPrim MO_F64_Fabs          [dst]   [src]          = genFloatAbs W64 dst src
genPrim MO_F32_Sqrt          [dst]   [src]          = genFloatSqrt FF32 dst src
genPrim MO_F64_Sqrt          [dst]   [src]          = genFloatSqrt FF64 dst src
genPrim (MO_Clz width)       [dst]   [src]          = genClz width dst src
genPrim (MO_Ctz width)       [dst]   [src]          = genCtz width dst src
genPrim (MO_BSwap width)     [dst]   [src]          = genByteSwap width dst src
genPrim (MO_BRev width)      [dst]   [src]          = genBitRev width dst src
genPrim MO_AcquireFence      []      []             = return $ unitOL (DBAR HintAcquire)
genPrim MO_ReleaseFence      []      []             = return $ unitOL (DBAR HintRelease)
genPrim MO_SeqCstFence       []      []             = return $ unitOL (DBAR HintSeqcst)
genPrim MO_Touch             []      [_]            = return nilOL
genPrim (MO_Prefetch_Data _n) []     [_]            = return nilOL
genPrim (MO_AtomicRead w mo)  [dst]  [addr]         = genAtomicRead w mo dst addr
genPrim (MO_AtomicWrite w mo) []     [addr,val]     = genAtomicWrite w mo addr val

genPrim mop@(MO_S_Mul2     _w) _         _          = unsupported mop
genPrim mop@(MO_S_QuotRem  _w) _         _          = unsupported mop
genPrim mop@(MO_U_QuotRem  _w) _         _          = unsupported mop
genPrim mop@(MO_U_QuotRem2 _w) _         _          = unsupported mop
genPrim mop@(MO_Add2       _w) _         _          = unsupported mop
genPrim mop@(MO_AddWordC   _w) _         _          = unsupported mop
genPrim mop@(MO_SubWordC   _w) _         _          = unsupported mop
genPrim mop@(MO_AddIntC    _w) _         _          = unsupported mop
genPrim mop@(MO_SubIntC    _w) _         _          = unsupported mop
genPrim mop@(MO_U_Mul2     _w) _         _          = unsupported mop
genPrim mop@MO_I64X2_Min       _         _          = unsupported mop
genPrim mop@MO_I64X2_Max       _         _          = unsupported mop
genPrim mop@MO_W64X2_Min       _         _          = unsupported mop
genPrim mop@MO_W64X2_Max       _         _          = unsupported mop
genPrim mop@MO_VS_Quot {}      _         _          = unsupported mop
genPrim mop@MO_VS_Rem  {}      _         _          = unsupported mop
genPrim mop@MO_VU_Quot {}      _         _          = unsupported mop
genPrim mop@MO_VU_Rem  {}      _         _          = unsupported mop

genPrim (MO_PopCnt width)    [dst]   [src]          = genLibCCall (popCntLabel width) [dst] [src]
genPrim (MO_Pdep width)      [dst]   [src,mask]     = genLibCCall (pdepLabel width) [dst] [src,mask]
genPrim (MO_Pext width)      [dst]   [src,mask]     = genLibCCall (pextLabel width) [dst] [src,mask]
genPrim (MO_UF_Conv width)   [dst]   [src]          = genLibCCall (word2FloatLabel width) [dst] [src]
genPrim (MO_AtomicRMW width amop)    [dst] [addr,n] = genLibCCall (atomicRMWLabel width amop) [dst] [addr,n]
genPrim (MO_Cmpxchg width)   [dst]   [addr,old,new] = genLibCCall (cmpxchgLabel width) [dst] [addr,old,new]
genPrim (MO_Xchg width)      [dst]   [addr,val]     = genLibCCall (xchgLabel width) [dst] [addr,val]
genPrim (MO_Memcpy _align)   []   [dst,src,n]       = genLibCCall (fsLit "memcpy")  [] [dst,src,n]
genPrim (MO_Memmove _align)  []   [dst,src,n]       = genLibCCall (fsLit "memmove") [] [dst,src,n]
genPrim (MO_Memcmp _align)   [rst]   [dst,src,n]    = genLibCCall (fsLit "memcmp")  [rst] [dst,src,n]
genPrim (MO_Memset _align)   []   [dst,cnt,n]       = genLibCCall (fsLit "memset")  [] [dst,cnt,n]
genPrim MO_F32_Sin           [dst]   [src]          = genLibCCall (fsLit "sinf") [dst] [src]
genPrim MO_F32_Cos           [dst]   [src]          = genLibCCall (fsLit "cosf") [dst] [src]
genPrim MO_F32_Tan           [dst]   [src]          = genLibCCall (fsLit "tanf") [dst] [src]
genPrim MO_F32_Exp           [dst]   [src]          = genLibCCall (fsLit "expf") [dst] [src]
genPrim MO_F32_ExpM1         [dst]   [src]          = genLibCCall (fsLit "expm1f") [dst] [src]
genPrim MO_F32_Log           [dst]   [src]          = genLibCCall (fsLit "logf") [dst] [src]
genPrim MO_F32_Log1P         [dst]   [src]          = genLibCCall (fsLit "log1pf") [dst] [src]
genPrim MO_F32_Asin          [dst]   [src]          = genLibCCall (fsLit "asinf") [dst] [src]
genPrim MO_F32_Acos          [dst]   [src]          = genLibCCall (fsLit "acosf") [dst] [src]
genPrim MO_F32_Atan          [dst]   [src]          = genLibCCall (fsLit "atanf") [dst] [src]
genPrim MO_F32_Sinh          [dst]   [src]          = genLibCCall (fsLit "sinhf") [dst] [src]
genPrim MO_F32_Cosh          [dst]   [src]          = genLibCCall (fsLit "coshf") [dst] [src]
genPrim MO_F32_Tanh          [dst]   [src]          = genLibCCall (fsLit "tanhf") [dst] [src]
genPrim MO_F32_Pwr           [dst]   [x,y]          = genLibCCall (fsLit "powf")  [dst] [x,y]
genPrim MO_F32_Asinh         [dst]   [src]          = genLibCCall (fsLit "asinhf") [dst] [src]
genPrim MO_F32_Acosh         [dst]   [src]          = genLibCCall (fsLit "acoshf") [dst] [src]
genPrim MO_F32_Atanh         [dst]   [src]          = genLibCCall (fsLit "atanhf") [dst] [src]
genPrim MO_F64_Sin           [dst]   [src]          = genLibCCall (fsLit "sin") [dst] [src]
genPrim MO_F64_Cos           [dst]   [src]          = genLibCCall (fsLit "cos") [dst] [src]
genPrim MO_F64_Tan           [dst]   [src]          = genLibCCall (fsLit "tan") [dst] [src]
genPrim MO_F64_Exp           [dst]   [src]          = genLibCCall (fsLit "exp") [dst] [src]
genPrim MO_F64_ExpM1         [dst]   [src]          = genLibCCall (fsLit "expm1") [dst] [src]
genPrim MO_F64_Log           [dst]   [src]          = genLibCCall (fsLit "log") [dst] [src]
genPrim MO_F64_Log1P         [dst]   [src]          = genLibCCall (fsLit "log1p") [dst] [src]
genPrim MO_F64_Asin          [dst]   [src]          = genLibCCall (fsLit "asin") [dst] [src]
genPrim MO_F64_Acos          [dst]   [src]          = genLibCCall (fsLit "acos") [dst] [src]
genPrim MO_F64_Atan          [dst]   [src]          = genLibCCall (fsLit "atan") [dst] [src]
genPrim MO_F64_Sinh          [dst]   [src]          = genLibCCall (fsLit "sinh") [dst] [src]
genPrim MO_F64_Cosh          [dst]   [src]          = genLibCCall (fsLit "cosh") [dst] [src]
genPrim MO_F64_Tanh          [dst]   [src]          = genLibCCall (fsLit "tanh") [dst] [src]
genPrim MO_F64_Pwr           [dst]   [x,y]          = genLibCCall (fsLit "pow")  [dst] [x,y]
genPrim MO_F64_Asinh         [dst]   [src]          = genLibCCall (fsLit "asinh") [dst] [src]
genPrim MO_F64_Acosh         [dst]   [src]          = genLibCCall (fsLit "acosh") [dst] [src]
genPrim MO_F64_Atanh         [dst]   [src]          = genLibCCall (fsLit "atanh") [dst] [src]
genPrim MO_SuspendThread     [tok]   [rs,i]         = genLibCCall (fsLit "suspendThread") [tok] [rs,i]
genPrim MO_ResumeThread      [rs]    [tok]          = genLibCCall (fsLit "resumeThread") [rs] [tok]
genPrim MO_I64_ToI           [dst]   [src]          = genLibCCall (fsLit "hs_int64ToInt") [dst] [src]
genPrim MO_I64_FromI         [dst]   [src]          = genLibCCall (fsLit "hs_intToInt64") [dst] [src]
genPrim MO_W64_ToW           [dst]   [src]          = genLibCCall (fsLit "hs_word64ToWord") [dst] [src]
genPrim MO_W64_FromW         [dst]   [src]          = genLibCCall (fsLit "hs_wordToWord64") [dst] [src]
genPrim MO_x64_Neg           [dst]   [src]          = genLibCCall (fsLit "hs_neg64") [dst] [src]
genPrim MO_x64_Add           [dst]   [src]          = genLibCCall (fsLit "hs_add64") [dst] [src]
genPrim MO_x64_Sub           [dst]   [src]          = genLibCCall (fsLit "hs_sub64") [dst] [src]
genPrim MO_x64_Mul           [dst]   [src]          = genLibCCall (fsLit "hs_mul64") [dst] [src]
genPrim MO_I64_Quot          [dst]   [src]          = genLibCCall (fsLit "hs_quotInt64") [dst] [src]
genPrim MO_I64_Rem           [dst]   [src]          = genLibCCall (fsLit "hs_remInt64") [dst] [src]
genPrim MO_W64_Quot          [dst]   [src]          = genLibCCall (fsLit "hs_quotWord64") [dst] [src]
genPrim MO_W64_Rem           [dst]   [src]          = genLibCCall (fsLit "hs_remWord64") [dst] [src]
genPrim MO_x64_And           [dst]   [src]          = genLibCCall (fsLit "hs_and64") [dst] [src]
genPrim MO_x64_Or            [dst]   [src]          = genLibCCall (fsLit "hs_or64") [dst] [src]
genPrim MO_x64_Xor           [dst]   [src]          = genLibCCall (fsLit "hs_xor64") [dst] [src]
genPrim MO_x64_Not           [dst]   [src]          = genLibCCall (fsLit "hs_not64") [dst] [src]
genPrim MO_x64_Shl           [dst]   [src]          = genLibCCall (fsLit "hs_uncheckedShiftL64") [dst] [src]
genPrim MO_I64_Shr           [dst]   [src]          = genLibCCall (fsLit "hs_uncheckedIShiftRA64") [dst] [src]
genPrim MO_W64_Shr           [dst]   [src]          = genLibCCall (fsLit "hs_uncheckedShiftRL64") [dst] [src]
genPrim MO_x64_Eq            [dst]   [src]          = genLibCCall (fsLit "hs_eq64") [dst] [src]
genPrim MO_x64_Ne            [dst]   [src]          = genLibCCall (fsLit "hs_ne64") [dst] [src]
genPrim MO_I64_Ge            [dst]   [src]          = genLibCCall (fsLit "hs_geInt64") [dst] [src]
genPrim MO_I64_Gt            [dst]   [src]          = genLibCCall (fsLit "hs_gtInt64") [dst] [src]
genPrim MO_I64_Le            [dst]   [src]          = genLibCCall (fsLit "hs_leInt64") [dst] [src]
genPrim MO_I64_Lt            [dst]   [src]          = genLibCCall (fsLit "hs_ltInt64") [dst] [src]
genPrim MO_W64_Ge            [dst]   [src]          = genLibCCall (fsLit "hs_geWord64") [dst] [src]
genPrim MO_W64_Gt            [dst]   [src]          = genLibCCall (fsLit "hs_gtWord64") [dst] [src]
genPrim MO_W64_Le            [dst]   [src]          = genLibCCall (fsLit "hs_leWord64") [dst] [src]
genPrim MO_W64_Lt            [dst]   [src]          = genLibCCall (fsLit "hs_ltWord64") [dst] [src]
genPrim op                   dst     args           = do
  platform <- ncgPlatform <$> getConfig
  pprPanic "genPrim: unknown primOp" (ppr (pprCallishMachOp op, dst, fmap (pdoc platform) args))


genFloatAbs :: Width -> LocalReg -> CmmExpr -> NatM InstrBlock
genFloatAbs w dst src = do
  platform <- getPlatform
  (reg_fx, _, code_fx) <- getFloatReg src
  let dst_reg = getRegisterReg platform (CmmLocal dst)
  return (code_fx `appOL` toOL
          [
            FABS (OpReg w dst_reg) (OpReg w reg_fx)
          ]
         )

genFloatSqrt :: Format -> LocalReg -> CmmExpr -> NatM InstrBlock
genFloatSqrt f dst src = do
  platform <- getPlatform
  (reg_fx, _, code_fx) <- getFloatReg src
  let dst_reg = getRegisterReg platform (CmmLocal dst)
  return (code_fx `appOL` toOL
          [
            FSQRT (OpReg w dst_reg) (OpReg w reg_fx)
          ]
         )
  where
    w = case f of
      FF32 -> W32
      _ -> W64

genClz :: Width -> LocalReg -> CmmExpr -> NatM InstrBlock
genClz w dst src = do
  platform <- getPlatform
  (reg_x, _, code_x) <- getSomeReg src
  let dst_reg = getRegisterReg platform (CmmLocal dst)
  if w `elem` [W32, W64] then do
    return (code_x `snocOL` CLZ (OpReg w dst_reg) (OpReg w reg_x))
  else if w `elem` [W8, W16] then do
    return (code_x `appOL` toOL
                 [
                  MOV (OpReg W64 dst_reg) (OpImm (ImmInt 1)),
                  SLL (OpReg W64 dst_reg) (OpReg W64 dst_reg) (OpImm (ImmInt (31-shift))),
                  SLL (OpReg W64 reg_x) (OpReg W64 reg_x) (OpImm (ImmInt (32-shift))),
                  OR (OpReg W64 dst_reg) (OpReg W64 dst_reg) (OpReg W64 reg_x),
                  CLZ (OpReg W64 dst_reg) (OpReg W32 dst_reg)
                 ]
           )
  else do
    pprPanic "genClz: invalid width: " (ppr w)
  where
    shift = widthToInt w

genCtz :: Width -> LocalReg -> CmmExpr -> NatM InstrBlock
genCtz w dst src = do
  platform <- getPlatform
  (reg_x, _, code_x) <- getSomeReg src
  let dst_reg = getRegisterReg platform (CmmLocal dst)
  if w `elem` [W32, W64] then do
    return (code_x `snocOL` CTZ (OpReg w dst_reg) (OpReg w reg_x))
  else if w `elem` [W8, W16] then do
    return (code_x `appOL` toOL
                 [
                  MOV (OpReg W64 dst_reg) (OpImm (ImmInt 1)),
                  SLL (OpReg W64 dst_reg) (OpReg W64 dst_reg) (OpImm (ImmInt shift)),
                  BSTRPICK II64 (OpReg W64 reg_x) (OpReg W64 reg_x) (OpImm (ImmInt (shift-1))) (OpImm (ImmInt 0)),
                  OR  (OpReg W64 dst_reg) (OpReg W64 dst_reg) (OpReg W64 reg_x),
                  CTZ (OpReg W64 dst_reg) (OpReg W64 dst_reg)
                 ]
           )
  else do
    pprPanic "genCtz: invalid width: " (ppr w)
  where
    shift = (widthToInt w)

genByteSwap :: Width -> LocalReg -> CmmExpr -> NatM InstrBlock
genByteSwap w dst src = do
  platform <- getPlatform
  (reg_x, _, code_x) <- getSomeReg src
  let dst_reg = getRegisterReg platform (CmmLocal dst)
  case w of
    W64 ->
      return (code_x `appOL` toOL
              [
                REVBD (OpReg w dst_reg) (OpReg w reg_x)
              ]
             )
    W32 ->
      return (code_x `appOL` toOL
              [
                REVB2W (OpReg w dst_reg) (OpReg w reg_x)
              ]
             )
    W16 ->
      return (code_x `appOL` toOL
              [
                REVB2H (OpReg w dst_reg) (OpReg w reg_x)
              ]
             )
    _ -> pprPanic "genBSwap: invalid width: " (ppr w)

genBitRev :: Width -> LocalReg -> CmmExpr -> NatM InstrBlock
genBitRev w dst src = do
  platform <- getPlatform
  (reg_x, _, code_x) <- getSomeReg src
  let dst_reg = getRegisterReg platform (CmmLocal dst)
  case w of
    W8 ->
      return (code_x `appOL` toOL
              [
                BITREV4B (OpReg W32 reg_x) (OpReg W32 reg_x),
                AND (OpReg W64 dst_reg) (OpReg W64 reg_x) (OpImm (ImmInt 255))
              ]
             )
    W16 ->
      return (code_x `appOL` toOL
              [
                BITREV (OpReg W64 reg_x) (OpReg W64 reg_x),
                SRL (OpReg W64 dst_reg) (OpReg W64 reg_x) (OpImm (ImmInt 48))
              ]
             )
    _ -> return ( code_x `snocOL` BITREV (OpReg w dst_reg) (OpReg w reg_x))

-- Generate C call to the given function in libc
genLibCCall :: FastString -> [CmmFormal] -> [CmmActual] -> NatM InstrBlock
genLibCCall name dsts args = do
  config <- getConfig
  target <-
    cmmMakeDynamicReference config CallReference
      $ mkForeignLabel name ForeignLabelInThisPackage IsFunction
  let cconv = ForeignConvention CCallConv [NoHint] [NoHint] CmmMayReturn
  genCCall target cconv dsts args

unsupported :: Show a => a -> b
unsupported mop = panic ("outOfLineCmmOp: " ++ show mop
                      ++ " not supported here")

-- AMSWAP_DB* insns implentment a fully functional synchronization barrier, like DBAR 0x0.
-- This is terrible. And AMSWAPDB only supports ISA version greater than LA64V1_0. So,
-- implement with DBAR
genAtomicRead :: Width -> MemoryOrdering -> LocalReg -> CmmExpr -> NatM InstrBlock
genAtomicRead w mo dst arg = do
  (addr_p, _, code_p) <- getSomeReg arg
  platform <- getPlatform
  let d = getRegisterReg platform (CmmLocal dst)
  case mo of
    MemOrderRelaxed ->
      return (code_p `appOL` toOL
              [
                LD (intFormat w) (OpReg w d) (OpAddr $ AddrReg addr_p)
              ]
             )

    MemOrderAcquire ->
      return (code_p `appOL` toOL
              [
                LD (intFormat w) (OpReg w d) (OpAddr $ AddrReg addr_p),
                DBAR HintAcquire
              ]
             )
    MemOrderSeqCst ->
      return (code_p `appOL` toOL
              [
                LD (intFormat w) (OpReg w d) (OpAddr $ AddrReg addr_p),
                DBAR HintSeqcst
              ]
             )
    _ -> panic $ "Unexpected MemOrderRelease on an AtomicRead: " ++ show mo

genAtomicWrite :: Width -> MemoryOrdering -> CmmExpr -> CmmExpr -> NatM InstrBlock
genAtomicWrite w mo addr val = do
  (addr_p, _, code_p) <- getSomeReg addr
  (val_reg, fmt_val, code_val) <- getSomeReg val
  case mo of
    MemOrderRelaxed ->
      return (code_p `appOL`code_val `appOL` toOL
              [
                ST fmt_val (OpReg w val_reg) (OpAddr $ AddrReg addr_p)
              ]
             )
    MemOrderRelease ->
      return (code_p `appOL`code_val `appOL` toOL
              [
                DBAR HintRelease,
                ST fmt_val (OpReg w val_reg) (OpAddr $ AddrReg addr_p)
              ]
             )
    MemOrderSeqCst ->
      return (code_p `appOL`code_val `appOL` toOL
              [
                DBAR HintSeqcst,
                ST fmt_val (OpReg w val_reg) (OpAddr $ AddrReg addr_p)
              ]
             )
    _ ->  panic $ "Unexpected MemOrderAcquire on an AtomicWrite" ++ show mo

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
  :: CmmExpr            -- address of func call
  -> ForeignConvention  -- calling convention
  -> [CmmFormal]        -- results
  -> [CmmActual]        -- arguments
  -> NatM InstrBlock


genCCall expr _conv@(ForeignConvention _ argHints _resHints _) dest_regs arg_regs = do
  (call_target, call_target_code) <- case expr of
    -- if this is a label, let's just directly to it.
    (CmmLit (CmmLabel lbl)) -> pure (TLabel lbl, nilOL)
    -- if it's not a label, let's compute the expression into a
    -- register and jump to that.
    _ -> do
      (reg, _format, reg_code) <- getSomeReg expr
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
  let
      arg_hints = take (length arg_regs) (argHints ++ repeat NoHint)
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
          `snocOL` CALL call_target passRegs -- branch and link (C calls aren't tail calls, but return)
          `appOL` readResultsCode -- parse the results into registers
          `appOL` moveStackUp (stackSpaceWords)
  return code

  where
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
      pprPanic "genCCall, out of gp registers when reading results" (pdoc platform expr)
    readResults _ [] _ _ _ = do
      platform <- getPlatform
      pprPanic "genCCall, out of fp registers when reading results" (pdoc platform expr)
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

data BlockInRange = InRange | NotInRange BlockId

genCondFarJump :: (MonadGetUnique m) => Cond -> Operand -> Operand -> BlockId -> m InstrBlock
genCondFarJump cond op1 op2 far_target = do
  return $ toOL [ ann (text "Conditional far jump to: " <> ppr far_target)
                $ BCOND cond op1 op2 (TBlock far_target)
                ]

makeFarBranches ::
  Platform ->
  LabelMap RawCmmStatics ->
  [NatBasicBlock Instr] ->
  UniqDSM [NatBasicBlock Instr]

makeFarBranches {- only used when debugging -} _platform statics basic_blocks = do
  -- All offsets/positions are counted in multiples of 4 bytes (the size of LoongArch64 instructions)
  -- That is an offset of 1 represents a 4-byte/one instruction offset.
  let (func_size, lblMap) = foldl' calc_lbl_positions (0, mapEmpty) basic_blocks
  if func_size < max_cond_jump_dist
    then pure basic_blocks
    else do
      (_, blocks) <- mapAccumLM (replace_blk lblMap) 0 basic_blocks
      pure $ concat blocks
  where
    max_cond_jump_dist = 2 ^ (15 :: Int) - 8 :: Int
    -- Currently all inline info tables fit into 64 bytes.
    max_info_size = 16 :: Int
    long_bc_jump_dist = 2 :: Int

    -- Replace out of range conditional jumps with unconditional jumps.
    replace_blk :: LabelMap Int -> Int -> GenBasicBlock Instr -> UniqDSM (Int, [GenBasicBlock Instr])
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

    replace_jump :: LabelMap Int -> Int -> Instr -> UniqDSM (Int, [Instr])
    replace_jump !m !pos instr = do
      case instr of
        ANN ann instr -> do
          replace_jump m pos instr >>= \case
            (idx, instr' : instrs') -> pure (idx, ANN ann instr' : instrs')
            (idx, []) -> pprPanic "replace_jump" (text "empty return list for " <+> ppr idx)

        BCOND1 cond op1 op2 t ->
          case target_in_range m t pos of
            InRange -> pure (pos + 1, [instr])
            NotInRange far_target -> do
              jmp_code <- genCondFarJump cond op1 op2 far_target
              pure (pos + long_bc_jump_dist, fromOL jmp_code)

        _ -> pure (pos + instr_size instr, [instr])

    target_in_range :: LabelMap Int -> Target -> Int -> BlockInRange
    target_in_range m target src =
      case target of
        (TReg{}) -> InRange
        (TBlock bid) -> block_in_range m src bid
        (TLabel clbl)
          | Just bid <- maybeLocalBlockLabel clbl
          -> block_in_range m src bid
          | otherwise
          -> InRange

    block_in_range :: LabelMap Int -> Int -> BlockId -> BlockInRange
    block_in_range m src_pos dest_lbl =
      case mapLookup dest_lbl m of
        Nothing ->
          pprTrace "not in range" (ppr dest_lbl) $ NotInRange dest_lbl
        Just dest_pos ->
          if abs (dest_pos - src_pos) < max_cond_jump_dist
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
      CSET {} -> 2
      LD _ _ (OpImm (ImmIndex _ _)) -> 3
      LD _ _ (OpImm (ImmCLbl _)) -> 2
      SCVTF {} -> 2
      FCVTZS {} -> 4
      BCOND {} -> long_bc_jump_dist
      CALL (TReg _) _ -> 1
      CALL {} -> 2
      CALL36 {} -> 2
      TAIL36 {} -> 2
      _ -> 1
