{-# language GADTs, LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module GHC.CmmToAsm.AArch64.CodeGen (
      cmmTopCodeGen
    , generateJumpTableForInstr
    , makeFarBranches
)

where

-- NCG stuff:
import GHC.Prelude hiding (EQ)

import Data.Word

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
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Dataflow.Graph
import GHC.Types.Tickish ( GenTickish(..) )
import GHC.Types.SrcLoc  ( srcSpanFile, srcSpanStartLine, srcSpanStartCol )
import GHC.Types.Unique.DSM

-- The rest:
import GHC.Data.OrdList
import GHC.Utils.Outputable

import Control.Monad    ( mapAndUnzipM, foldM )
import GHC.Float

import GHC.Types.Basic
import GHC.Types.ForeignCall
import GHC.Data.FastString
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Constants (debugIsOn)
import GHC.Utils.Monad (mapAccumLM)

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
      Just _picBase -> panic "AArch64.cmmTopCodeGen: picBase not implemented"
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
    Just (SourceNote span (LexicalFastString name))
      -> do fileId <- getFileId (srcSpanFile span)
            let line = srcSpanStartLine span; col = srcSpanStartCol span
            return $ unitOL $ LOCATION fileId line col (unpackFS name)
    _ -> return nilOL
  mid_instrs <- stmtsToInstrs stmts
  (!tail_instrs) <- stmtToInstrs tail
  let instrs = header_comment_instr `appOL` loc_instrs `appOL` mid_instrs `appOL` tail_instrs
  -- TODO: Then x86 backend run @verifyBasicBlock@ here and inserts
  --      unwinding info. See Ticket 19913
  -- code generation may introduce new basic block boundaries, which
  -- are indicated by the NEWBLOCK instruction.  We must split up the
  -- instruction stream into basic blocks again. Also, we may extract
  -- LDATAs here too (if they are implemented by AArch64 again - See
  -- PPC how to do that.)
  let
        (top,other_blocks,statics) = foldrOL mkBlocks ([],[],[]) instrs

  return (BasicBlock id top : other_blocks, statics)

mkBlocks :: Instr
          -> ([Instr], [GenBasicBlock Instr], [GenCmmDecl RawCmmStatics h g])
          -> ([Instr], [GenBasicBlock Instr], [GenCmmDecl RawCmmStatics h g])
mkBlocks (NEWBLOCK id) (instrs,blocks,statics)
  = ([], BasicBlock id instrs : blocks, statics)
mkBlocks instr (instrs,blocks,statics)
  = (instr:instrs, blocks, statics)
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
annExpr e instr {- debugIsOn -} = ANN (text . show $ e) instr
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
                 toOL [ CMP (OpReg w reg) (OpReg w keyReg)
                      , BCOND EQ (TBlock bid)
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

stmtsToInstrs :: [CmmNode O O] -- ^ Cmm Statements
              -> NatM InstrBlock -- ^ Resulting instructions
stmtsToInstrs stmts =
    go stmts nilOL
  where
    go []        instrs = return instrs
    go (s:stmts) instrs = do
      instrs' <- stmtToInstrs s
      go stmts (instrs `appOL` instrs')

stmtToInstrs :: CmmNode e x -- ^ Cmm Statement
             -> NatM InstrBlock -- ^ Resulting Instructions
stmtToInstrs stmt = do
  -- traceM $ "-- -------------------------- stmtToInstrs -------------------------- --\n"
  --     ++ showSDocUnsafe (ppr stmt)
  platform <- getPlatform
  case stmt of
    CmmUnsafeForeignCall target result_regs args
       -> genCCall target result_regs args

    _ -> case stmt of
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

getRegisterReg platform (CmmGlobal reg@(GlobalRegUse mid _))
  = case globalRegMaybe platform mid of
        Just reg -> RegReal reg
        Nothing  -> pprPanic "getRegisterReg-memory" (ppr $ CmmGlobal reg)
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

{- Note [Aarch64 immediates]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Aarch64 with it's fixed width instruction encoding uses leftover space for
immediates.
If you want the full rundown consult the arch reference document:
"Arm® Architecture Reference Manual" - "C3.4 Data processing - immediate"

The gist of it is that different instructions allow for different immediate encodings.
The ones we care about for better code generation are:

* Simple but potentially repeated bit-patterns for logic instructions.
* 16bit numbers shifted by multiples of 16.
* 12 bit numbers optionally shifted by 12 bits.

It might seem like the ISA allows for 64bit immediates but this isn't the case.
Rather there are some instruction aliases which allow for large unencoded immediates
which will then be transalted to one of the immediate encodings implicitly.

For example mov x1, #0x10000 is allowed but will be assembled to movz x1, #0x1, lsl #16
-}

-- | Move (wide immediate)
-- Allows for 16bit immediate which can be shifted by 0/16/32/48 bits.
-- Used with MOVZ,MOVN, MOVK
-- See Note [Aarch64 immediates]
getMovWideImm :: Integer -> Width -> Maybe Operand
getMovWideImm n w
  -- TODO: Handle sign extension/negatives
  | n <= 0
  = Nothing
  -- Fits in 16 bits
  | sized_n < 2^(16 :: Int)
  = Just $ OpImm (ImmInteger truncated)

  -- 0x0000 0000 xxxx 0000
  | trailing_zeros >= 16 && sized_n < 2^(32 :: Int)
  = Just $ OpImmShift (ImmInteger $ truncated `shiftR` 16) SLSL 16

  -- 0x 0000 xxxx 0000 0000
  | trailing_zeros >= 32 && sized_n < 2^(48 :: Int)
  = Just $ OpImmShift (ImmInteger $ truncated `shiftR` 32) SLSL 32

  -- 0x xxxx 0000 0000 0000
  | trailing_zeros >= 48
  = Just $ OpImmShift (ImmInteger $ truncated `shiftR` 48) SLSL 48

  | otherwise
  = Nothing
  where
    truncated = narrowU w n
    sized_n = fromIntegral truncated :: Word64
    trailing_zeros = countTrailingZeros sized_n

-- | Arithmetic(immediate)
--  Allows for 12bit immediates which can be shifted by 0 or 12 bits.
-- Used with ADD, ADDS, SUB, SUBS, CMP
-- See Note [Aarch64 immediates]
getArithImm :: Integer -> Width -> Maybe Operand
getArithImm n w
  -- TODO: Handle sign extension
  | n <= 0
  = Nothing
  -- Fits in 16 bits
  -- Fits in 12 bits
  | sized_n < 2^(12::Int)
  = Just $ OpImm (ImmInteger truncated)

  -- 12 bits shifted by 12 places.
  | trailing_zeros >= 12 && sized_n < 2^(24::Int)
  = Just $ OpImmShift (ImmInteger $ truncated `shiftR` 12) SLSL 12

  | otherwise
  = Nothing
  where
    sized_n = fromIntegral truncated :: Word64
    truncated = narrowU w n
    trailing_zeros = countTrailingZeros sized_n

-- |  Logical (immediate)
-- Allows encoding of some repeated bitpatterns
-- Used with AND, EOR, ORR
-- and their aliases which includes at least MOV (bitmask immediate)
-- See Note [Aarch64 immediates]
getBitmaskImm :: Integer -> Width -> Maybe Operand
getBitmaskImm n w
  | isAArch64Bitmask (opRegWidth w) truncated = Just $ OpImm (ImmInteger truncated)
  | otherwise = Nothing
  where
    truncated = narrowU w n

-- | Load/store immediate.
-- Depends on the width of the store to some extent.
isOffsetImm :: Int -> Width -> Bool
isOffsetImm off w
  -- 8 bits + sign for unscaled offsets
  | -256 <= off, off <= 255 = True
  -- Offset using 12-bit positive immediate, scaled by width
  -- LDR/STR: imm12: if reg is 32bit: 0 -- 16380 in multiples of 4
  -- LDR/STR: imm12: if reg is 64bit: 0 -- 32760 in multiples of 8
  -- 16-bit: 0 .. 8188, 8-bit: 0 -- 4095
  | 0 <= off, off < 4096 * byte_width, off `mod` byte_width == 0 = True
  | otherwise = False
  where
    byte_width = widthInBytes w




-- TODO OPT: we might be able give getRegister
--          a hint, what kind of register we want.
getFloatReg :: HasDebugCallStack => CmmExpr -> NatM (Reg, Format, InstrBlock)
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

-- Note [Signed arithmetic on AArch64]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Handling signed arithmetic on sub-word-size values on AArch64 is a bit
-- tricky as Cmm's type system does not capture signedness. While 32-bit values
-- are fairly easy to handle due to AArch64's 32-bit instruction variants
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
-- TODO:
--   Don't use Width in Operands
--   Instructions should rather carry a RegWidth
--
-- Note [Handling PIC on AArch64]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- AArch64 does not have a special PIC register, the general approach is to
-- simply go through the GOT, and there is assembly support for this:
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
--    TODO: this expression shouldn't even be generated to begin with.
getRegister' config plat (CmmMachOp (MO_Add w0) [x, CmmLit (CmmInt i w1)]) | i < 0
  = getRegister' config plat (CmmMachOp (MO_Sub w0) [x, CmmLit (CmmInt (-i) w1)])

getRegister' config plat (CmmMachOp (MO_Sub w0) [x, CmmLit (CmmInt i w1)]) | i < 0
  = getRegister' config plat (CmmMachOp (MO_Add w0) [x, CmmLit (CmmInt (-i) w1)])


-- Generic case.
getRegister' config plat expr
  = case expr of
    CmmReg (CmmGlobal (GlobalRegUse PicBaseReg _))
      -> pprPanic "getRegisterReg-memory" (ppr $ PicBaseReg)
    CmmLit lit
      -> case lit of

        -- Use wzr xzr for CmmInt 0 if the width matches up, otherwise do a move.
        -- TODO: Reenable after https://gitlab.haskell.org/ghc/ghc/-/issues/23632 is fixed.
        -- CmmInt 0 W32 -> do
        --   let format = intFormat W32
        --   return (Fixed format reg_zero (unitOL $ (COMMENT ((text . show $ expr))) ))
        -- CmmInt 0 W64 -> do
        --   let format = intFormat W64
        --   return (Fixed format reg_zero (unitOL $ (COMMENT ((text . show $ expr))) ))
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
        CmmInt i w | i >= 0
                   , Just imm_op <- getMovWideImm i w -> do
          return (Any (intFormat w) (\dst -> unitOL $ annExpr expr (MOVZ (OpReg w dst) imm_op)))

        CmmInt i w | isNbitEncodeable 16 i, i >= 0 -> do
          return (Any (intFormat w) (\dst -> unitOL $ annExpr expr (MOV (OpReg W16 dst) (OpImm (ImmInteger i)))))

        CmmInt i w | isNbitEncodeable 32 i, i >= 0 -> do
          let  half0 = fromIntegral (fromIntegral i :: Word16)
               half1 = fromIntegral (fromIntegral (i `shiftR` 16) :: Word16)
          return (Any (intFormat w) (\dst -> toOL [ annExpr expr
                                                  $ MOV (OpReg W32 dst) (OpImm (ImmInt half0))
                                                  , MOVK (OpReg W32 dst) (OpImmShift (ImmInt half1) SLSL 16)
                                                  ]))
        -- fallback for W32
        CmmInt i W32 -> do
          let  half0 = fromIntegral (fromIntegral i :: Word16)
               half1 = fromIntegral (fromIntegral (i `shiftR` 16) :: Word16)
          return (Any (intFormat W32) (\dst -> toOL [ annExpr expr
                                                    $ MOV (OpReg W32 dst) (OpImm (ImmInt half0))
                                                    , MOVK (OpReg W32 dst) (OpImmShift (ImmInt half1) SLSL 16)
                                                    ]))
        -- anything else
        CmmInt i W64 -> do
          let  half0 = fromIntegral (fromIntegral i :: Word16)
               half1 = fromIntegral (fromIntegral (i `shiftR` 16) :: Word16)
               half2 = fromIntegral (fromIntegral (i `shiftR` 32) :: Word16)
               half3 = fromIntegral (fromIntegral (i `shiftR` 48) :: Word16)
          return (Any (intFormat W64) (\dst -> toOL [ annExpr expr
                                                    $ MOV (OpReg W64 dst) (OpImm (ImmInt half0))
                                                    , MOVK (OpReg W64 dst) (OpImmShift (ImmInt half1) SLSL 16)
                                                    , MOVK (OpReg W64 dst) (OpImmShift (ImmInt half2) SLSL 32)
                                                    , MOVK (OpReg W64 dst) (OpImmShift (ImmInt half3) SLSL 48)
                                                    ]))
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
              half0 = fromIntegral (fromIntegral word :: Word16)
              half1 = fromIntegral (fromIntegral (word `shiftR` 16) :: Word16)
          tmp <- getNewRegNat (intFormat W32)
          return (Any (floatFormat W32) (\dst -> toOL [ annExpr expr
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
          return (Any (floatFormat W64) (\dst -> toOL [ annExpr expr
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
          return (Any format (\dst -> imm_code `snocOL` (annExpr expr $ LDR format (OpReg (formatToWidth format) dst) op)))

        CmmLabelOff _lbl off | isNbitEncodeable 12 (fromIntegral off) -> do
          (op, imm_code) <- litToImm' lit
          let rep = cmmLitType plat lit
              format = cmmTypeFormat rep
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
      -> return (Fixed (cmmTypeFormat (cmmRegType reg))
                       (getRegisterReg plat reg)
                       nilOL)
    CmmRegOff reg off ->
      -- If we got here we will load the address into a register either way. So we might as well just expand
      -- and re-use the existing code path to handle "reg + off".
      let !width = cmmRegWidth reg
      in getRegister' config plat (CmmMachOp (MO_Add width) [CmmReg reg, CmmLit (CmmInt (fromIntegral off) width)])

    -- for MachOps, see GHC.Cmm.MachOp
    -- For CmmMachOp, see GHC.Cmm.Expr

    -- Handle MO_RelaxedRead as a normal CmmLoad, to allow
    -- non-trivial addressing modes to be used.
    CmmMachOp (MO_RelaxedRead w) [e] ->
      getRegister (CmmLoad e (cmmBits w) NaturallyAligned)

    CmmMachOp op [e] -> do
      (reg, _format, code) <- getSomeReg e
      case op of
        MO_Not w -> return $ Any (intFormat w) $ \dst ->
            let w' = opRegWidth w
             in code `snocOL`
                MVN (OpReg w' dst) (OpReg w' reg) `appOL`
                truncateReg w' w dst -- See Note [Signed arithmetic on AArch64]

        MO_S_Neg w -> negate code w reg
        MO_F_Neg w -> return $ Any (floatFormat w) (\dst -> code `snocOL` NEG (OpReg w dst) (OpReg w reg))

        MO_SF_Round    from to -> return $ Any (floatFormat to) (\dst -> code `snocOL` SCVTF (OpReg to dst) (OpReg from reg))  -- (Signed ConVerT Float)
        MO_FS_Truncate from to -> return $ Any (intFormat to) (\dst -> code `snocOL` FCVTZS (OpReg to dst) (OpReg from reg)) -- (float convert (-> zero) signed)

        -- TODO this is very hacky
        -- Note, UBFM and SBFM expect source and target register to be of the same size, so we'll use @max from to@
        -- UBFM will set the high bits to 0. SBFM will copy the sign (sign extend).
        MO_UU_Conv from to -> return $ Any (intFormat to) (\dst -> code `snocOL` UBFM (OpReg (max from to) dst) (OpReg (max from to) reg) (OpImm (ImmInt 0)) (toImm (min from to)))
        MO_SS_Conv from to -> ss_conv from to reg code
        MO_FF_Conv from to -> return $ Any (floatFormat to) (\dst -> code `snocOL` FCVT (OpReg to dst) (OpReg from reg))
        MO_WF_Bitcast w    -> return $ Any (floatFormat w)  (\dst -> code `snocOL` FMOV (OpReg w dst) (OpReg w reg))
        MO_FW_Bitcast w    -> return $ Any (intFormat w)    (\dst -> code `snocOL` FMOV (OpReg w dst) (OpReg w reg))

        -- Conversions
        MO_XX_Conv _from to -> swizzleRegisterRep (intFormat to) <$> getRegister e

        MO_Eq {} -> notUnary
        MO_Ne {} -> notUnary
        MO_Mul {} -> notUnary
        MO_S_MulMayOflo {} -> notUnary
        MO_S_Quot {} -> notUnary
        MO_S_Rem {} -> notUnary
        MO_U_Quot {} -> notUnary
        MO_U_Rem {} -> notUnary
        MO_S_Ge {} -> notUnary
        MO_S_Le {} -> notUnary
        MO_S_Gt {} -> notUnary
        MO_S_Lt {} -> notUnary
        MO_U_Ge {} -> notUnary
        MO_U_Le {} -> notUnary
        MO_U_Gt {} -> notUnary
        MO_U_Lt {} -> notUnary
        MO_F_Add {} -> notUnary
        MO_F_Sub {} -> notUnary
        MO_F_Mul {} -> notUnary
        MO_F_Quot {} -> notUnary
        MO_FMA {} -> notUnary
        MO_F_Eq {} -> notUnary
        MO_F_Ne {} -> notUnary
        MO_F_Ge {} -> notUnary
        MO_F_Le {} -> notUnary
        MO_F_Gt {} -> notUnary
        MO_F_Lt {} -> notUnary
        MO_And {} -> notUnary
        MO_Or {} -> notUnary
        MO_Xor {} -> notUnary
        MO_Shl {} -> notUnary
        MO_U_Shr {} -> notUnary
        MO_S_Shr {} -> notUnary
        MO_V_Insert {} -> notUnary
        MO_V_Extract {} -> notUnary
        MO_V_Add {} -> notUnary
        MO_V_Sub {} -> notUnary
        MO_V_Mul {} -> notUnary
        MO_VS_Quot {} -> notUnary
        MO_VS_Rem {} -> notUnary
        MO_VS_Neg {} -> notUnary
        MO_VU_Quot {} -> notUnary
        MO_VU_Rem {} -> notUnary
        MO_V_Shuffle {} -> notUnary
        MO_VF_Shuffle  {} -> notUnary
        MO_VF_Insert {} -> notUnary
        MO_VF_Extract {} -> notUnary
        MO_VF_Add {} -> notUnary
        MO_VF_Sub {} -> notUnary
        MO_VF_Mul {} -> notUnary
        MO_VF_Quot {} -> notUnary
        MO_Add {} -> notUnary
        MO_Sub {} -> notUnary

        MO_AlignmentCheck {} ->
          pprPanic "getRegister' (monadic CmmMachOp):" (pdoc plat expr)

        MO_VF_Neg {} -> vectorsNeedLlvm
      where
        notUnary = pprPanic "getRegister' (non-unary CmmMachOp with 1 argument):" (pdoc plat expr)
        vectorsNeedLlvm =
            sorry "SIMD operations on AArch64 currently require the LLVM backend"
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

        ss_conv from to reg code =
            let w' = opRegWidth (max from to)
            in return $ Any (intFormat to) $ \dst ->
                code `snocOL`
                SBFM (OpReg w' dst) (OpReg w' reg) (OpImm (ImmInt 0)) (toImm (min from to)) `appOL`
                -- At this point an 8- or 16-bit value would be sign-extended
                -- to 32-bits. Truncate back down the final width.
                truncateReg w' to dst

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
    -- Immediates are handled via `getArithImm` in the generic code path.

    CmmMachOp (MO_U_Quot w) [x, y] | w == W8 -> do
      (reg_x, _format_x, code_x) <- getSomeReg x
      (reg_y, _format_y, code_y) <- getSomeReg y
      return $ Any (intFormat w) (\dst -> code_x `appOL` code_y `snocOL` annExpr expr (UXTB (OpReg w reg_x) (OpReg w reg_x)) `snocOL`
                                                                        (UXTB (OpReg w reg_y) (OpReg w reg_y)) `snocOL`
                                                                        (UDIV (OpReg w dst) (OpReg w reg_x) (OpReg w reg_y)))
    CmmMachOp (MO_U_Quot w) [x, y] | w == W16 -> do
      (reg_x, _format_x, code_x) <- getSomeReg x
      (reg_y, _format_y, code_y) <- getSomeReg y
      return $ Any (intFormat w) (\dst -> code_x `appOL` code_y `snocOL` annExpr expr (UXTH (OpReg w reg_x) (OpReg w reg_x)) `snocOL`
                                                                        (UXTH (OpReg w reg_y) (OpReg w reg_y)) `snocOL`
                                                                        (UDIV (OpReg w dst) (OpReg w reg_x) (OpReg w reg_y)))

    -- 2. Shifts. x << n, x >> n.
    CmmMachOp (MO_Shl w) [x, (CmmLit (CmmInt n _))]
      | w == W32 || w == W64
      , 0 <= n, n < fromIntegral (widthInBits w) -> do
      (reg_x, _format_x, code_x) <- getSomeReg x
      return $ Any (intFormat w) (\dst -> code_x `snocOL` annExpr expr (LSL (OpReg w dst) (OpReg w reg_x) (OpImm (ImmInteger n))))

    CmmMachOp (MO_S_Shr w) [x, (CmmLit (CmmInt n _))] | w == W8, 0 <= n, n < 8 -> do
      (reg_x, _format_x, code_x) <- getSomeReg x
      return $ Any (intFormat w) (\dst -> code_x `snocOL` annExpr expr (SBFX (OpReg w dst) (OpReg w reg_x) (OpImm (ImmInteger n)) (OpImm (ImmInteger (8-n)))))
    CmmMachOp (MO_S_Shr w) [x, y] | w == W8 -> do
      (reg_x, _format_x, code_x) <- getSomeReg x
      (reg_y, _format_y, code_y) <- getSomeReg y
      return $ Any (intFormat w) (\dst -> code_x `appOL` code_y `snocOL` annExpr expr (SXTB (OpReg w reg_x) (OpReg w reg_x)) `snocOL`
                                                                         (ASR (OpReg w dst) (OpReg w reg_x) (OpReg w reg_y)))

    CmmMachOp (MO_S_Shr w) [x, (CmmLit (CmmInt n _))] | w == W16, 0 <= n, n < 16 -> do
      (reg_x, _format_x, code_x) <- getSomeReg x
      return $ Any (intFormat w) (\dst -> code_x `snocOL` annExpr expr (SBFX (OpReg w dst) (OpReg w reg_x) (OpImm (ImmInteger n)) (OpImm (ImmInteger (16-n)))))
    CmmMachOp (MO_S_Shr w) [x, y] | w == W16 -> do
      (reg_x, _format_x, code_x) <- getSomeReg x
      (reg_y, _format_y, code_y) <- getSomeReg y
      return $ Any (intFormat w) (\dst -> code_x `appOL` code_y `snocOL` annExpr expr (SXTH (OpReg w reg_x) (OpReg w reg_x)) `snocOL`
                                                                         (ASR (OpReg w dst) (OpReg w reg_x) (OpReg w reg_y)))

    CmmMachOp (MO_S_Shr w) [x, (CmmLit (CmmInt n _))]
      | w == W32 || w == W64
      , 0 <= n, n < fromIntegral (widthInBits w) -> do
      (reg_x, _format_x, code_x) <- getSomeReg x
      return $ Any (intFormat w) (\dst -> code_x `snocOL` annExpr expr (ASR (OpReg w dst) (OpReg w reg_x) (OpImm (ImmInteger n))))

    CmmMachOp (MO_U_Shr w) [x, (CmmLit (CmmInt n _))] | w == W8, 0 <= n, n < 8 -> do
      (reg_x, _format_x, code_x) <- getSomeReg x
      return $ Any (intFormat w) (\dst -> code_x `snocOL` annExpr expr (UBFX (OpReg w dst) (OpReg w reg_x) (OpImm (ImmInteger n)) (OpImm (ImmInteger (8-n)))))
    CmmMachOp (MO_U_Shr w) [x, y] | w == W8 -> do
      (reg_x, _format_x, code_x) <- getSomeReg x
      (reg_y, _format_y, code_y) <- getSomeReg y
      return $ Any (intFormat w) (\dst -> code_x `appOL` code_y `snocOL` annExpr expr (UXTB (OpReg w reg_x) (OpReg w reg_x)) `snocOL`
                                                                        (ASR (OpReg w dst) (OpReg w reg_x) (OpReg w reg_y)))

    CmmMachOp (MO_U_Shr w) [x, (CmmLit (CmmInt n _))] | w == W16, 0 <= n, n < 16 -> do
      (reg_x, _format_x, code_x) <- getSomeReg x
      return $ Any (intFormat w) (\dst -> code_x `snocOL` annExpr expr (UBFX (OpReg w dst) (OpReg w reg_x) (OpImm (ImmInteger n)) (OpImm (ImmInteger (16-n)))))
    CmmMachOp (MO_U_Shr w) [x, y] | w == W16 -> do
      (reg_x, _format_x, code_x) <- getSomeReg x
      (reg_y, _format_y, code_y) <- getSomeReg y
      return $ Any (intFormat w) (\dst -> code_x `appOL` code_y `snocOL` annExpr expr (UXTH (OpReg w reg_x) (OpReg w reg_x))
                                                                `snocOL` (ASR (OpReg w dst) (OpReg w reg_x) (OpReg w reg_y)))

    CmmMachOp (MO_U_Shr w) [x, (CmmLit (CmmInt n _))]
      | w == W32 || w == W64
      , 0 <= n, n < fromIntegral (widthInBits w) -> do
      (reg_x, _format_x, code_x) <- getSomeReg x
      return $ Any (intFormat w) (\dst -> code_x `snocOL` annExpr expr (LSR (OpReg w dst) (OpReg w reg_x) (OpImm (ImmInteger n))))

    -- 3. Logic &&, ||
    CmmMachOp (MO_And w) [(CmmReg reg), CmmLit (CmmInt n _)] | isAArch64Bitmask (opRegWidth w') (fromIntegral n) ->
      return $ Any (intFormat w) (\d -> unitOL $ annExpr expr (AND (OpReg w d) (OpReg w' r') (OpImm (ImmInteger n))))
      where w' = formatToWidth (cmmTypeFormat (cmmRegType reg))
            r' = getRegisterReg plat reg

    CmmMachOp (MO_Or w) [(CmmReg reg), CmmLit (CmmInt n _)] | isAArch64Bitmask (opRegWidth w') (fromIntegral n) ->
      return $ Any (intFormat w) (\d -> unitOL $ annExpr expr (ORR (OpReg w d) (OpReg w' r') (OpImm (ImmInteger n))))
      where w' = formatToWidth (cmmTypeFormat (cmmRegType reg))
            r' = getRegisterReg plat reg

    -- Generic binary case.
    CmmMachOp op [x, y] -> do
      -- alright, so we have an operation, and two expressions. And we want to essentially do
      -- ensure we get float regs (TODO(Ben): What?)
      let withTempIntReg w op = OpReg w <$> getNewRegNat (intFormat w) >>= op
          -- withTempFloatReg w op = OpReg w <$> getNewRegNat (floatFormat w) >>= op

          -- A "plain" operation.
          bitOpImm w op encode_imm = do
            -- compute x<m> <- x
            -- compute x<o> <- y
            -- <OP> x<n>, x<m>, x<o>
            (reg_x, format_x, code_x) <- getSomeReg x
            (op_y, format_y, code_y) <- case y of
              CmmLit (CmmInt n w)
                | Just imm_operand_y <- encode_imm n w
                -> return (imm_operand_y, intFormat w, nilOL)
              _ -> do
                  (reg_y, format_y, code_y) <- getSomeReg y
                  return (OpReg w reg_y, format_y, code_y)
            massertPpr (isIntFormat format_x == isIntFormat format_y) $ text "bitOpImm: incompatible"
            return $ Any (intFormat w) (\dst ->
                code_x `appOL`
                code_y `appOL`
                op (OpReg w dst) (OpReg w reg_x) op_y)

          -- A (potentially signed) integer operation.
          -- In the case of 8- and 16-bit signed arithmetic we must first
          -- sign-extend both arguments to 32-bits.
          -- See Note [Signed arithmetic on AArch64].
          intOpImm :: Bool -> Width -> (Operand -> Operand -> Operand -> OrdList Instr) -> (Integer -> Width -> Maybe Operand) -> NatM (Register)
          intOpImm {- is signed -} True  w op _encode_imm = intOp True w op
          intOpImm                 False w op  encode_imm = do
              -- compute x<m> <- x
              -- compute x<o> <- y
              -- <OP> x<n>, x<m>, x<o>
              (reg_x, format_x, code_x) <- getSomeReg x
              (op_y, format_y, code_y) <- case y of
                CmmLit (CmmInt n w)
                  | Just imm_operand_y <- encode_imm n w
                  -> return (imm_operand_y, intFormat w, nilOL)
                _ -> do
                    (reg_y, format_y, code_y) <- getSomeReg y
                    return (OpReg w reg_y, format_y, code_y)
              massertPpr (isIntFormat format_x && isIntFormat format_y) $ text "intOp: non-int"
              -- This is the width of the registers on which the operation
              -- should be performed.
              let w' = opRegWidth w
              return $ Any (intFormat w) $ \dst ->
                  code_x `appOL`
                  code_y `appOL`
                  op (OpReg w' dst) (OpReg w' reg_x) (op_y) `appOL`
                  truncateReg w' w dst -- truncate back to the operand's original width

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
        MO_Add w -> intOpImm False w (\d x y -> unitOL $ annExpr expr (ADD d x y)) getArithImm
        -- TODO: Handle sub-word case
        MO_Sub w -> intOpImm False w (\d x y -> unitOL $ annExpr expr (SUB d x y)) getArithImm

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
        MO_Eq w     -> bitOpImm w (\d x y -> toOL [ CMP x y, CSET d EQ ]) getArithImm
        MO_Ne w     -> bitOpImm w (\d x y -> toOL [ CMP x y, CSET d NE ]) getArithImm

        -- Signed multiply/divide
        MO_Mul w          -> intOp True w (\d x y -> unitOL $ MUL d x y)
        MO_S_MulMayOflo w -> do_mul_may_oflo w x y
        MO_S_Quot w       -> intOp True w (\d x y -> unitOL $ SDIV d x y)

        -- No native rem instruction. So we'll compute the following
        -- Rd  <- Rx / Ry             | 2 <- 7 / 3      -- SDIV Rd Rx Ry
        -- Rd' <- Rx - Rd * Ry        | 1 <- 7 - 2 * 3  -- MSUB Rd' Rd Ry Rx
        --        |     '---|----------------|---'   |
        --        |         '----------------|-------'
        --        '--------------------------'
        -- Note the swap in Rx and Ry.
        MO_S_Rem w -> withTempIntReg w $ \t ->
                      intOp True w (\d x y -> toOL [ SDIV t x y, MSUB d t y x ])

        -- Unsigned multiply/divide
        MO_U_Quot w -> intOp False w (\d x y -> unitOL $ UDIV d x y)
        MO_U_Rem w  -> withTempIntReg w $ \t ->
                       intOp False w (\d x y -> toOL [ UDIV t x y, MSUB d t y x ])

        -- Signed comparisons -- see Note [CSET]
        MO_S_Ge w     -> intOp True  w (\d x y -> toOL [ CMP x y, CSET d SGE ])
        MO_S_Le w     -> intOp True  w (\d x y -> toOL [ CMP x y, CSET d SLE ])
        MO_S_Gt w     -> intOp True  w (\d x y -> toOL [ CMP x y, CSET d SGT ])
        MO_S_Lt w     -> intOp True  w (\d x y -> toOL [ CMP x y, CSET d SLT ])

        -- Unsigned comparisons
        MO_U_Ge w     -> intOpImm False w (\d x y -> toOL [ CMP x y, CSET d UGE ]) getArithImm
        MO_U_Le w     -> intOpImm False w (\d x y -> toOL [ CMP x y, CSET d ULE ]) getArithImm
        MO_U_Gt w     -> intOpImm False w (\d x y -> toOL [ CMP x y, CSET d UGT ]) getArithImm
        MO_U_Lt w     -> intOpImm False w (\d x y -> toOL [ CMP x y, CSET d ULT ]) getArithImm

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
        -- This is a bit counter-intuitive. Don't let yourself be fooled by
        -- the S/U prefix for floats, it's only meaningful for integers.
        MO_F_Ge w    -> floatCond w (\d x y -> toOL [ CMP x y, CSET d OGE ])
        MO_F_Le w    -> floatCond w (\d x y -> toOL [ CMP x y, CSET d OLE ]) -- x <= y <=> y > x
        MO_F_Gt w    -> floatCond w (\d x y -> toOL [ CMP x y, CSET d OGT ])
        MO_F_Lt w    -> floatCond w (\d x y -> toOL [ CMP x y, CSET d OLT ]) -- x < y <=> y >= x

        -- Bitwise operations
        MO_And   w -> bitOpImm w (\d x y -> unitOL $ AND d x y) getBitmaskImm
        MO_Or    w -> bitOpImm w (\d x y -> unitOL $ ORR d x y) getBitmaskImm
        MO_Xor   w -> bitOpImm w (\d x y -> unitOL $ EOR d x y) getBitmaskImm
        MO_Shl   w -> intOp False w (\d x y -> unitOL $ LSL d x y)
        MO_U_Shr w -> intOp False w (\d x y -> unitOL $ LSR d x y)
        MO_S_Shr w -> intOp True  w (\d x y -> unitOL $ ASR d x y)

        -- Non-dyadic MachOp with 2 arguments
        MO_S_Neg {} -> notDyadic
        MO_F_Neg {} -> notDyadic
        MO_FMA {} -> notDyadic
        MO_Not {} -> notDyadic
        MO_SF_Round {} -> notDyadic
        MO_FS_Truncate {} -> notDyadic
        MO_SS_Conv {} -> notDyadic
        MO_UU_Conv {} -> notDyadic
        MO_XX_Conv {} -> notDyadic
        MO_FF_Conv {} -> notDyadic
        MO_WF_Bitcast {} -> notDyadic
        MO_FW_Bitcast {} -> notDyadic
        MO_V_Insert {} -> notDyadic
        MO_VF_Insert {} -> notDyadic
        MO_AlignmentCheck {} -> notDyadic
        MO_RelaxedRead {} -> notDyadic

        -- Vector operations: currently unsupported in the AArch64 NCG.
        MO_V_Extract {} -> vectorsNeedLlvm
        MO_V_Add {} -> vectorsNeedLlvm
        MO_V_Sub {} -> vectorsNeedLlvm
        MO_V_Mul {} -> vectorsNeedLlvm
        MO_VS_Quot {} -> vectorsNeedLlvm
        MO_VS_Rem {} -> vectorsNeedLlvm
        MO_VS_Neg {} -> vectorsNeedLlvm
        MO_VU_Quot {} -> vectorsNeedLlvm
        MO_VU_Rem {} -> vectorsNeedLlvm
        MO_VF_Extract {} -> vectorsNeedLlvm
        MO_VF_Add {} -> vectorsNeedLlvm
        MO_VF_Sub {} -> vectorsNeedLlvm
        MO_VF_Neg {} -> vectorsNeedLlvm
        MO_VF_Mul {} -> vectorsNeedLlvm
        MO_VF_Quot {} -> vectorsNeedLlvm
        MO_V_Shuffle {} -> vectorsNeedLlvm
        MO_VF_Shuffle {} -> vectorsNeedLlvm
        where
          notDyadic =
            pprPanic "getRegister' (non-dyadic CmmMachOp with 2 arguments): " $
              (pprMachOp op) <+> text "in" <+> (pdoc plat expr)
          vectorsNeedLlvm =
            sorry "SIMD operations on AArch64 currently require the LLVM backend"

    -- Generic ternary case.
    CmmMachOp op [x, y, z] ->

      case op of

        -- Floating-point fused multiply-add operations

        -- x86 fmadd    x * y + z <=> AArch64 fmadd : d =   r1 * r2 + r3
        -- x86 fmsub    x * y - z <=> AArch64 fnmsub: d =   r1 * r2 - r3
        -- x86 fnmadd - x * y + z <=> AArch64 fmsub : d = - r1 * r2 + r3
        -- x86 fnmsub - x * y - z <=> AArch64 fnmadd: d = - r1 * r2 - r3

        MO_FMA var l w
          | l == 1
          -> case var of
            FMAdd  -> float3Op w (\d n m a -> unitOL $ FMA FMAdd  d n m a)
            FMSub  -> float3Op w (\d n m a -> unitOL $ FMA FNMSub d n m a)
            FNMAdd -> float3Op w (\d n m a -> unitOL $ FMA FMSub  d n m a)
            FNMSub -> float3Op w (\d n m a -> unitOL $ FMA FNMAdd d n m a)
          | otherwise
          -> vectorsNeedLlvm

        MO_V_Insert {} -> vectorsNeedLlvm
        MO_VF_Insert {} -> vectorsNeedLlvm

        _ -> pprPanic "getRegister' (unhandled ternary CmmMachOp): " $
                (pprMachOp op) <+> text "in" <+> (pdoc plat expr)

      where
          vectorsNeedLlvm =
            sorry "SIMD operations on AArch64 currently require the LLVM backend"
          float3Op w op = do
            (reg_fx, format_x, code_fx) <- getFloatReg x
            (reg_fy, format_y, code_fy) <- getFloatReg y
            (reg_fz, format_z, code_fz) <- getFloatReg z
            massertPpr (isFloatFormat format_x && isFloatFormat format_y && isFloatFormat format_z) $
              text "float3Op: non-float"
            return $
              Any (floatFormat w) $ \ dst ->
                code_fx `appOL`
                code_fy `appOL`
                code_fz `appOL`
                op (OpReg w dst) (OpReg w reg_fx) (OpReg w reg_fy) (OpReg w reg_fz)

    CmmMachOp _op _xs
      -> pprPanic "getRegister' (variadic CmmMachOp): " (pdoc plat expr)

  where
    isNbitEncodeable :: Int -> Integer -> Bool
    isNbitEncodeable n_bits i = let shift = n_bits - 1 in (-1 `shiftL` shift) <= i && i < (1 `shiftL` shift)

    -- N.B. MUL does not set the overflow flag.
    -- These implementations are based on output from GCC 11.
    do_mul_may_oflo :: Width -> CmmExpr -> CmmExpr -> NatM Register
    do_mul_may_oflo w@W64 x y = do
        (reg_x, _format_x, code_x) <- getSomeReg x
        (reg_y, _format_y, code_y) <- getSomeReg y
        lo <- getNewRegNat II64
        hi <- getNewRegNat II64
        return $ Any (intFormat w) (\dst ->
            code_x `appOL`
            code_y `snocOL`
            MUL (OpReg w lo) (OpReg w reg_x) (OpReg w reg_y) `snocOL`
            SMULH (OpReg w hi) (OpReg w reg_x) (OpReg w reg_y) `snocOL`
            CMP (OpReg w hi) (OpRegShift w lo SASR 63) `snocOL`
            CSET (OpReg w dst) NE)

    do_mul_may_oflo W32 x y = do
        (reg_x, _format_x, code_x) <- getSomeReg x
        (reg_y, _format_y, code_y) <- getSomeReg y
        tmp1 <- getNewRegNat II64
        tmp2 <- getNewRegNat II64
        return $ Any (intFormat W32) (\dst ->
            code_x `appOL`
            code_y `snocOL`
            SMULL (OpReg W64 tmp1) (OpReg W32 reg_x) (OpReg W32 reg_y) `snocOL`
            ASR (OpReg W64 tmp2) (OpReg W64 tmp1) (OpImm (ImmInt 31)) `snocOL`
            CMP (OpReg W32 tmp2) (OpRegShift W32 tmp1 SASR 31) `snocOL`
            CSET (OpReg W32 dst) NE)

    do_mul_may_oflo w x y = do
        (reg_x, _format_x, code_x) <- getSomeReg x
        (reg_y, _format_y, code_y) <- getSomeReg y
        tmp1 <- getNewRegNat II32
        tmp2 <- getNewRegNat II32
        let extend dst arg =
              case w of
                W16 -> SXTH (OpReg W32 dst) (OpReg W32 arg)
                W8  -> SXTB (OpReg W32 dst) (OpReg W32 arg)
                _   -> panic "unreachable"
            cmp_ext_mode =
              case w of
                W16 -> EUXTH
                W8  -> EUXTB
                _   -> panic "unreachable"
            width = widthInBits w
            opInt = OpImm . ImmInt

        return $ Any (intFormat w) (\dst ->
            code_x `appOL`
            code_y `snocOL`
            extend tmp1 reg_x `snocOL`
            extend tmp2 reg_y `snocOL`
            MUL (OpReg W32 tmp1) (OpReg W32 tmp1) (OpReg W32 tmp2) `snocOL`
            SBFX (OpReg W64 tmp2) (OpReg W64 tmp1) (opInt $ width - 1) (opInt 1) `snocOL`
            UBFX (OpReg W32 tmp1) (OpReg W32 tmp1) (opInt width) (opInt width) `snocOL`
            CMP (OpReg W32 tmp1) (OpRegExt W32 tmp2 cmp_ext_mode 0) `snocOL`
            CSET (OpReg w dst) NE)

-- | Is a given number encodable as a bitmask immediate?
--
-- https://stackoverflow.com/questions/30904718/range-of-immediate-values-in-armv8-a64-assembly
isAArch64Bitmask :: Width -> Integer -> Bool
-- N.B. zero and ~0 are not encodable as bitmask immediates
isAArch64Bitmask width n =
  assert (width `elem` [W32,W64]) $
  case n of
    0 -> False
    _ | n == bit (widthInBits width) - 1
      -> False -- 1111...1111
      | otherwise
      -> (width == W64 && check 64) || check 32 || check 16 || check 8
  where
    -- Check whether @n@ can be represented as a subpattern of the given
    -- width.
    check width
      | hasOneRun subpat =
          let n' = fromIntegral (mkPat width subpat)
          in n == n'
      | otherwise = False
      where
        subpat :: Word64
        subpat = fromIntegral (n .&. (bit width - 1))

    -- Construct a bit-pattern from a repeated subpatterns the given width.
    mkPat :: Int -> Word64 -> Word64
    mkPat width subpat =
        foldl' (.|.) 0 [ subpat `shiftL` p | p <- [0, width..63] ]

    -- Does the given number's bit representation match the regular expression
    -- @0*1*0*@?
    hasOneRun :: Word64 -> Bool
    hasOneRun m =
        64 == popCount m + countLeadingZeros m + countTrailingZeros m

-- | Instructions to sign-extend the value in the given register from width @w@
-- up to width @w'@.
signExtendReg :: Width -> Width -> Reg -> NatM (Reg, OrdList Instr)
signExtendReg w w' r =
    case w of
      W64 -> noop
      W32
        | w' == W32 -> noop
        | otherwise -> extend SXTH
      W16           -> extend SXTH
      W8            -> extend SXTB
      _             -> panic "intOp"
  where
    noop = return (r, nilOL)
    extend instr = do
        r' <- getNewRegNat II64
        return (r', unitOL $ instr (OpReg w' r') (OpReg w' r))

-- | Instructions to truncate the value in the given register from width @w@
-- down to width @w'@.
truncateReg :: Width -> Width -> Reg -> OrdList Instr
truncateReg w w' r =
    case w of
      W64 -> nilOL
      W32
        | w' == W32 -> nilOL
      _   -> unitOL $ UBFM (OpReg w r)
                           (OpReg w r)
                           (OpImm (ImmInt 0))
                           (OpImm $ ImmInt $ widthInBits w' - 1)

-- -----------------------------------------------------------------------------
--  The 'Amode' type: Memory addressing modes passed up the tree.
data Amode = Amode AddrMode InstrBlock

getAmode :: Platform
         -> Width     -- ^ width of loaded value
         -> CmmExpr
         -> NatM Amode
-- TODO: Specialize stuff we can destructure here.

-- OPTIMIZATION WARNING: Addressing modes.
-- Addressing options:
getAmode platform w (CmmRegOff reg off)
  | isOffsetImm off w
  = return $ Amode (AddrRegImm reg' off') nilOL
    where reg' = getRegisterReg platform reg
          off' = ImmInt off

-- For Stores we often see something like this:
-- CmmStore (CmmMachOp (MO_Add w) [CmmLoad expr, CmmLit (CmmInt n w')]) (expr2)
-- E.g. a CmmStoreOff really. This can be translated to `str $expr2, [$expr, #n ]
-- for `n` in range.
getAmode _platform w (CmmMachOp (MO_Add _w) [expr, CmmLit (CmmInt off _w')])
  | isOffsetImm (fromIntegral off) w
  = do (reg, _format, code) <- getSomeReg expr
       return $ Amode (AddrRegImm reg (ImmInteger off)) code

getAmode _platform w (CmmMachOp (MO_Sub _w) [expr, CmmLit (CmmInt off _w')])
  | isOffsetImm (fromIntegral $ -off) w
  = do (reg, _format, code) <- getSomeReg expr
       return $ Amode (AddrRegImm reg (ImmInteger $ -off)) code

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

genJump :: CmmExpr{-the branch target-} -> NatM InstrBlock
genJump expr@(CmmLit (CmmLabel lbl))
  = return $ unitOL (annExpr expr (J (TLabel lbl)))

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
                (reg_x, _format_x, code_x) <- getSomeReg x
                (reg_y, _format_y, code_y) <- getSomeReg y
                let x' = OpReg w reg_x
                    y' = OpReg w reg_y
                return $ case w of
                  W8  -> code_x `appOL` code_y `appOL` toOL [ UXTB x' x', UXTB y' y', CMP x' y', (annExpr expr (BCOND cmp (TBlock bid))) ]
                  W16 -> code_x `appOL` code_y `appOL` toOL [ UXTH x' x', UXTH y' y', CMP x' y', (annExpr expr (BCOND cmp (TBlock bid))) ]
                  _   -> code_x `appOL` code_y `appOL` toOL [                         CMP x' y', (annExpr expr (BCOND cmp (TBlock bid))) ]

            sbcond w cmp = do
                -- compute both sides.
                (reg_x, _format_x, code_x) <- getSomeReg x
                (reg_y, _format_y, code_y) <- getSomeReg y
                let x' = OpReg w reg_x
                    y' = OpReg w reg_y
                return $ case w of
                  W8  -> code_x `appOL` code_y `appOL` toOL [ SXTB x' x', SXTB y' y', CMP x' y', (annExpr expr (BCOND cmp (TBlock bid))) ]
                  W16 -> code_x `appOL` code_y `appOL` toOL [ SXTH x' x', SXTH y' y', CMP x' y', (annExpr expr (BCOND cmp (TBlock bid))) ]
                  _   -> code_x `appOL` code_y `appOL` toOL [                         CMP x' y', (annExpr expr (BCOND cmp (TBlock bid))) ]

            fbcond w cmp = do
              -- ensure we get float regs
              (reg_fx, _format_fx, code_fx) <- getFloatReg x
              (reg_fy, _format_fy, code_fy) <- getFloatReg y
              return $ code_fx `appOL` code_fy `snocOL` CMP (OpReg w reg_fx) (OpReg w reg_fy) `snocOL` (annExpr expr (BCOND cmp (TBlock bid)))

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
          _ -> pprPanic "AArch64.genCondJump:case mop: " (text $ show expr)
      _ -> pprPanic "AArch64.genCondJump: " (text $ show expr)

-- A conditional jump with at least +/-128M jump range
genCondFarJump :: MonadGetUnique m => Cond -> Target -> m InstrBlock
genCondFarJump cond far_target = do
  skip_lbl_id <- newBlockId
  jmp_lbl_id <- newBlockId

  -- TODO: We can improve this by inverting the condition
  -- but it's not quite trivial since we don't know if we
  -- need to consider float orderings.
  -- So we take the hit of the additional jump in the false
  -- case for now.
  return $ toOL [ BCOND cond (TBlock jmp_lbl_id)
                , B (TBlock skip_lbl_id)
                , NEWBLOCK jmp_lbl_id
                , B far_target
                , NEWBLOCK skip_lbl_id]

genCondBranch :: BlockId      -- the true branch target
    -> BlockId      -- the false branch target
    -> CmmExpr      -- the condition on which to branch
    -> NatM InstrBlock -- Instructions

genCondBranch true false expr = do
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
-- instead of the add instruction.
--
-- As the memory model for AArch64 for PIC is considered to be +/- 4GB, we do
-- not need to go through the GOT, unless we want to address the full address
-- range within 64bit.

genCCall
    :: ForeignTarget      -- function to call
    -> [CmmFormal]        -- where to put the result
    -> [CmmActual]        -- arguments (of mixed type)
    -> NatM InstrBlock
-- TODO: Specialize where we can.
-- Generic impl
genCCall target dest_regs arg_regs = do
  -- we want to pass arg_regs into allArgRegs
  -- pprTraceM "genCCall target" (ppr target)
  -- pprTraceM "genCCall formal" (ppr dest_regs)
  -- pprTraceM "genCCall actual" (ppr arg_regs)
  platform <- getPlatform
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

      let packStack = platformOS platform == OSDarwin

      (stackSpace', passRegs, passArgumentsCode) <- passArguments packStack allGpArgRegs allFpArgRegs arg_regs'' 0 [] nilOL

      -- if we pack the stack, we may need to adjust to multiple of 8byte.
      -- if we don't pack the stack, it will always be multiple of 8.
      let stackSpace = if stackSpace' `mod` 8 /= 0
                       then 8 * (stackSpace' `div` 8 + 1)
                       else stackSpace'

      readResultsCode <- readResults allGpArgRegs allFpArgRegs dest_regs [] nilOL

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
            `appOL` (unitOL $ BL call_target passRegs) -- branch and link.
            `appOL` readResultsCode           -- parse the results into registers
            `appOL` moveStackUp (stackSpace `div` 8)
      return code

    PrimTarget MO_F32_Fabs
      | [arg_reg] <- arg_regs, [dest_reg] <- dest_regs ->
        unaryFloatOp W32 (\d x -> unitOL $ FABS d x) arg_reg dest_reg
      | otherwise -> panic "mal-formed MO_F32_Fabs"
    PrimTarget MO_F64_Fabs
      | [arg_reg] <- arg_regs, [dest_reg] <- dest_regs ->
        unaryFloatOp W64 (\d x -> unitOL $ FABS d x) arg_reg dest_reg
      | otherwise -> panic "mal-formed MO_F64_Fabs"
    PrimTarget MO_F32_Sqrt
      | [arg_reg] <- arg_regs, [dest_reg] <- dest_regs ->
        unaryFloatOp W32 (\d x -> unitOL $ FSQRT d x) arg_reg dest_reg
      | otherwise -> panic "mal-formed MO_F32_Sqrt"
    PrimTarget MO_F64_Sqrt
      | [arg_reg] <- arg_regs, [dest_reg] <- dest_regs ->
        unaryFloatOp W64 (\d x -> unitOL $ FSQRT d x) arg_reg dest_reg
      | otherwise -> panic "mal-formed MO_F64_Sqrt"


    PrimTarget (MO_S_Mul2 w)
          -- Life is easier when we're working with word sized operands,
          -- we can use SMULH to compute the high 64 bits, and dst_needed
          -- checks if the high half's bits are all the same as the low half's
          -- top bit.
          | w == W64
          , [src_a, src_b] <- arg_regs
          -- dst_needed = did the result fit into just the low half
          , [dst_needed, dst_hi, dst_lo] <- dest_regs
            ->  do
              (reg_a, _format_x, code_x) <- getSomeReg src_a
              (reg_b, _format_y, code_y) <- getSomeReg src_b

              let lo = getRegisterReg platform (CmmLocal dst_lo)
                  hi = getRegisterReg platform (CmmLocal dst_hi)
                  nd = getRegisterReg platform (CmmLocal dst_needed)
              return $
                  code_x `appOL`
                  code_y `snocOL`
                  MUL   (OpReg W64 lo) (OpReg W64 reg_a) (OpReg W64 reg_b) `snocOL`
                  SMULH (OpReg W64 hi) (OpReg W64 reg_a) (OpReg W64 reg_b) `snocOL`
                  -- Are all high bits equal to the sign bit of the low word?
                  -- nd = (hi == ASR(lo,width-1)) ? 1 : 0
                  CMP   (OpReg W64 hi) (OpRegShift W64 lo SASR (widthInBits w - 1)) `snocOL`
                  CSET  (OpReg W64 nd) NE
            -- For sizes < platform width, we can just perform a multiply and shift
            -- using the normal 64 bit multiply. Calculating the dst_needed value is
            -- complicated a little by the need to be careful when truncation happens.
            -- Currently this case can't be generated since
            -- timesInt2# :: Int# -> Int# -> (# Int#, Int#, Int# #)
            -- TODO: Should this be removed or would other primops be useful?
          | w < W64
          , [src_a, src_b] <- arg_regs
          , [dst_needed, dst_hi, dst_lo] <- dest_regs
            ->  do
              (reg_a', _format_x, code_a) <- getSomeReg src_a
              (reg_b', _format_y, code_b) <- getSomeReg src_b

              let lo = getRegisterReg platform (CmmLocal dst_lo)
                  hi = getRegisterReg platform (CmmLocal dst_hi)
                  nd = getRegisterReg platform (CmmLocal dst_needed)
                  -- Do everything in a full 64 bit registers
                  w' = platformWordWidth platform

              (reg_a, code_a') <- signExtendReg w w' reg_a'
              (reg_b, code_b') <- signExtendReg w w' reg_b'

              return $
                  code_a  `appOL`
                  code_b  `appOL`
                  code_a' `appOL`
                  code_b' `snocOL`
                  -- the low 2w' of lo contains the full multiplication;
                  -- eg: int8 * int8 -> int16 result
                  -- so lo is in the last w of the register, and hi is in the second w.
                  SMULL (OpReg w' lo) (OpReg w' reg_a) (OpReg w' reg_b) `snocOL`
                  -- Make sure we hold onto the sign bits for dst_needed
                  ASR (OpReg w' hi) (OpReg w' lo)    (OpImm (ImmInt $ widthInBits w)) `appOL`
                  -- lo can now be truncated so we can get at it's top bit easily.
                  truncateReg w' w lo `snocOL`
                  -- Note the use of CMN (compare negative), not CMP: we want to
                  -- test if the top half is negative one and the top
                  -- bit of the bottom half is positive one. eg:
                  -- hi = 0b1111_1111  (actually 64 bits)
                  -- lo = 0b1010_1111  (-81, so the result didn't need the top half)
                  -- lo' = ASR(lo,7)   (second reg of SMN)
                  --     = 0b0000_0001 (theeshift gives us 1 for negative,
                  --                    and 0 for positive)
                  -- hi == -lo'?
                  -- 0b1111_1111 == 0b1111_1111 (yes, top half is just overflow)
                  -- Another way to think of this is if hi + lo' == 0, which is what
                  -- CMN really is under the hood.
                  CMN   (OpReg w' hi) (OpRegShift w' lo SLSR (widthInBits w - 1)) `snocOL`
                  -- Set dst_needed to 1 if hi and lo' were (negatively) equal
                  CSET  (OpReg w' nd) EQ `appOL`
                  -- Finally truncate hi to drop any extraneous sign bits.
                  truncateReg w' w hi
          -- Can't handle > 64 bit operands
          | otherwise -> unsupported (MO_S_Mul2 w)
    PrimTarget (MO_U_Mul2  w)
          -- The unsigned case is much simpler than the signed, all we need to
          -- do is the multiplication straight into the destination registers.
          | w == W64
          , [src_a, src_b] <- arg_regs
          , [dst_hi, dst_lo] <- dest_regs
            ->  do
              (reg_a, _format_x, code_x) <- getSomeReg src_a
              (reg_b, _format_y, code_y) <- getSomeReg src_b

              let lo = getRegisterReg platform (CmmLocal dst_lo)
                  hi = getRegisterReg platform (CmmLocal dst_hi)
              return (
                  code_x `appOL`
                  code_y `snocOL`
                  MUL   (OpReg W64 lo) (OpReg W64 reg_a) (OpReg W64 reg_b) `snocOL`
                  UMULH (OpReg W64 hi) (OpReg W64 reg_a) (OpReg W64 reg_b)
                  )
            -- For sizes < platform width, we can just perform a multiply and shift
            -- Need to be careful to truncate the low half, but the upper half should be
            -- be ok if the invariant in [Signed arithmetic on AArch64] is maintained.
            -- Currently this case can't be produced by the compiler since
            -- timesWord2# :: Word# -> Word# -> (# Word#, Word# #)
            -- TODO: Remove? Or would the extra primop be useful for avoiding the extra
            -- steps needed to do this in userland?
          | w < W64
          , [src_a, src_b] <- arg_regs
          , [dst_hi, dst_lo] <- dest_regs
            ->  do
              (reg_a, _format_x, code_x) <- getSomeReg src_a
              (reg_b, _format_y, code_y) <- getSomeReg src_b

              let lo = getRegisterReg platform (CmmLocal dst_lo)
                  hi = getRegisterReg platform (CmmLocal dst_hi)
                  w' = opRegWidth w
              return (
                  code_x `appOL`
                  code_y `snocOL`
                  -- UMULL: Xd = Wa * Wb with 64 bit result
                  -- W64 inputs should have been caught by case above
                  UMULL (OpReg W64 lo) (OpReg w' reg_a) (OpReg w' reg_b) `snocOL`
                  -- Extract and truncate high result
                  -- hi[w:0] = lo[2w:w]
                  UBFX (OpReg W64 hi) (OpReg W64 lo)
                      (OpImm (ImmInt $ widthInBits w)) -- lsb
                      (OpImm (ImmInt $ widthInBits w)) -- width to extract
                      `appOL`
                  truncateReg W64 w lo
                  )
          | otherwise -> unsupported (MO_U_Mul2  w)
    PrimTarget (MO_Clz  w)
          | w == W64 || w == W32
          , [src] <- arg_regs
          , [dst] <- dest_regs
          -> do
              (reg_a, _format_x, code_x) <- getSomeReg src
              let dst_reg = getRegisterReg platform (CmmLocal dst)
              return (
                  code_x `snocOL`
                  CLZ   (OpReg w dst_reg) (OpReg w reg_a)
                  )
          | w == W16
          , [src] <- arg_regs
          , [dst] <- dest_regs
          -> do
              (reg_a, _format_x, code_x) <- getSomeReg src
              let dst' = getRegisterReg platform (CmmLocal dst)
                  r n = OpReg W32 n
                  imm n = OpImm (ImmInt n)
              {- dst = clz(x << 16 | 0x0000_8000) -}
              return (
                  code_x `appOL` toOL
                    [ LSL (r dst') (r reg_a) (imm 16)
                    , ORR (r dst') (r dst')  (imm 0x00008000)
                    , CLZ (r dst') (r dst')
                    ]
                  )
          | w == W8
          , [src] <- arg_regs
          , [dst] <- dest_regs
          -> do
              (reg_a, _format_x, code_x) <- getSomeReg src
              let dst' = getRegisterReg platform (CmmLocal dst)
                  r n = OpReg W32 n
                  imm n = OpImm (ImmInt n)
              {- dst = clz(x << 24 | 0x0080_0000) -}
              return $
                  code_x `appOL` toOL
                    [ LSL (r dst') (r reg_a) (imm 24)
                    , ORR (r dst') (r dst')  (imm 0x00800000)
                    , CLZ (r dst') (r dst')
                    ]
            | otherwise -> unsupported (MO_Clz  w)
    PrimTarget (MO_Ctz  w)
          | w == W64 || w == W32
          , [src] <- arg_regs
          , [dst] <- dest_regs
          -> do
              (reg_a, _format_x, code_x) <- getSomeReg src
              let dst_reg = getRegisterReg platform (CmmLocal dst)
              return $
                  code_x `snocOL`
                  RBIT (OpReg w dst_reg) (OpReg w reg_a) `snocOL`
                  CLZ  (OpReg w dst_reg) (OpReg w dst_reg)
          | w == W16
          , [src] <- arg_regs
          , [dst] <- dest_regs
          -> do
              (reg_a, _format_x, code_x) <- getSomeReg src
              let dst' = getRegisterReg platform (CmmLocal dst)
                  r n = OpReg W32 n
                  imm n = OpImm (ImmInt n)
              {- dst = clz(reverseBits(x) | 0x0000_8000) -}
              return $
                  code_x `appOL` toOL
                    [ RBIT (r dst') (r reg_a)
                    , ORR  (r dst') (r dst') (imm 0x00008000)
                    , CLZ  (r dst') (r dst')
                    ]
          | w == W8
          , [src] <- arg_regs
          , [dst] <- dest_regs
          -> do
              (reg_a, _format_x, code_x) <- getSomeReg src
              let dst' = getRegisterReg platform (CmmLocal dst)
                  r n = OpReg W32 n
                  imm n = OpImm (ImmInt n)
              {- dst = clz(reverseBits(x) | 0x0080_0000) -}
              return $
                  code_x `appOL` toOL
                    [ RBIT (r dst') (r reg_a)
                    , ORR (r dst')  (r dst') (imm 0x00800000)
                    , CLZ  (r dst')  (r dst')
                    ]
            | otherwise -> unsupported (MO_Ctz  w)
    PrimTarget (MO_BRev  w)
          | w == W64 || w == W32
          , [src] <- arg_regs
          , [dst] <- dest_regs
          -> do
              (reg_a, _format_x, code_x) <- getSomeReg src
              let dst_reg = getRegisterReg platform (CmmLocal dst)
              return $
                  code_x `snocOL`
                  RBIT (OpReg w dst_reg) (OpReg w reg_a)
          | w == W16
          , [src] <- arg_regs
          , [dst] <- dest_regs
          -> do
              (reg_a, _format_x, code_x) <- getSomeReg src
              let dst' = getRegisterReg platform (CmmLocal dst)
                  r n = OpReg W32 n
                  imm n = OpImm (ImmInt n)
              {- dst = reverseBits32(x << 16) -}
              return $
                  code_x `appOL` toOL
                    [ LSL  (r dst') (r reg_a) (imm 16)
                    , RBIT (r dst') (r dst')
                    ]
          | w == W8
          , [src] <- arg_regs
          , [dst] <- dest_regs
          -> do
              (reg_a, _format_x, code_x) <- getSomeReg src
              let dst' = getRegisterReg platform (CmmLocal dst)
                  r n = OpReg W32 n
                  imm n = OpImm (ImmInt n)
              {- dst = reverseBits32(x << 24) -}
              return $
                  code_x `appOL` toOL
                    [ LSL  (r dst') (r reg_a) (imm 24)
                    , RBIT (r dst') (r dst')
                    ]
            | otherwise -> unsupported (MO_BRev  w)
    PrimTarget (MO_BSwap  w)
          | w == W64 || w == W32
          , [src] <- arg_regs
          , [dst] <- dest_regs
          -> do
              (reg_a, _format_x, code_x) <- getSomeReg src
              let dst_reg = getRegisterReg platform (CmmLocal dst)
              return $ code_x `snocOL` REV (OpReg w dst_reg) (OpReg w reg_a)
          | w == W16
          , [src] <- arg_regs
          , [dst] <- dest_regs
          -> do
              (reg_a, _format_x, code_x) <- getSomeReg src
              let dst' = getRegisterReg platform (CmmLocal dst)
                  r n = OpReg W32 n
              -- Swaps the bytes in each 16bit word
              -- TODO: Expose the 32 & 64 bit version of this?
              return $ code_x `snocOL` REV16 (r dst') (r reg_a)
          | otherwise -> unsupported (MO_BSwap w)

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
        MO_S_QuotRem  _w -> unsupported mop
        MO_U_QuotRem  _w -> unsupported mop
        MO_U_QuotRem2 _w -> unsupported mop
        MO_Add2       _w -> unsupported mop
        MO_AddWordC   _w -> unsupported mop
        MO_SubWordC   _w -> unsupported mop
        MO_AddIntC    _w -> unsupported mop
        MO_SubIntC    _w -> unsupported mop

        -- Memory Ordering
        -- Set flags according to their C pendants (stdatomic.h):
        -- atomic_thread_fence(memory_order_acquire); // -> dmb ishld
        MO_AcquireFence     ->  return . unitOL $ DMBISH DmbLoad
        -- atomic_thread_fence(memory_order_release); // -> dmb ish
        MO_ReleaseFence     ->  return . unitOL $ DMBISH DmbLoadStore
        -- atomic_thread_fence(memory_order_seq_cst); // -> dmb ish
        MO_SeqCstFence      ->  return . unitOL $ DMBISH DmbLoadStore
        MO_Touch            ->  return nilOL -- Keep variables live (when using interior pointers)
        -- Prefetch
        MO_Prefetch_Data _n -> return nilOL -- Prefetch hint.

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

        -- -- Atomic read-modify-write.
        MO_AtomicRead w ord
          | [p_reg] <- arg_regs
          , [dst_reg] <- dest_regs -> do
              (p, _fmt_p, code_p) <- getSomeReg p_reg
              platform <- getPlatform
              let instr = case ord of
                      MemOrderRelaxed -> LDR
                      _               -> LDAR
                  dst = getRegisterReg platform (CmmLocal dst_reg)
                  code =
                    code_p `snocOL`
                    instr (intFormat w) (OpReg w dst) (OpAddr $ AddrReg p)
              return code
          | otherwise -> panic "mal-formed AtomicRead"
        MO_AtomicWrite w ord
          | [p_reg, val_reg] <- arg_regs -> do
              (p, _fmt_p, code_p) <- getSomeReg p_reg
              (val, fmt_val, code_val) <- getSomeReg val_reg
              let instr = case ord of
                      MemOrderRelaxed -> STR
                      _               -> STLR
                  code =
                    code_p `appOL`
                    code_val `snocOL`
                    instr fmt_val (OpReg w val) (OpAddr $ AddrReg p)
              return code
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
    mkCCall :: FastString -> NatM InstrBlock
    mkCCall name = do
      config <- getConfig
      target <- cmmMakeDynamicReference config CallReference $
          mkForeignLabel name ForeignLabelInThisPackage IsFunction
      let cconv = ForeignConvention CCallConv [NoHint] [NoHint] CmmMayReturn
      genCCall (ForeignTarget target cconv) dest_regs arg_regs

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

    -- AArch64-Darwin: stack packing and alignment
    --
    -- According to the "Writing ARM64 Code for Apple Platforms" document form
    -- Apple, specifically the section "Handle Data Types and Data Alignment Properly"
    -- we need to not only pack, but also align arguments on the stack.
    --
    -- Data type   Size (in bytes)   Natural alignment (in bytes)
    -- BOOL, bool  1                 1
    -- char        1                 1
    -- short       2                 2
    -- int         4                 4
    -- long        8                 8
    -- long long   8                 8
    -- pointer     8                 8
    -- size_t      8                 8
    -- NSInteger   8                 8
    -- CFIndex     8                 8
    -- fpos_t      8                 8
    -- off_t       8                 8
    --
    -- We can see that types are aligned by their sizes so the easiest way to
    -- guarantee alignment during packing seems to be to pad to a multiple of the
    -- size we want to pack. Failure to get this right can result in pretty
    -- subtle bugs, e.g. #20137.

    passArguments pack (gpReg:gpRegs) fpRegs ((r, format, hint, code_r):args) stackSpace accumRegs accumCode | isIntFormat format = do
      platform <- getPlatform
      let w = formatToWidth format
          mov
            -- Specifically, Darwin/AArch64's ABI requires that the caller
            -- sign-extend arguments which are smaller than 32-bits.
            | w < W32
            , platformCConvNeedsExtension platform
            , SignedHint <- hint
            = case w of
                W8  -> SXTB (OpReg W64 gpReg) (OpReg w r)
                W16 -> SXTH (OpReg W64 gpReg) (OpReg w r)
                _   -> panic "impossible"
            | otherwise
            = MOV (OpReg w gpReg) (OpReg w r)
          accumCode' = accumCode `appOL`
                       code_r `snocOL`
                       ann (text "Pass gp argument: " <> ppr r) mov
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
          str = STR format (OpReg w r) (OpAddr (AddrRegImm (regSingle 31) (ImmInt stackSpace')))
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
          str = STR format (OpReg w r) (OpAddr (AddrRegImm (regSingle 31) (ImmInt stackSpace')))
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
          str = STR format (OpReg w r) (OpAddr (AddrRegImm (regSingle 31) (ImmInt stackSpace')))
          stackCode = code_r `snocOL`
                      ann (text "Pass argument (size " <> ppr w <> text ") on the stack: " <> ppr r) str
      passArguments pack gpRegs [] args (stackSpace'+space) accumRegs (stackCode `appOL` accumCode)

    passArguments _ _ _ _ _ _ _ = pprPanic "passArguments" (text "invalid state")

    readResults :: [Reg] -> [Reg] -> [LocalReg] -> [Reg]-> InstrBlock -> NatM (InstrBlock)
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
          w   = cmmRegWidth (CmmLocal dst)
          r_dst = getRegisterReg platform (CmmLocal dst)
      if isFloatFormat format
        then readResults (gpReg:gpRegs) fpRegs dsts (fpReg:accumRegs) (accumCode `snocOL` MOV (OpReg w r_dst) (OpReg w fpReg))
        else readResults gpRegs (fpReg:fpRegs) dsts (gpReg:accumRegs) (accumCode `snocOL` MOV (OpReg w r_dst) (OpReg w gpReg))

    unaryFloatOp w op arg_reg dest_reg = do
      platform <- getPlatform
      (reg_fx, _format_x, code_fx) <- getFloatReg arg_reg
      let dst = getRegisterReg platform (CmmLocal dest_reg)
      let code = code_fx `appOL` op (OpReg w dst) (OpReg w reg_fx)
      return code

{- Note [AArch64 far jumps]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
AArch conditional jump instructions can only encode an offset of +/-1MB
which is usually enough but can be exceeded in edge cases. In these cases
we will replace:

  b.cond <cond> foo

with the sequence:

  b.cond <cond> <lbl_true>
  b <lbl_false>
  <lbl_true>:
  b foo
  <lbl_false>:

Note the encoding of the `b` instruction still limits jumps to
+/-128M offsets, but that seems like an acceptable limitation.

Since AArch64 instructions are all of equal length we can reasonably estimate jumps
in range by counting the instructions between a jump and its target label.

We make some simplifications in the name of performance which can result in overestimating
jump <-> label offsets:

* To avoid having to recalculate the label offsets once we replaced a jump we simply
  assume all jumps will be expanded to a three instruction far jump sequence.
* For labels associated with a info table we assume the info table is 64byte large.
  Most info tables are smaller than that but it means we don't have to distinguish
  between multiple types of info tables.

In terms of implementation we walk the instruction stream at least once calculating
label offsets, and if we determine during this that the functions body is big enough
to potentially contain out of range jumps we walk the instructions a second time, replacing
out of range jumps with the sequence of instructions described above.

-}

-- See Note [AArch64 far jumps]
data BlockInRange = InRange | NotInRange Target

-- See Note [AArch64 far jumps]
makeFarBranches :: Platform -> LabelMap RawCmmStatics -> [NatBasicBlock Instr]
                -> UniqDSM [NatBasicBlock Instr]
makeFarBranches {- only used when debugging -} _platform statics basic_blocks = do
  -- All offsets/positions are counted in multiples of 4 bytes (the size of AArch64 instructions)
  -- That is an offset of 1 represents a 4-byte/one instruction offset.
  let (func_size, lblMap) = foldl' calc_lbl_positions (0, mapEmpty) basic_blocks
  if func_size < max_jump_dist
    then pure basic_blocks
    else do
      (_,blocks) <- mapAccumLM (replace_blk lblMap) 0 basic_blocks
      pure $ concat blocks
      -- pprTrace "lblMap" (ppr lblMap) $ basic_blocks

  where
    -- 2^18, 19 bit immediate with one bit is reserved for the sign
    max_jump_dist = 2^(18::Int) - 1 :: Int
    -- Currently all inline info tables fit into 64 bytes.
    max_info_size     = 16 :: Int
    long_bc_jump_size =  3 :: Int
    long_bz_jump_size =  4 :: Int

    -- Replace out of range conditional jumps with unconditional jumps.
    replace_blk :: LabelMap Int -> Int -> GenBasicBlock Instr -> UniqDSM (Int, [GenBasicBlock Instr])
    replace_blk !m !pos (BasicBlock lbl instrs) = do
      -- Account for a potential info table before the label.
      let !block_pos = pos + infoTblSize_maybe lbl
      (!pos', instrs') <- mapAccumLM (replace_jump m) block_pos instrs
      let instrs'' = concat instrs'
      -- We might have introduced new labels, so split the instructions into basic blocks again if neccesary.
      let (top, split_blocks, no_data) = foldr mkBlocks ([],[],[]) instrs''
      -- There should be no data in the instruction stream at this point
      massert (null no_data)

      let final_blocks = BasicBlock lbl top : split_blocks
      pure (pos', final_blocks)

    replace_jump :: LabelMap Int -> Int -> Instr -> UniqDSM (Int, [Instr])
    replace_jump !m !pos instr = do
      case instr of
        ANN ann instr -> do
          replace_jump m pos instr >>= \case
            (idx,instr':instrs') ->
              pure (idx, ANN ann instr':instrs')
            (idx,[]) -> pprPanic "replace_jump" (text "empty return list for " <+> ppr idx)
        BCOND cond t
          -> case target_in_range m t pos of
              InRange -> pure (pos+long_bc_jump_size,[instr])
              NotInRange far_target -> do
                jmp_code <- genCondFarJump cond far_target
                pure (pos+long_bc_jump_size, fromOL jmp_code)
        CBZ op t -> long_zero_jump op t EQ
        CBNZ op t -> long_zero_jump op t NE
        instr
          | isMetaInstr instr -> pure (pos,[instr])
          | otherwise -> pure (pos+1, [instr])

      where
        -- cmp_op: EQ = CBZ, NEQ = CBNZ
        long_zero_jump op t cmp_op =
          case target_in_range m t pos of
              InRange -> pure (pos+long_bz_jump_size,[instr])
              NotInRange far_target -> do
                jmp_code <- genCondFarJump cmp_op far_target
                -- TODO: Fix zero reg so we can use it here
                pure (pos + long_bz_jump_size, CMP op (OpImm (ImmInt 0)) : fromOL jmp_code)


    target_in_range :: LabelMap Int -> Target -> Int -> BlockInRange
    target_in_range m target src =
      case target of
        (TReg{}) -> InRange
        (TBlock bid) -> block_in_range m src bid
        (TLabel clbl)
          | Just bid <- maybeLocalBlockLabel clbl
          -> block_in_range m src bid
          | otherwise
          -- Maybe we should be pessimistic here, for now just fixing intra proc jumps
          -> InRange

    block_in_range :: LabelMap Int -> Int -> BlockId -> BlockInRange
    block_in_range m src_pos dest_lbl =
      case mapLookup dest_lbl m of
        Nothing       ->
          pprTrace "not in range" (ppr dest_lbl) $
            NotInRange (TBlock dest_lbl)
        Just dest_pos -> if abs (dest_pos - src_pos) < max_jump_dist
          then InRange
          else NotInRange (TBlock dest_lbl)

    calc_lbl_positions :: (Int, LabelMap Int) -> GenBasicBlock Instr -> (Int, LabelMap Int)
    calc_lbl_positions (pos, m) (BasicBlock lbl instrs)
      = let !pos' = pos + infoTblSize_maybe lbl
        in foldl' instr_pos (pos',mapInsert lbl pos' m) instrs

    instr_pos :: (Int, LabelMap Int) -> Instr -> (Int, LabelMap Int)
    instr_pos (pos, m) instr =
      case instr of
        ANN _ann instr -> instr_pos (pos, m) instr
        NEWBLOCK _bid -> panic "mkFarBranched - unexpected NEWBLOCK" -- At this point there should be no NEWBLOCK
                                                                     -- in the instruction stream
                                                                     -- (pos, mapInsert bid pos m)
        COMMENT{} -> (pos, m)
        instr
          | Just jump_size <- is_expandable_jump instr -> (pos+jump_size, m)
          | otherwise -> (pos+1, m)

    infoTblSize_maybe bid =
      case mapLookup bid statics of
        Nothing           -> 0 :: Int
        Just _info_static -> max_info_size

    -- These jumps have a 19bit immediate as offset which is quite
    -- limiting so we potentially have to expand them into
    -- multiple instructions.
    is_expandable_jump i = case i of
      CBZ{}   -> Just long_bz_jump_size
      CBNZ{}  -> Just long_bz_jump_size
      BCOND{} -> Just long_bc_jump_size
      _ -> Nothing
