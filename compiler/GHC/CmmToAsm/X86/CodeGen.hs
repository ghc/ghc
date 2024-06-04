{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NondecreasingIndentation #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-----------------------------------------------------------------------------
--
-- Generating machine code (instruction selection)
--
-- (c) The University of Glasgow 1996-2004
--
-----------------------------------------------------------------------------

-- This is a big module, but, if you pay attention to
-- (a) the sectioning, and (b) the type signatures, the
-- structure should not be too overwhelming.

module GHC.CmmToAsm.X86.CodeGen (
        cmmTopCodeGen,
        generateJumpTableForInstr,
        extractUnwindPoints,
        invertCondBranches,
        InstrBlock
)

where

-- NCG stuff:
import GHC.Prelude

import GHC.CmmToAsm.X86.Instr
import GHC.CmmToAsm.X86.Cond
import GHC.CmmToAsm.X86.Regs
import GHC.CmmToAsm.X86.Ppr
import GHC.CmmToAsm.X86.RegInfo

import GHC.Platform.Regs
import GHC.CmmToAsm.CPrim
import GHC.CmmToAsm.Types
import GHC.Cmm.DebugBlock
   ( DebugBlock(..), UnwindPoint(..), UnwindTable
   , UnwindExpr(UwReg), toUnwindExpr
   )
import GHC.CmmToAsm.PIC
import GHC.CmmToAsm.Monad
   ( NatM, getNewRegNat, getNewLabelNat, setDeltaNat
   , getDeltaNat, getBlockIdNat, getPicBaseNat
   , Reg64(..), RegCode64(..), getNewReg64, localReg64
   , getPicBaseMaybeNat, getDebugBlock, getFileId
   , addImmediateSuccessorNat, updateCfgNat, getConfig, getPlatform
   , getCfgWeights
   )
import GHC.CmmToAsm.CFG
import GHC.CmmToAsm.Format
import GHC.CmmToAsm.Config
import GHC.Platform.Reg
import GHC.Platform

-- Our intermediate code:
import GHC.Types.Basic
import GHC.Cmm.BlockId
import GHC.Unit.Types ( primUnitId )
import GHC.Cmm.Utils
import GHC.Cmm.Switch
import GHC.Cmm
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.CLabel
import GHC.Types.Tickish ( GenTickish(..) )
import GHC.Types.SrcLoc  ( srcSpanFile, srcSpanStartLine, srcSpanStartCol )

-- The rest:
import GHC.Types.ForeignCall ( CCallConv(..) )
import GHC.Data.OrdList
import GHC.Utils.Outputable
import GHC.Utils.Constants (debugIsOn)
import GHC.Utils.Panic
import GHC.Data.FastString
import GHC.Utils.Misc
import GHC.Types.Unique.Supply ( getUniqueM )

import Control.Monad
import Data.Foldable (fold)
import Data.Int
import Data.Maybe
import Data.Word

import qualified Data.Map as M

is32BitPlatform :: NatM Bool
is32BitPlatform = do
    platform <- getPlatform
    return $ target32Bit platform

expect32BitPlatform :: SDoc -> NatM ()
expect32BitPlatform doc = do
  is32Bit <- is32BitPlatform
  when (not is32Bit) $
    pprPanic "Expecting 32-bit platform" doc

sse2Enabled :: NatM Bool
sse2Enabled = do
  config <- getConfig
  return (ncgSseVersion config >= Just SSE2)

sse4_2Enabled :: NatM Bool
sse4_2Enabled = do
  config <- getConfig
  return (ncgSseVersion config >= Just SSE42)

cmmTopCodeGen
        :: RawCmmDecl
        -> NatM [NatCmmDecl (Alignment, RawCmmStatics) Instr]

cmmTopCodeGen (CmmProc info lab live graph) = do
  let blocks = toBlockListEntryFirst graph
  (nat_blocks,statics) <- mapAndUnzipM basicBlockCodeGen blocks
  picBaseMb <- getPicBaseMaybeNat
  platform <- getPlatform
  let proc = CmmProc info lab live (ListGraph $ concat nat_blocks)
      tops = proc : concat statics
      os   = platformOS platform

  case picBaseMb of
      Just picBase -> initializePicBase_x86 os picBase tops
      Nothing -> return tops

cmmTopCodeGen (CmmData sec dat) =
  return [CmmData sec (mkAlignment 1, dat)]  -- no translation, we just use CmmStatic

{- Note [Verifying basic blocks]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   We want to guarantee a few things about the results
   of instruction selection.

   Namely that each basic blocks consists of:
    * A (potentially empty) sequence of straight line instructions
  followed by
    * A (potentially empty) sequence of jump like instructions.

    We can verify this by going through the instructions and
    making sure that any non-jumpish instruction can't appear
    after a jumpish instruction.

    There are gotchas however:
    * CALLs are strictly speaking control flow but here we care
      not about them. Hence we treat them as regular instructions.

      It's safe for them to appear inside a basic block
      as (ignoring side effects inside the call) they will result in
      straight line code.

    * NEWBLOCK marks the start of a new basic block so can
      be followed by any instructions.
-}

-- Verifying basic blocks is cheap, but not cheap enough to enable it unconditionally.
verifyBasicBlock :: Platform -> [Instr] -> ()
verifyBasicBlock platform instrs
  | debugIsOn     = go False instrs
  | otherwise     = ()
  where
    go _     [] = ()
    go atEnd (i:instr)
        = case i of
            -- Start a new basic block
            NEWBLOCK {} -> go False instr
            -- Calls are not viable block terminators
            CALL {}     | atEnd -> faultyBlockWith i
                        | not atEnd -> go atEnd instr
            -- All instructions ok, check if we reached the end and continue.
            _ | not atEnd -> go (isJumpishInstr i) instr
              -- Only jumps allowed at the end of basic blocks.
              | otherwise -> if isJumpishInstr i
                                then go True instr
                                else faultyBlockWith i
    faultyBlockWith i
        = pprPanic "Non control flow instructions after end of basic block."
                   (pprInstr platform i <+> text "in:" $$ vcat (map (pprInstr platform) instrs))

basicBlockCodeGen
        :: CmmBlock
        -> NatM ( [NatBasicBlock Instr]
                , [NatCmmDecl (Alignment, RawCmmStatics) Instr])

basicBlockCodeGen block = do
  let (_, nodes, tail)  = blockSplit block
      id = entryLabel block
      stmts = blockToList nodes
  -- Generate location directive
  dbg <- getDebugBlock (entryLabel block)
  loc_instrs <- case dblSourceTick =<< dbg of
    Just (SourceNote span (LexicalFastString name))
      -> do fileId <- getFileId (srcSpanFile span)
            let line = srcSpanStartLine span; col = srcSpanStartCol span
            return $ unitOL $ LOCATION fileId line col (unpackFS name)
    _ -> return nilOL
  (mid_instrs,mid_bid) <- stmtsToInstrs id stmts
  (!tail_instrs,_) <- stmtToInstrs mid_bid tail
  let instrs = loc_instrs `appOL` mid_instrs `appOL` tail_instrs
  platform <- getPlatform
  return $! verifyBasicBlock platform (fromOL instrs)
  instrs' <- fold <$> traverse addSpUnwindings instrs
  -- code generation may introduce new basic block boundaries, which
  -- are indicated by the NEWBLOCK instruction.  We must split up the
  -- instruction stream into basic blocks again.  Also, we extract
  -- LDATAs here too.
  let
        (top,other_blocks,statics) = foldrOL mkBlocks ([],[],[]) instrs'

        mkBlocks (NEWBLOCK id) (instrs,blocks,statics)
          = ([], BasicBlock id instrs : blocks, statics)
        mkBlocks (LDATA sec dat) (instrs,blocks,statics)
          = (instrs, blocks, CmmData sec dat:statics)
        mkBlocks instr (instrs,blocks,statics)
          = (instr:instrs, blocks, statics)
  return (BasicBlock id top : other_blocks, statics)

-- | Convert 'DELTA' instructions into 'UNWIND' instructions to capture changes
-- in the @sp@ register. See Note [What is this unwinding business?] in "GHC.Cmm.DebugBlock"
-- for details.
addSpUnwindings :: Instr -> NatM (OrdList Instr)
addSpUnwindings instr@(DELTA d) = do
    config <- getConfig
    let platform = ncgPlatform config
    if ncgDwarfUnwindings config
        then do lbl <- mkAsmTempLabel <$> getUniqueM
                let unwind = M.singleton MachSp (Just $ UwReg (GlobalRegUse MachSp (bWord platform)) $ negate d)
                return $ toOL [ instr, UNWIND lbl unwind ]
        else return (unitOL instr)
addSpUnwindings instr = return $ unitOL instr

{- Note [Keeping track of the current block]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When generating instructions for Cmm we sometimes require
the current block for things like retry loops.

We also sometimes change the current block, if a MachOP
results in branching control flow.

Issues arise if we have two statements in the same block,
which both depend on the current block id *and* change the
basic block after them. This happens for atomic primops
in the X86 backend where we want to update the CFG data structure
when introducing new basic blocks.

For example in #17334 we got this Cmm code:

        c3Bf: // global
            (_s3t1::I64) = call MO_AtomicRMW W64 AMO_And(_s3sQ::P64 + 88, 18);
            (_s3t4::I64) = call MO_AtomicRMW W64 AMO_Or(_s3sQ::P64 + 88, 0);
            _s3sT::I64 = _s3sV::I64;
            goto c3B1;

This resulted in two new basic blocks being inserted:

        c3Bf:
                movl $18,%vI_n3Bo
                movq 88(%vI_s3sQ),%rax
                jmp _n3Bp
        n3Bp:
                ...
                cmpxchgq %vI_n3Bq,88(%vI_s3sQ)
                jne _n3Bp
                ...
                jmp _n3Bs
        n3Bs:
                ...
                cmpxchgq %vI_n3Bt,88(%vI_s3sQ)
                jne _n3Bs
                ...
                jmp _c3B1
        ...

Based on the Cmm we called stmtToInstrs we translated both atomic operations under
the assumption they would be placed into their Cmm basic block `c3Bf`.
However for the retry loop we introduce new labels, so this is not the case
for the second statement.
This resulted in a desync between the explicit control flow graph
we construct as a separate data type and the actual control flow graph in the code.

Instead we now return the new basic block if a statement causes a change
in the current block and use the block for all following statements.

For this reason genForeignCall is also split into two parts.  One for calls which
*won't* change the basic blocks in which successive instructions will be
placed (since they only evaluate CmmExpr, which can only contain MachOps, which
cannot introduce basic blocks in their lowerings).  A different one for calls
which *are* known to change the basic block.

-}

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
  is32Bit <- is32BitPlatform
  platform <- getPlatform
  case stmt of
    CmmUnsafeForeignCall target result_regs args
       -> genForeignCall target result_regs args bid

    _ -> (,Nothing) <$> case stmt of
      CmmComment s   -> return (unitOL (COMMENT s))
      CmmTick {}     -> return nilOL

      CmmUnwind regs -> do
        let to_unwind_entry :: (GlobalReg, Maybe CmmExpr) -> UnwindTable
            to_unwind_entry (reg, expr) = M.singleton reg (fmap (toUnwindExpr platform) expr)
        case foldMap to_unwind_entry regs of
          tbl | M.null tbl -> return nilOL
              | otherwise  -> do
                  lbl <- mkAsmTempLabel <$> getUniqueM
                  return $ unitOL $ UNWIND lbl tbl

      CmmAssign reg src
        | isFloatType ty         -> assignReg_FltCode format reg src
        | is32Bit && isWord64 ty -> assignReg_I64Code      reg src
        | otherwise              -> assignReg_IntCode format reg src
          where ty = cmmRegType reg
                format = cmmTypeFormat ty

      CmmStore addr src _alignment
        | isFloatType ty         -> assignMem_FltCode format addr src
        | is32Bit && isWord64 ty -> assignMem_I64Code      addr src
        | otherwise              -> assignMem_IntCode format addr src
          where ty = cmmExprType platform src
                format = cmmTypeFormat ty

      CmmBranch id          -> return $ genBranch id

      --We try to arrange blocks such that the likely branch is the fallthrough
      --in GHC.Cmm.ContFlowOpt. So we can assume the condition is likely false here.
      CmmCondBranch arg true false _ -> genCondBranch bid true false arg
      CmmSwitch arg ids -> genSwitch arg ids
      CmmCall { cml_target = arg
              , cml_args_regs = gregs } -> genJump arg (jumpRegs platform gregs)
      _ ->
        panic "stmtToInstrs: statement should have been cps'd away"


jumpRegs :: Platform -> [GlobalReg] -> [Reg]
jumpRegs platform gregs = [ RegReal r | Just r <- map (globalRegMaybe platform) gregs ]

--------------------------------------------------------------------------------
-- | 'InstrBlock's are the insn sequences generated by the insn selectors.
--      They are really trees of insns to facilitate fast appending, where a
--      left-to-right traversal yields the insns in the correct order.
--
type InstrBlock
        = OrdList Instr


-- | Condition codes passed up the tree.
--
data CondCode
        = CondCode Bool Cond InstrBlock


-- | Register's passed up the tree.  If the stix code forces the register
--      to live in a pre-decided machine register, it comes out as @Fixed@;
--      otherwise, it comes out as @Any@, and the parent can decide which
--      register to put it in.
--
data Register
        = Fixed Format Reg InstrBlock
        | Any   Format (Reg -> InstrBlock)


swizzleRegisterRep :: Register -> Format -> Register
swizzleRegisterRep (Fixed _ reg code) format = Fixed format reg code
swizzleRegisterRep (Any _ codefn)     format = Any   format codefn

getLocalRegReg :: LocalReg -> Reg
getLocalRegReg (LocalReg u pk)
  = -- by Assuming SSE2, Int,Word,Float,Double all can be register allocated
    RegVirtual (mkVirtualReg u (cmmTypeFormat pk))

-- | Grab the Reg for a CmmReg
getRegisterReg :: Platform  -> CmmReg -> Reg

getRegisterReg _   (CmmLocal lreg) = getLocalRegReg lreg

getRegisterReg platform  (CmmGlobal mid)
  = case globalRegMaybe platform $ globalRegUseGlobalReg mid of
        Just reg -> RegReal $ reg
        Nothing  -> pprPanic "getRegisterReg-memory" (ppr $ CmmGlobal mid)
        -- By this stage, the only MagicIds remaining should be the
        -- ones which map to a real machine register on this
        -- platform.  Hence ...


-- | Memory addressing modes passed up the tree.
data Amode
        = Amode AddrMode InstrBlock

{-
Now, given a tree (the argument to a CmmLoad) that references memory,
produce a suitable addressing mode.

A Rule of the Game (tm) for Amodes: use of the addr bit must
immediately follow use of the code part, since the code part puts
values in registers which the addr then refers to.  So you can't put
anything in between, lest it overwrite some of those registers.  If
you need to do some other computation between the code part and use of
the addr bit, first store the effective address from the amode in a
temporary, then do the other computation, and then use the temporary:

    code
    LEA amode, tmp
    ... other computation ...
    ... (tmp) ...
-}

{-
Note [%rip-relative addressing on x86-64]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
On x86-64 GHC produces code for use in the "small" or, when `-fPIC` is set,
"small PIC" code models defined by the x86-64 System V ABI (section 3.5.1 of
specification version 0.99).

In general the small code model would allow us to assume that code is located
between 0 and 2^31 - 1. However, this is not true on Windows which, due to
high-entropy ASLR, may place the executable image anywhere in 64-bit address
space. This is problematic since immediate operands in x86-64 are generally
32-bit sign-extended values (with the exception of the 64-bit MOVABS encoding).
Consequently, to avoid overflowing we use %rip-relative addressing universally.
Since %rip-relative addressing comes essentially for free and makes linking far
easier, we use it even on non-Windows platforms.

See also: the documentation for GCC's `-mcmodel=small` flag.
-}


-- | Check whether an integer will fit in 32 bits.
--      A CmmInt is intended to be truncated to the appropriate
--      number of bits, so here we truncate it to Int64.  This is
--      important because e.g. -1 as a CmmInt might be either
--      -1 or 18446744073709551615.
--
is32BitInteger :: Integer -> Bool
is32BitInteger i = i64 <= 0x7fffffff && i64 >= -0x80000000
  where i64 = fromIntegral i :: Int64


-- | Convert a BlockId to some CmmStatic data
jumpTableEntry :: NCGConfig -> Maybe BlockId -> CmmStatic
jumpTableEntry config Nothing = CmmStaticLit (CmmInt 0 (ncgWordWidth config))
jumpTableEntry _ (Just blockid) = CmmStaticLit (CmmLabel blockLabel)
    where blockLabel = blockLbl blockid


-- -----------------------------------------------------------------------------
-- General things for putting together code sequences

-- Expand CmmRegOff.  ToDo: should we do it this way around, or convert
-- CmmExprs into CmmRegOff?
mangleIndexTree :: CmmReg -> Int -> CmmExpr
mangleIndexTree reg off
  = CmmMachOp (MO_Add width) [CmmReg reg, CmmLit (CmmInt (fromIntegral off) width)]
  where width = typeWidth (cmmRegType reg)

-- | The dual to getAnyReg: compute an expression into a register, but
--      we don't mind which one it is.
getSomeReg :: CmmExpr -> NatM (Reg, InstrBlock)
getSomeReg expr = do
  r <- getRegister expr
  case r of
    Any rep code -> do
        tmp <- getNewRegNat rep
        return (tmp, code tmp)
    Fixed _ reg code ->
        return (reg, code)


assignMem_I64Code :: CmmExpr -> CmmExpr -> NatM InstrBlock
assignMem_I64Code addrTree valueTree = do
  Amode addr addr_code <- getAmode addrTree
  RegCode64 vcode rhi rlo <- iselExpr64 valueTree
  let
        -- Little-endian store
        mov_lo = MOV II32 (OpReg rlo) (OpAddr addr)
        mov_hi = MOV II32 (OpReg rhi) (OpAddr (fromJust (addrOffset addr 4)))
  return (vcode `appOL` addr_code `snocOL` mov_lo `snocOL` mov_hi)


assignReg_I64Code :: CmmReg  -> CmmExpr -> NatM InstrBlock
assignReg_I64Code (CmmLocal dst) valueTree = do
   RegCode64 vcode r_src_hi r_src_lo <- iselExpr64 valueTree
   let
         Reg64 r_dst_hi r_dst_lo = localReg64 dst
         mov_lo = MOV II32 (OpReg r_src_lo) (OpReg r_dst_lo)
         mov_hi = MOV II32 (OpReg r_src_hi) (OpReg r_dst_hi)
   return (
        vcode `snocOL` mov_lo `snocOL` mov_hi
     )

assignReg_I64Code _ _
   = panic "assignReg_I64Code(i386): invalid lvalue"

iselExpr64 :: HasDebugCallStack => CmmExpr -> NatM (RegCode64 InstrBlock)
iselExpr64 (CmmLit (CmmInt i _)) = do
  Reg64 rhi rlo <- getNewReg64
  let
        r = fromIntegral (fromIntegral i :: Word32)
        q = fromIntegral (fromIntegral (i `shiftR` 32) :: Word32)
        code = toOL [
                MOV II32 (OpImm (ImmInteger r)) (OpReg rlo),
                MOV II32 (OpImm (ImmInteger q)) (OpReg rhi)
                ]
  return (RegCode64 code rhi rlo)

iselExpr64 (CmmLoad addrTree ty _) | isWord64 ty = do
   Amode addr addr_code <- getAmode addrTree
   Reg64 rhi rlo <- getNewReg64
   let
        mov_lo = MOV II32 (OpAddr addr) (OpReg rlo)
        mov_hi = MOV II32 (OpAddr (fromJust (addrOffset addr 4))) (OpReg rhi)
   return (
            RegCode64 (addr_code `snocOL` mov_lo `snocOL` mov_hi) rhi rlo
     )

iselExpr64 (CmmReg (CmmLocal local_reg)) = do
  let Reg64 hi lo = localReg64 local_reg
  return (RegCode64 nilOL hi lo)

iselExpr64 (CmmMachOp (MO_Add _) [e1, CmmLit (CmmInt i _)]) = do
   RegCode64 code1 r1hi r1lo <- iselExpr64 e1
   Reg64 rhi rlo <- getNewReg64
   let
        r = fromIntegral (fromIntegral i :: Word32)
        q = fromIntegral (fromIntegral (i `shiftR` 32) :: Word32)
        code =  code1 `appOL`
                toOL [ MOV II32 (OpReg r1lo) (OpReg rlo),
                       ADD II32 (OpImm (ImmInteger r)) (OpReg rlo),
                       MOV II32 (OpReg r1hi) (OpReg rhi),
                       ADC II32 (OpImm (ImmInteger q)) (OpReg rhi) ]
   return (RegCode64 code rhi rlo)

iselExpr64 (CmmMachOp (MO_Add _) [e1,e2]) = do
   RegCode64 code1 r1hi r1lo <- iselExpr64 e1
   RegCode64 code2 r2hi r2lo <- iselExpr64 e2
   Reg64 rhi rlo <- getNewReg64
   let
        code =  code1 `appOL`
                code2 `appOL`
                toOL [ MOV II32 (OpReg r1lo) (OpReg rlo),
                       ADD II32 (OpReg r2lo) (OpReg rlo),
                       MOV II32 (OpReg r1hi) (OpReg rhi),
                       ADC II32 (OpReg r2hi) (OpReg rhi) ]
   return (RegCode64 code rhi rlo)

iselExpr64 (CmmMachOp (MO_Sub _) [e1,e2]) = do
   RegCode64 code1 r1hi r1lo <- iselExpr64 e1
   RegCode64 code2 r2hi r2lo <- iselExpr64 e2
   Reg64 rhi rlo <- getNewReg64
   let
        code =  code1 `appOL`
                code2 `appOL`
                toOL [ MOV II32 (OpReg r1lo) (OpReg rlo),
                       SUB II32 (OpReg r2lo) (OpReg rlo),
                       MOV II32 (OpReg r1hi) (OpReg rhi),
                       SBB II32 (OpReg r2hi) (OpReg rhi) ]
   return (RegCode64 code rhi rlo)

iselExpr64 (CmmMachOp (MO_UU_Conv W32 W64) [expr]) = do
     code <- getAnyReg expr
     Reg64 r_dst_hi r_dst_lo <- getNewReg64
     return $ RegCode64 (code r_dst_lo `snocOL`
                          XOR II32 (OpReg r_dst_hi) (OpReg r_dst_hi))
                          r_dst_hi
                          r_dst_lo

iselExpr64 (CmmMachOp (MO_UU_Conv W16 W64) [expr]) = do
     (rsrc, code) <- getByteReg expr
     Reg64 r_dst_hi r_dst_lo <- getNewReg64
     return $ RegCode64 (code `appOL` toOL [
                          MOVZxL II16 (OpReg rsrc) (OpReg r_dst_lo),
                          XOR    II32 (OpReg r_dst_hi) (OpReg r_dst_hi)
                          ])
                          r_dst_hi
                          r_dst_lo

iselExpr64 (CmmMachOp (MO_UU_Conv W8 W64) [expr]) = do
     (rsrc, code) <- getByteReg expr
     Reg64 r_dst_hi r_dst_lo <- getNewReg64
     return $ RegCode64 (code `appOL` toOL [
                          MOVZxL II8 (OpReg rsrc) (OpReg r_dst_lo),
                          XOR    II32 (OpReg r_dst_hi) (OpReg r_dst_hi)
                          ])
                          r_dst_hi
                          r_dst_lo

iselExpr64 (CmmMachOp (MO_SS_Conv W32 W64) [expr]) = do
     code <- getAnyReg expr
     Reg64 r_dst_hi r_dst_lo <- getNewReg64
     return $ RegCode64 (code r_dst_lo `snocOL`
                          MOV II32 (OpReg r_dst_lo) (OpReg eax) `snocOL`
                          CLTD II32 `snocOL`
                          MOV II32 (OpReg eax) (OpReg r_dst_lo) `snocOL`
                          MOV II32 (OpReg edx) (OpReg r_dst_hi))
                          r_dst_hi
                          r_dst_lo

iselExpr64 (CmmMachOp (MO_SS_Conv W16 W64) [expr]) = do
     (r, code) <- getByteReg expr
     Reg64 r_dst_hi r_dst_lo <- getNewReg64
     return $ RegCode64 (code `appOL` toOL [
                          MOVSxL II16 (OpReg r) (OpReg eax),
                          CLTD II32,
                          MOV II32 (OpReg eax) (OpReg r_dst_lo),
                          MOV II32 (OpReg edx) (OpReg r_dst_hi)])
                          r_dst_hi
                          r_dst_lo

iselExpr64 (CmmMachOp (MO_SS_Conv W8 W64) [expr]) = do
     (r, code) <- getByteReg expr
     Reg64 r_dst_hi r_dst_lo <- getNewReg64
     return $ RegCode64 (code `appOL` toOL [
                          MOVSxL II8 (OpReg r) (OpReg eax),
                          CLTD II32,
                          MOV II32 (OpReg eax) (OpReg r_dst_lo),
                          MOV II32 (OpReg edx) (OpReg r_dst_hi)])
                          r_dst_hi
                          r_dst_lo

iselExpr64 (CmmMachOp (MO_S_Neg _) [expr]) = do
   RegCode64 code rhi rlo <- iselExpr64 expr
   Reg64 rohi rolo <- getNewReg64
   let
        ocode = code `appOL`
                toOL [ MOV II32 (OpReg rlo) (OpReg rolo),
                       XOR II32 (OpReg rohi) (OpReg rohi),
                       NEGI II32 (OpReg rolo),
                       SBB II32 (OpReg rhi) (OpReg rohi) ]
   return (RegCode64 ocode rohi rolo)

-- To multiply two 64-bit numbers we use the following decomposition (in C notation):
--
--     ((r1hi << 32) + r1lo) * ((r2hi << 32) + r2lo)
--      = ((r2lo * r1hi) << 32)
--      + ((r1lo * r2hi) << 32)
--      + r1lo * r2lo
--
-- Note that @(r1hi * r2hi) << 64@ can be dropped because it overflows completely.

iselExpr64 (CmmMachOp (MO_Mul _) [e1,e2]) = do
   RegCode64 code1 r1hi r1lo <- iselExpr64 e1
   RegCode64 code2 r2hi r2lo <- iselExpr64 e2
   Reg64 rhi rlo <- getNewReg64
   tmp <- getNewRegNat II32
   let
        code =  code1 `appOL`
                code2 `appOL`
                toOL [ MOV  II32 (OpReg r1lo) (OpReg eax),
                       MOV  II32 (OpReg r2lo) (OpReg tmp),
                       MOV  II32 (OpReg r1hi) (OpReg rhi),
                       IMUL II32 (OpReg tmp) (OpReg rhi),
                       MOV  II32 (OpReg r2hi) (OpReg rlo),
                       IMUL II32 (OpReg eax) (OpReg rlo),
                       ADD  II32 (OpReg rlo) (OpReg rhi),
                       MUL2 II32 (OpReg tmp),
                       ADD  II32 (OpReg edx) (OpReg rhi),
                       MOV  II32 (OpReg eax) (OpReg rlo)
                     ]
   return (RegCode64 code rhi rlo)

iselExpr64 (CmmMachOp (MO_S_MulMayOflo W64) _) = do
   -- Performance sensitive users won't use 32 bit so let's keep it simple:
   -- We always return a (usually false) positive.
   Reg64 rhi rlo <- getNewReg64
   let code = toOL   [
                       MOV  II32 (OpImm (ImmInt 1)) (OpReg rhi),
                       MOV  II32 (OpImm (ImmInt 1)) (OpReg rlo)
                     ]
   return (RegCode64 code rhi rlo)


-- To shift a 64-bit number to the left we use the SHLD and SHL instructions.
-- We use SHLD to shift the bits in @rhi@ to the left while copying
-- high bits from @rlo@ to fill the new space in the low bits of @rhi@.
-- That leaves @rlo@ unchanged, so we use SHL to shift the bits of @rlo@ left.
-- However, both these instructions only use the lowest 5 bits from %cl to do
-- their shifting. So if the sixth bit (0x32) is set then we additionally move
-- the contents of @rlo@ to @rhi@ and clear @rlo@.

iselExpr64 (CmmMachOp (MO_Shl _) [e1,e2]) = do
   RegCode64 code1 r1hi r1lo <- iselExpr64 e1
   code2 <- getAnyReg e2
   Reg64 rhi rlo <- getNewReg64
   lbl1 <- newBlockId
   lbl2 <- newBlockId
   let
        code =  code1 `appOL`
                code2 ecx `appOL`
                toOL [ MOV II32 (OpReg r1lo) (OpReg rlo),
                       MOV II32 (OpReg r1hi) (OpReg rhi),
                       SHLD II32 (OpReg ecx) (OpReg rlo) (OpReg rhi),
                       SHL II32 (OpReg ecx) (OpReg rlo),
                       TEST II32 (OpImm (ImmInt 32)) (OpReg ecx),
                       JXX EQQ lbl2,
                       JXX ALWAYS lbl1,
                       NEWBLOCK lbl1,
                       MOV II32 (OpReg rlo) (OpReg rhi),
                       XOR II32 (OpReg rlo) (OpReg rlo),
                       JXX ALWAYS lbl2,
                       NEWBLOCK lbl2
                     ]
   return (RegCode64 code rhi rlo)

-- Similar to above, however now we're shifting to the right
-- and we're doing a signed shift which means that @rhi@ needs
-- to be set to either 0 if @rhi@ is positive or 0xffffffff otherwise,
-- and if the sixth bit of %cl is set (so the shift amount is more than 32).
-- To accomplish that we shift @rhi@ by 31.

iselExpr64 (CmmMachOp (MO_S_Shr _) [e1,e2]) = do
   RegCode64 code1 r1hi r1lo <- iselExpr64 e1
   (r2, code2) <- getSomeReg e2
   Reg64 rhi rlo <- getNewReg64
   lbl1 <- newBlockId
   lbl2 <- newBlockId
   let
        code =  code1 `appOL`
                code2 `appOL`
                toOL [ MOV II32 (OpReg r1lo) (OpReg rlo),
                       MOV II32 (OpReg r1hi) (OpReg rhi),
                       MOV II32 (OpReg r2) (OpReg ecx),
                       SHRD II32 (OpReg ecx) (OpReg rhi) (OpReg rlo),
                       SAR II32 (OpReg ecx) (OpReg rhi),
                       TEST II32 (OpImm (ImmInt 32)) (OpReg ecx),
                       JXX EQQ lbl2,
                       JXX ALWAYS lbl1,
                       NEWBLOCK lbl1,
                       MOV II32 (OpReg rhi) (OpReg rlo),
                       SAR II32 (OpImm (ImmInt 31)) (OpReg rhi),
                       JXX ALWAYS lbl2,
                       NEWBLOCK lbl2
                     ]
   return (RegCode64 code rhi rlo)

-- Similar to the above.

iselExpr64 (CmmMachOp (MO_U_Shr _) [e1,e2]) = do
   RegCode64 code1 r1hi r1lo <- iselExpr64 e1
   (r2, code2) <- getSomeReg e2
   Reg64 rhi rlo <- getNewReg64
   lbl1 <- newBlockId
   lbl2 <- newBlockId
   let
        code =  code1 `appOL`
                code2 `appOL`
                toOL [ MOV II32 (OpReg r1lo) (OpReg rlo),
                       MOV II32 (OpReg r1hi) (OpReg rhi),
                       MOV II32 (OpReg r2) (OpReg ecx),
                       SHRD II32 (OpReg ecx) (OpReg rhi) (OpReg rlo),
                       SHR II32 (OpReg ecx) (OpReg rhi),
                       TEST II32 (OpImm (ImmInt 32)) (OpReg ecx),
                       JXX EQQ lbl2,
                       JXX ALWAYS lbl1,
                       NEWBLOCK lbl1,
                       MOV II32 (OpReg rhi) (OpReg rlo),
                       XOR II32 (OpReg rhi) (OpReg rhi),
                       JXX ALWAYS lbl2,
                       NEWBLOCK lbl2
                     ]
   return (RegCode64 code rhi rlo)

iselExpr64 (CmmMachOp (MO_And _) [e1,e2]) = iselExpr64ParallelBin AND e1 e2
iselExpr64 (CmmMachOp (MO_Or  _) [e1,e2]) = iselExpr64ParallelBin OR  e1 e2
iselExpr64 (CmmMachOp (MO_Xor _) [e1,e2]) = iselExpr64ParallelBin XOR e1 e2

iselExpr64 (CmmMachOp (MO_Not _) [e1]) = do
   RegCode64 code1 r1hi r1lo <- iselExpr64 e1
   Reg64 rhi rlo <- getNewReg64
   let
        code =  code1 `appOL`
                toOL [ MOV II32 (OpReg r1lo) (OpReg rlo),
                       MOV II32 (OpReg r1hi) (OpReg rhi),
                       NOT II32 (OpReg rlo),
                       NOT II32 (OpReg rhi)
                     ]
   return (RegCode64 code rhi rlo)

iselExpr64 (CmmRegOff r i) = iselExpr64 (mangleIndexTree r i)

iselExpr64 expr
   = do
      platform <- getPlatform
      pprPanic "iselExpr64(i386)" (pdoc platform expr $+$ text (show expr))

iselExpr64ParallelBin :: (Format -> Operand -> Operand -> Instr)
                      -> CmmExpr -> CmmExpr -> NatM (RegCode64 (OrdList Instr))
iselExpr64ParallelBin op e1 e2 = do
   RegCode64 code1 r1hi r1lo <- iselExpr64 e1
   RegCode64 code2 r2hi r2lo <- iselExpr64 e2
   Reg64 rhi rlo <- getNewReg64
   let
        code =  code1 `appOL`
                code2 `appOL`
                toOL [ MOV II32 (OpReg r1lo) (OpReg rlo),
                       MOV II32 (OpReg r1hi) (OpReg rhi),
                       op  II32 (OpReg r2lo) (OpReg rlo),
                       op  II32 (OpReg r2hi) (OpReg rhi)
                     ]
   return (RegCode64 code rhi rlo)

--------------------------------------------------------------------------------
getRegister :: CmmExpr -> NatM Register
getRegister e = do platform <- getPlatform
                   is32Bit <- is32BitPlatform
                   getRegister' platform is32Bit e

getRegister' :: Platform -> Bool -> CmmExpr -> NatM Register

getRegister' _ is32Bit (CmmReg reg)
  = case reg of
        CmmGlobal (GlobalRegUse PicBaseReg _)
         | is32Bit ->
            -- on x86_64, we have %rip for PicBaseReg, but it's not
            -- a full-featured register, it can only be used for
            -- rip-relative addressing.
            do reg' <- getPicBaseNat (archWordFormat is32Bit)
               return (Fixed (archWordFormat is32Bit) reg' nilOL)
        _ ->
            do
               let
                 fmt = cmmTypeFormat (cmmRegType reg)
               platform <- ncgPlatform <$> getConfig
               return (Fixed fmt
                             (getRegisterReg platform reg)
                             nilOL)


getRegister' platform is32Bit (CmmRegOff r n)
  = getRegister' platform is32Bit $ mangleIndexTree r n

getRegister' platform is32Bit (CmmMachOp (MO_RelaxedRead w) [e])
  = getRegister' platform is32Bit (CmmLoad e (cmmBits w) NaturallyAligned)

getRegister' platform is32Bit (CmmMachOp (MO_AlignmentCheck align _) [e])
  = addAlignmentCheck align <$> getRegister' platform is32Bit e

-- for 32-bit architectures, support some 64 -> 32 bit conversions:
-- TO_W_(x), TO_W_(x >> 32)

getRegister' _ is32Bit (CmmMachOp (MO_UU_Conv W64 W32)
                     [CmmMachOp (MO_U_Shr W64) [x,CmmLit (CmmInt 32 _)]])
 | is32Bit = do
  RegCode64 code rhi _rlo <- iselExpr64 x
  return $ Fixed II32 rhi code

getRegister' _ is32Bit (CmmMachOp (MO_SS_Conv W64 W32)
                     [CmmMachOp (MO_U_Shr W64) [x,CmmLit (CmmInt 32 _)]])
 | is32Bit = do
  RegCode64 code rhi _rlo <- iselExpr64 x
  return $ Fixed II32 rhi code

getRegister' _ is32Bit (CmmMachOp (MO_UU_Conv W64 W32) [x])
 | is32Bit = do
  RegCode64 code _rhi rlo <- iselExpr64 x
  return $ Fixed II32 rlo code

getRegister' _ is32Bit (CmmMachOp (MO_SS_Conv W64 W32) [x])
 | is32Bit = do
  RegCode64 code _rhi rlo <- iselExpr64 x
  return $ Fixed II32 rlo code

getRegister' _ is32Bit (CmmMachOp (MO_UU_Conv W64 W8) [x])
 | is32Bit = do
  RegCode64 code _rhi rlo <- iselExpr64 x
  ro <- getNewRegNat II8
  return $ Fixed II8 ro (code `appOL` toOL [ MOVZxL II8 (OpReg rlo) (OpReg ro) ])

getRegister' _ is32Bit (CmmMachOp (MO_UU_Conv W64 W16) [x])
 | is32Bit = do
  RegCode64 code _rhi rlo <- iselExpr64 x
  ro <- getNewRegNat II16
  return $ Fixed II16 ro (code `appOL` toOL [ MOVZxL II16 (OpReg rlo) (OpReg ro) ])

getRegister' _ _ (CmmLit lit@(CmmFloat f w)) =
  float_const_sse2  where
  float_const_sse2
    | f == 0.0 = do
      let
          format = floatFormat w
          code dst = unitOL  (XOR format (OpReg dst) (OpReg dst))
        -- I don't know why there are xorpd, xorps, and pxor instructions.
        -- They all appear to do the same thing --SDM
      return (Any format code)

   | otherwise = do
      Amode addr code <- memConstant (mkAlignment $ widthInBytes w) lit
      loadFloatAmode w addr code

-- catch simple cases of zero- or sign-extended load
getRegister' _ _ (CmmMachOp (MO_UU_Conv W8 W32) [CmmLoad addr _ _]) = do
  code <- intLoadCode (MOVZxL II8) addr
  return (Any II32 code)

getRegister' _ _ (CmmMachOp (MO_SS_Conv W8 W32) [CmmLoad addr _ _]) = do
  code <- intLoadCode (MOVSxL II8) addr
  return (Any II32 code)

getRegister' _ _ (CmmMachOp (MO_UU_Conv W16 W32) [CmmLoad addr _ _]) = do
  code <- intLoadCode (MOVZxL II16) addr
  return (Any II32 code)

getRegister' _ _ (CmmMachOp (MO_SS_Conv W16 W32) [CmmLoad addr _ _]) = do
  code <- intLoadCode (MOVSxL II16) addr
  return (Any II32 code)

-- catch simple cases of zero- or sign-extended load
getRegister' _ is32Bit (CmmMachOp (MO_UU_Conv W8 W64) [CmmLoad addr _ _])
 | not is32Bit = do
  code <- intLoadCode (MOVZxL II8) addr
  return (Any II64 code)

getRegister' _ is32Bit (CmmMachOp (MO_SS_Conv W8 W64) [CmmLoad addr _ _])
 | not is32Bit = do
  code <- intLoadCode (MOVSxL II8) addr
  return (Any II64 code)

getRegister' _ is32Bit (CmmMachOp (MO_UU_Conv W16 W64) [CmmLoad addr _ _])
 | not is32Bit = do
  code <- intLoadCode (MOVZxL II16) addr
  return (Any II64 code)

getRegister' _ is32Bit (CmmMachOp (MO_SS_Conv W16 W64) [CmmLoad addr _ _])
 | not is32Bit = do
  code <- intLoadCode (MOVSxL II16) addr
  return (Any II64 code)

getRegister' _ is32Bit (CmmMachOp (MO_UU_Conv W32 W64) [CmmLoad addr _ _])
 | not is32Bit = do
  code <- intLoadCode (MOV II32) addr -- 32-bit loads zero-extend
  return (Any II64 code)

getRegister' _ is32Bit (CmmMachOp (MO_SS_Conv W32 W64) [CmmLoad addr _ _])
 | not is32Bit = do
  code <- intLoadCode (MOVSxL II32) addr
  return (Any II64 code)

getRegister' _ is32Bit (CmmMachOp (MO_Add W64) [CmmReg (CmmGlobal (GlobalRegUse PicBaseReg _)),
                                     CmmLit displacement])
 | not is32Bit =
      return $ Any II64 (\dst -> unitOL $
        LEA II64 (OpAddr (ripRel (litToImm displacement))) (OpReg dst))

getRegister' platform is32Bit (CmmMachOp mop [x]) = -- unary MachOps
    case mop of
      MO_F_Neg w  -> sse2NegCode w x


      MO_S_Neg w -> triv_ucode NEGI (intFormat w)
      MO_Not w   -> triv_ucode NOT  (intFormat w)

      -- Nop conversions
      MO_UU_Conv W32 W8  -> toI8Reg  W32 x
      MO_SS_Conv W32 W8  -> toI8Reg  W32 x
      MO_XX_Conv W32 W8  -> toI8Reg  W32 x
      MO_UU_Conv W16 W8  -> toI8Reg  W16 x
      MO_SS_Conv W16 W8  -> toI8Reg  W16 x
      MO_XX_Conv W16 W8  -> toI8Reg  W16 x
      MO_UU_Conv W32 W16 -> toI16Reg W32 x
      MO_SS_Conv W32 W16 -> toI16Reg W32 x
      MO_XX_Conv W32 W16 -> toI16Reg W32 x

      MO_UU_Conv W64 W32 | not is32Bit -> conversionNop II64 x
      MO_SS_Conv W64 W32 | not is32Bit -> conversionNop II64 x
      MO_XX_Conv W64 W32 | not is32Bit -> conversionNop II64 x
      MO_UU_Conv W64 W16 | not is32Bit -> toI16Reg W64 x
      MO_SS_Conv W64 W16 | not is32Bit -> toI16Reg W64 x
      MO_XX_Conv W64 W16 | not is32Bit -> toI16Reg W64 x
      MO_UU_Conv W64 W8  | not is32Bit -> toI8Reg  W64 x
      MO_SS_Conv W64 W8  | not is32Bit -> toI8Reg  W64 x
      MO_XX_Conv W64 W8  | not is32Bit -> toI8Reg  W64 x

      MO_UU_Conv rep1 rep2 | rep1 == rep2 -> conversionNop (intFormat rep1) x
      MO_SS_Conv rep1 rep2 | rep1 == rep2 -> conversionNop (intFormat rep1) x
      MO_XX_Conv rep1 rep2 | rep1 == rep2 -> conversionNop (intFormat rep1) x

      MO_FW_Bitcast W32 -> bitcast FF32 II32 x
      MO_WF_Bitcast W32 -> bitcast II32 FF32 x
      MO_FW_Bitcast W64 -> bitcast FF64 II64 x
      MO_WF_Bitcast W64 -> bitcast II64 FF64 x

      -- widenings
      MO_UU_Conv W8  W32 -> integerExtend W8  W32 MOVZxL x
      MO_UU_Conv W16 W32 -> integerExtend W16 W32 MOVZxL x
      MO_UU_Conv W8  W16 -> integerExtend W8  W16 MOVZxL x

      MO_SS_Conv W8  W32 -> integerExtend W8  W32 MOVSxL x
      MO_SS_Conv W16 W32 -> integerExtend W16 W32 MOVSxL x
      MO_SS_Conv W8  W16 -> integerExtend W8  W16 MOVSxL x

      -- We don't care about the upper bits for MO_XX_Conv, so MOV is enough. However, on 32-bit we
      -- have 8-bit registers only for a few registers (as opposed to x86-64 where every register
      -- has 8-bit version). So for 32-bit code, we'll just zero-extend.
      MO_XX_Conv W8  W32
          | is32Bit   -> integerExtend W8 W32 MOVZxL x
          | otherwise -> integerExtend W8 W32 MOV x
      MO_XX_Conv W8  W16
          | is32Bit   -> integerExtend W8 W16 MOVZxL x
          | otherwise -> integerExtend W8 W16 MOV x
      MO_XX_Conv W16 W32 -> integerExtend W16 W32 MOV x

      MO_UU_Conv W8  W64 | not is32Bit -> integerExtend W8  W64 MOVZxL x
      MO_UU_Conv W16 W64 | not is32Bit -> integerExtend W16 W64 MOVZxL x
      MO_UU_Conv W32 W64 | not is32Bit -> integerExtend W32 W64 MOVZxL x
      MO_SS_Conv W8  W64 | not is32Bit -> integerExtend W8  W64 MOVSxL x
      MO_SS_Conv W16 W64 | not is32Bit -> integerExtend W16 W64 MOVSxL x
      MO_SS_Conv W32 W64 | not is32Bit -> integerExtend W32 W64 MOVSxL x
      -- For 32-to-64 bit zero extension, amd64 uses an ordinary movl.
      -- However, we don't want the register allocator to throw it
      -- away as an unnecessary reg-to-reg move, so we keep it in
      -- the form of a movzl and print it as a movl later.
      -- This doesn't apply to MO_XX_Conv since in this case we don't care about
      -- the upper bits. So we can just use MOV.
      MO_XX_Conv W8  W64 | not is32Bit -> integerExtend W8  W64 MOV x
      MO_XX_Conv W16 W64 | not is32Bit -> integerExtend W16 W64 MOV x
      MO_XX_Conv W32 W64 | not is32Bit -> integerExtend W32 W64 MOV x

      MO_FF_Conv W32 W64 -> coerceFP2FP W64 x


      MO_FF_Conv W64 W32 -> coerceFP2FP W32 x

      MO_FS_Truncate from to -> coerceFP2Int from to x
      MO_SF_Round    from to -> coerceInt2FP from to x

      MO_V_Insert {}   -> needLlvm
      MO_V_Extract {}  -> needLlvm
      MO_V_Add {}      -> needLlvm
      MO_V_Sub {}      -> needLlvm
      MO_V_Mul {}      -> needLlvm
      MO_VS_Quot {}    -> needLlvm
      MO_VS_Rem {}     -> needLlvm
      MO_VS_Neg {}     -> needLlvm
      MO_VU_Quot {}    -> needLlvm
      MO_VU_Rem {}     -> needLlvm
      MO_VF_Insert {}  -> needLlvm
      MO_VF_Extract {} -> needLlvm
      MO_VF_Add {}     -> needLlvm
      MO_VF_Sub {}     -> needLlvm
      MO_VF_Mul {}     -> needLlvm
      MO_VF_Quot {}    -> needLlvm
      MO_VF_Neg {}     -> needLlvm

      _other -> pprPanic "getRegister" (pprMachOp mop)
   where
        triv_ucode :: (Format -> Operand -> Instr) -> Format -> NatM Register
        triv_ucode instr format = trivialUCode format (instr format) x

        -- signed or unsigned extension.
        integerExtend :: Width -> Width
                      -> (Format -> Operand -> Operand -> Instr)
                      -> CmmExpr -> NatM Register
        integerExtend from to instr expr = do
            (reg,e_code) <- if from == W8 then getByteReg expr
                                          else getSomeReg expr
            let
                code dst =
                  e_code `snocOL`
                  instr (intFormat from) (OpReg reg) (OpReg dst)
            return (Any (intFormat to) code)

        bitcast :: Format -> Format -> CmmExpr -> NatM Register
        bitcast fmt rfmt expr =
          do (src, e_code) <- getSomeReg expr
             let code = \dst -> e_code `snocOL` (MOVD fmt (OpReg src) (OpReg dst))
             return (Any rfmt code)

        toI8Reg :: Width -> CmmExpr -> NatM Register
        toI8Reg new_rep expr
            = do codefn <- getAnyReg expr
                 return (Any (intFormat new_rep) codefn)
                -- HACK: use getAnyReg to get a byte-addressable register.
                -- If the source was a Fixed register, this will add the
                -- mov instruction to put it into the desired destination.
                -- We're assuming that the destination won't be a fixed
                -- non-byte-addressable register; it won't be, because all
                -- fixed registers are word-sized.

        toI16Reg = toI8Reg -- for now

        conversionNop :: Format -> CmmExpr -> NatM Register
        conversionNop new_format expr
            = do e_code <- getRegister' platform is32Bit expr
                 return (swizzleRegisterRep e_code new_format)


getRegister' _ is32Bit (CmmMachOp mop [x, y]) = -- dyadic MachOps
  case mop of
      MO_F_Eq _ -> condFltReg is32Bit EQQ x y
      MO_F_Ne _ -> condFltReg is32Bit NE  x y
      MO_F_Gt _ -> condFltReg is32Bit GTT x y
      MO_F_Ge _ -> condFltReg is32Bit GE  x y
      -- Invert comparison condition and swap operands
      -- See Note [SSE Parity Checks]
      MO_F_Lt _ -> condFltReg is32Bit GTT  y x
      MO_F_Le _ -> condFltReg is32Bit GE   y x

      MO_Eq _   -> condIntReg EQQ x y
      MO_Ne _   -> condIntReg NE  x y

      MO_S_Gt _ -> condIntReg GTT x y
      MO_S_Ge _ -> condIntReg GE  x y
      MO_S_Lt _ -> condIntReg LTT x y
      MO_S_Le _ -> condIntReg LE  x y

      MO_U_Gt _ -> condIntReg GU  x y
      MO_U_Ge _ -> condIntReg GEU x y
      MO_U_Lt _ -> condIntReg LU  x y
      MO_U_Le _ -> condIntReg LEU x y

      MO_F_Add  w -> trivialFCode_sse2 w ADD  x y
      MO_F_Sub  w -> trivialFCode_sse2 w SUB  x y
      MO_F_Quot w -> trivialFCode_sse2 w FDIV x y
      MO_F_Mul  w -> trivialFCode_sse2 w MUL  x y

      MO_Add rep -> add_code rep x y
      MO_Sub rep -> sub_code rep x y

      MO_S_Quot rep -> div_code rep True  True  x y
      MO_S_Rem  rep -> div_code rep True  False x y
      MO_U_Quot rep -> div_code rep False True  x y
      MO_U_Rem  rep -> div_code rep False False x y

      MO_S_MulMayOflo rep -> imulMayOflo rep x y

      MO_Mul W8  -> imulW8 x y
      MO_Mul rep -> triv_op rep IMUL
      MO_And rep -> triv_op rep AND
      MO_Or  rep -> triv_op rep OR
      MO_Xor rep -> triv_op rep XOR

        {- Shift ops on x86s have constraints on their source, it
           either has to be Imm, CL or 1
            => trivialCode is not restrictive enough (sigh.)
        -}
      MO_Shl rep   -> shift_code rep SHL x y {-False-}
      MO_U_Shr rep -> shift_code rep SHR x y {-False-}
      MO_S_Shr rep -> shift_code rep SAR x y {-False-}

      MO_V_Insert {}   -> needLlvm
      MO_V_Extract {}  -> needLlvm
      MO_V_Add {}      -> needLlvm
      MO_V_Sub {}      -> needLlvm
      MO_V_Mul {}      -> needLlvm
      MO_VS_Quot {}    -> needLlvm
      MO_VS_Rem {}     -> needLlvm
      MO_VS_Neg {}     -> needLlvm
      MO_VF_Insert {}  -> needLlvm
      MO_VF_Extract {} -> needLlvm
      MO_VF_Add {}     -> needLlvm
      MO_VF_Sub {}     -> needLlvm
      MO_VF_Mul {}     -> needLlvm
      MO_VF_Quot {}    -> needLlvm
      MO_VF_Neg {}     -> needLlvm

      _other -> pprPanic "getRegister(x86) - binary CmmMachOp (1)" (pprMachOp mop)
  where
    --------------------
    triv_op width instr = trivialCode width op (Just op) x y
                        where op   = instr (intFormat width)

    -- Special case for IMUL for bytes, since the result of IMULB will be in
    -- %ax, the split to %dx/%edx/%rdx and %ax/%eax/%rax happens only for wider
    -- values.
    imulW8 :: CmmExpr -> CmmExpr -> NatM Register
    imulW8 arg_a arg_b = do
        (a_reg, a_code) <- getNonClobberedReg arg_a
        b_code <- getAnyReg arg_b

        let code = a_code `appOL` b_code eax `appOL`
                   toOL [ IMUL2 format (OpReg a_reg) ]
            format = intFormat W8

        return (Fixed format eax code)

    imulMayOflo :: Width -> CmmExpr -> CmmExpr -> NatM Register
    imulMayOflo W8 a b = do
         -- The general case (W16, W32, W64) doesn't work for W8 as its
         -- multiplication doesn't use two registers.
         --
         -- The plan is:
         -- 1. truncate and sign-extend a and b to 8bit width
         -- 2. multiply a' = a * b in 32bit width
         -- 3. copy and sign-extend 8bit from a' to c
         -- 4. compare a' and c: they are equal if there was no overflow
         (a_reg, a_code) <- getNonClobberedReg a
         (b_reg, b_code) <- getNonClobberedReg b
         let
             code = a_code `appOL` b_code `appOL`
                        toOL [
                           MOVSxL II8 (OpReg a_reg) (OpReg a_reg),
                           MOVSxL II8 (OpReg b_reg) (OpReg b_reg),
                           IMUL II32 (OpReg b_reg) (OpReg a_reg),
                           MOVSxL II8 (OpReg a_reg) (OpReg eax),
                           CMP II16 (OpReg a_reg) (OpReg eax),
                           SETCC NE (OpReg eax)
                        ]
         return (Fixed II8 eax code)
    imulMayOflo rep a b = do
         (a_reg, a_code) <- getNonClobberedReg a
         b_code <- getAnyReg b
         let
             shift_amt  = case rep of
                           W16 -> 15
                           W32 -> 31
                           W64 -> 63
                           w -> panic ("shift_amt: " ++ show w)

             format = intFormat rep
             code = a_code `appOL` b_code eax `appOL`
                        toOL [
                           IMUL2 format (OpReg a_reg),   -- result in %edx:%eax
                           SAR format (OpImm (ImmInt shift_amt)) (OpReg eax),
                                -- sign extend lower part
                           SUB format (OpReg edx) (OpReg eax)
                                -- compare against upper
                           -- eax==0 if high part == sign extended low part
                        ]
         return (Fixed format eax code)

    --------------------
    shift_code :: Width
               -> (Format -> Operand -> Operand -> Instr)
               -> CmmExpr
               -> CmmExpr
               -> NatM Register

    {- Case1: shift length as immediate -}
    shift_code width instr x (CmmLit lit)
      -- Handle the case of a shift larger than the width of the shifted value.
      -- This is necessary since x86 applies a mask of 0x1f to the shift
      -- amount, meaning that, e.g., `shr 47, $eax` will actually shift by
      -- `47 & 0x1f == 15`. See #20626.
      | CmmInt n _ <- lit
      , n >= fromIntegral (widthInBits width)
      = getRegister $ CmmLit $ CmmInt 0 width

      | otherwise = do
          x_code <- getAnyReg x
          let
               format = intFormat width
               code dst
                  = x_code dst `snocOL`
                    instr format (OpImm (litToImm lit)) (OpReg dst)
          return (Any format code)

    {- Case2: shift length is complex (non-immediate)
      * y must go in %ecx.
      * we cannot do y first *and* put its result in %ecx, because
        %ecx might be clobbered by x.
      * if we do y second, then x cannot be
        in a clobbered reg.  Also, we cannot clobber x's reg
        with the instruction itself.
      * so we can either:
        - do y first, put its result in a fresh tmp, then copy it to %ecx later
        - do y second and put its result into %ecx.  x gets placed in a fresh
          tmp.  This is likely to be better, because the reg alloc can
          eliminate this reg->reg move here (it won't eliminate the other one,
          because the move is into the fixed %ecx).
      * in the case of C calls the use of ecx here can interfere with arguments.
        We avoid this with the hack described in Note [Evaluate C-call
        arguments before placing in destination registers]
    -}
    shift_code width instr x y{-amount-} = do
        x_code <- getAnyReg x
        let format = intFormat width
        tmp <- getNewRegNat format
        y_code <- getAnyReg y
        let
           code = x_code tmp `appOL`
                  y_code ecx `snocOL`
                  instr format (OpReg ecx) (OpReg tmp)
        return (Fixed format tmp code)

    --------------------
    add_code :: Width -> CmmExpr -> CmmExpr -> NatM Register
    add_code rep x (CmmLit (CmmInt y _))
        | is32BitInteger y
        , rep /= W8 -- LEA doesn't support byte size (#18614)
        = add_int rep x y
    add_code rep x y = trivialCode rep (ADD format) (Just (ADD format)) x y
      where format = intFormat rep
    -- TODO: There are other interesting patterns we want to replace
    --     with a LEA, e.g. `(x + offset) + (y << shift)`.

    --------------------
    sub_code :: Width -> CmmExpr -> CmmExpr -> NatM Register
    sub_code rep x (CmmLit (CmmInt y _))
        | is32BitInteger (-y)
        , rep /= W8 -- LEA doesn't support byte size (#18614)
        = add_int rep x (-y)
    sub_code rep x y = trivialCode rep (SUB (intFormat rep)) Nothing x y

    -- our three-operand add instruction:
    add_int width x y = do
        (x_reg, x_code) <- getSomeReg x
        let
            format = intFormat width
            imm = ImmInt (fromInteger y)
            code dst
               = x_code `snocOL`
                 LEA format
                        (OpAddr (AddrBaseIndex (EABaseReg x_reg) EAIndexNone imm))
                        (OpReg dst)
        --
        return (Any format code)

    ----------------------

    -- See Note [DIV/IDIV for bytes]
    div_code W8 signed quotient x y = do
        let widen | signed    = MO_SS_Conv W8 W16
                  | otherwise = MO_UU_Conv W8 W16
        div_code
            W16
            signed
            quotient
            (CmmMachOp widen [x])
            (CmmMachOp widen [y])

    div_code width signed quotient x y = do
           (y_op, y_code) <- getRegOrMem y -- cannot be clobbered
           x_code <- getAnyReg x
           let
             format = intFormat width
             widen | signed    = CLTD format
                   | otherwise = XOR format (OpReg edx) (OpReg edx)

             instr | signed    = IDIV
                   | otherwise = DIV

             code = y_code `appOL`
                    x_code eax `appOL`
                    toOL [widen, instr format y_op]

             result | quotient  = eax
                    | otherwise = edx

           return (Fixed format result code)

getRegister' _plat _is32Bit (CmmMachOp mop [x, y, z]) = -- ternary MachOps
  case mop of
      -- Floating point fused multiply-add operations @ ± x*y ± z@
      MO_FMA var w -> genFMA3Code w var x y z

      _other -> pprPanic "getRegister(x86) - ternary CmmMachOp (1)"
                  (pprMachOp mop)

getRegister' _ _ (CmmLoad mem pk _)
  | isFloatType pk
  = do
    Amode addr mem_code <- getAmode mem
    loadFloatAmode  (typeWidth pk) addr mem_code

getRegister' _ is32Bit (CmmLoad mem pk _)
  | is32Bit && not (isWord64 pk)
  = do
    code <- intLoadCode instr mem
    return (Any format code)
  where
    width = typeWidth pk
    format = intFormat width
    instr = case width of
                W8     -> MOVZxL II8
                _other -> MOV format
        -- We always zero-extend 8-bit loads, if we
        -- can't think of anything better.  This is because
        -- we can't guarantee access to an 8-bit variant of every register
        -- (esi and edi don't have 8-bit variants), so to make things
        -- simpler we do our 8-bit arithmetic with full 32-bit registers.

-- Simpler memory load code on x86_64
getRegister' _ is32Bit (CmmLoad mem pk _)
 | not is32Bit
  = do
    code <- intLoadCode (MOV format) mem
    return (Any format code)
  where format = intFormat $ typeWidth pk

getRegister' _ is32Bit (CmmLit (CmmInt 0 width))
  = let
        format = intFormat width

        -- x86_64: 32-bit xor is one byte shorter, and zero-extends to 64 bits
        format1 = if is32Bit then format
                           else case format of
                                II64 -> II32
                                _ -> format
        code dst
           = unitOL (XOR format1 (OpReg dst) (OpReg dst))
    in
        return (Any format code)

-- Handle symbol references with LEA and %rip-relative addressing.
-- See Note [%rip-relative addressing on x86-64].
getRegister' platform is32Bit (CmmLit lit)
  | is_label lit
  , not is32Bit
  = do let format = cmmTypeFormat (cmmLitType platform lit)
           imm = litToImm lit
           op = OpAddr (AddrBaseIndex EABaseRip EAIndexNone imm)
           code dst = unitOL (LEA format op (OpReg dst))
       return (Any format code)
  where
    is_label (CmmLabel {})        = True
    is_label (CmmLabelOff {})     = True
    is_label (CmmLabelDiffOff {}) = True
    is_label _                    = False

  -- optimisation for loading small literals on x86_64: take advantage
  -- of the automatic zero-extension from 32 to 64 bits, because the 32-bit
  -- instruction forms are shorter.
getRegister' platform is32Bit (CmmLit lit)
  | not is32Bit, isWord64 (cmmLitType platform lit), not (isBigLit lit)
  = let
        imm = litToImm lit
        code dst = unitOL (MOV II32 (OpImm imm) (OpReg dst))
    in
        return (Any II64 code)
  where
   isBigLit (CmmInt i _) = i < 0 || i > 0xffffffff
   isBigLit _ = False
        -- note1: not the same as (not.is32BitLit), because that checks for
        -- signed literals that fit in 32 bits, but we want unsigned
        -- literals here.
        -- note2: all labels are small, because we're assuming the
        -- small memory model. See Note [%rip-relative addressing on x86-64].

getRegister' platform _ (CmmLit lit)
  = do let format = cmmTypeFormat (cmmLitType platform lit)
           imm = litToImm lit
           code dst = unitOL (MOV format (OpImm imm) (OpReg dst))
       return (Any format code)

getRegister' platform _ other
    | isVecExpr other  = needLlvm
    | otherwise        = pprPanic "getRegister(x86)" (pdoc platform other)


intLoadCode :: (Operand -> Operand -> Instr) -> CmmExpr
   -> NatM (Reg -> InstrBlock)
intLoadCode instr mem = do
  Amode src mem_code <- getAmode mem
  return (\dst -> mem_code `snocOL` instr (OpAddr src) (OpReg dst))

-- Compute an expression into *any* register, adding the appropriate
-- move instruction if necessary.
getAnyReg :: CmmExpr -> NatM (Reg -> InstrBlock)
getAnyReg expr = do
  r <- getRegister expr
  anyReg r

anyReg :: Register -> NatM (Reg -> InstrBlock)
anyReg (Any _ code)          = return code
anyReg (Fixed rep reg fcode) = return (\dst -> fcode `snocOL` reg2reg rep reg dst)

-- A bit like getSomeReg, but we want a reg that can be byte-addressed.
-- Fixed registers might not be byte-addressable, so we make sure we've
-- got a temporary, inserting an extra reg copy if necessary.
getByteReg :: CmmExpr -> NatM (Reg, InstrBlock)
getByteReg expr = do
  is32Bit <- is32BitPlatform
  if is32Bit
      then do r <- getRegister expr
              case r of
                Any rep code -> do
                    tmp <- getNewRegNat rep
                    return (tmp, code tmp)
                Fixed rep reg code
                    | isVirtualReg reg -> return (reg,code)
                    | otherwise -> do
                        tmp <- getNewRegNat rep
                        return (tmp, code `snocOL` reg2reg rep reg tmp)
                    -- ToDo: could optimise slightly by checking for
                    -- byte-addressable real registers, but that will
                    -- happen very rarely if at all.
      else getSomeReg expr -- all regs are byte-addressable on x86_64

-- Another variant: this time we want the result in a register that cannot
-- be modified by code to evaluate an arbitrary expression.
getNonClobberedReg :: CmmExpr -> NatM (Reg, InstrBlock)
getNonClobberedReg expr = do
  r <- getRegister expr
  platform <- ncgPlatform <$> getConfig
  case r of
    Any rep code -> do
        tmp <- getNewRegNat rep
        return (tmp, code tmp)
    Fixed rep reg code
        -- only certain regs can be clobbered
        | reg `elem` instrClobberedRegs platform
        -> do
                tmp <- getNewRegNat rep
                return (tmp, code `snocOL` reg2reg rep reg tmp)
        | otherwise ->
                return (reg, code)

reg2reg :: Format -> Reg -> Reg -> Instr
reg2reg format src dst = MOV format (OpReg src) (OpReg dst)


--------------------------------------------------------------------------------

-- | Convert a 'CmmExpr' representing a memory address into an 'Amode'.
--
-- An 'Amode' is a datatype representing a valid address form for the target
-- (e.g. "Base + Index + disp" or immediate) and the code to compute it.
getAmode :: CmmExpr -> NatM Amode
getAmode e = do
   platform <- getPlatform
   let is32Bit = target32Bit platform

   case e of
      CmmRegOff r n
         -> getAmode $ mangleIndexTree r n

      CmmMachOp (MO_Add W64) [CmmReg (CmmGlobal (GlobalRegUse PicBaseReg _)), CmmLit displacement]
         | not is32Bit
         -> return $ Amode (ripRel (litToImm displacement)) nilOL

      -- This is all just ridiculous, since it carefully undoes
      -- what mangleIndexTree has just done.
      CmmMachOp (MO_Sub _rep) [x, CmmLit lit@(CmmInt i _)]
         | is32BitLit platform lit
         -- assert (rep == II32)???
         -> do
            (x_reg, x_code) <- getSomeReg x
            let off = ImmInt (-(fromInteger i))
            return (Amode (AddrBaseIndex (EABaseReg x_reg) EAIndexNone off) x_code)

      CmmMachOp (MO_Add _rep) [x, CmmLit lit]
         | is32BitLit platform lit
         -- assert (rep == II32)???
         -> do
            (x_reg, x_code) <- getSomeReg x
            let off = litToImm lit
            return (Amode (AddrBaseIndex (EABaseReg x_reg) EAIndexNone off) x_code)

      -- Turn (lit1 << n  + lit2) into  (lit2 + lit1 << n) so it will be
      -- recognised by the next rule.
      CmmMachOp (MO_Add rep) [a@(CmmMachOp (MO_Shl _) _), b@(CmmLit _)]
         -> getAmode (CmmMachOp (MO_Add rep) [b,a])

      -- Matches: (x + offset) + (y << shift)
      CmmMachOp (MO_Add _) [CmmRegOff x offset, CmmMachOp (MO_Shl _) [y, CmmLit (CmmInt shift _)]]
         | shift == 0 || shift == 1 || shift == 2 || shift == 3
         -> x86_complex_amode (CmmReg x) y shift (fromIntegral offset)

      CmmMachOp (MO_Add _) [x, CmmMachOp (MO_Shl _) [y, CmmLit (CmmInt shift _)]]
         | shift == 0 || shift == 1 || shift == 2 || shift == 3
         -> x86_complex_amode x y shift 0

      CmmMachOp (MO_Add _) [x, CmmMachOp (MO_Add _) [CmmMachOp (MO_Shl _)
                                                    [y, CmmLit (CmmInt shift _)], CmmLit (CmmInt offset _)]]
         | shift == 0 || shift == 1 || shift == 2 || shift == 3
         && is32BitInteger offset
         -> x86_complex_amode x y shift offset

      CmmMachOp (MO_Add _) [x,y]
         | not (isLit y) -- we already handle valid literals above.
         -> x86_complex_amode x y 0 0

      -- Handle labels with %rip-relative addressing since in general the image
      -- may be loaded anywhere in the 64-bit address space (e.g. on Windows
      -- with high-entropy ASLR). See Note [%rip-relative addressing on x86-64].
      CmmLit lit
         | not is32Bit
         , is_label lit
         -> return (Amode (AddrBaseIndex EABaseRip EAIndexNone (litToImm lit)) nilOL)

      CmmLit lit
         | is32BitLit platform lit
         -> return (Amode (ImmAddr (litToImm lit) 0) nilOL)

      -- Literal with offsets too big (> 32 bits) fails during the linking phase
      -- (#15570). We already handled valid literals above so we don't have to
      -- test anything here.
      CmmLit (CmmLabelOff l off)
         -> getAmode (CmmMachOp (MO_Add W64) [ CmmLit (CmmLabel l)
                                             , CmmLit (CmmInt (fromIntegral off) W64)
                                             ])
      CmmLit (CmmLabelDiffOff l1 l2 off w)
         -> getAmode (CmmMachOp (MO_Add W64) [ CmmLit (CmmLabelDiffOff l1 l2 0 w)
                                             , CmmLit (CmmInt (fromIntegral off) W64)
                                             ])

      -- in case we can't do something better, we just compute the expression
      -- and put the result in a register
      _ -> do
        (reg,code) <- getSomeReg e
        return (Amode (AddrBaseIndex (EABaseReg reg) EAIndexNone (ImmInt 0)) code)
  where
    is_label (CmmLabel{}) = True
    is_label (CmmLabelOff{}) = True
    is_label (CmmLabelDiffOff{}) = True
    is_label _ = False


-- | Like 'getAmode', but on 32-bit use simple register addressing
-- (i.e. no index register). This stops us from running out of
-- registers on x86 when using instructions such as cmpxchg, which can
-- use up to three virtual registers and one fixed register.
getSimpleAmode :: CmmExpr -> NatM Amode
getSimpleAmode addr = is32BitPlatform >>= \case
  False -> getAmode addr
  True  -> do
    addr_code <- getAnyReg addr
    config <- getConfig
    addr_r <- getNewRegNat (intFormat (ncgWordWidth config))
    let amode = AddrBaseIndex (EABaseReg addr_r) EAIndexNone (ImmInt 0)
    return $! Amode amode (addr_code addr_r)

x86_complex_amode :: CmmExpr -> CmmExpr -> Integer -> Integer -> NatM Amode
x86_complex_amode base index shift offset
  = do (x_reg, x_code) <- getNonClobberedReg base
        -- x must be in a temp, because it has to stay live over y_code
        -- we could compare x_reg and y_reg and do something better here...
       (y_reg, y_code) <- getSomeReg index
       let
           code = x_code `appOL` y_code
           base = case shift of 0 -> 1; 1 -> 2; 2 -> 4; 3 -> 8;
                                n -> panic $ "x86_complex_amode: unhandled shift! (" ++ show n ++ ")"
       return (Amode (AddrBaseIndex (EABaseReg x_reg) (EAIndex y_reg base) (ImmInt (fromIntegral offset)))
               code)




-- -----------------------------------------------------------------------------
-- getOperand: sometimes any operand will do.

-- getNonClobberedOperand: the value of the operand will remain valid across
-- the computation of an arbitrary expression, unless the expression
-- is computed directly into a register which the operand refers to
-- (see trivialCode where this function is used for an example).

getNonClobberedOperand :: CmmExpr -> NatM (Operand, InstrBlock)
getNonClobberedOperand (CmmLit lit) =
  if isSuitableFloatingPointLit lit
  then do
    let CmmFloat _ w = lit
    Amode addr code <- memConstant (mkAlignment $ widthInBytes w) lit
    return (OpAddr addr, code)
  else do
    platform <- getPlatform
    if is32BitLit platform lit && not (isFloatType (cmmLitType platform lit))
    then return (OpImm (litToImm lit), nilOL)
    else getNonClobberedOperand_generic (CmmLit lit)

getNonClobberedOperand (CmmLoad mem pk _) = do
  is32Bit <- is32BitPlatform
  -- this logic could be simplified
  -- TODO FIXME
  if   (if is32Bit then not (isWord64 pk) else True)
      -- if 32bit and pk is at float/double/simd value
      -- or if 64bit
      --  this could use some eyeballs or i'll need to stare at it more later
    then do
      platform <- ncgPlatform <$> getConfig
      Amode src mem_code <- getAmode mem
      (src',save_code) <-
        if (amodeCouldBeClobbered platform src)
                then do
                   tmp <- getNewRegNat (archWordFormat is32Bit)
                   return (AddrBaseIndex (EABaseReg tmp) EAIndexNone (ImmInt 0),
                           unitOL (LEA (archWordFormat is32Bit)
                                       (OpAddr src)
                                       (OpReg tmp)))
                else
                   return (src, nilOL)
      return (OpAddr src', mem_code `appOL` save_code)
    else
      -- if its a word or gcptr on 32bit?
      getNonClobberedOperand_generic (CmmLoad mem pk NaturallyAligned)

getNonClobberedOperand e = getNonClobberedOperand_generic e

getNonClobberedOperand_generic :: CmmExpr -> NatM (Operand, InstrBlock)
getNonClobberedOperand_generic e = do
  (reg, code) <- getNonClobberedReg e
  return (OpReg reg, code)

amodeCouldBeClobbered :: Platform -> AddrMode -> Bool
amodeCouldBeClobbered platform amode = any (regClobbered platform) (addrModeRegs amode)

regClobbered :: Platform -> Reg -> Bool
regClobbered platform (RegReal (RealRegSingle rr)) = freeReg platform rr
regClobbered _ _ = False

-- getOperand: the operand is not required to remain valid across the
-- computation of an arbitrary expression.
getOperand :: CmmExpr -> NatM (Operand, InstrBlock)

getOperand (CmmLit lit) = do
  use_sse2 <- sse2Enabled
  if (use_sse2 && isSuitableFloatingPointLit lit)
    then do
      let CmmFloat _ w = lit
      Amode addr code <- memConstant (mkAlignment $ widthInBytes w) lit
      return (OpAddr addr, code)
    else do

  platform <- getPlatform
  if is32BitLit platform lit && not (isFloatType (cmmLitType platform lit))
    then return (OpImm (litToImm lit), nilOL)
    else getOperand_generic (CmmLit lit)

getOperand (CmmLoad mem pk _) = do
  is32Bit <- is32BitPlatform
  use_sse2 <- sse2Enabled
  if (not (isFloatType pk) || use_sse2) && (if is32Bit then not (isWord64 pk) else True)
     then do
       Amode src mem_code <- getAmode mem
       return (OpAddr src, mem_code)
     else
       getOperand_generic (CmmLoad mem pk NaturallyAligned)

getOperand e = getOperand_generic e

getOperand_generic :: CmmExpr -> NatM (Operand, InstrBlock)
getOperand_generic e = do
    (reg, code) <- getSomeReg e
    return (OpReg reg, code)

isOperand :: Platform -> CmmExpr -> Bool
isOperand _ (CmmLoad _ _ _) = True
isOperand platform (CmmLit lit)
                          = is32BitLit platform lit
                          || isSuitableFloatingPointLit lit
isOperand _ _            = False

-- | Given a 'Register', produce a new 'Register' with an instruction block
-- which will check the value for alignment. Used for @-falignment-sanitisation@.
addAlignmentCheck :: Int -> Register -> Register
addAlignmentCheck align reg =
    case reg of
      Fixed fmt reg code -> Fixed fmt reg (code `appOL` check fmt reg)
      Any fmt f          -> Any fmt (\reg -> f reg `appOL` check fmt reg)
  where
    check :: Format -> Reg -> InstrBlock
    check fmt reg =
        assert (not $ isFloatFormat fmt) $
        toOL [ TEST fmt (OpImm $ ImmInt $ align-1) (OpReg reg)
             , JXX_GBL NE $ ImmCLbl mkBadAlignmentLabel
             ]

memConstant :: Alignment -> CmmLit -> NatM Amode
memConstant align lit = do
  lbl <- getNewLabelNat
  let rosection = Section ReadOnlyData lbl
  config <- getConfig
  platform <- getPlatform
  (addr, addr_code) <- if target32Bit platform
                       then do dynRef <- cmmMakeDynamicReference
                                             config
                                             DataReference
                                             lbl
                               Amode addr addr_code <- getAmode dynRef
                               return (addr, addr_code)
                       else return (ripRel (ImmCLbl lbl), nilOL)
  let code =
        LDATA rosection (align, CmmStaticsRaw lbl [CmmStaticLit lit])
        `consOL` addr_code
  return (Amode addr code)


loadFloatAmode :: Width -> AddrMode -> InstrBlock -> NatM Register
loadFloatAmode w addr addr_code = do
  let format = floatFormat w
      code dst = addr_code `snocOL`
                    MOV format (OpAddr addr) (OpReg dst)

  return (Any format code)


-- if we want a floating-point literal as an operand, we can
-- use it directly from memory.  However, if the literal is
-- zero, we're better off generating it into a register using
-- xor.
isSuitableFloatingPointLit :: CmmLit -> Bool
isSuitableFloatingPointLit (CmmFloat f _) = f /= 0.0
isSuitableFloatingPointLit _ = False

getRegOrMem :: CmmExpr -> NatM (Operand, InstrBlock)
getRegOrMem e@(CmmLoad mem pk _) = do
  is32Bit <- is32BitPlatform
  use_sse2 <- sse2Enabled
  if (not (isFloatType pk) || use_sse2) && (if is32Bit then not (isWord64 pk) else True)
     then do
       Amode src mem_code <- getAmode mem
       return (OpAddr src, mem_code)
     else do
       (reg, code) <- getNonClobberedReg e
       return (OpReg reg, code)
getRegOrMem e = do
    (reg, code) <- getNonClobberedReg e
    return (OpReg reg, code)

is32BitLit :: Platform -> CmmLit -> Bool
is32BitLit platform _lit
   | target32Bit platform = True
is32BitLit platform lit =
   case lit of
      CmmInt i W64              -> is32BitInteger i
      -- Except on Windows, assume that labels are in the range 0-2^31-1: this
      -- assumes the small memory model. Note [%rip-relative addressing on
      -- x86-64].
      CmmLabel _                -> low_image
      -- however we can't assume that label offsets are in this range
      -- (see #15570)
      CmmLabelOff _ off         -> low_image && is32BitInteger (fromIntegral off)
      CmmLabelDiffOff _ _ off _ -> low_image && is32BitInteger (fromIntegral off)
      _                         -> True
  where
    -- Is the executable image certain to be located below 4GB? As noted in
    -- Note [%rip-relative addressing on x86-64], this is not true on Windows.
    low_image =
      case platformOS platform of
        OSMinGW32 -> False   -- See Note [%rip-relative addressing on x86-64]
        _         -> True


-- Set up a condition code for a conditional branch.

getCondCode :: CmmExpr -> NatM CondCode

-- yes, they really do seem to want exactly the same!

getCondCode (CmmMachOp mop [x, y])
  =
    case mop of
      MO_F_Eq W32 -> condFltCode EQQ x y
      MO_F_Ne W32 -> condFltCode NE  x y
      MO_F_Gt W32 -> condFltCode GTT x y
      MO_F_Ge W32 -> condFltCode GE  x y
      -- Invert comparison condition and swap operands
      -- See Note [SSE Parity Checks]
      MO_F_Lt W32 -> condFltCode GTT  y x
      MO_F_Le W32 -> condFltCode GE   y x

      MO_F_Eq W64 -> condFltCode EQQ x y
      MO_F_Ne W64 -> condFltCode NE  x y
      MO_F_Gt W64 -> condFltCode GTT x y
      MO_F_Ge W64 -> condFltCode GE  x y
      MO_F_Lt W64 -> condFltCode GTT y x
      MO_F_Le W64 -> condFltCode GE  y x

      _ -> condIntCode (machOpToCond mop) x y

getCondCode other = do
   platform <- getPlatform
   pprPanic "getCondCode(2)(x86,x86_64)" (pdoc platform other)

machOpToCond :: MachOp -> Cond
machOpToCond mo = case mo of
  MO_Eq _   -> EQQ
  MO_Ne _   -> NE
  MO_S_Gt _ -> GTT
  MO_S_Ge _ -> GE
  MO_S_Lt _ -> LTT
  MO_S_Le _ -> LE
  MO_U_Gt _ -> GU
  MO_U_Ge _ -> GEU
  MO_U_Lt _ -> LU
  MO_U_Le _ -> LEU
  _other -> pprPanic "machOpToCond" (pprMachOp mo)

{-  Note [64-bit integer comparisons on 32-bit]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    When doing these comparisons there are 2 kinds of
    comparisons.

    * Comparison for equality (or lack thereof)

    We use xor to check if high/low bits are
    equal. Then combine the results using or.

    * Other comparisons:

    We first compare the low registers
    and use a subtraction with borrow to compare the high registers.

    For signed numbers the condition is determined by
    the sign and overflow flags agreeing or not
    and for unsigned numbers the condition is the carry flag.

-}

-- @cond(Int|Flt)Code@: Turn a boolean expression into a condition, to be
-- passed back up the tree.

condIntCode :: Cond -> CmmExpr -> CmmExpr -> NatM CondCode
condIntCode cond x y = do platform <- getPlatform
                          condIntCode' platform cond x y

condIntCode' :: Platform -> Cond -> CmmExpr -> CmmExpr -> NatM CondCode

-- 64-bit integer comparisons on 32-bit
-- See Note [64-bit integer comparisons on 32-bit]
condIntCode' platform cond x y
  | target32Bit platform && isWord64 (cmmExprType platform x) = do

  RegCode64 code1 r1hi r1lo <- iselExpr64 x
  RegCode64 code2 r2hi r2lo <- iselExpr64 y

  -- we mustn't clobber r1/r2 so we use temporaries
  tmp1 <- getNewRegNat II32
  tmp2 <- getNewRegNat II32

  let (cond', cmpCode) = intComparison cond r1hi r1lo r2hi r2lo tmp1 tmp2
  return $ CondCode False cond' (code1 `appOL` code2 `appOL` cmpCode)

  where
    intComparison cond r1_hi r1_lo r2_hi r2_lo tmp1 tmp2 =
      case cond of
        -- These don't occur as argument of condIntCode'
        ALWAYS  -> panic "impossible"
        NEG     -> panic "impossible"
        POS     -> panic "impossible"
        CARRY   -> panic "impossible"
        OFLO    -> panic "impossible"
        PARITY  -> panic "impossible"
        NOTPARITY -> panic "impossible"
        -- Special case #1 x == y and x != y
        EQQ -> (EQQ, cmpExact)
        NE  -> (NE, cmpExact)
        -- [x >= y]
        GE  -> (GE, cmpGE)
        GEU -> (GEU, cmpGE)
        -- [x >  y]
        GTT -> (LTT, cmpLE)
        GU  -> (LU, cmpLE)
        -- [x <= y]
        LE  -> (GE, cmpLE)
        LEU -> (GEU, cmpLE)
        -- [x <  y]
        LTT -> (LTT, cmpGE)
        LU  -> (LU, cmpGE)
      where
        cmpExact :: OrdList Instr
        cmpExact =
          toOL
            [ MOV II32 (OpReg r1_hi) (OpReg tmp1)
            , MOV II32 (OpReg r1_lo) (OpReg tmp2)
            , XOR II32 (OpReg r2_hi) (OpReg tmp1)
            , XOR II32 (OpReg r2_lo) (OpReg tmp2)
            , OR  II32 (OpReg tmp1)  (OpReg tmp2)
            ]
        cmpGE = toOL
            [ MOV II32 (OpReg r1_hi) (OpReg tmp1)
            , CMP II32 (OpReg r2_lo) (OpReg r1_lo)
            , SBB II32 (OpReg r2_hi) (OpReg tmp1)
            ]
        cmpLE = toOL
            [ MOV II32 (OpReg r2_hi) (OpReg tmp1)
            , CMP II32 (OpReg r1_lo) (OpReg r2_lo)
            , SBB II32 (OpReg r1_hi) (OpReg tmp1)
            ]

-- memory vs immediate
condIntCode' platform cond (CmmLoad x pk _) (CmmLit lit)
 | is32BitLit platform lit = do
    Amode x_addr x_code <- getAmode x
    let
        imm  = litToImm lit
        code = x_code `snocOL`
                  CMP (cmmTypeFormat pk) (OpImm imm) (OpAddr x_addr)
    --
    return (CondCode False cond code)

-- anything vs zero, using a mask
-- TODO: Add some sanity checking!!!!
condIntCode' platform cond (CmmMachOp (MO_And _) [x,o2]) (CmmLit (CmmInt 0 pk))
    | (CmmLit lit@(CmmInt mask _)) <- o2, is32BitLit platform lit
    = do
      (x_reg, x_code) <- getSomeReg x
      let
         code = x_code `snocOL`
                TEST (intFormat pk) (OpImm (ImmInteger mask)) (OpReg x_reg)
      --
      return (CondCode False cond code)

-- anything vs zero
condIntCode' _ cond x (CmmLit (CmmInt 0 pk)) = do
    (x_reg, x_code) <- getSomeReg x
    let
        code = x_code `snocOL`
                  TEST (intFormat pk) (OpReg x_reg) (OpReg x_reg)
    --
    return (CondCode False cond code)

-- anything vs operand
condIntCode' platform cond x y
 | isOperand platform y = do
    (x_reg, x_code) <- getNonClobberedReg x
    (y_op,  y_code) <- getOperand y
    let
        code = x_code `appOL` y_code `snocOL`
                  CMP (cmmTypeFormat (cmmExprType platform x)) y_op (OpReg x_reg)
    return (CondCode False cond code)
-- operand vs. anything: invert the comparison so that we can use a
-- single comparison instruction.
 | isOperand platform x
 , Just revcond <- maybeFlipCond cond = do
    (y_reg, y_code) <- getNonClobberedReg y
    (x_op,  x_code) <- getOperand x
    let
        code = y_code `appOL` x_code `snocOL`
                  CMP (cmmTypeFormat (cmmExprType platform x)) x_op (OpReg y_reg)
    return (CondCode False revcond code)

-- anything vs anything
condIntCode' platform cond x y = do
  (y_reg, y_code) <- getNonClobberedReg y
  (x_op, x_code) <- getRegOrMem x
  let
        code = y_code `appOL`
               x_code `snocOL`
                  CMP (cmmTypeFormat (cmmExprType platform x)) (OpReg y_reg) x_op
  return (CondCode False cond code)



--------------------------------------------------------------------------------
condFltCode :: Cond -> CmmExpr -> CmmExpr -> NatM CondCode

condFltCode cond x y
  =  condFltCode_sse2
  where


  -- in the SSE2 comparison ops (ucomiss, ucomisd) the left arg may be
  -- an operand, but the right must be a reg.  We can probably do better
  -- than this general case...
  condFltCode_sse2 = do
    platform <- getPlatform
    (x_reg, x_code) <- getNonClobberedReg x
    (y_op, y_code) <- getOperand y
    let
        code = x_code `appOL`
               y_code `snocOL`
                  CMP (floatFormat $ cmmExprWidth platform x) y_op (OpReg x_reg)
        -- NB(1): we need to use the unsigned comparison operators on the
        -- result of this comparison.
    return (CondCode True (condToUnsigned cond) code)

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


-- integer assignment to memory

-- specific case of adding/subtracting an integer to a particular address.
-- ToDo: catch other cases where we can use an operation directly on a memory
-- address.
assignMem_IntCode pk addr (CmmMachOp op [CmmLoad addr2 _ _,
                                                 CmmLit (CmmInt i _)])
   | addr == addr2, pk /= II64 || is32BitInteger i,
     Just instr <- check op
   = do Amode amode code_addr <- getAmode addr
        let code = code_addr `snocOL`
                   instr pk (OpImm (ImmInt (fromIntegral i))) (OpAddr amode)
        return code
   where
        check (MO_Add _) = Just ADD
        check (MO_Sub _) = Just SUB
        check _ = Nothing
        -- ToDo: more?

-- general case
assignMem_IntCode pk addr src = do
    platform <- getPlatform
    Amode addr code_addr <- getAmode addr
    (code_src, op_src)   <- get_op_RI platform src
    let
        code = code_src `appOL`
               code_addr `snocOL`
                  MOV pk op_src (OpAddr addr)
        -- NOTE: op_src is stable, so it will still be valid
        -- after code_addr.  This may involve the introduction
        -- of an extra MOV to a temporary register, but we hope
        -- the register allocator will get rid of it.
    --
    return code
  where
    get_op_RI :: Platform -> CmmExpr -> NatM (InstrBlock,Operand)   -- code, operator
    get_op_RI platform (CmmLit lit) | is32BitLit platform lit
      = return (nilOL, OpImm (litToImm lit))
    get_op_RI _ op
      = do (reg,code) <- getNonClobberedReg op
           return (code, OpReg reg)


-- Assign; dst is a reg, rhs is mem
assignReg_IntCode pk reg (CmmLoad src _ _) = do
  load_code <- intLoadCode (MOV pk) src
  platform <- ncgPlatform <$> getConfig
  return (load_code (getRegisterReg platform reg))

-- dst is a reg, but src could be anything
assignReg_IntCode _ reg src = do
  platform <- ncgPlatform <$> getConfig
  code <- getAnyReg src
  return (code (getRegisterReg platform reg))


-- Floating point assignment to memory
assignMem_FltCode pk addr src = do
  (src_reg, src_code) <- getNonClobberedReg src
  Amode addr addr_code <- getAmode addr
  let
        code = src_code `appOL`
               addr_code `snocOL`
               MOV pk (OpReg src_reg) (OpAddr addr)

  return code

-- Floating point assignment to a register/temporary
assignReg_FltCode _ reg src = do
  src_code <- getAnyReg src
  platform <- ncgPlatform <$> getConfig
  return (src_code (getRegisterReg platform reg))


genJump :: CmmExpr{-the branch target-} -> [Reg] -> NatM InstrBlock

genJump (CmmLoad mem _ _) regs = do
  Amode target code <- getAmode mem
  return (code `snocOL` JMP (OpAddr target) regs)

genJump (CmmLit lit) regs =
  return (unitOL (JMP (OpImm (litToImm lit)) regs))

genJump expr regs = do
  (reg,code) <- getSomeReg expr
  return (code `snocOL` JMP (OpReg reg) regs)


-- -----------------------------------------------------------------------------
--  Unconditional branches

genBranch :: BlockId -> InstrBlock
genBranch = toOL . mkJumpInstr



-- -----------------------------------------------------------------------------
--  Conditional jumps/branches

{-
Conditional jumps are always to local labels, so we can use branch
instructions.  We peek at the arguments to decide what kind of
comparison to do.

I386: First, we have to ensure that the condition
codes are set according to the supplied comparison operation.
-}

genCondBranch
    :: BlockId      -- the source of the jump
    -> BlockId      -- the true branch target
    -> BlockId      -- the false branch target
    -> CmmExpr      -- the condition on which to branch
    -> NatM InstrBlock -- Instructions

genCondBranch bid id false expr = do
  is32Bit <- is32BitPlatform
  genCondBranch' is32Bit bid id false expr

-- | We return the instructions generated.
genCondBranch' :: Bool -> BlockId -> BlockId -> BlockId -> CmmExpr
               -> NatM InstrBlock

genCondBranch' _ bid id false bool = do
  CondCode is_float cond cond_code <- getCondCode bool
  use_sse2 <- sse2Enabled
  if not is_float || not use_sse2
    then
        return (cond_code `snocOL` JXX cond id `appOL` genBranch false)
    else do
        -- See Note [SSE Parity Checks]
        let jmpFalse = genBranch false
            code
                = case cond of
                  NE  -> or_unordered
                  GU  -> plain_test
                  GEU -> plain_test
                  -- Use ASSERT so we don't break releases if
                  -- LTT/LE creep in somehow.
                  LTT ->
                    assertPpr False (text "Should have been turned into >")
                    and_ordered
                  LE  ->
                    assertPpr False (text "Should have been turned into >=")
                    and_ordered
                  _   -> and_ordered

            plain_test = unitOL (
                  JXX cond id
                ) `appOL` jmpFalse
            or_unordered = toOL [
                  JXX cond id,
                  JXX PARITY id
                ] `appOL` jmpFalse
            and_ordered = toOL [
                  JXX PARITY false,
                  JXX cond id,
                  JXX ALWAYS false
                ]
        updateCfgNat (\cfg -> adjustEdgeWeight cfg (+3) bid false)
        return (cond_code `appOL` code)

{-  Note [Introducing cfg edges inside basic blocks]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    During instruction selection a statement `s`
    in a block B with control of the sort: B -> C
    will sometimes result in control
    flow of the sort:

            ┌ < ┐
            v   ^
      B ->  B1  ┴ -> C

    as is the case for some atomic operations.

    Now to keep the CFG in sync when introducing B1 we clearly
    want to insert it between B and C. However there is
    a catch when we have to deal with self loops.

    We might start with code and a CFG of these forms:

    loop:
        stmt1               ┌ < ┐
        ....                v   ^
        stmtX              loop ┘
        stmtY
        ....
        goto loop:

    Now we introduce B1:
                            ┌ ─ ─ ─ ─ ─┐
        loop:               │   ┌ <  ┐ │
        instrs              v   │    │ ^
        ....               loop ┴ B1 ┴ ┘
        instrsFromX
        stmtY
        goto loop:

    This is simple, all outgoing edges from loop now simply
    start from B1 instead and the code generator knows which
    new edges it introduced for the self loop of B1.

    Disaster strikes if the statement Y follows the same pattern.
    If we apply the same rule that all outgoing edges change then
    we end up with:

        loop ─> B1 ─> B2 ┬─┐
          │      │    └─<┤ │
          │      └───<───┘ │
          └───────<────────┘

    This is problematic. The edge B1->B1 is modified as expected.
    However the modification is wrong!

    The assembly in this case looked like this:

    _loop:
        <instrs>
    _B1:
        ...
        cmpxchgq ...
        jne _B1
        <instrs>
        <end _B1>
    _B2:
        ...
        cmpxchgq ...
        jne _B2
        <instrs>
        jmp loop

    There is no edge _B2 -> _B1 here. It's still a self loop onto _B1.

    The problem here is that really B1 should be two basic blocks.
    Otherwise we have control flow in the *middle* of a basic block.
    A contradiction!

    So to account for this we add yet another basic block marker:

    _B:
        <instrs>
    _B1:
        ...
        cmpxchgq ...
        jne _B1
        jmp _B1'
    _B1':
        <instrs>
        <end _B1>
    _B2:
        ...

    Now when inserting B2 we will only look at the outgoing edges of B1' and
    everything will work out nicely.

    You might also wonder why we don't insert jumps at the end of _B1'. There is
    no way another block ends up jumping to the labels _B1 or _B2 since they are
    essentially invisible to other blocks. View them as control flow labels local
    to the basic block if you'd like.

    Not doing this ultimately caused (part 2 of) #17334.
-}


-- -----------------------------------------------------------------------------
--  Generating C calls

-- Now the biggest nightmare---calls.  Most of the nastiness is buried in
-- @get_arg@, which moves the arguments to the correct registers/stack
-- locations.  Apart from that, the code is easy.
--
-- (If applicable) Do not fill the delay slots here; you will confuse the
-- register allocator.
--
-- See Note [Keeping track of the current block] for information why we need
-- to take/return a block id.

genForeignCall
    :: ForeignTarget -- ^ function to call
    -> [CmmFormal]   -- ^ where to put the result
    -> [CmmActual]   -- ^ arguments (of mixed type)
    -> BlockId       -- ^ The block we are in
    -> NatM (InstrBlock, Maybe BlockId)

genForeignCall target dst args bid = do
  case target of
    PrimTarget prim         -> genPrim bid prim dst args
    ForeignTarget addr conv -> (,Nothing) <$> genCCall bid addr conv dst args

genPrim
    :: BlockId       -- ^ The block we are in
    -> CallishMachOp -- ^ MachOp
    -> [CmmFormal]   -- ^ where to put the result
    -> [CmmActual]   -- ^ arguments (of mixed type)
    -> NatM (InstrBlock, Maybe BlockId)

-- First we deal with cases which might introduce new blocks in the stream.
genPrim bid (MO_AtomicRMW width amop) [dst] [addr, n]
  = genAtomicRMW bid width amop dst addr n
genPrim bid (MO_Ctz width) [dst] [src]
  = genCtz bid width dst src

-- Then we deal with cases which not introducing new blocks in the stream.
genPrim bid prim dst args
  = (,Nothing) <$> genSimplePrim bid prim dst args

genSimplePrim
    :: BlockId       -- ^ the block we are in
    -> CallishMachOp -- ^ MachOp
    -> [CmmFormal]   -- ^ where to put the result
    -> [CmmActual]   -- ^ arguments (of mixed type)
    -> NatM InstrBlock
genSimplePrim bid (MO_Memcpy align)    []      [dst,src,n]    = genMemCpy  bid align dst src n
genSimplePrim bid (MO_Memmove align)   []      [dst,src,n]    = genMemMove bid align dst src n
genSimplePrim bid (MO_Memcmp align)    [res]   [dst,src,n]    = genMemCmp  bid align res dst src n
genSimplePrim bid (MO_Memset align)    []      [dst,c,n]      = genMemSet  bid align dst c n
genSimplePrim _   MO_AcquireFence      []      []             = return nilOL -- barriers compile to no code on x86/x86-64;
genSimplePrim _   MO_ReleaseFence      []      []             = return nilOL -- we keep it this long in order to prevent earlier optimisations.
genSimplePrim _   MO_SeqCstFence       []      []             = return $ unitOL MFENCE
genSimplePrim _   MO_Touch             []      [_]            = return nilOL
genSimplePrim _   (MO_Prefetch_Data n) []      [src]          = genPrefetchData n src
genSimplePrim _   (MO_BSwap width)     [dst]   [src]          = genByteSwap width dst src
genSimplePrim bid (MO_BRev width)      [dst]   [src]          = genBitRev bid width dst src
genSimplePrim bid (MO_PopCnt width)    [dst]   [src]          = genPopCnt bid width dst src
genSimplePrim bid (MO_Pdep width)      [dst]   [src,mask]     = genPdep bid width dst src mask
genSimplePrim bid (MO_Pext width)      [dst]   [src,mask]     = genPext bid width dst src mask
genSimplePrim bid (MO_Clz width)       [dst]   [src]          = genClz bid width dst src
genSimplePrim bid (MO_UF_Conv width)   [dst]   [src]          = genWordToFloat bid width dst src
genSimplePrim _   (MO_AtomicRead w mo)  [dst]  [addr]         = genAtomicRead w mo dst addr
genSimplePrim _   (MO_AtomicWrite w mo) []     [addr,val]     = genAtomicWrite w mo addr val
genSimplePrim bid (MO_Cmpxchg width)   [dst]   [addr,old,new] = genCmpXchg bid width dst addr old new
genSimplePrim _   (MO_Xchg width)      [dst]   [addr, value]  = genXchg width dst addr value
genSimplePrim _   (MO_AddWordC w)      [r,c]   [x,y]          = genAddSubRetCarry w ADD_CC (const Nothing) CARRY r c x y
genSimplePrim _   (MO_SubWordC w)      [r,c]   [x,y]          = genAddSubRetCarry w SUB_CC (const Nothing) CARRY r c x y
genSimplePrim _   (MO_AddIntC w)       [r,c]   [x,y]          = genAddSubRetCarry w ADD_CC (Just . ADD_CC) OFLO  r c x y
genSimplePrim _   (MO_SubIntC w)       [r,c]   [x,y]          = genAddSubRetCarry w SUB_CC (const Nothing) OFLO  r c x y
genSimplePrim _   (MO_Add2 w)          [h,l]   [x,y]          = genAddWithCarry w h l x y
genSimplePrim _   (MO_U_Mul2 w)        [h,l]   [x,y]          = genUnsignedLargeMul w h l x y
genSimplePrim _   (MO_S_Mul2 w)        [c,h,l] [x,y]          = genSignedLargeMul w c h l x y
genSimplePrim _   (MO_S_QuotRem w)     [q,r]   [x,y]          = genQuotRem w True  q r Nothing   x  y
genSimplePrim _   (MO_U_QuotRem w)     [q,r]   [x,y]          = genQuotRem w False q r Nothing   x  y
genSimplePrim _   (MO_U_QuotRem2 w)    [q,r]   [hx,lx,y]      = genQuotRem w False q r (Just hx) lx y
genSimplePrim _   MO_F32_Fabs          [dst]   [src]          = genFloatAbs W32 dst src
genSimplePrim _   MO_F64_Fabs          [dst]   [src]          = genFloatAbs W64 dst src
genSimplePrim _   MO_F32_Sqrt          [dst]   [src]          = genFloatSqrt FF32 dst src
genSimplePrim _   MO_F64_Sqrt          [dst]   [src]          = genFloatSqrt FF64 dst src
genSimplePrim bid MO_F32_Sin           [dst]   [src]          = genLibCCall bid (fsLit "sinf") [dst] [src]
genSimplePrim bid MO_F32_Cos           [dst]   [src]          = genLibCCall bid (fsLit "cosf") [dst] [src]
genSimplePrim bid MO_F32_Tan           [dst]   [src]          = genLibCCall bid (fsLit "tanf") [dst] [src]
genSimplePrim bid MO_F32_Exp           [dst]   [src]          = genLibCCall bid (fsLit "expf") [dst] [src]
genSimplePrim bid MO_F32_ExpM1         [dst]   [src]          = genLibCCall bid (fsLit "expm1f") [dst] [src]
genSimplePrim bid MO_F32_Log           [dst]   [src]          = genLibCCall bid (fsLit "logf") [dst] [src]
genSimplePrim bid MO_F32_Log1P         [dst]   [src]          = genLibCCall bid (fsLit "log1pf") [dst] [src]
genSimplePrim bid MO_F32_Asin          [dst]   [src]          = genLibCCall bid (fsLit "asinf") [dst] [src]
genSimplePrim bid MO_F32_Acos          [dst]   [src]          = genLibCCall bid (fsLit "acosf") [dst] [src]
genSimplePrim bid MO_F32_Atan          [dst]   [src]          = genLibCCall bid (fsLit "atanf") [dst] [src]
genSimplePrim bid MO_F32_Sinh          [dst]   [src]          = genLibCCall bid (fsLit "sinhf") [dst] [src]
genSimplePrim bid MO_F32_Cosh          [dst]   [src]          = genLibCCall bid (fsLit "coshf") [dst] [src]
genSimplePrim bid MO_F32_Tanh          [dst]   [src]          = genLibCCall bid (fsLit "tanhf") [dst] [src]
genSimplePrim bid MO_F32_Pwr           [dst]   [x,y]          = genLibCCall bid (fsLit "powf")  [dst] [x,y]
genSimplePrim bid MO_F32_Asinh         [dst]   [src]          = genLibCCall bid (fsLit "asinhf") [dst] [src]
genSimplePrim bid MO_F32_Acosh         [dst]   [src]          = genLibCCall bid (fsLit "acoshf") [dst] [src]
genSimplePrim bid MO_F32_Atanh         [dst]   [src]          = genLibCCall bid (fsLit "atanhf") [dst] [src]
genSimplePrim bid MO_F64_Sin           [dst]   [src]          = genLibCCall bid (fsLit "sin") [dst] [src]
genSimplePrim bid MO_F64_Cos           [dst]   [src]          = genLibCCall bid (fsLit "cos") [dst] [src]
genSimplePrim bid MO_F64_Tan           [dst]   [src]          = genLibCCall bid (fsLit "tan") [dst] [src]
genSimplePrim bid MO_F64_Exp           [dst]   [src]          = genLibCCall bid (fsLit "exp") [dst] [src]
genSimplePrim bid MO_F64_ExpM1         [dst]   [src]          = genLibCCall bid (fsLit "expm1") [dst] [src]
genSimplePrim bid MO_F64_Log           [dst]   [src]          = genLibCCall bid (fsLit "log") [dst] [src]
genSimplePrim bid MO_F64_Log1P         [dst]   [src]          = genLibCCall bid (fsLit "log1p") [dst] [src]
genSimplePrim bid MO_F64_Asin          [dst]   [src]          = genLibCCall bid (fsLit "asin") [dst] [src]
genSimplePrim bid MO_F64_Acos          [dst]   [src]          = genLibCCall bid (fsLit "acos") [dst] [src]
genSimplePrim bid MO_F64_Atan          [dst]   [src]          = genLibCCall bid (fsLit "atan") [dst] [src]
genSimplePrim bid MO_F64_Sinh          [dst]   [src]          = genLibCCall bid (fsLit "sinh") [dst] [src]
genSimplePrim bid MO_F64_Cosh          [dst]   [src]          = genLibCCall bid (fsLit "cosh") [dst] [src]
genSimplePrim bid MO_F64_Tanh          [dst]   [src]          = genLibCCall bid (fsLit "tanh") [dst] [src]
genSimplePrim bid MO_F64_Pwr           [dst]   [x,y]          = genLibCCall bid (fsLit "pow")  [dst] [x,y]
genSimplePrim bid MO_F64_Asinh         [dst]   [src]          = genLibCCall bid (fsLit "asinh") [dst] [src]
genSimplePrim bid MO_F64_Acosh         [dst]   [src]          = genLibCCall bid (fsLit "acosh") [dst] [src]
genSimplePrim bid MO_F64_Atanh         [dst]   [src]          = genLibCCall bid (fsLit "atanh") [dst] [src]
genSimplePrim bid MO_SuspendThread     [tok]   [rs,i]         = genRTSCCall bid (fsLit "suspendThread") [tok] [rs,i]
genSimplePrim bid MO_ResumeThread      [rs]    [tok]          = genRTSCCall bid (fsLit "resumeThread") [rs] [tok]
genSimplePrim _   MO_I64_ToI           [dst]   [src]          = genInt64ToInt dst src
genSimplePrim _   MO_I64_FromI         [dst]   [src]          = genIntToInt64 dst src
genSimplePrim _   MO_W64_ToW           [dst]   [src]          = genWord64ToWord dst src
genSimplePrim _   MO_W64_FromW         [dst]   [src]          = genWordToWord64 dst src
genSimplePrim _   MO_x64_Neg           [dst]   [src]          = genNeg64 dst src
genSimplePrim _   MO_x64_Add           [dst]   [x,y]          = genAdd64 dst x y
genSimplePrim _   MO_x64_Sub           [dst]   [x,y]          = genSub64 dst x y
genSimplePrim bid MO_x64_Mul           [dst]   [x,y]          = genPrimCCall bid (fsLit "hs_mul64") [dst] [x,y]
genSimplePrim bid MO_I64_Quot          [dst]   [x,y]          = genPrimCCall bid (fsLit "hs_quotInt64") [dst] [x,y]
genSimplePrim bid MO_I64_Rem           [dst]   [x,y]          = genPrimCCall bid (fsLit "hs_remInt64") [dst] [x,y]
genSimplePrim bid MO_W64_Quot          [dst]   [x,y]          = genPrimCCall bid (fsLit "hs_quotWord64") [dst] [x,y]
genSimplePrim bid MO_W64_Rem           [dst]   [x,y]          = genPrimCCall bid (fsLit "hs_remWord64") [dst] [x,y]
genSimplePrim _   MO_x64_And           [dst]   [x,y]          = genAnd64 dst x y
genSimplePrim _   MO_x64_Or            [dst]   [x,y]          = genOr64  dst x y
genSimplePrim _   MO_x64_Xor           [dst]   [x,y]          = genXor64 dst x y
genSimplePrim _   MO_x64_Not           [dst]   [src]          = genNot64 dst src
genSimplePrim bid MO_x64_Shl           [dst]   [x,n]          = genPrimCCall bid (fsLit "hs_uncheckedShiftL64") [dst] [x,n]
genSimplePrim bid MO_I64_Shr           [dst]   [x,n]          = genPrimCCall bid (fsLit "hs_uncheckedIShiftRA64") [dst] [x,n]
genSimplePrim bid MO_W64_Shr           [dst]   [x,n]          = genPrimCCall bid (fsLit "hs_uncheckedShiftRL64") [dst] [x,n]
genSimplePrim _   MO_x64_Eq            [dst]   [x,y]          = genEq64 dst x y
genSimplePrim _   MO_x64_Ne            [dst]   [x,y]          = genNe64 dst x y
genSimplePrim _   MO_I64_Ge            [dst]   [x,y]          = genGeInt64 dst x y
genSimplePrim _   MO_I64_Gt            [dst]   [x,y]          = genGtInt64 dst x y
genSimplePrim _   MO_I64_Le            [dst]   [x,y]          = genLeInt64 dst x y
genSimplePrim _   MO_I64_Lt            [dst]   [x,y]          = genLtInt64 dst x y
genSimplePrim _   MO_W64_Ge            [dst]   [x,y]          = genGeWord64 dst x y
genSimplePrim _   MO_W64_Gt            [dst]   [x,y]          = genGtWord64 dst x y
genSimplePrim _   MO_W64_Le            [dst]   [x,y]          = genLeWord64 dst x y
genSimplePrim _   MO_W64_Lt            [dst]   [x,y]          = genLtWord64 dst x y
genSimplePrim _   op                   dst     args           = do
  platform <- ncgPlatform <$> getConfig
  pprPanic "genSimplePrim: unhandled primop" (ppr (pprCallishMachOp op, dst, fmap (pdoc platform) args))

{-
Note [Evaluate C-call arguments before placing in destination registers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When producing code for C calls we must take care when placing arguments
in their final registers. Specifically, we must ensure that temporary register
usage due to evaluation of one argument does not clobber a register in which we
already placed a previous argument (e.g. as the code generation logic for
MO_Shl can clobber %rcx due to x86 instruction limitations).

This is precisely what happened in #18527. Consider this C--:

    (result::I64) = call "ccall" doSomething(_s2hp::I64, 2244, _s2hq::I64, _s2hw::I64 | (1 << _s2hz::I64));

Here we are calling the C function `doSomething` with three arguments, the last
involving a non-trivial expression involving MO_Shl. In this case the NCG could
naively generate the following assembly (where $tmp denotes some temporary
register and $argN denotes the register for argument N, as dictated by the
platform's calling convention):

    mov _s2hp, $arg1   # place first argument
    mov _s2hq, $arg2   # place second argument

    # Compute 1 << _s2hz
    mov _s2hz, %rcx
    shl %cl, $tmp

    # Compute (_s2hw | (1 << _s2hz))
    mov _s2hw, $arg3
    or $tmp, $arg3

    # Perform the call
    call func

This code is outright broken on Windows which assigns $arg1 to %rcx. This means
that the evaluation of the last argument clobbers the first argument.

To avoid this we use a rather awful hack: when producing code for a C call with
at least one non-trivial argument, we first evaluate all of the arguments into
local registers before moving them into their final calling-convention-defined
homes.  This is performed by 'evalArgs'. Here we define "non-trivial" to be an
expression which might contain a MachOp since these are the only cases which
might clobber registers. Furthermore, we use a conservative approximation of
this condition (only looking at the top-level of CmmExprs) to avoid spending
too much effort trying to decide whether we want to take the fast path.

Note that this hack *also* applies to calls to out-of-line PrimTargets (which
are lowered via a C call), which will ultimately end up in
genForeignCall{32,64}.
-}

-- | See Note [Evaluate C-call arguments before placing in destination registers]
evalArgs :: BlockId -> [CmmActual] -> NatM (InstrBlock, [CmmActual])
evalArgs bid actuals
  | any mightContainMachOp actuals = do
      regs_blks <- mapM evalArg actuals
      return (concatOL $ map fst regs_blks, map snd regs_blks)
  | otherwise = return (nilOL, actuals)
  where
    mightContainMachOp (CmmReg _)      = False
    mightContainMachOp (CmmRegOff _ _) = False
    mightContainMachOp (CmmLit _)      = False
    mightContainMachOp _               = True

    evalArg :: CmmActual -> NatM (InstrBlock, CmmExpr)
    evalArg actual = do
        platform <- getPlatform
        lreg <- newLocalReg $ cmmExprType platform actual
        (instrs, bid1) <- stmtToInstrs bid $ CmmAssign (CmmLocal lreg) actual
        -- The above assignment shouldn't change the current block
        massert (isNothing bid1)
        return (instrs, CmmReg $ CmmLocal lreg)

    newLocalReg :: CmmType -> NatM LocalReg
    newLocalReg ty = LocalReg <$> getUniqueM <*> pure ty

-- Note [DIV/IDIV for bytes]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- IDIV reminder:
--   Size    Dividend   Divisor   Quotient    Remainder
--   byte    %ax         r/m8      %al          %ah
--   word    %dx:%ax     r/m16     %ax          %dx
--   dword   %edx:%eax   r/m32     %eax         %edx
--   qword   %rdx:%rax   r/m64     %rax         %rdx
--
-- We do a special case for the byte division because the current
-- codegen doesn't deal well with accessing %ah register (also,
-- accessing %ah in 64-bit mode is complicated because it cannot be an
-- operand of many instructions). So we just widen operands to 16 bits
-- and get the results from %al, %dl. This is not optimal, but a few
-- register moves are probably not a huge deal when doing division.


-- | Generate C call to the given function in ghc-prim
genPrimCCall
  :: BlockId
  -> FastString
  -> [CmmFormal]
  -> [CmmActual]
  -> NatM InstrBlock
genPrimCCall bid lbl_txt dsts args = do
  config <- getConfig
  -- FIXME: we should use mkForeignLabel instead of mkCmmCodeLabel
  let lbl = mkCmmCodeLabel primUnitId lbl_txt
  addr <- cmmMakeDynamicReference config CallReference lbl
  let conv = ForeignConvention CCallConv [] [] CmmMayReturn
  genCCall bid addr conv dsts args

-- | Generate C call to the given function in libc
genLibCCall
  :: BlockId
  -> FastString
  -> [CmmFormal]
  -> [CmmActual]
  -> NatM InstrBlock
genLibCCall bid lbl_txt dsts args = do
  config <- getConfig
  -- Assume we can call these functions directly, and that they're not in a dynamic library.
  -- TODO: Why is this ok? Under linux this code will be in libm.so
  --       Is it because they're really implemented as a primitive instruction by the assembler??  -- BL 2009/12/31
  let lbl = mkForeignLabel lbl_txt ForeignLabelInThisPackage IsFunction
  addr <- cmmMakeDynamicReference config CallReference lbl
  let conv = ForeignConvention CCallConv [] [] CmmMayReturn
  genCCall bid addr conv dsts args

-- | Generate C call to the given function in the RTS
genRTSCCall
  :: BlockId
  -> FastString
  -> [CmmFormal]
  -> [CmmActual]
  -> NatM InstrBlock
genRTSCCall bid lbl_txt dsts args = do
  config <- getConfig
  -- Assume we can call these functions directly, and that they're not in a dynamic library.
  let lbl = mkForeignLabel lbl_txt ForeignLabelInThisPackage IsFunction
  addr <- cmmMakeDynamicReference config CallReference lbl
  let conv = ForeignConvention CCallConv [] [] CmmMayReturn
  genCCall bid addr conv dsts args

-- | Generate a real C call to the given address with the given convention
genCCall
  :: BlockId
  -> CmmExpr
  -> ForeignConvention
  -> [CmmFormal]
  -> [CmmActual]
  -> NatM InstrBlock
genCCall bid addr conv dest_regs args = do
  is32Bit <- is32BitPlatform
  (instrs0, args') <- evalArgs bid args
  instrs1 <- if is32Bit
    then genCCall32 addr conv dest_regs args'
    else genCCall64 addr conv dest_regs args'
  return (instrs0 `appOL` instrs1)

genCCall32 :: CmmExpr           -- ^ address of the function to call
           -> ForeignConvention -- ^ calling convention
           -> [CmmFormal]       -- ^ where to put the result
           -> [CmmActual]       -- ^ arguments (of mixed type)
           -> NatM InstrBlock
genCCall32 addr _ dest_regs args = do
        config <- getConfig
        let platform = ncgPlatform config
            prom_args = map (maybePromoteCArg platform W32) args

            -- If the size is smaller than the word, we widen things (see maybePromoteCArg)
            arg_size_bytes :: CmmType -> Int
            arg_size_bytes ty = max (widthInBytes (typeWidth ty)) (widthInBytes (wordWidth platform))

            roundTo a x | x `mod` a == 0 = x
                        | otherwise = x + a - (x `mod` a)

            push_arg :: CmmActual {-current argument-}
                            -> NatM InstrBlock  -- code

            push_arg  arg -- we don't need the hints on x86
              | isWord64 arg_ty = do
                RegCode64 code r_hi r_lo <- iselExpr64 arg
                delta <- getDeltaNat
                setDeltaNat (delta - 8)
                return (       code `appOL`
                               toOL [PUSH II32 (OpReg r_hi), DELTA (delta - 4),
                                     PUSH II32 (OpReg r_lo), DELTA (delta - 8),
                                     DELTA (delta-8)]
                    )

              | isFloatType arg_ty = do
                (reg, code) <- getSomeReg arg
                delta <- getDeltaNat
                setDeltaNat (delta-size)
                return (code `appOL`
                                toOL [SUB II32 (OpImm (ImmInt size)) (OpReg esp),
                                      DELTA (delta-size),
                                      let addr = AddrBaseIndex (EABaseReg esp)
                                                                EAIndexNone
                                                                (ImmInt 0)
                                          format = floatFormat (typeWidth arg_ty)
                                      in

                                      -- assume SSE2
                                       MOV format (OpReg reg) (OpAddr addr)

                                     ]
                               )

              | otherwise = do
                -- Arguments can be smaller than 32-bit, but we still use @PUSH
                -- II32@ - the usual calling conventions expect integers to be
                -- 4-byte aligned.
                massert ((typeWidth arg_ty) <= W32)
                (operand, code) <- getOperand arg
                delta <- getDeltaNat
                setDeltaNat (delta-size)
                return (code `snocOL`
                        PUSH II32 operand `snocOL`
                        DELTA (delta-size))

              where
                 arg_ty = cmmExprType platform arg
                 size = arg_size_bytes arg_ty -- Byte size

        let
            -- Align stack to 16n for calls, assuming a starting stack
            -- alignment of 16n - word_size on procedure entry. Which we
            -- maintiain. See Note [Stack Alignment on X86] in rts/StgCRun.c.
            sizes               = map (arg_size_bytes . cmmExprType platform) (reverse args)
            raw_arg_size        = sum sizes + platformWordSizeInBytes platform
            arg_pad_size        = (roundTo 16 $ raw_arg_size) - raw_arg_size
            tot_arg_size        = raw_arg_size + arg_pad_size - platformWordSizeInBytes platform


        delta0 <- getDeltaNat
        setDeltaNat (delta0 - arg_pad_size)

        push_codes <- mapM push_arg (reverse prom_args)
        delta <- getDeltaNat
        massert (delta == delta0 - tot_arg_size)

        -- deal with static vs dynamic call targets
        callinsns <-
          case addr of
            CmmLit (CmmLabel lbl)
               -> return $ unitOL (CALL (Left fn_imm) [])
               where fn_imm = ImmCLbl lbl
            _
               -> do { (dyn_r, dyn_c) <- getSomeReg addr
                     ; massert (isWord32 (cmmExprType platform addr))
                     ; return $ dyn_c `snocOL` CALL (Right dyn_r) [] }
        let push_code
                | arg_pad_size /= 0
                = toOL [SUB II32 (OpImm (ImmInt arg_pad_size)) (OpReg esp),
                        DELTA (delta0 - arg_pad_size)]
                  `appOL` concatOL push_codes
                | otherwise
                = concatOL push_codes

            call = callinsns `appOL`
                   toOL (
                      (if tot_arg_size == 0 then [] else
                       [ADD II32 (OpImm (ImmInt tot_arg_size)) (OpReg esp)])
                      ++
                      [DELTA delta0]
                   )
        setDeltaNat delta0

        let
            -- assign the results, if necessary
            assign_code []     = nilOL
            assign_code [dest]
              | isFloatType ty =
                  -- we assume SSE2
                  let tmp_amode = AddrBaseIndex (EABaseReg esp)
                                                       EAIndexNone
                                                       (ImmInt 0)
                      fmt = floatFormat w
                         in toOL [ SUB II32 (OpImm (ImmInt b)) (OpReg esp),
                                   DELTA (delta0 - b),
                                   X87Store fmt  tmp_amode,
                                   -- X87Store only supported for the CDECL ABI
                                   -- NB: This code will need to be
                                   -- revisited once GHC does more work around
                                   -- SIGFPE f
                                   MOV fmt (OpAddr tmp_amode) (OpReg r_dest),
                                   ADD II32 (OpImm (ImmInt b)) (OpReg esp),
                                   DELTA delta0]
              | isWord64 ty    = toOL [MOV II32 (OpReg eax) (OpReg r_dest),
                                        MOV II32 (OpReg edx) (OpReg r_dest_hi)]
              | otherwise      = unitOL (MOV (intFormat w)
                                             (OpReg eax)
                                             (OpReg r_dest))
              where
                    ty = localRegType dest
                    w  = typeWidth ty
                    b  = widthInBytes w
                    r_dest_hi = getHiVRegFromLo r_dest
                    r_dest    = getLocalRegReg dest
            assign_code many = pprPanic "genForeignCall.assign_code - too many return values:" (ppr many)

        return (push_code `appOL`
                call `appOL`
                assign_code dest_regs)

genCCall64 :: CmmExpr           -- ^ address of function to call
           -> ForeignConvention -- ^ calling convention
           -> [CmmFormal]       -- ^ where to put the result
           -> [CmmActual]       -- ^ arguments (of mixed type)
           -> NatM InstrBlock
genCCall64 addr conv dest_regs args = do
    platform <- getPlatform
    -- load up the register arguments
    let prom_args = map (maybePromoteCArg platform W32) args

    let load_args :: [CmmExpr]
                  -> [Reg]         -- int regs avail for args
                  -> [Reg]         -- FP regs avail for args
                  -> InstrBlock    -- code computing args
                  -> InstrBlock    -- code assigning args to ABI regs
                  -> NatM ([CmmExpr],[Reg],[Reg],InstrBlock,InstrBlock)
        -- no more regs to use
        load_args args [] [] code acode     =
            return (args, [], [], code, acode)

        -- no more args to push
        load_args [] aregs fregs code acode =
            return ([], aregs, fregs, code, acode)

        load_args (arg : rest) aregs fregs code acode
            | isFloatType arg_rep = case fregs of
                 []     -> push_this_arg
                 (r:rs) -> do
                    (code',acode') <- reg_this_arg r
                    load_args rest aregs rs code' acode'
            | otherwise           = case aregs of
                 []     -> push_this_arg
                 (r:rs) -> do
                    (code',acode') <- reg_this_arg r
                    load_args rest rs fregs code' acode'
            where

              -- put arg into the list of stack pushed args
              push_this_arg = do
                 (args',ars,frs,code',acode')
                     <- load_args rest aregs fregs code acode
                 return (arg:args', ars, frs, code', acode')

              -- pass the arg into the given register
              reg_this_arg r
                -- "operand" args can be directly assigned into r
                | isOperand platform arg = do
                    arg_code <- getAnyReg arg
                    return (code, (acode `appOL` arg_code r))
                -- The last non-operand arg can be directly assigned after its
                -- computation without going into a temporary register
                | all (isOperand platform) rest = do
                    arg_code   <- getAnyReg arg
                    return (code `appOL` arg_code r,acode)

                -- other args need to be computed beforehand to avoid clobbering
                -- previously assigned registers used to pass parameters (see
                -- #11792, #12614). They are assigned into temporary registers
                -- and get assigned to proper call ABI registers after they all
                -- have been computed.
                | otherwise     = do
                    arg_code <- getAnyReg arg
                    tmp      <- getNewRegNat arg_fmt
                    let
                      code'  = code `appOL` arg_code tmp
                      acode' = acode `snocOL` reg2reg arg_fmt tmp r
                    return (code',acode')

              arg_rep = cmmExprType platform arg
              arg_fmt = cmmTypeFormat arg_rep

        load_args_win :: [CmmExpr]
                      -> [Reg]        -- used int regs
                      -> [Reg]        -- used FP regs
                      -> [(Reg, Reg)] -- (int, FP) regs avail for args
                      -> InstrBlock
                      -> NatM ([CmmExpr],[Reg],[Reg],InstrBlock,InstrBlock)
        load_args_win args usedInt usedFP [] code
            = return (args, usedInt, usedFP, code, nilOL)
            -- no more regs to use
        load_args_win [] usedInt usedFP _ code
            = return ([], usedInt, usedFP, code, nilOL)
            -- no more args to push
        load_args_win (arg : rest) usedInt usedFP
                      ((ireg, freg) : regs) code
            | isFloatType arg_rep = do
                 arg_code <- getAnyReg arg
                 load_args_win rest (ireg : usedInt) (freg : usedFP) regs
                               (code `appOL`
                                arg_code freg `snocOL`
                                -- If we are calling a varargs function
                                -- then we need to define ireg as well
                                -- as freg
                                MOV II64 (OpReg freg) (OpReg ireg))
            | otherwise = do
                 arg_code <- getAnyReg arg
                 load_args_win rest (ireg : usedInt) usedFP regs
                               (code `appOL` arg_code ireg)
            where
              arg_rep = cmmExprType platform arg

        arg_size = 8 -- always, at the mo

        push_args [] code = return code
        push_args (arg:rest) code
           | isFloatType arg_rep = do
             (arg_reg, arg_code) <- getSomeReg arg
             delta <- getDeltaNat
             setDeltaNat (delta-arg_size)
             let code' = code `appOL` arg_code `appOL` toOL [
                            SUB (intFormat (wordWidth platform)) (OpImm (ImmInt arg_size)) (OpReg rsp),
                            DELTA (delta-arg_size),
                            MOV (floatFormat width) (OpReg arg_reg) (OpAddr (spRel platform 0))]
             push_args rest code'

           | otherwise = do
             -- Arguments can be smaller than 64-bit, but we still use @PUSH
             -- II64@ - the usual calling conventions expect integers to be
             -- 8-byte aligned.
             massert (width <= W64)
             (arg_op, arg_code) <- getOperand arg
             delta <- getDeltaNat
             setDeltaNat (delta-arg_size)
             let code' = code `appOL` arg_code `appOL` toOL [
                                    PUSH II64 arg_op,
                                    DELTA (delta-arg_size)]
             push_args rest code'
            where
              arg_rep = cmmExprType platform arg
              width = typeWidth arg_rep

        leaveStackSpace n = do
             delta <- getDeltaNat
             setDeltaNat (delta - n * arg_size)
             return $ toOL [
                         SUB II64 (OpImm (ImmInt (n * platformWordSizeInBytes platform))) (OpReg rsp),
                         DELTA (delta - n * arg_size)]

    (stack_args, int_regs_used, fp_regs_used, load_args_code, assign_args_code)
         <-
        if platformOS platform == OSMinGW32
        then load_args_win prom_args [] [] (allArgRegs platform) nilOL
        else do
           (stack_args, aregs, fregs, load_args_code, assign_args_code)
               <- load_args prom_args (allIntArgRegs platform)
                                      (allFPArgRegs platform)
                                      nilOL nilOL
           let used_regs rs as = dropTail (length rs) as
               fregs_used      = used_regs fregs (allFPArgRegs platform)
               aregs_used      = used_regs aregs (allIntArgRegs platform)
           return (stack_args, aregs_used, fregs_used, load_args_code
                                                      , assign_args_code)

    let
        arg_regs_used = int_regs_used ++ fp_regs_used
        arg_regs = [eax] ++ arg_regs_used
                -- for annotating the call instruction with
        sse_regs = length fp_regs_used
        arg_stack_slots = if platformOS platform == OSMinGW32
                          then length stack_args + length (allArgRegs platform)
                          else length stack_args
        tot_arg_size = arg_size * arg_stack_slots


    -- Align stack to 16n for calls, assuming a starting stack
    -- alignment of 16n - word_size on procedure entry. Which we
    -- maintain. See Note [Stack Alignment on X86] in rts/StgCRun.c
    let word_size = platformWordSizeInBytes platform
    (real_size, adjust_rsp) <-
        if (tot_arg_size + word_size) `rem` 16 == 0
            then return (tot_arg_size, nilOL)
            else do -- we need to adjust...
                delta <- getDeltaNat
                setDeltaNat (delta - word_size)
                return (tot_arg_size + word_size, toOL [
                                SUB II64 (OpImm (ImmInt word_size)) (OpReg rsp),
                                DELTA (delta - word_size) ])

    -- push the stack args, right to left
    push_code <- push_args (reverse stack_args) nilOL
    -- On Win64, we also have to leave stack space for the arguments
    -- that we are passing in registers
    lss_code <- if platformOS platform == OSMinGW32
                then leaveStackSpace (length (allArgRegs platform))
                else return nilOL
    delta <- getDeltaNat

    -- deal with static vs dynamic call targets
    (callinsns,_cconv) <- case addr of
      CmmLit (CmmLabel lbl) ->
        return (unitOL (CALL (Left (ImmCLbl lbl)) arg_regs), conv)
      _ -> do
        (dyn_r, dyn_c) <- getSomeReg addr
        return (dyn_c `snocOL` CALL (Right dyn_r) arg_regs, conv)

    let
        -- The x86_64 ABI requires us to set %al to the number of SSE2
        -- registers that contain arguments, if the called routine
        -- is a varargs function.  We don't know whether it's a
        -- varargs function or not, so we have to assume it is.
        --
        -- It's not safe to omit this assignment, even if the number
        -- of SSE2 regs in use is zero.  If %al is larger than 8
        -- on entry to a varargs function, seg faults ensue.
        assign_eax n = unitOL (MOV II32 (OpImm (ImmInt n)) (OpReg eax))

    let call = callinsns `appOL`
               toOL (
                    -- Deallocate parameters after call for ccall
                  (if real_size==0 then [] else
                   [ADD (intFormat (platformWordWidth platform)) (OpImm (ImmInt real_size)) (OpReg esp)])
                  ++
                  [DELTA (delta + real_size)]
               )
    setDeltaNat (delta + real_size)

    let
        -- assign the results, if necessary
        assign_code []     = nilOL
        assign_code [dest] =
          case typeWidth rep of
                W32 | isFloatType rep -> unitOL (MOV (floatFormat W32)
                                                     (OpReg xmm0)
                                                     (OpReg r_dest))
                W64 | isFloatType rep -> unitOL (MOV (floatFormat W64)
                                                     (OpReg xmm0)
                                                     (OpReg r_dest))
                _ -> unitOL (MOV (cmmTypeFormat rep) (OpReg rax) (OpReg r_dest))
          where
                rep = localRegType dest
                r_dest = getRegisterReg platform  (CmmLocal dest)
        assign_code _many = panic "genForeignCall.assign_code many"

    return (adjust_rsp          `appOL`
            push_code           `appOL`
            load_args_code      `appOL`
            assign_args_code    `appOL`
            lss_code            `appOL`
            assign_eax sse_regs `appOL`
            call                `appOL`
            assign_code dest_regs)


maybePromoteCArg :: Platform -> Width -> CmmExpr -> CmmExpr
maybePromoteCArg platform wto arg
 | wfrom < wto = CmmMachOp (MO_UU_Conv wfrom wto) [arg]
 | otherwise   = arg
 where
   wfrom = cmmExprWidth platform arg

-- -----------------------------------------------------------------------------
-- Generating a table-branch

{-
Note [Sub-word subtlety during jump-table indexing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Offset the index by the start index of the jump table.
It's important that we do this *before* the widening below. To see
why, consider a switch with a sub-word, signed discriminant such as:

    switch [-5...+2] x::I16 {
        case -5: ...
        ...
        case +2: ...
    }

Consider what happens if we offset *after* widening in the case that
x=-4:

                                         // x == -4 == 0xfffc::I16
    indexWidened = UU_Conv(x);           // == 0xfffc::I64
    indexExpr    = indexWidened - (-5);  // == 0x10000::I64

This index is clearly nonsense given that the jump table only has
eight entries.

By contrast, if we widen *after* we offset then we get the correct
index (1),

                                         // x == -4 == 0xfffc::I16
    indexOffset  = x - (-5);             // == 1::I16
    indexExpr    = UU_Conv(indexOffset); // == 1::I64

See #21186.
-}

genSwitch :: CmmExpr -> SwitchTargets -> NatM InstrBlock

genSwitch expr targets = do
  config <- getConfig
  let platform = ncgPlatform config
      expr_w = cmmExprWidth platform expr
      indexExpr0 = cmmOffset platform expr offset
      -- We widen to a native-width register because we cannot use arbitrary sizes
      -- in x86 addressing modes.
      -- See Note [Sub-word subtlety during jump-table indexing].
      indexExpr = CmmMachOp
        (MO_UU_Conv expr_w (platformWordWidth platform))
        [indexExpr0]
  if ncgPIC config
  then do
        (reg,e_code) <- getNonClobberedReg indexExpr
           -- getNonClobberedReg because it needs to survive across t_code
        lbl <- getNewLabelNat
        let is32bit = target32Bit platform
            os = platformOS platform
            -- Might want to use .rodata.<function we're in> instead, but as
            -- long as it's something unique it'll work out since the
            -- references to the jump table are in the appropriate section.
            rosection = case os of
              -- on Mac OS X/x86_64, put the jump table in the text section to
              -- work around a limitation of the linker.
              -- ld64 is unable to handle the relocations for
              --     .quad L1 - L0
              -- if L0 is not preceded by a non-anonymous label in its section.
              OSDarwin | not is32bit -> Section Text lbl
              _ -> Section ReadOnlyData lbl
        dynRef <- cmmMakeDynamicReference config DataReference lbl
        (tableReg,t_code) <- getSomeReg $ dynRef
        let op = OpAddr (AddrBaseIndex (EABaseReg tableReg)
                                       (EAIndex reg (platformWordSizeInBytes platform)) (ImmInt 0))

        return $ e_code `appOL` t_code `appOL` toOL [
                                ADD (intFormat (platformWordWidth platform)) op (OpReg tableReg),
                                JMP_TBL (OpReg tableReg) ids rosection lbl
                       ]
  else do
        (reg,e_code) <- getSomeReg indexExpr
        lbl <- getNewLabelNat
        let is32bit = target32Bit platform
        if is32bit
          then let op = OpAddr (AddrBaseIndex EABaseNone (EAIndex reg (platformWordSizeInBytes platform)) (ImmCLbl lbl))
                   jmp_code = JMP_TBL op ids (Section ReadOnlyData lbl) lbl
               in return $ e_code `appOL` unitOL jmp_code
          else do
            -- See Note [%rip-relative addressing on x86-64].
            tableReg <- getNewRegNat (intFormat (platformWordWidth platform))
            targetReg <- getNewRegNat (intFormat (platformWordWidth platform))
            let op = OpAddr (AddrBaseIndex (EABaseReg tableReg) (EAIndex reg (platformWordSizeInBytes platform)) (ImmInt 0))
                code = e_code `appOL` toOL
                    [ LEA (archWordFormat is32bit) (OpAddr (AddrBaseIndex EABaseRip EAIndexNone (ImmCLbl lbl))) (OpReg tableReg)
                    , MOV (archWordFormat is32bit) op (OpReg targetReg)
                    , JMP_TBL (OpReg targetReg) ids (Section ReadOnlyData lbl) lbl
                    ]
            return code
  where
    (offset, blockIds) = switchTargetsToTable targets
    ids = map (fmap DestBlockId) blockIds

generateJumpTableForInstr :: NCGConfig -> Instr -> Maybe (NatCmmDecl (Alignment, RawCmmStatics) Instr)
generateJumpTableForInstr config (JMP_TBL _ ids section lbl)
    = let getBlockId (DestBlockId id) = id
          getBlockId _ = panic "Non-Label target in Jump Table"
          blockIds = map (fmap getBlockId) ids
      in Just (createJumpTable config blockIds section lbl)
generateJumpTableForInstr _ _ = Nothing

createJumpTable :: NCGConfig -> [Maybe BlockId] -> Section -> CLabel
                -> GenCmmDecl (Alignment, RawCmmStatics) h g
createJumpTable config ids section lbl
    = let jumpTable
            | ncgPIC config =
                  let ww = ncgWordWidth config
                      jumpTableEntryRel Nothing
                          = CmmStaticLit (CmmInt 0 ww)
                      jumpTableEntryRel (Just blockid)
                          = CmmStaticLit (CmmLabelDiffOff blockLabel lbl 0 ww)
                          where blockLabel = blockLbl blockid
                  in map jumpTableEntryRel ids
            | otherwise = map (jumpTableEntry config) ids
      in CmmData section (mkAlignment 1, CmmStaticsRaw lbl jumpTable)

extractUnwindPoints :: [Instr] -> [UnwindPoint]
extractUnwindPoints instrs =
    [ UnwindPoint lbl unwinds | UNWIND lbl unwinds <- instrs]

-- -----------------------------------------------------------------------------
-- 'condIntReg' and 'condFltReg': condition codes into registers

-- Turn those condition codes into integers now (when they appear on
-- the right hand side of an assignment).
--
-- (If applicable) Do not fill the delay slots here; you will confuse the
-- register allocator.

condIntReg :: Cond -> CmmExpr -> CmmExpr -> NatM Register

condIntReg cond x y = do
  CondCode _ cond cond_code <- condIntCode cond x y
  tmp <- getNewRegNat II8
  let
        code dst = cond_code `appOL` toOL [
                    SETCC cond (OpReg tmp),
                    MOVZxL II8 (OpReg tmp) (OpReg dst)
                  ]
  return (Any II32 code)


-- Note [SSE Parity Checks]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
-- We have to worry about unordered operands (eg. comparisons
-- against NaN).  If the operands are unordered, the comparison
-- sets the parity flag, carry flag and zero flag.
-- All comparisons are supposed to return false for unordered
-- operands except for !=, which returns true.
--
-- Optimisation: we don't have to test the parity flag if we
-- know the test has already excluded the unordered case: eg >
-- and >= test for a zero carry flag, which can only occur for
-- ordered operands.
--
-- By reversing comparisons we can avoid testing the parity
-- for < and <= as well. If any of the arguments is an NaN we
-- return false either way. If both arguments are valid then
-- x <= y  <->  y >= x  holds. So it's safe to swap these.
--
-- We invert the condition inside getRegister'and  getCondCode
-- which should cover all invertable cases.
-- All other functions translating FP comparisons to assembly
-- use these to two generate the comparison code.
--
-- As an example consider a simple check:
--
-- func :: Float -> Float -> Int
-- func x y = if x < y then 1 else 0
--
-- Which in Cmm gives the floating point comparison.
--
--  if (%MO_F_Lt_W32(F1, F2)) goto c2gg; else goto c2gf;
--
-- We used to compile this to an assembly code block like this:
-- _c2gh:
--  ucomiss %xmm2,%xmm1
--  jp _c2gf
--  jb _c2gg
--  jmp _c2gf
--
-- Where we have to introduce an explicit
-- check for unordered results (using jmp parity):
--
-- We can avoid this by exchanging the arguments and inverting the direction
-- of the comparison. This results in the sequence of:
--
--  ucomiss %xmm1,%xmm2
--  ja _c2g2
--  jmp _c2g1
--
-- Removing the jump reduces the pressure on the branch prediction system
-- and plays better with the uOP cache.

condFltReg :: Bool -> Cond -> CmmExpr -> CmmExpr -> NatM Register
condFltReg is32Bit cond x y = condFltReg_sse2
 where


  condFltReg_sse2 = do
    CondCode _ cond cond_code <- condFltCode cond x y
    tmp1 <- getNewRegNat (archWordFormat is32Bit)
    tmp2 <- getNewRegNat (archWordFormat is32Bit)
    let -- See Note [SSE Parity Checks]
        code dst =
           cond_code `appOL`
             (case cond of
                NE  -> or_unordered dst
                GU  -> plain_test   dst
                GEU -> plain_test   dst
                -- Use ASSERT so we don't break releases if these creep in.
                LTT -> assertPpr False (text "Should have been turned into >") $
                       and_ordered  dst
                LE  -> assertPpr False (text "Should have been turned into >=") $
                       and_ordered  dst
                _   -> and_ordered  dst)

        plain_test dst = toOL [
                    SETCC cond (OpReg tmp1),
                    MOVZxL II8 (OpReg tmp1) (OpReg dst)
                 ]
        or_unordered dst = toOL [
                    SETCC cond (OpReg tmp1),
                    SETCC PARITY (OpReg tmp2),
                    OR II8 (OpReg tmp1) (OpReg tmp2),
                    MOVZxL II8 (OpReg tmp2) (OpReg dst)
                  ]
        and_ordered dst = toOL [
                    SETCC cond (OpReg tmp1),
                    SETCC NOTPARITY (OpReg tmp2),
                    AND II8 (OpReg tmp1) (OpReg tmp2),
                    MOVZxL II8 (OpReg tmp2) (OpReg dst)
                  ]
    return (Any II32 code)


-- -----------------------------------------------------------------------------
-- 'trivial*Code': deal with trivial instructions

-- Trivial (dyadic: 'trivialCode', floating-point: 'trivialFCode',
-- unary: 'trivialUCode', unary fl-pt:'trivialUFCode') instructions.
-- Only look for constants on the right hand side, because that's
-- where the generic optimizer will have put them.

-- Similarly, for unary instructions, we don't have to worry about
-- matching an StInt as the argument, because genericOpt will already
-- have handled the constant-folding.


{-
The Rules of the Game are:

* You cannot assume anything about the destination register dst;
  it may be anything, including a fixed reg.

* You may compute an operand into a fixed reg, but you may not
  subsequently change the contents of that fixed reg.  If you
  want to do so, first copy the value either to a temporary
  or into dst.  You are free to modify dst even if it happens
  to be a fixed reg -- that's not your problem.

* You cannot assume that a fixed reg will stay live over an
  arbitrary computation.  The same applies to the dst reg.

* Temporary regs obtained from getNewRegNat are distinct from
  each other and from all other regs, and stay live over
  arbitrary computations.

--------------------

SDM's version of The Rules:

* If getRegister returns Any, that means it can generate correct
  code which places the result in any register, period.  Even if that
  register happens to be read during the computation.

  Corollary #1: this means that if you are generating code for an
  operation with two arbitrary operands, you cannot assign the result
  of the first operand into the destination register before computing
  the second operand.  The second operand might require the old value
  of the destination register.

  Corollary #2: A function might be able to generate more efficient
  code if it knows the destination register is a new temporary (and
  therefore not read by any of the sub-computations).

* If getRegister returns Any, then the code it generates may modify only:
        (a) fresh temporaries
        (b) the destination register
        (c) known registers (eg. %ecx is used by shifts)
  In particular, it may *not* modify global registers, unless the global
  register happens to be the destination register.
-}

trivialCode :: Width -> (Operand -> Operand -> Instr)
            -> Maybe (Operand -> Operand -> Instr)
            -> CmmExpr -> CmmExpr -> NatM Register
trivialCode width instr m a b
    = do platform <- getPlatform
         trivialCode' platform width instr m a b

trivialCode' :: Platform -> Width -> (Operand -> Operand -> Instr)
             -> Maybe (Operand -> Operand -> Instr)
             -> CmmExpr -> CmmExpr -> NatM Register
trivialCode' platform width _ (Just revinstr) (CmmLit lit_a) b
  | is32BitLit platform lit_a = do
  b_code <- getAnyReg b
  let
       code dst
         = b_code dst `snocOL`
           revinstr (OpImm (litToImm lit_a)) (OpReg dst)
  return (Any (intFormat width) code)

trivialCode' _ width instr _ a b
  = genTrivialCode (intFormat width) instr a b

-- This is re-used for floating pt instructions too.
genTrivialCode :: Format -> (Operand -> Operand -> Instr)
               -> CmmExpr -> CmmExpr -> NatM Register
genTrivialCode rep instr a b = do
  (b_op, b_code) <- getNonClobberedOperand b
  a_code <- getAnyReg a
  tmp <- getNewRegNat rep
  let
     -- We want the value of 'b' to stay alive across the computation of 'a'.
     -- But, we want to calculate 'a' straight into the destination register,
     -- because the instruction only has two operands (dst := dst `op` src).
     -- The troublesome case is when the result of 'b' is in the same register
     -- as the destination 'reg'.  In this case, we have to save 'b' in a
     -- new temporary across the computation of 'a'.
     code dst
        | dst `regClashesWithOp` b_op =
                b_code `appOL`
                unitOL (MOV rep b_op (OpReg tmp)) `appOL`
                a_code dst `snocOL`
                instr (OpReg tmp) (OpReg dst)
        | otherwise =
                b_code `appOL`
                a_code dst `snocOL`
                instr b_op (OpReg dst)
  return (Any rep code)

regClashesWithOp :: Reg -> Operand -> Bool
reg `regClashesWithOp` OpReg reg2   = reg == reg2
reg `regClashesWithOp` OpAddr amode = any (==reg) (addrModeRegs amode)
_   `regClashesWithOp` _            = False

-- | Generate code for a fused multiply-add operation, of the form @± x * y ± z@,
-- with 3 operands (FMA3 instruction set).
genFMA3Code :: Width
            -> FMASign
            -> CmmExpr -> CmmExpr -> CmmExpr -> NatM Register
genFMA3Code w signs x y z = do
  -- For the FMA instruction, we want to compute x * y + z
  --
  -- There are three possible instructions we could emit:
  --
  --   - fmadd213 z y x, result in x, z can be a memory address
  --   - fmadd132 x z y, result in y, x can be a memory address
  --   - fmadd231 y x z, result in z, y can be a memory address
  --
  -- This suggests two possible optimisations:
  --
  --   - OPTIMISATION 1
  --     If one argument is an address, use the instruction that allows
  --     a memory address in that position.
  --
  --   - OPTIMISATION 2
  --     If one argument is in a fixed register, use the instruction that puts
  --     the result in that same register.
  --
  -- Currently we follow neither of these optimisations,
  -- opting to always use fmadd213 for simplicity.
  --
  -- We would like to compute the result directly into the requested register.
  -- To do so we must first compute `x` into the destination register. This is
  -- only possible if the other arguments don't use the destination register.
  -- We check for this and if there is a conflict we move the result only after
  -- the computation. See #24496 how this went wrong in the past.
  let rep = floatFormat w
  (y_reg, y_code) <- getNonClobberedReg y
  (z_op, z_code) <- getNonClobberedOperand z
  x_code <- getAnyReg x
  x_tmp <- getNewRegNat rep
  let
     fma213 = FMA3 rep signs FMA213

     code, code_direct, code_mov :: Reg -> InstrBlock
     -- Ideal: Compute the result directly into dst
     code_direct dst = x_code  dst   `snocOL`
                       fma213 z_op y_reg dst
     -- Fallback: Compute the result into a tmp reg and then move it.
     code_mov dst    = x_code x_tmp `snocOL`
                       fma213 z_op y_reg x_tmp `snocOL`
                       MOV rep (OpReg x_tmp) (OpReg dst)

     code dst =
         y_code `appOL`
          z_code `appOL`
          ( if arg_regs_conflict then code_mov dst else code_direct dst )

      where

        arg_regs_conflict =
          y_reg == dst ||
          case z_op of
            OpReg z_reg -> z_reg == dst
            OpAddr amode -> dst `elem` addrModeRegs amode
            OpImm {} -> False

  -- NB: Computing the result into a desired register using Any can be tricky.
  -- So for now, we keep it simple. (See #24496).
  return (Any rep code)

-----------

trivialUCode :: Format -> (Operand -> Instr)
             -> CmmExpr -> NatM Register
trivialUCode rep instr x = do
  x_code <- getAnyReg x
  let
     code dst =
        x_code dst `snocOL`
        instr (OpReg dst)
  return (Any rep code)

-----------


trivialFCode_sse2 :: Width -> (Format -> Operand -> Operand -> Instr)
                  -> CmmExpr -> CmmExpr -> NatM Register
trivialFCode_sse2 pk instr x y
    = genTrivialCode format (instr format) x y
    where format = floatFormat pk


--------------------------------------------------------------------------------
coerceInt2FP :: Width -> Width -> CmmExpr -> NatM Register
coerceInt2FP from to x =  coerce_sse2
 where

   coerce_sse2 = do
     (x_op, x_code) <- getOperand x  -- ToDo: could be a safe operand
     let
           opc  = case to of W32 -> CVTSI2SS; W64 -> CVTSI2SD
                             n -> panic $ "coerceInt2FP.sse: unhandled width ("
                                         ++ show n ++ ")"
           code dst = x_code `snocOL` opc (intFormat from) x_op dst
     return (Any (floatFormat to) code)
        -- works even if the destination rep is <II32

--------------------------------------------------------------------------------
coerceFP2Int :: Width -> Width -> CmmExpr -> NatM Register
coerceFP2Int from to x =  coerceFP2Int_sse2
 where
   coerceFP2Int_sse2 = do
     (x_op, x_code) <- getOperand x  -- ToDo: could be a safe operand
     let
           opc  = case from of W32 -> CVTTSS2SIQ; W64 -> CVTTSD2SIQ;
                               n -> panic $ "coerceFP2Init.sse: unhandled width ("
                                           ++ show n ++ ")"
           code dst = x_code `snocOL` opc (intFormat to) x_op dst
     return (Any (intFormat to) code)
         -- works even if the destination rep is <II32


--------------------------------------------------------------------------------
coerceFP2FP :: Width -> CmmExpr -> NatM Register
coerceFP2FP to x = do
  (x_reg, x_code) <- getSomeReg x
  let
        opc  = case to of W32 -> CVTSD2SS; W64 -> CVTSS2SD;
                                     n -> panic $ "coerceFP2FP: unhandled width ("
                                                 ++ show n ++ ")"
        code dst = x_code `snocOL` opc x_reg dst
  return (Any ( floatFormat to) code)

--------------------------------------------------------------------------------

sse2NegCode :: Width -> CmmExpr -> NatM Register
sse2NegCode w x = do
  let fmt = floatFormat w
  x_code <- getAnyReg x
  -- This is how gcc does it, so it can't be that bad:
  let
    const = case fmt of
      FF32 -> CmmInt 0x80000000 W32
      FF64 -> CmmInt 0x8000000000000000 W64
      x@II8  -> wrongFmt x
      x@II16 -> wrongFmt x
      x@II32 -> wrongFmt x
      x@II64 -> wrongFmt x

      where
        wrongFmt x = panic $ "sse2NegCode: " ++ show x
  Amode amode amode_code <- memConstant (mkAlignment $ widthInBytes w) const
  tmp <- getNewRegNat fmt
  let
    code dst = x_code dst `appOL` amode_code `appOL` toOL [
        MOV fmt (OpAddr amode) (OpReg tmp),
        XOR fmt (OpReg tmp) (OpReg dst)
        ]
  --
  return (Any fmt code)

isVecExpr :: CmmExpr -> Bool
isVecExpr (CmmMachOp (MO_V_Insert {}) _)   = True
isVecExpr (CmmMachOp (MO_V_Extract {}) _)  = True
isVecExpr (CmmMachOp (MO_V_Add {}) _)      = True
isVecExpr (CmmMachOp (MO_V_Sub {}) _)      = True
isVecExpr (CmmMachOp (MO_V_Mul {}) _)      = True
isVecExpr (CmmMachOp (MO_VS_Quot {}) _)    = True
isVecExpr (CmmMachOp (MO_VS_Rem {}) _)     = True
isVecExpr (CmmMachOp (MO_VS_Neg {}) _)     = True
isVecExpr (CmmMachOp (MO_VF_Insert {}) _)  = True
isVecExpr (CmmMachOp (MO_VF_Extract {}) _) = True
isVecExpr (CmmMachOp (MO_VF_Add {}) _)     = True
isVecExpr (CmmMachOp (MO_VF_Sub {}) _)     = True
isVecExpr (CmmMachOp (MO_VF_Mul {}) _)     = True
isVecExpr (CmmMachOp (MO_VF_Quot {}) _)    = True
isVecExpr (CmmMachOp (MO_VF_Neg {}) _)     = True
isVecExpr (CmmMachOp _ [e])                = isVecExpr e
isVecExpr _                                = False

needLlvm :: NatM a
needLlvm =
    sorry $ unlines ["The native code generator does not support vector"
                    ,"instructions. Please use -fllvm."]

-- | This works on the invariant that all jumps in the given blocks are required.
--   Starting from there we try to make a few more jumps redundant by reordering
--   them.
--   We depend on the information in the CFG to do so so without a given CFG
--   we do nothing.
invertCondBranches :: Maybe CFG  -- ^ CFG if present
                   -> LabelMap a -- ^ Blocks with info tables
                   -> [NatBasicBlock Instr] -- ^ List of basic blocks
                   -> [NatBasicBlock Instr]
invertCondBranches Nothing _       bs = bs
invertCondBranches (Just cfg) keep bs =
    invert bs
  where
    invert :: [NatBasicBlock Instr] -> [NatBasicBlock Instr]
    invert (BasicBlock lbl1 ins:b2@(BasicBlock lbl2 _):bs)
      | --pprTrace "Block" (ppr lbl1) True,
        Just (jmp1,jmp2) <- last2 ins
      , JXX cond1 target1 <- jmp1
      , target1 == lbl2
      --, pprTrace "CutChance" (ppr b1) True
      , JXX ALWAYS target2 <- jmp2
      -- We have enough information to check if we can perform the inversion
      -- TODO: We could also check for the last asm instruction which sets
      -- status flags instead. Which I suspect is worse in terms of compiler
      -- performance, but might be applicable to more cases
      , Just edgeInfo1 <- getEdgeInfo lbl1 target1 cfg
      , Just edgeInfo2 <- getEdgeInfo lbl1 target2 cfg
      -- Both jumps come from the same cmm statement
      , transitionSource edgeInfo1 == transitionSource edgeInfo2
      , CmmSource {trans_cmmNode = cmmCondBranch} <- transitionSource edgeInfo1

      --Int comparisons are invertable
      , CmmCondBranch (CmmMachOp op _args) _ _ _ <- cmmCondBranch
      , Just _ <- maybeIntComparison op
      , Just invCond <- maybeInvertCond cond1

      --Swap the last two jumps, invert the conditional jumps condition.
      = let jumps =
              case () of
                -- We are free the eliminate the jmp. So we do so.
                _ | not (mapMember target1 keep)
                    -> [JXX invCond target2]
                -- If the conditional target is unlikely we put the other
                -- target at the front.
                  | edgeWeight edgeInfo2 > edgeWeight edgeInfo1
                    -> [JXX invCond target2, JXX ALWAYS target1]
                -- Keep things as-is otherwise
                  | otherwise
                    -> [jmp1, jmp2]
        in --pprTrace "Cutable" (ppr [jmp1,jmp2] <+> text "=>" <+> ppr jumps) $
           (BasicBlock lbl1
            (dropTail 2 ins ++ jumps))
            : invert (b2:bs)
    invert (b:bs) = b : invert bs
    invert [] = []

genAtomicRMW
  :: BlockId
  -> Width
  -> AtomicMachOp
  -> LocalReg
  -> CmmExpr
  -> CmmExpr
  -> NatM (InstrBlock, Maybe BlockId)
genAtomicRMW bid width amop dst addr n = do
    Amode amode addr_code <-
        if amop `elem` [AMO_Add, AMO_Sub]
        then getAmode addr
        else getSimpleAmode addr  -- See genForeignCall for MO_Cmpxchg
    arg <- getNewRegNat format
    arg_code <- getAnyReg n
    platform <- ncgPlatform <$> getConfig

    let dst_r    = getRegisterReg platform  (CmmLocal dst)
    (code, lbl) <- op_code dst_r arg amode
    return (addr_code `appOL` arg_code arg `appOL` code, Just lbl)
  where
    -- Code for the operation
    op_code :: Reg       -- Destination reg
            -> Reg       -- Register containing argument
            -> AddrMode  -- Address of location to mutate
            -> NatM (OrdList Instr,BlockId) -- TODO: Return Maybe BlockId
    op_code dst_r arg amode = case amop of
        -- In the common case where dst_r is a virtual register the
        -- final move should go away, because it's the last use of arg
        -- and the first use of dst_r.
        AMO_Add  -> return $ (toOL [ LOCK (XADD format (OpReg arg) (OpAddr amode))
                                   , MOV format (OpReg arg) (OpReg dst_r)
                                   ], bid)
        AMO_Sub  -> return $ (toOL [ NEGI format (OpReg arg)
                                   , LOCK (XADD format (OpReg arg) (OpAddr amode))
                                   , MOV format (OpReg arg) (OpReg dst_r)
                                   ], bid)
        -- In these cases we need a new block id, and have to return it so
        -- that later instruction selection can reference it.
        AMO_And  -> cmpxchg_code (\ src dst -> unitOL $ AND format src dst)
        AMO_Nand -> cmpxchg_code (\ src dst -> toOL [ AND format src dst
                                                    , NOT format dst
                                                    ])
        AMO_Or   -> cmpxchg_code (\ src dst -> unitOL $ OR format src dst)
        AMO_Xor  -> cmpxchg_code (\ src dst -> unitOL $ XOR format src dst)
      where
        -- Simulate operation that lacks a dedicated instruction using
        -- cmpxchg.
        cmpxchg_code :: (Operand -> Operand -> OrdList Instr)
                     -> NatM (OrdList Instr, BlockId)
        cmpxchg_code instrs = do
            lbl1 <- getBlockIdNat
            lbl2 <- getBlockIdNat
            tmp <- getNewRegNat format

            --Record inserted blocks
            --  We turn A -> B into A -> A' -> A'' -> B
            --  with a self loop on A'.
            addImmediateSuccessorNat bid lbl1
            addImmediateSuccessorNat lbl1 lbl2
            updateCfgNat (addWeightEdge lbl1 lbl1 0)

            return $ (toOL
                [ MOV format (OpAddr amode) (OpReg eax)
                , JXX ALWAYS lbl1
                , NEWBLOCK lbl1
                  -- Keep old value so we can return it:
                , MOV format (OpReg eax) (OpReg dst_r)
                , MOV format (OpReg eax) (OpReg tmp)
                ]
                `appOL` instrs (OpReg arg) (OpReg tmp) `appOL` toOL
                [ LOCK (CMPXCHG format (OpReg tmp) (OpAddr amode))
                , JXX NE lbl1
                -- See Note [Introducing cfg edges inside basic blocks]
                -- why this basic block is required.
                , JXX ALWAYS lbl2
                , NEWBLOCK lbl2
                ],
                lbl2)
    format = intFormat width

-- | Count trailing zeroes
genCtz :: BlockId -> Width -> LocalReg -> CmmExpr -> NatM (InstrBlock, Maybe BlockId)
genCtz bid width dst src = do
  is32Bit <- is32BitPlatform
  if is32Bit && width == W64
    then genCtz64_32 bid dst src
    else (,Nothing) <$> genCtzGeneric width dst src

-- | Count trailing zeroes
--
-- 64-bit width on 32-bit architecture
genCtz64_32
  :: BlockId
  -> LocalReg
  -> CmmExpr
  -> NatM (InstrBlock, Maybe BlockId)
genCtz64_32 bid dst src = do
  RegCode64 vcode rhi rlo <- iselExpr64 src
  let dst_r = getLocalRegReg dst
  lbl1 <- getBlockIdNat
  lbl2 <- getBlockIdNat
  tmp_r <- getNewRegNat II64

  -- New CFG Edges:
  --  bid -> lbl2
  --  bid -> lbl1 -> lbl2
  --  We also changes edges originating at bid to start at lbl2 instead.
  weights <- getCfgWeights
  updateCfgNat (addWeightEdge bid lbl1 110 .
                addWeightEdge lbl1 lbl2 110 .
                addImmediateSuccessor weights bid lbl2)

  -- The following instruction sequence corresponds to the pseudo-code
  --
  --  if (src) {
  --    dst = src.lo32 ? BSF(src.lo32) : (BSF(src.hi32) + 32);
  --  } else {
  --    dst = 64;
  --  }
  let instrs = vcode `appOL` toOL
           ([ MOV      II32 (OpReg rhi)         (OpReg tmp_r)
            , OR       II32 (OpReg rlo)         (OpReg tmp_r)
            , MOV      II32 (OpImm (ImmInt 64)) (OpReg dst_r)
            , JXX EQQ    lbl2
            , JXX ALWAYS lbl1

            , NEWBLOCK   lbl1
            , BSF     II32 (OpReg rhi)         dst_r
            , ADD     II32 (OpImm (ImmInt 32)) (OpReg dst_r)
            , BSF     II32 (OpReg rlo)         tmp_r
            , CMOV NE II32 (OpReg tmp_r)       dst_r
            , JXX ALWAYS lbl2

            , NEWBLOCK   lbl2
            ])
  return (instrs, Just lbl2)

-- | Count trailing zeroes
--
-- Generic case (width <= word size)
genCtzGeneric :: Width -> LocalReg -> CmmExpr -> NatM InstrBlock
genCtzGeneric width dst src = do
  code_src <- getAnyReg src
  config <- getConfig
  let bw = widthInBits width
  let dst_r = getLocalRegReg dst
  if ncgBmiVersion config >= Just BMI2
  then do
      src_r <- getNewRegNat (intFormat width)
      let instrs = appOL (code_src src_r) $ case width of
              W8 -> toOL
                  [ OR    II32 (OpImm (ImmInteger 0xFFFFFF00)) (OpReg src_r)
                  , TZCNT II32 (OpReg src_r) dst_r
                  ]
              W16 -> toOL
                  [ TZCNT  II16 (OpReg src_r) dst_r
                  , MOVZxL II16 (OpReg dst_r) (OpReg dst_r)
                  ]
              _ -> unitOL $ TZCNT (intFormat width) (OpReg src_r) dst_r
      return instrs
  else do
      -- The following insn sequence makes sure 'ctz 0' has a defined value.
      -- starting with Haswell, one could use the TZCNT insn instead.
      let format = if width == W8 then II16 else intFormat width
      src_r <- getNewRegNat format
      tmp_r <- getNewRegNat format
      let instrs = code_src src_r `appOL` toOL
               ([ MOVZxL  II8    (OpReg src_r) (OpReg src_r) | width == W8 ] ++
                [ BSF     format (OpReg src_r) tmp_r
                , MOV     II32   (OpImm (ImmInt bw)) (OpReg dst_r)
                , CMOV NE format (OpReg tmp_r) dst_r
                ]) -- NB: We don't need to zero-extend the result for the
                   -- W8/W16 cases because the 'MOV' insn already
                   -- took care of implicitly clearing the upper bits
      return instrs



-- | Copy memory
--
-- Unroll memcpy calls if the number of bytes to copy isn't too large (cf
-- ncgInlineThresholdMemcpy).  Otherwise, call C's memcpy.
genMemCpy
  :: BlockId
  -> Int
  -> CmmExpr
  -> CmmExpr
  -> CmmExpr
  -> NatM InstrBlock
genMemCpy bid align dst src arg_n = do

  let libc_memcpy = genLibCCall bid (fsLit "memcpy") [] [dst,src,arg_n]

  case arg_n of
    CmmLit (CmmInt n _) -> do
      -- try to inline it
      mcode <- genMemCpyInlineMaybe align dst src n
      -- if it didn't inline, call the C function
      case mcode of
        Nothing -> libc_memcpy
        Just c  -> pure c

    -- not a literal size argument: call the C function
    _ -> libc_memcpy



genMemCpyInlineMaybe
  :: Int
  -> CmmExpr
  -> CmmExpr
  -> Integer
  -> NatM (Maybe InstrBlock)
genMemCpyInlineMaybe align dst src n = do
  config <- getConfig
  let
    platform     = ncgPlatform config
    maxAlignment = wordAlignment platform
                   -- only machine word wide MOVs are supported
    effectiveAlignment = min (alignmentOf align) maxAlignment
    format = intFormat . widthFromBytes $ alignmentBytes effectiveAlignment


  -- The size of each move, in bytes.
  let sizeBytes :: Integer
      sizeBytes = fromIntegral (formatInBytes format)

  -- The number of instructions we will generate (approx). We need 2
  -- instructions per move.
  let insns = 2 * ((n + sizeBytes - 1) `div` sizeBytes)

      go :: Reg -> Reg -> Reg -> Integer -> OrdList Instr
      go dst src tmp i
          | i >= sizeBytes =
              unitOL (MOV format (OpAddr src_addr) (OpReg tmp)) `appOL`
              unitOL (MOV format (OpReg tmp) (OpAddr dst_addr)) `appOL`
              go dst src tmp (i - sizeBytes)
          -- Deal with remaining bytes.
          | i >= 4 =  -- Will never happen on 32-bit
              unitOL (MOV II32 (OpAddr src_addr) (OpReg tmp)) `appOL`
              unitOL (MOV II32 (OpReg tmp) (OpAddr dst_addr)) `appOL`
              go dst src tmp (i - 4)
          | i >= 2 =
              unitOL (MOVZxL II16 (OpAddr src_addr) (OpReg tmp)) `appOL`
              unitOL (MOV II16 (OpReg tmp) (OpAddr dst_addr)) `appOL`
              go dst src tmp (i - 2)
          | i >= 1 =
              unitOL (MOVZxL II8 (OpAddr src_addr) (OpReg tmp)) `appOL`
              unitOL (MOV II8 (OpReg tmp) (OpAddr dst_addr)) `appOL`
              go dst src tmp (i - 1)
          | otherwise = nilOL
        where
          src_addr = AddrBaseIndex (EABaseReg src) EAIndexNone
                       (ImmInteger (n - i))

          dst_addr = AddrBaseIndex (EABaseReg dst) EAIndexNone
                       (ImmInteger (n - i))

  if insns > fromIntegral (ncgInlineThresholdMemcpy config)
    then pure Nothing
    else do
      code_dst <- getAnyReg dst
      dst_r <- getNewRegNat format
      code_src <- getAnyReg src
      src_r <- getNewRegNat format
      tmp_r <- getNewRegNat format
      pure $ Just $ code_dst dst_r `appOL` code_src src_r `appOL`
                      go dst_r src_r tmp_r (fromInteger n)

-- | Set memory to the given byte
--
-- Unroll memset calls if the number of bytes to copy isn't too large (cf
-- ncgInlineThresholdMemset).  Otherwise, call C's memset.
genMemSet
  :: BlockId
  -> Int
  -> CmmExpr
  -> CmmExpr
  -> CmmExpr
  -> NatM InstrBlock
genMemSet bid align dst arg_c arg_n = do

  let libc_memset = genLibCCall bid (fsLit "memset") [] [dst,arg_c,arg_n]

  case (arg_c,arg_n) of
    (CmmLit (CmmInt c _), CmmLit (CmmInt n _)) -> do
      -- try to inline it
      mcode <- genMemSetInlineMaybe align dst c n
      -- if it didn't inline, call the C function
      case mcode of
        Nothing -> libc_memset
        Just c  -> pure c

    -- not literal size arguments: call the C function
    _ -> libc_memset

genMemSetInlineMaybe
  :: Int
  -> CmmExpr
  -> Integer
  -> Integer
  -> NatM (Maybe InstrBlock)
genMemSetInlineMaybe align dst c n = do
  config <- getConfig
  let
    platform = ncgPlatform config
    maxAlignment = wordAlignment platform -- only machine word wide MOVs are supported
    effectiveAlignment = min (alignmentOf align) maxAlignment
    format = intFormat . widthFromBytes $ alignmentBytes effectiveAlignment
    c2 = c `shiftL` 8 .|. c
    c4 = c2 `shiftL` 16 .|. c2
    c8 = c4 `shiftL` 32 .|. c4

    -- The number of instructions we will generate (approx). We need 1
    -- instructions per move.
    insns = (n + sizeBytes - 1) `div` sizeBytes

    -- The size of each move, in bytes.
    sizeBytes :: Integer
    sizeBytes = fromIntegral (formatInBytes format)

    -- Depending on size returns the widest MOV instruction and its
    -- width.
    gen4 :: AddrMode -> Integer -> (InstrBlock, Integer)
    gen4 addr size
        | size >= 4 =
            (unitOL (MOV II32 (OpImm (ImmInteger c4)) (OpAddr addr)), 4)
        | size >= 2 =
            (unitOL (MOV II16 (OpImm (ImmInteger c2)) (OpAddr addr)), 2)
        | size >= 1 =
            (unitOL (MOV II8 (OpImm (ImmInteger c)) (OpAddr addr)), 1)
        | otherwise = (nilOL, 0)

    -- Generates a 64-bit wide MOV instruction from REG to MEM.
    gen8 :: AddrMode -> Reg -> InstrBlock
    gen8 addr reg8byte =
      unitOL (MOV format (OpReg reg8byte) (OpAddr addr))

    -- Unrolls memset when the widest MOV is <= 4 bytes.
    go4 :: Reg -> Integer -> InstrBlock
    go4 dst left =
      if left <= 0 then nilOL
      else curMov `appOL` go4 dst (left - curWidth)
      where
        possibleWidth = minimum [left, sizeBytes]
        dst_addr = AddrBaseIndex (EABaseReg dst) EAIndexNone (ImmInteger (n - left))
        (curMov, curWidth) = gen4 dst_addr possibleWidth

    -- Unrolls memset when the widest MOV is 8 bytes (thus another Reg
    -- argument). Falls back to go4 when all 8 byte moves are
    -- exhausted.
    go8 :: Reg -> Reg -> Integer -> InstrBlock
    go8 dst reg8byte left =
      if possibleWidth >= 8 then
        let curMov = gen8 dst_addr reg8byte
        in  curMov `appOL` go8 dst reg8byte (left - 8)
      else go4 dst left
      where
        possibleWidth = minimum [left, sizeBytes]
        dst_addr = AddrBaseIndex (EABaseReg dst) EAIndexNone (ImmInteger (n - left))

  if fromInteger insns > ncgInlineThresholdMemset config
    then pure Nothing
    else do
        code_dst <- getAnyReg dst
        dst_r <- getNewRegNat format
        if format == II64 && n >= 8
          then do
            code_imm8byte <- getAnyReg (CmmLit (CmmInt c8 W64))
            imm8byte_r <- getNewRegNat II64
            return $ Just $ code_dst dst_r `appOL`
                              code_imm8byte imm8byte_r `appOL`
                              go8 dst_r imm8byte_r (fromInteger n)
          else
            return $ Just $ code_dst dst_r `appOL`
                              go4 dst_r (fromInteger n)


genMemMove :: BlockId -> p -> CmmActual -> CmmActual -> CmmActual -> NatM InstrBlock
genMemMove bid _align dst src n = do
  -- TODO: generate inline assembly when under a given threshold (similarly to
  -- memcpy and memset)
  genLibCCall bid (fsLit "memmove") [] [dst,src,n]

genMemCmp :: BlockId -> p -> CmmFormal -> CmmActual -> CmmActual -> CmmActual -> NatM InstrBlock
genMemCmp bid _align res dst src n = do
  -- TODO: generate inline assembly when under a given threshold (similarly to
  -- memcpy and memset)
  genLibCCall bid (fsLit "memcmp") [res] [dst,src,n]

genPrefetchData :: Int -> CmmExpr -> NatM (OrdList Instr)
genPrefetchData n src = do
  is32Bit <- is32BitPlatform
  let
    format = archWordFormat is32Bit
    -- need to know what register width for pointers!
    genPrefetch inRegSrc prefetchCTor = do
      code_src <- getAnyReg inRegSrc
      src_r <- getNewRegNat format
      return $ code_src src_r `appOL`
        (unitOL (prefetchCTor  (OpAddr
                    ((AddrBaseIndex (EABaseReg src_r )   EAIndexNone (ImmInt 0))))  ))
        -- prefetch always takes an address

  -- the c / llvm prefetch convention is 0, 1, 2, and 3
  -- the x86 corresponding names are : NTA, 2 , 1, and 0
  case n of
      0 -> genPrefetch src $ PREFETCH NTA  format
      1 -> genPrefetch src $ PREFETCH Lvl2 format
      2 -> genPrefetch src $ PREFETCH Lvl1 format
      3 -> genPrefetch src $ PREFETCH Lvl0 format
      l -> pprPanic "genPrefetchData: unexpected prefetch level" (ppr l)

genByteSwap :: Width -> LocalReg -> CmmExpr -> NatM InstrBlock
genByteSwap width dst src = do
  is32Bit <- is32BitPlatform
  let format = intFormat width
  case width of
      W64 | is32Bit -> do
        let Reg64 dst_hi dst_lo = localReg64 dst
        RegCode64 vcode rhi rlo <- iselExpr64 src
        return $ vcode `appOL`
                 toOL [ MOV II32 (OpReg rlo) (OpReg dst_hi),
                        MOV II32 (OpReg rhi) (OpReg dst_lo),
                        BSWAP II32 dst_hi,
                        BSWAP II32 dst_lo ]
      W16 -> do
        let dst_r = getLocalRegReg dst
        code_src <- getAnyReg src
        return $ code_src dst_r `appOL`
                 unitOL (BSWAP II32 dst_r) `appOL`
                 unitOL (SHR II32 (OpImm $ ImmInt 16) (OpReg dst_r))
      _   -> do
        let dst_r = getLocalRegReg dst
        code_src <- getAnyReg src
        return $ code_src dst_r `appOL` unitOL (BSWAP format dst_r)

genBitRev :: BlockId -> Width -> CmmFormal -> CmmActual -> NatM InstrBlock
genBitRev bid width dst src = do
  -- Here the C implementation (hs_bitrevN) is used as there is no x86
  -- instruction to reverse a word's bit order.
  genPrimCCall bid (bRevLabel width) [dst] [src]

genPopCnt :: BlockId -> Width -> LocalReg -> CmmExpr -> NatM InstrBlock
genPopCnt bid width dst src = do
  config <- getConfig
  let
    platform = ncgPlatform config
    format = intFormat width

  sse4_2Enabled >>= \case

    True -> do
      code_src <- getAnyReg src
      src_r <- getNewRegNat format
      let dst_r = getRegisterReg platform  (CmmLocal dst)
      return $ code_src src_r `appOL`
          (if width == W8 then
               -- The POPCNT instruction doesn't take a r/m8
               unitOL (MOVZxL II8 (OpReg src_r) (OpReg src_r)) `appOL`
               unitOL (POPCNT II16 (OpReg src_r) dst_r)
           else
               unitOL (POPCNT format (OpReg src_r) dst_r)) `appOL`
          (if width == W8 || width == W16 then
               -- We used a 16-bit destination register above,
               -- so zero-extend
               unitOL (MOVZxL II16 (OpReg dst_r) (OpReg dst_r))
           else nilOL)

    False ->
      -- generate C call to hs_popcntN in ghc-prim
      -- TODO: we could directly generate the assembly to index popcount_tab
      -- here instead of doing it by calling a C function
      genPrimCCall bid (popCntLabel width) [dst] [src]


genPdep :: BlockId -> Width -> LocalReg -> CmmExpr -> CmmExpr -> NatM InstrBlock
genPdep bid width dst src mask = do
  config <- getConfig
  let
    platform = ncgPlatform config
    format = intFormat width

  if ncgBmiVersion config >= Just BMI2
    then do
      code_src  <- getAnyReg src
      code_mask <- getAnyReg mask
      src_r     <- getNewRegNat format
      mask_r    <- getNewRegNat format
      let dst_r = getRegisterReg platform  (CmmLocal dst)
      return $ code_src src_r `appOL` code_mask mask_r `appOL`
          -- PDEP only supports > 32 bit args
          ( if width == W8 || width == W16 then
              toOL
                [ MOVZxL format (OpReg src_r ) (OpReg src_r )
                , MOVZxL format (OpReg mask_r) (OpReg mask_r)
                , PDEP   II32 (OpReg mask_r) (OpReg src_r ) dst_r
                , MOVZxL format (OpReg dst_r) (OpReg dst_r) -- Truncate to op width
                ]
            else
              unitOL (PDEP format (OpReg mask_r) (OpReg src_r) dst_r)
          )
    else
      -- generate C call to hs_pdepN in ghc-prim
      genPrimCCall bid (pdepLabel width) [dst] [src,mask]


genPext :: BlockId -> Width -> LocalReg -> CmmExpr -> CmmExpr -> NatM InstrBlock
genPext bid width dst src mask = do
  config <- getConfig
  if ncgBmiVersion config >= Just BMI2
    then do
      let format   = intFormat width
      let dst_r    = getLocalRegReg dst
      code_src  <- getAnyReg src
      code_mask <- getAnyReg mask
      src_r     <- getNewRegNat format
      mask_r    <- getNewRegNat format
      return $ code_src src_r `appOL` code_mask mask_r `appOL`
          (if width == W8 || width == W16 then
               -- The PEXT instruction doesn't take a r/m8 or 16
              toOL
                [ MOVZxL format (OpReg src_r ) (OpReg src_r )
                , MOVZxL format (OpReg mask_r) (OpReg mask_r)
                , PEXT   II32 (OpReg mask_r) (OpReg src_r ) dst_r
                , MOVZxL format (OpReg dst_r) (OpReg dst_r) -- Truncate to op width
                ]
            else
              unitOL (PEXT format (OpReg mask_r) (OpReg src_r) dst_r)
          )
    else
      -- generate C call to hs_pextN in ghc-prim
      genPrimCCall bid (pextLabel width) [dst] [src,mask]

genClz :: BlockId -> Width -> CmmFormal -> CmmActual -> NatM InstrBlock
genClz bid width dst src = do
  is32Bit <- is32BitPlatform
  config <- getConfig
  if is32Bit && width == W64

    then
      -- Fallback to `hs_clz64` on i386
      genPrimCCall bid (clzLabel width) [dst] [src]

    else do
      code_src <- getAnyReg src
      let dst_r = getLocalRegReg dst
      if ncgBmiVersion config >= Just BMI2
        then do
          src_r <- getNewRegNat (intFormat width)
          return $ appOL (code_src src_r) $ case width of
            W8 -> toOL
                [ MOVZxL II8  (OpReg src_r)       (OpReg src_r) -- zero-extend to 32 bit
                , LZCNT  II32 (OpReg src_r)       dst_r         -- lzcnt with extra 24 zeros
                , SUB    II32 (OpImm (ImmInt 24)) (OpReg dst_r) -- compensate for extra zeros
                ]
            W16 -> toOL
                [ LZCNT  II16 (OpReg src_r) dst_r
                , MOVZxL II16 (OpReg dst_r) (OpReg dst_r) -- zero-extend from 16 bit
                ]
            _ -> unitOL (LZCNT (intFormat width) (OpReg src_r) dst_r)
        else do
          let format = if width == W8 then II16 else intFormat width
          let bw = widthInBits width
          src_r <- getNewRegNat format
          tmp_r <- getNewRegNat format
          return $ code_src src_r `appOL` toOL
                   ([ MOVZxL  II8    (OpReg src_r) (OpReg src_r) | width == W8 ] ++
                    [ BSR     format (OpReg src_r) tmp_r
                    , MOV     II32   (OpImm (ImmInt (2*bw-1))) (OpReg dst_r)
                    , CMOV NE format (OpReg tmp_r) dst_r
                    , XOR     format (OpImm (ImmInt (bw-1))) (OpReg dst_r)
                    ]) -- NB: We don't need to zero-extend the result for the
                       -- W8/W16 cases because the 'MOV' insn already
                       -- took care of implicitly clearing the upper bits

genWordToFloat :: BlockId -> Width -> CmmFormal -> CmmActual -> NatM InstrBlock
genWordToFloat bid width dst src =
  -- TODO: generate assembly instead
  genPrimCCall bid (word2FloatLabel width) [dst] [src]

genAtomicRead :: Width -> MemoryOrdering -> LocalReg -> CmmExpr -> NatM InstrBlock
genAtomicRead width _mord dst addr = do
  load_code <- intLoadCode (MOV (intFormat width)) addr
  return (load_code (getLocalRegReg dst))

genAtomicWrite :: Width -> MemoryOrdering -> CmmExpr -> CmmExpr -> NatM InstrBlock
genAtomicWrite width mord addr val = do
  code <- assignMem_IntCode (intFormat width) addr val
  let needs_fence = case mord of
        MemOrderSeqCst  -> True
        MemOrderRelease -> False
        MemOrderAcquire -> pprPanic "genAtomicWrite: acquire ordering on write" empty
        MemOrderRelaxed -> False
  return $ if needs_fence then code `snocOL` MFENCE else code

genCmpXchg
  :: BlockId
  -> Width
  -> LocalReg
  -> CmmExpr
  -> CmmExpr
  -> CmmExpr
  -> NatM InstrBlock
genCmpXchg bid width dst addr old new = do
  is32Bit <- is32BitPlatform
  -- On x86 we don't have enough registers to use cmpxchg with a
  -- complicated addressing mode, so on that architecture we
  -- pre-compute the address first.
  if not (is32Bit && width == W64)
    then do
      let format = intFormat width
      Amode amode addr_code <- getSimpleAmode addr
      newval <- getNewRegNat format
      newval_code <- getAnyReg new
      oldval <- getNewRegNat format
      oldval_code <- getAnyReg old
      platform <- getPlatform
      let dst_r    = getRegisterReg platform  (CmmLocal dst)
          code     = toOL
                     [ MOV format (OpReg oldval) (OpReg eax)
                     , LOCK (CMPXCHG format (OpReg newval) (OpAddr amode))
                     , MOV format (OpReg eax) (OpReg dst_r)
                     ]
      return $ addr_code `appOL` newval_code newval `appOL` oldval_code oldval
          `appOL` code
    else
      -- generate C call to hs_cmpxchgN in ghc-prim
      genPrimCCall bid (cmpxchgLabel width) [dst] [addr,old,new]
      -- TODO: implement cmpxchg8b instruction

genXchg :: Width -> LocalReg -> CmmExpr -> CmmExpr -> NatM InstrBlock
genXchg width dst addr value = do
  is32Bit <- is32BitPlatform

  when (is32Bit && width == W64) $
    panic "genXchg: 64bit atomic exchange not supported on 32bit platforms"

  Amode amode addr_code <- getSimpleAmode addr
  (newval, newval_code) <- getSomeReg value
  let format   = intFormat width
  let dst_r    = getLocalRegReg dst
  -- Copy the value into the target register, perform the exchange.
  let code     = toOL
                 [ MOV format (OpReg newval) (OpReg dst_r)
                  -- On X86 xchg implies a lock prefix if we use a memory argument.
                  -- so this is atomic.
                 , XCHG format (OpAddr amode) dst_r
                 ]
  return $ addr_code `appOL` newval_code `appOL` code


genFloatAbs :: Width -> LocalReg -> CmmExpr -> NatM InstrBlock
genFloatAbs width dst src = do
  let
    format = floatFormat width
    const = case width of
      W32 -> CmmInt 0x7fffffff W32
      W64 -> CmmInt 0x7fffffffffffffff W64
      _   -> pprPanic "genFloatAbs: invalid width" (ppr width)
  src_code <- getAnyReg src
  Amode amode amode_code <- memConstant (mkAlignment $ widthInBytes width) const
  tmp <- getNewRegNat format
  let dst_r = getLocalRegReg dst
  pure $ src_code dst_r `appOL` amode_code `appOL` toOL
           [ MOV format (OpAddr amode) (OpReg tmp)
           , AND format (OpReg tmp) (OpReg dst_r)
           ]


genFloatSqrt :: Format -> LocalReg -> CmmExpr -> NatM InstrBlock
genFloatSqrt format dst src = do
  let dst_r = getLocalRegReg dst
  src_code <- getAnyReg src
  pure $ src_code dst_r `snocOL` SQRT format (OpReg dst_r) dst_r


genAddSubRetCarry
  :: Width
  -> (Format -> Operand -> Operand -> Instr)
  -> (Format -> Maybe (Operand -> Operand -> Instr))
  -> Cond
  -> LocalReg
  -> LocalReg
  -> CmmExpr
  -> CmmExpr
  -> NatM InstrBlock
genAddSubRetCarry width instr mrevinstr cond res_r res_c arg_x arg_y = do
  platform <- ncgPlatform <$> getConfig
  let format = intFormat width
  rCode <- anyReg =<< trivialCode width (instr format)
                        (mrevinstr format) arg_x arg_y
  reg_tmp <- getNewRegNat II8
  let reg_c = getRegisterReg platform  (CmmLocal res_c)
      reg_r = getRegisterReg platform  (CmmLocal res_r)
      code = rCode reg_r `snocOL`
             SETCC cond (OpReg reg_tmp) `snocOL`
             MOVZxL II8 (OpReg reg_tmp) (OpReg reg_c)
  return code


genAddWithCarry
  :: Width
  -> LocalReg
  -> LocalReg
  -> CmmExpr
  -> CmmExpr
  -> NatM InstrBlock
genAddWithCarry width res_h res_l arg_x arg_y = do
  hCode <- getAnyReg (CmmLit (CmmInt 0 width))
  let format = intFormat width
  lCode <- anyReg =<< trivialCode width (ADD_CC format)
                        (Just (ADD_CC format)) arg_x arg_y
  let reg_l = getLocalRegReg res_l
      reg_h = getLocalRegReg res_h
      code = hCode reg_h `appOL`
             lCode reg_l `snocOL`
             ADC format (OpImm (ImmInteger 0)) (OpReg reg_h)
  return code


genSignedLargeMul
  :: Width
  -> LocalReg
  -> LocalReg
  -> LocalReg
  -> CmmExpr
  -> CmmExpr
  -> NatM (OrdList Instr)
genSignedLargeMul width res_c res_h res_l arg_x arg_y = do
  (y_reg, y_code) <- getRegOrMem arg_y
  x_code <- getAnyReg arg_x
  reg_tmp <- getNewRegNat II8
  let format = intFormat width
      reg_h = getLocalRegReg res_h
      reg_l = getLocalRegReg res_l
      reg_c = getLocalRegReg res_c
      code = y_code `appOL`
             x_code rax `appOL`
             toOL [ IMUL2 format y_reg
                  , MOV format (OpReg rdx) (OpReg reg_h)
                  , MOV format (OpReg rax) (OpReg reg_l)
                  , SETCC CARRY (OpReg reg_tmp)
                  , MOVZxL II8 (OpReg reg_tmp) (OpReg reg_c)
                  ]
  return code

genUnsignedLargeMul
  :: Width
  -> LocalReg
  -> LocalReg
  -> CmmExpr
  -> CmmExpr
  -> NatM (OrdList Instr)
genUnsignedLargeMul width res_h res_l arg_x arg_y = do
  (y_reg, y_code) <- getRegOrMem arg_y
  x_code <- getAnyReg arg_x
  let format = intFormat width
      reg_h = getLocalRegReg res_h
      reg_l = getLocalRegReg res_l
      code = y_code `appOL`
             x_code rax `appOL`
             toOL [MUL2 format y_reg,
                   MOV format (OpReg rdx) (OpReg reg_h),
                   MOV format (OpReg rax) (OpReg reg_l)]
  return code


genQuotRem
  :: Width
  -> Bool
  -> LocalReg
  -> LocalReg
  -> Maybe CmmExpr
  -> CmmExpr
  -> CmmExpr
  -> NatM InstrBlock
genQuotRem width signed res_q res_r m_arg_x_high arg_x_low arg_y = do
  case width of
    W8 -> do
      -- See Note [DIV/IDIV for bytes]
      let widen | signed = MO_SS_Conv W8 W16
                | otherwise = MO_UU_Conv W8 W16
          arg_x_low_16 = CmmMachOp widen [arg_x_low]
          arg_y_16 = CmmMachOp widen [arg_y]
          m_arg_x_high_16 = (\p -> CmmMachOp widen [p]) <$> m_arg_x_high
      genQuotRem W16 signed res_q res_r m_arg_x_high_16 arg_x_low_16 arg_y_16

    _ -> do
      let format = intFormat width
          reg_q = getLocalRegReg res_q
          reg_r = getLocalRegReg res_r
          widen | signed    = CLTD format
                | otherwise = XOR format (OpReg rdx) (OpReg rdx)
          instr | signed    = IDIV
                | otherwise = DIV
      (y_reg, y_code) <- getRegOrMem arg_y
      x_low_code <- getAnyReg arg_x_low
      x_high_code <- case m_arg_x_high of
                     Just arg_x_high ->
                         getAnyReg arg_x_high
                     Nothing ->
                         return $ const $ unitOL widen
      return $ y_code `appOL`
               x_low_code rax `appOL`
               x_high_code rdx `appOL`
               toOL [instr format y_reg,
                     MOV format (OpReg rax) (OpReg reg_q),
                     MOV format (OpReg rdx) (OpReg reg_r)]


----------------------------------------------------------------------------
-- The following functions implement certain 64-bit MachOps inline for 32-bit
-- architectures. On 64-bit architectures, those MachOps aren't supported and
-- calling these functions for a 64-bit target platform is considered an error
-- (hence the use of `expect32BitPlatform`).
--
-- On 64-bit platforms, generic MachOps should be used instead of these 64-bit
-- specific ones (e.g. use MO_Add instead of MO_x64_Add). This MachOp selection
-- is done by StgToCmm.

genInt64ToInt :: LocalReg -> CmmExpr -> NatM InstrBlock
genInt64ToInt dst src = do
  expect32BitPlatform (text "genInt64ToInt")
  RegCode64 code _src_hi src_lo <- iselExpr64 src
  let dst_r = getLocalRegReg dst
  pure $ code `snocOL` MOV II32 (OpReg src_lo) (OpReg dst_r)

genWord64ToWord :: LocalReg -> CmmExpr -> NatM InstrBlock
genWord64ToWord dst src = do
  expect32BitPlatform (text "genWord64ToWord")
  RegCode64 code _src_hi src_lo <- iselExpr64 src
  let dst_r = getLocalRegReg dst
  pure $ code `snocOL` MOV II32 (OpReg src_lo) (OpReg dst_r)

genIntToInt64 :: LocalReg -> CmmExpr -> NatM InstrBlock
genIntToInt64 dst src = do
  expect32BitPlatform (text "genIntToInt64")
  let Reg64 dst_hi dst_lo = localReg64 dst
  src_code <- getAnyReg src
  pure $ src_code rax `appOL` toOL
          [ CLTD II32 -- sign extend EAX in EDX:EAX
          , MOV II32 (OpReg rax) (OpReg dst_lo)
          , MOV II32 (OpReg rdx) (OpReg dst_hi)
          ]

genWordToWord64 :: LocalReg -> CmmExpr -> NatM InstrBlock
genWordToWord64 dst src = do
  expect32BitPlatform (text "genWordToWord64")
  let Reg64 dst_hi dst_lo = localReg64 dst
  src_code <- getAnyReg src
  pure $ src_code dst_lo
          `snocOL` XOR II32 (OpReg dst_hi) (OpReg dst_hi)

genNeg64 :: LocalReg -> CmmExpr -> NatM InstrBlock
genNeg64 dst src = do
  expect32BitPlatform (text "genNeg64")
  let Reg64 dst_hi dst_lo = localReg64 dst
  RegCode64 code src_hi src_lo <- iselExpr64 src
  pure $ code `appOL` toOL
          [ MOV  II32 (OpReg src_lo) (OpReg dst_lo)
          , MOV  II32 (OpReg src_hi) (OpReg dst_hi)
          , NEGI II32 (OpReg dst_lo)
          , ADC  II32 (OpImm (ImmInt 0)) (OpReg dst_hi)
          , NEGI II32 (OpReg dst_hi)
          ]

genAdd64 :: LocalReg -> CmmExpr -> CmmExpr -> NatM InstrBlock
genAdd64 dst x y = do
  expect32BitPlatform (text "genAdd64")
  let Reg64 dst_hi dst_lo = localReg64 dst
  RegCode64 x_code x_hi x_lo <- iselExpr64 x
  RegCode64 y_code y_hi y_lo <- iselExpr64 y
  pure $ x_code `appOL` y_code `appOL` toOL
          [ MOV  II32 (OpReg x_lo) (OpReg dst_lo)
          , MOV  II32 (OpReg x_hi) (OpReg dst_hi)
          , ADD  II32 (OpReg y_lo) (OpReg dst_lo)
          , ADC  II32 (OpReg y_hi) (OpReg dst_hi)
          ]

genSub64 :: LocalReg -> CmmExpr -> CmmExpr -> NatM InstrBlock
genSub64 dst x y = do
  expect32BitPlatform (text "genSub64")
  let Reg64 dst_hi dst_lo = localReg64 dst
  RegCode64 x_code x_hi x_lo <- iselExpr64 x
  RegCode64 y_code y_hi y_lo <- iselExpr64 y
  pure $ x_code `appOL` y_code `appOL` toOL
          [ MOV  II32 (OpReg x_lo) (OpReg dst_lo)
          , MOV  II32 (OpReg x_hi) (OpReg dst_hi)
          , SUB  II32 (OpReg y_lo) (OpReg dst_lo)
          , SBB  II32 (OpReg y_hi) (OpReg dst_hi)
          ]

genAnd64 :: LocalReg -> CmmExpr -> CmmExpr -> NatM InstrBlock
genAnd64 dst x y = do
  expect32BitPlatform (text "genAnd64")
  let Reg64 dst_hi dst_lo = localReg64 dst
  RegCode64 x_code x_hi x_lo <- iselExpr64 x
  RegCode64 y_code y_hi y_lo <- iselExpr64 y
  pure $ x_code `appOL` y_code `appOL` toOL
          [ MOV II32 (OpReg x_lo) (OpReg dst_lo)
          , MOV II32 (OpReg x_hi) (OpReg dst_hi)
          , AND II32 (OpReg y_lo) (OpReg dst_lo)
          , AND II32 (OpReg y_hi) (OpReg dst_hi)
          ]

genOr64 :: LocalReg -> CmmExpr -> CmmExpr -> NatM InstrBlock
genOr64 dst x y = do
  expect32BitPlatform (text "genOr64")
  let Reg64 dst_hi dst_lo = localReg64 dst
  RegCode64 x_code x_hi x_lo <- iselExpr64 x
  RegCode64 y_code y_hi y_lo <- iselExpr64 y
  pure $ x_code `appOL` y_code `appOL` toOL
          [ MOV II32 (OpReg x_lo) (OpReg dst_lo)
          , MOV II32 (OpReg x_hi) (OpReg dst_hi)
          , OR  II32 (OpReg y_lo) (OpReg dst_lo)
          , OR  II32 (OpReg y_hi) (OpReg dst_hi)
          ]

genXor64 :: LocalReg -> CmmExpr -> CmmExpr -> NatM InstrBlock
genXor64 dst x y = do
  expect32BitPlatform (text "genXor64")
  let Reg64 dst_hi dst_lo = localReg64 dst
  RegCode64 x_code x_hi x_lo <- iselExpr64 x
  RegCode64 y_code y_hi y_lo <- iselExpr64 y
  pure $ x_code `appOL` y_code `appOL` toOL
          [ MOV II32 (OpReg x_lo) (OpReg dst_lo)
          , MOV II32 (OpReg x_hi) (OpReg dst_hi)
          , XOR II32 (OpReg y_lo) (OpReg dst_lo)
          , XOR II32 (OpReg y_hi) (OpReg dst_hi)
          ]

genNot64 :: LocalReg -> CmmExpr -> NatM InstrBlock
genNot64 dst src = do
  expect32BitPlatform (text "genNot64")
  let Reg64 dst_hi dst_lo = localReg64 dst
  RegCode64 src_code src_hi src_lo <- iselExpr64 src
  pure $ src_code `appOL` toOL
          [ MOV II32 (OpReg src_lo) (OpReg dst_lo)
          , MOV II32 (OpReg src_hi) (OpReg dst_hi)
          , NOT II32 (OpReg dst_lo)
          , NOT II32 (OpReg dst_hi)
          ]

genEq64 :: LocalReg -> CmmExpr -> CmmExpr -> NatM InstrBlock
genEq64 dst x y = do
  expect32BitPlatform (text "genEq64")
  let dst_r = getLocalRegReg dst
  RegCode64 x_code x_hi x_lo <- iselExpr64 x
  RegCode64 y_code y_hi y_lo <- iselExpr64 y
  Reg64 tmp_hi tmp_lo <- getNewReg64
  pure $ x_code `appOL` y_code `appOL` toOL
          [ MOV II32 (OpReg x_lo)   (OpReg tmp_lo)
          , MOV II32 (OpReg x_hi)   (OpReg tmp_hi)
          , XOR II32 (OpReg y_lo)   (OpReg tmp_lo)
          , XOR II32 (OpReg y_hi)   (OpReg tmp_hi)
          , OR  II32 (OpReg tmp_lo) (OpReg tmp_hi)
          , SETCC EQQ (OpReg dst_r)
          , MOVZxL II8 (OpReg dst_r) (OpReg dst_r)
          ]

genNe64 :: LocalReg -> CmmExpr -> CmmExpr -> NatM InstrBlock
genNe64 dst x y = do
  expect32BitPlatform (text "genNe64")
  let dst_r = getLocalRegReg dst
  RegCode64 x_code x_hi x_lo <- iselExpr64 x
  RegCode64 y_code y_hi y_lo <- iselExpr64 y
  Reg64 tmp_hi tmp_lo <- getNewReg64
  pure $ x_code `appOL` y_code `appOL` toOL
          [ MOV II32 (OpReg x_lo)   (OpReg tmp_lo)
          , MOV II32 (OpReg x_hi)   (OpReg tmp_hi)
          , XOR II32 (OpReg y_lo)   (OpReg tmp_lo)
          , XOR II32 (OpReg y_hi)   (OpReg tmp_hi)
          , OR  II32 (OpReg tmp_lo) (OpReg tmp_hi)
          , SETCC NE (OpReg dst_r)
          , MOVZxL II8 (OpReg dst_r) (OpReg dst_r)
          ]

genGtWord64 :: LocalReg -> CmmExpr -> CmmExpr -> NatM InstrBlock
genGtWord64 dst x y = do
  expect32BitPlatform (text "genGtWord64")
  genPred64 LU dst y x

genLtWord64 :: LocalReg -> CmmExpr -> CmmExpr -> NatM InstrBlock
genLtWord64 dst x y = do
  expect32BitPlatform (text "genLtWord64")
  genPred64 LU dst x y

genGeWord64 :: LocalReg -> CmmExpr -> CmmExpr -> NatM InstrBlock
genGeWord64 dst x y = do
  expect32BitPlatform (text "genGeWord64")
  genPred64 GEU dst x y

genLeWord64 :: LocalReg -> CmmExpr -> CmmExpr -> NatM InstrBlock
genLeWord64 dst x y = do
  expect32BitPlatform (text "genLeWord64")
  genPred64 GEU dst y x

genGtInt64 :: LocalReg -> CmmExpr -> CmmExpr -> NatM InstrBlock
genGtInt64 dst x y = do
  expect32BitPlatform (text "genGtInt64")
  genPred64 LTT dst y x

genLtInt64 :: LocalReg -> CmmExpr -> CmmExpr -> NatM InstrBlock
genLtInt64 dst x y = do
  expect32BitPlatform (text "genLtInt64")
  genPred64 LTT dst x y

genGeInt64 :: LocalReg -> CmmExpr -> CmmExpr -> NatM InstrBlock
genGeInt64 dst x y = do
  expect32BitPlatform (text "genGeInt64")
  genPred64 GE dst x y

genLeInt64 :: LocalReg -> CmmExpr -> CmmExpr -> NatM InstrBlock
genLeInt64 dst x y = do
  expect32BitPlatform (text "genLeInt64")
  genPred64 GE dst y x

genPred64 :: Cond -> LocalReg -> CmmExpr -> CmmExpr -> NatM InstrBlock
genPred64 cond dst x y = do
  -- we can only rely on CF/SF/OF flags!
  -- Not on ZF, which doesn't take into account the lower parts.
  massert (cond `elem` [LU,GEU,LTT,GE])

  let dst_r = getLocalRegReg dst
  RegCode64 x_code x_hi x_lo <- iselExpr64 x
  RegCode64 y_code y_hi y_lo <- iselExpr64 y
  -- Basically we perform a subtraction with borrow.
  -- As we don't need to result, we can use CMP instead of SUB for the low part
  -- (it sets the borrow flag just like SUB does)
  pure $ x_code `appOL` y_code `appOL` toOL
          [ MOV II32 (OpReg x_hi) (OpReg dst_r)
          , CMP II32 (OpReg y_lo) (OpReg x_lo)
          , SBB II32 (OpReg y_hi) (OpReg dst_r)
          , SETCC cond (OpReg dst_r)
          , MOVZxL II8 (OpReg dst_r) (OpReg dst_r)
          ]
