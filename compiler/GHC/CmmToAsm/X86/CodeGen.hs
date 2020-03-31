{-# LANGUAGE CPP, GADTs, NondecreasingIndentation #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

#if __GLASGOW_HASKELL__ <= 808
-- GHC 8.10 deprecates this flag, but GHC 8.8 needs it
-- The default iteration limit is a bit too low for the definitions
-- in this module.
{-# OPTIONS_GHC -fmax-pmcheck-iterations=10000000 #-}
#endif

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

#include "HsVersions.h"

-- NCG stuff:
import GhcPrelude

import GHC.CmmToAsm.X86.Instr
import GHC.CmmToAsm.X86.Cond
import GHC.CmmToAsm.X86.Regs
import GHC.CmmToAsm.X86.Ppr
import GHC.CmmToAsm.X86.RegInfo

import GHC.Platform.Regs
import GHC.CmmToAsm.CPrim
import GHC.Cmm.DebugBlock
   ( DebugBlock(..), UnwindPoint(..), UnwindTable
   , UnwindExpr(UwReg), toUnwindExpr
   )
import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.PIC
import GHC.CmmToAsm.Monad
   ( NatM, getNewRegNat, getNewLabelNat, setDeltaNat
   , getDeltaNat, getBlockIdNat, getPicBaseNat, getNewRegPairNat
   , getPicBaseMaybeNat, getDebugBlock, getFileId
   , addImmediateSuccessorNat, updateCfgNat, getConfig, getPlatform
   )
import GHC.CmmToAsm.CFG
import GHC.CmmToAsm.Format
import GHC.CmmToAsm.Config
import GHC.Platform.Reg
import GHC.Platform

-- Our intermediate code:
import GHC.Types.Basic
import GHC.Cmm.BlockId
import GHC.Types.Module ( primUnitId )
import GHC.Cmm.Utils
import GHC.Cmm.Switch
import GHC.Cmm
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.CLabel
import GHC.Core          ( Tickish(..) )
import GHC.Types.SrcLoc  ( srcSpanFile, srcSpanStartLine, srcSpanStartCol )

-- The rest:
import GHC.Types.ForeignCall ( CCallConv(..) )
import OrdList
import Outputable
import FastString
import GHC.Driver.Session
import Util
import GHC.Types.Unique.Supply ( getUniqueM )

import Control.Monad
import Data.Bits
import Data.Foldable (fold)
import Data.Int
import Data.Maybe
import Data.Word

import qualified Data.Map as M

is32BitPlatform :: NatM Bool
is32BitPlatform = do
    platform <- getPlatform
    return $ target32Bit platform

sse2Enabled :: NatM Bool
sse2Enabled = do
  platform <- getPlatform
  case platformArch platform of
  -- We Assume  SSE1 and SSE2 operations are available on both
  -- x86 and x86_64. Historically we didn't default to SSE2 and
  -- SSE1 on x86, which results in defacto nondeterminism for how
  -- rounding behaves in the associated x87 floating point instructions
  -- because variations in the spill/fpu stack placement of arguments for
  -- operations would change the precision and final result of what
  -- would otherwise be the same expressions with respect to single or
  -- double precision IEEE floating point computations.
    ArchX86_64 -> return True
    ArchX86    -> return True
    _          -> panic "trying to generate x86/x86_64 on the wrong platform"


sse4_2Enabled :: NatM Bool
sse4_2Enabled = do
  dflags <- getDynFlags
  return (isSse4_2Enabled dflags)


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
      Just picBase -> initializePicBase_x86 ArchX86 os picBase tops
      Nothing -> return tops

cmmTopCodeGen (CmmData sec dat) = do
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
    Just (SourceNote span name)
      -> do fileId <- getFileId (srcSpanFile span)
            let line = srcSpanStartLine span; col = srcSpanStartCol span
            return $ unitOL $ LOCATION fileId line col name
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
-- in the @sp@ register. See Note [What is this unwinding business?] in Debug
-- for details.
addSpUnwindings :: Instr -> NatM (OrdList Instr)
addSpUnwindings instr@(DELTA d) = do
    config <- getConfig
    if ncgDebugLevel config >= 1
        then do lbl <- mkAsmTempLabel <$> getUniqueM
                let unwind = M.singleton MachSp (Just $ UwReg MachSp $ negate d)
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

For this reason genCCall is also split into two parts.
One for calls which *won't* change the basic blocks in
which successive instructions will be placed.
A different one for calls which *are* known to change the
basic block.

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
       -> genCCall is32Bit target result_regs args bid

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
          where ty = cmmRegType platform reg
                format = cmmTypeFormat ty

      CmmStore addr src
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


-- | a.k.a "Register64"
--      Reg is the lower 32-bit temporary which contains the result.
--      Use getHiVRegFromLo to find the other VRegUnique.
--
--      Rules of this simplified insn selection game are therefore that
--      the returned Reg may be modified
--
data ChildCode64
   = ChildCode64
        InstrBlock
        Reg


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


-- | Grab the Reg for a CmmReg
getRegisterReg :: Platform  -> CmmReg -> Reg

getRegisterReg _   (CmmLocal (LocalReg u pk))
  = -- by Assuming SSE2, Int,Word,Float,Double all can be register allocated
   let fmt = cmmTypeFormat pk in
        RegVirtual (mkVirtualReg u fmt)

getRegisterReg platform  (CmmGlobal mid)
  = case globalRegMaybe platform mid of
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
mangleIndexTree :: Platform -> CmmReg -> Int -> CmmExpr
mangleIndexTree platform reg off
  = CmmMachOp (MO_Add width) [CmmReg reg, CmmLit (CmmInt (fromIntegral off) width)]
  where width = typeWidth (cmmRegType platform reg)

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
  ChildCode64 vcode rlo <- iselExpr64 valueTree
  let
        rhi = getHiVRegFromLo rlo

        -- Little-endian store
        mov_lo = MOV II32 (OpReg rlo) (OpAddr addr)
        mov_hi = MOV II32 (OpReg rhi) (OpAddr (fromJust (addrOffset addr 4)))
  return (vcode `appOL` addr_code `snocOL` mov_lo `snocOL` mov_hi)


assignReg_I64Code :: CmmReg  -> CmmExpr -> NatM InstrBlock
assignReg_I64Code (CmmLocal (LocalReg u_dst _)) valueTree = do
   ChildCode64 vcode r_src_lo <- iselExpr64 valueTree
   let
         r_dst_lo = RegVirtual $ mkVirtualReg u_dst II32
         r_dst_hi = getHiVRegFromLo r_dst_lo
         r_src_hi = getHiVRegFromLo r_src_lo
         mov_lo = MOV II32 (OpReg r_src_lo) (OpReg r_dst_lo)
         mov_hi = MOV II32 (OpReg r_src_hi) (OpReg r_dst_hi)
   return (
        vcode `snocOL` mov_lo `snocOL` mov_hi
     )

assignReg_I64Code _ _
   = panic "assignReg_I64Code(i386): invalid lvalue"


iselExpr64        :: CmmExpr -> NatM ChildCode64
iselExpr64 (CmmLit (CmmInt i _)) = do
  (rlo,rhi) <- getNewRegPairNat II32
  let
        r = fromIntegral (fromIntegral i :: Word32)
        q = fromIntegral (fromIntegral (i `shiftR` 32) :: Word32)
        code = toOL [
                MOV II32 (OpImm (ImmInteger r)) (OpReg rlo),
                MOV II32 (OpImm (ImmInteger q)) (OpReg rhi)
                ]
  return (ChildCode64 code rlo)

iselExpr64 (CmmLoad addrTree ty) | isWord64 ty = do
   Amode addr addr_code <- getAmode addrTree
   (rlo,rhi) <- getNewRegPairNat II32
   let
        mov_lo = MOV II32 (OpAddr addr) (OpReg rlo)
        mov_hi = MOV II32 (OpAddr (fromJust (addrOffset addr 4))) (OpReg rhi)
   return (
            ChildCode64 (addr_code `snocOL` mov_lo `snocOL` mov_hi)
                        rlo
     )

iselExpr64 (CmmReg (CmmLocal (LocalReg vu ty))) | isWord64 ty
   = return (ChildCode64 nilOL (RegVirtual $ mkVirtualReg vu II32))

-- we handle addition, but rather badly
iselExpr64 (CmmMachOp (MO_Add _) [e1, CmmLit (CmmInt i _)]) = do
   ChildCode64 code1 r1lo <- iselExpr64 e1
   (rlo,rhi) <- getNewRegPairNat II32
   let
        r = fromIntegral (fromIntegral i :: Word32)
        q = fromIntegral (fromIntegral (i `shiftR` 32) :: Word32)
        r1hi = getHiVRegFromLo r1lo
        code =  code1 `appOL`
                toOL [ MOV II32 (OpReg r1lo) (OpReg rlo),
                       ADD II32 (OpImm (ImmInteger r)) (OpReg rlo),
                       MOV II32 (OpReg r1hi) (OpReg rhi),
                       ADC II32 (OpImm (ImmInteger q)) (OpReg rhi) ]
   return (ChildCode64 code rlo)

iselExpr64 (CmmMachOp (MO_Add _) [e1,e2]) = do
   ChildCode64 code1 r1lo <- iselExpr64 e1
   ChildCode64 code2 r2lo <- iselExpr64 e2
   (rlo,rhi) <- getNewRegPairNat II32
   let
        r1hi = getHiVRegFromLo r1lo
        r2hi = getHiVRegFromLo r2lo
        code =  code1 `appOL`
                code2 `appOL`
                toOL [ MOV II32 (OpReg r1lo) (OpReg rlo),
                       ADD II32 (OpReg r2lo) (OpReg rlo),
                       MOV II32 (OpReg r1hi) (OpReg rhi),
                       ADC II32 (OpReg r2hi) (OpReg rhi) ]
   return (ChildCode64 code rlo)

iselExpr64 (CmmMachOp (MO_Sub _) [e1,e2]) = do
   ChildCode64 code1 r1lo <- iselExpr64 e1
   ChildCode64 code2 r2lo <- iselExpr64 e2
   (rlo,rhi) <- getNewRegPairNat II32
   let
        r1hi = getHiVRegFromLo r1lo
        r2hi = getHiVRegFromLo r2lo
        code =  code1 `appOL`
                code2 `appOL`
                toOL [ MOV II32 (OpReg r1lo) (OpReg rlo),
                       SUB II32 (OpReg r2lo) (OpReg rlo),
                       MOV II32 (OpReg r1hi) (OpReg rhi),
                       SBB II32 (OpReg r2hi) (OpReg rhi) ]
   return (ChildCode64 code rlo)

iselExpr64 (CmmMachOp (MO_UU_Conv _ W64) [expr]) = do
     fn <- getAnyReg expr
     r_dst_lo <-  getNewRegNat II32
     let r_dst_hi = getHiVRegFromLo r_dst_lo
         code = fn r_dst_lo
     return (
             ChildCode64 (code `snocOL`
                          MOV II32 (OpImm (ImmInt 0)) (OpReg r_dst_hi))
                          r_dst_lo
            )

iselExpr64 (CmmMachOp (MO_SS_Conv W32 W64) [expr]) = do
     fn <- getAnyReg expr
     r_dst_lo <-  getNewRegNat II32
     let r_dst_hi = getHiVRegFromLo r_dst_lo
         code = fn r_dst_lo
     return (
             ChildCode64 (code `snocOL`
                          MOV II32 (OpReg r_dst_lo) (OpReg eax) `snocOL`
                          CLTD II32 `snocOL`
                          MOV II32 (OpReg eax) (OpReg r_dst_lo) `snocOL`
                          MOV II32 (OpReg edx) (OpReg r_dst_hi))
                          r_dst_lo
            )

iselExpr64 expr
   = pprPanic "iselExpr64(i386)" (ppr expr)


--------------------------------------------------------------------------------
getRegister :: CmmExpr -> NatM Register
getRegister e = do platform <- getPlatform
                   is32Bit <- is32BitPlatform
                   getRegister' platform is32Bit e

getRegister' :: Platform -> Bool -> CmmExpr -> NatM Register

getRegister' platform is32Bit (CmmReg reg)
  = case reg of
        CmmGlobal PicBaseReg
         | is32Bit ->
            -- on x86_64, we have %rip for PicBaseReg, but it's not
            -- a full-featured register, it can only be used for
            -- rip-relative addressing.
            do reg' <- getPicBaseNat (archWordFormat is32Bit)
               return (Fixed (archWordFormat is32Bit) reg' nilOL)
        _ ->
            do
               let
                 fmt = cmmTypeFormat (cmmRegType platform reg)
                 format  = fmt
               --
               platform <- ncgPlatform <$> getConfig
               return (Fixed format
                             (getRegisterReg platform reg)
                             nilOL)


getRegister' platform is32Bit (CmmRegOff r n)
  = getRegister' platform is32Bit $ mangleIndexTree platform r n

getRegister' platform is32Bit (CmmMachOp (MO_AlignmentCheck align _) [e])
  = addAlignmentCheck align <$> getRegister' platform is32Bit e

-- for 32-bit architectures, support some 64 -> 32 bit conversions:
-- TO_W_(x), TO_W_(x >> 32)

getRegister' _ is32Bit (CmmMachOp (MO_UU_Conv W64 W32)
                     [CmmMachOp (MO_U_Shr W64) [x,CmmLit (CmmInt 32 _)]])
 | is32Bit = do
  ChildCode64 code rlo <- iselExpr64 x
  return $ Fixed II32 (getHiVRegFromLo rlo) code

getRegister' _ is32Bit (CmmMachOp (MO_SS_Conv W64 W32)
                     [CmmMachOp (MO_U_Shr W64) [x,CmmLit (CmmInt 32 _)]])
 | is32Bit = do
  ChildCode64 code rlo <- iselExpr64 x
  return $ Fixed II32 (getHiVRegFromLo rlo) code

getRegister' _ is32Bit (CmmMachOp (MO_UU_Conv W64 W32) [x])
 | is32Bit = do
  ChildCode64 code rlo <- iselExpr64 x
  return $ Fixed II32 rlo code

getRegister' _ is32Bit (CmmMachOp (MO_SS_Conv W64 W32) [x])
 | is32Bit = do
  ChildCode64 code rlo <- iselExpr64 x
  return $ Fixed II32 rlo code

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
getRegister' _ _ (CmmMachOp (MO_UU_Conv W8 W32) [CmmLoad addr _]) = do
  code <- intLoadCode (MOVZxL II8) addr
  return (Any II32 code)

getRegister' _ _ (CmmMachOp (MO_SS_Conv W8 W32) [CmmLoad addr _]) = do
  code <- intLoadCode (MOVSxL II8) addr
  return (Any II32 code)

getRegister' _ _ (CmmMachOp (MO_UU_Conv W16 W32) [CmmLoad addr _]) = do
  code <- intLoadCode (MOVZxL II16) addr
  return (Any II32 code)

getRegister' _ _ (CmmMachOp (MO_SS_Conv W16 W32) [CmmLoad addr _]) = do
  code <- intLoadCode (MOVSxL II16) addr
  return (Any II32 code)

-- catch simple cases of zero- or sign-extended load
getRegister' _ is32Bit (CmmMachOp (MO_UU_Conv W8 W64) [CmmLoad addr _])
 | not is32Bit = do
  code <- intLoadCode (MOVZxL II8) addr
  return (Any II64 code)

getRegister' _ is32Bit (CmmMachOp (MO_SS_Conv W8 W64) [CmmLoad addr _])
 | not is32Bit = do
  code <- intLoadCode (MOVSxL II8) addr
  return (Any II64 code)

getRegister' _ is32Bit (CmmMachOp (MO_UU_Conv W16 W64) [CmmLoad addr _])
 | not is32Bit = do
  code <- intLoadCode (MOVZxL II16) addr
  return (Any II64 code)

getRegister' _ is32Bit (CmmMachOp (MO_SS_Conv W16 W64) [CmmLoad addr _])
 | not is32Bit = do
  code <- intLoadCode (MOVSxL II16) addr
  return (Any II64 code)

getRegister' _ is32Bit (CmmMachOp (MO_UU_Conv W32 W64) [CmmLoad addr _])
 | not is32Bit = do
  code <- intLoadCode (MOV II32) addr -- 32-bit loads zero-extend
  return (Any II64 code)

getRegister' _ is32Bit (CmmMachOp (MO_SS_Conv W32 W64) [CmmLoad addr _])
 | not is32Bit = do
  code <- intLoadCode (MOVSxL II32) addr
  return (Any II64 code)

getRegister' _ is32Bit (CmmMachOp (MO_Add W64) [CmmReg (CmmGlobal PicBaseReg),
                                     CmmLit displacement])
 | not is32Bit = do
      return $ Any II64 (\dst -> unitOL $
        LEA II64 (OpAddr (ripRel (litToImm displacement))) (OpReg dst))

getRegister' platform is32Bit (CmmMachOp mop [x]) = do -- unary MachOps
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

      MO_FS_Conv from to -> coerceFP2Int from to x
      MO_SF_Conv from to -> coerceInt2FP from to x

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


getRegister' _ is32Bit (CmmMachOp mop [x, y]) = do -- dyadic MachOps
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

      MO_F_Add w   -> trivialFCode_sse2 w ADD  x y

      MO_F_Sub w   -> trivialFCode_sse2 w SUB  x y

      MO_F_Quot w  -> trivialFCode_sse2 w FDIV x y

      MO_F_Mul w   -> trivialFCode_sse2 w MUL x y


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
    imulMayOflo rep a b = do
         (a_reg, a_code) <- getNonClobberedReg a
         b_code <- getAnyReg b
         let
             shift_amt  = case rep of
                           W32 -> 31
                           W64 -> 63
                           _ -> panic "shift_amt"

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
    shift_code width instr x (CmmLit lit) = do
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
        | is32BitInteger y = add_int rep x y
    add_code rep x y = trivialCode rep (ADD format) (Just (ADD format)) x y
      where format = intFormat rep
    -- TODO: There are other interesting patterns we want to replace
    --     with a LEA, e.g. `(x + offset) + (y << shift)`.

    --------------------
    sub_code :: Width -> CmmExpr -> CmmExpr -> NatM Register
    sub_code rep x (CmmLit (CmmInt y _))
        | is32BitInteger (-y) = add_int rep x (-y)
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


getRegister' _ _ (CmmLoad mem pk)
  | isFloatType pk
  = do
    Amode addr mem_code <- getAmode mem
    loadFloatAmode  (typeWidth pk) addr mem_code

getRegister' _ is32Bit (CmmLoad mem pk)
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
getRegister' _ is32Bit (CmmLoad mem pk)
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
        -- small memory model (see gcc docs, -mcmodel=small).

getRegister' platform _ (CmmLit lit)
  = do let format = cmmTypeFormat (cmmLitType platform lit)
           imm = litToImm lit
           code dst = unitOL (MOV format (OpImm imm) (OpReg dst))
       return (Any format code)

getRegister' _ _ other
    | isVecExpr other  = needLlvm
    | otherwise        = pprPanic "getRegister(x86)" (ppr other)


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
getAmode :: CmmExpr -> NatM Amode
getAmode e = do is32Bit <- is32BitPlatform
                getAmode' is32Bit e

getAmode' :: Bool -> CmmExpr -> NatM Amode
getAmode' _ (CmmRegOff r n) = do platform <- getPlatform
                                 getAmode $ mangleIndexTree platform r n

getAmode' is32Bit (CmmMachOp (MO_Add W64) [CmmReg (CmmGlobal PicBaseReg),
                                                  CmmLit displacement])
 | not is32Bit
    = return $ Amode (ripRel (litToImm displacement)) nilOL


-- This is all just ridiculous, since it carefully undoes
-- what mangleIndexTree has just done.
getAmode' is32Bit (CmmMachOp (MO_Sub _rep) [x, CmmLit lit@(CmmInt i _)])
  | is32BitLit is32Bit lit
  -- ASSERT(rep == II32)???
  = do (x_reg, x_code) <- getSomeReg x
       let off = ImmInt (-(fromInteger i))
       return (Amode (AddrBaseIndex (EABaseReg x_reg) EAIndexNone off) x_code)

getAmode' is32Bit (CmmMachOp (MO_Add _rep) [x, CmmLit lit])
  | is32BitLit is32Bit lit
  -- ASSERT(rep == II32)???
  = do (x_reg, x_code) <- getSomeReg x
       let off = litToImm lit
       return (Amode (AddrBaseIndex (EABaseReg x_reg) EAIndexNone off) x_code)

-- Turn (lit1 << n  + lit2) into  (lit2 + lit1 << n) so it will be
-- recognised by the next rule.
getAmode' is32Bit (CmmMachOp (MO_Add rep) [a@(CmmMachOp (MO_Shl _) _),
                                  b@(CmmLit _)])
  = getAmode' is32Bit (CmmMachOp (MO_Add rep) [b,a])

-- Matches: (x + offset) + (y << shift)
getAmode' _ (CmmMachOp (MO_Add _) [CmmRegOff x offset,
                                   CmmMachOp (MO_Shl _)
                                        [y, CmmLit (CmmInt shift _)]])
  | shift == 0 || shift == 1 || shift == 2 || shift == 3
  = x86_complex_amode (CmmReg x) y shift (fromIntegral offset)

getAmode' _ (CmmMachOp (MO_Add _) [x, CmmMachOp (MO_Shl _)
                                        [y, CmmLit (CmmInt shift _)]])
  | shift == 0 || shift == 1 || shift == 2 || shift == 3
  = x86_complex_amode x y shift 0

getAmode' _ (CmmMachOp (MO_Add _)
                [x, CmmMachOp (MO_Add _)
                        [CmmMachOp (MO_Shl _) [y, CmmLit (CmmInt shift _)],
                         CmmLit (CmmInt offset _)]])
  | shift == 0 || shift == 1 || shift == 2 || shift == 3
  && is32BitInteger offset
  = x86_complex_amode x y shift offset

getAmode' _ (CmmMachOp (MO_Add _) [x,y])
  = x86_complex_amode x y 0 0

getAmode' is32Bit (CmmLit lit) | is32BitLit is32Bit lit
  = return (Amode (ImmAddr (litToImm lit) 0) nilOL)

getAmode' _ expr = do
  (reg,code) <- getSomeReg expr
  return (Amode (AddrBaseIndex (EABaseReg reg) EAIndexNone (ImmInt 0)) code)

-- | Like 'getAmode', but on 32-bit use simple register addressing
-- (i.e. no index register). This stops us from running out of
-- registers on x86 when using instructions such as cmpxchg, which can
-- use up to three virtual registers and one fixed register.
getSimpleAmode :: Bool -> CmmExpr -> NatM Amode
getSimpleAmode is32Bit addr
    | is32Bit = do
        addr_code <- getAnyReg addr
        config <- getConfig
        addr_r <- getNewRegNat (intFormat (ncgWordWidth config))
        let amode = AddrBaseIndex (EABaseReg addr_r) EAIndexNone (ImmInt 0)
        return $! Amode amode (addr_code addr_r)
    | otherwise = getAmode addr

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
getNonClobberedOperand (CmmLit lit) = do
  if isSuitableFloatingPointLit lit
    then do
      let CmmFloat _ w = lit
      Amode addr code <- memConstant (mkAlignment $ widthInBytes w) lit
      return (OpAddr addr, code)
     else do

  is32Bit <- is32BitPlatform
  platform <- getPlatform
  if is32BitLit is32Bit lit && not (isFloatType (cmmLitType platform lit))
    then return (OpImm (litToImm lit), nilOL)
    else getNonClobberedOperand_generic (CmmLit lit)

getNonClobberedOperand (CmmLoad mem pk) = do
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
    else do
      -- if its a word or gcptr on 32bit?
      getNonClobberedOperand_generic (CmmLoad mem pk)

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

  is32Bit <- is32BitPlatform
  platform <- getPlatform
  if is32BitLit is32Bit lit && not (isFloatType (cmmLitType platform lit))
    then return (OpImm (litToImm lit), nilOL)
    else getOperand_generic (CmmLit lit)

getOperand (CmmLoad mem pk) = do
  is32Bit <- is32BitPlatform
  use_sse2 <- sse2Enabled
  if (not (isFloatType pk) || use_sse2) && (if is32Bit then not (isWord64 pk) else True)
     then do
       Amode src mem_code <- getAmode mem
       return (OpAddr src, mem_code)
     else
       getOperand_generic (CmmLoad mem pk)

getOperand e = getOperand_generic e

getOperand_generic :: CmmExpr -> NatM (Operand, InstrBlock)
getOperand_generic e = do
    (reg, code) <- getSomeReg e
    return (OpReg reg, code)

isOperand :: Bool -> CmmExpr -> Bool
isOperand _ (CmmLoad _ _) = True
isOperand is32Bit (CmmLit lit)  = is32BitLit is32Bit lit
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
        ASSERT(not $ isFloatFormat fmt)
        toOL [ TEST fmt (OpImm $ ImmInt $ align-1) (OpReg reg)
             , JXX_GBL NE $ ImmCLbl mkBadAlignmentLabel
             ]

memConstant :: Alignment -> CmmLit -> NatM Amode
memConstant align lit = do
  lbl <- getNewLabelNat
  let rosection = Section ReadOnlyData lbl
  dflags <- getDynFlags
  platform <- getPlatform
  (addr, addr_code) <- if target32Bit platform
                       then do dynRef <- cmmMakeDynamicReference
                                             dflags
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
getRegOrMem e@(CmmLoad mem pk) = do
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

is32BitLit :: Bool -> CmmLit -> Bool
is32BitLit is32Bit (CmmInt i W64)
 | not is32Bit
    = -- assume that labels are in the range 0-2^31-1: this assumes the
      -- small memory model (see gcc docs, -mcmodel=small).
      is32BitInteger i
is32BitLit _ _ = True




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

getCondCode other = pprPanic "getCondCode(2)(x86,x86_64)" (ppr other)

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


-- @cond(Int|Flt)Code@: Turn a boolean expression into a condition, to be
-- passed back up the tree.

condIntCode :: Cond -> CmmExpr -> CmmExpr -> NatM CondCode
condIntCode cond x y = do is32Bit <- is32BitPlatform
                          condIntCode' is32Bit cond x y

condIntCode' :: Bool -> Cond -> CmmExpr -> CmmExpr -> NatM CondCode

-- memory vs immediate
condIntCode' is32Bit cond (CmmLoad x pk) (CmmLit lit)
 | is32BitLit is32Bit lit = do
    Amode x_addr x_code <- getAmode x
    let
        imm  = litToImm lit
        code = x_code `snocOL`
                  CMP (cmmTypeFormat pk) (OpImm imm) (OpAddr x_addr)
    --
    return (CondCode False cond code)

-- anything vs zero, using a mask
-- TODO: Add some sanity checking!!!!
condIntCode' is32Bit cond (CmmMachOp (MO_And _) [x,o2]) (CmmLit (CmmInt 0 pk))
    | (CmmLit lit@(CmmInt mask _)) <- o2, is32BitLit is32Bit lit
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
condIntCode' is32Bit cond x y
 | isOperand is32Bit y = do
    platform <- getPlatform
    (x_reg, x_code) <- getNonClobberedReg x
    (y_op,  y_code) <- getOperand y
    let
        code = x_code `appOL` y_code `snocOL`
                  CMP (cmmTypeFormat (cmmExprType platform x)) y_op (OpReg x_reg)
    return (CondCode False cond code)
-- operand vs. anything: invert the comparison so that we can use a
-- single comparison instruction.
 | isOperand is32Bit x
 , Just revcond <- maybeFlipCond cond = do
    platform <- getPlatform
    (y_reg, y_code) <- getNonClobberedReg y
    (x_op,  x_code) <- getOperand x
    let
        code = y_code `appOL` x_code `snocOL`
                  CMP (cmmTypeFormat (cmmExprType platform x)) x_op (OpReg y_reg)
    return (CondCode False revcond code)

-- anything vs anything
condIntCode' _ cond x y = do
  platform <- getPlatform
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
assignMem_IntCode pk addr (CmmMachOp op [CmmLoad addr2 _,
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
    is32Bit <- is32BitPlatform
    Amode addr code_addr <- getAmode addr
    (code_src, op_src)   <- get_op_RI is32Bit src
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
    get_op_RI :: Bool -> CmmExpr -> NatM (InstrBlock,Operand)   -- code, operator
    get_op_RI is32Bit (CmmLit lit) | is32BitLit is32Bit lit
      = return (nilOL, OpImm (litToImm lit))
    get_op_RI _ op
      = do (reg,code) <- getNonClobberedReg op
           return (code, OpReg reg)


-- Assign; dst is a reg, rhs is mem
assignReg_IntCode pk reg (CmmLoad src _) = do
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

genJump (CmmLoad mem _) regs = do
  Amode target code <- getAmode mem
  return (code `snocOL` JMP (OpAddr target) regs)

genJump (CmmLit lit) regs = do
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

-- 64-bit integer comparisons on 32-bit
genCondBranch' is32Bit _bid true false (CmmMachOp mop [e1,e2])
  | is32Bit, Just W64 <- maybeIntComparison mop = do
  ChildCode64 code1 r1_lo <- iselExpr64 e1
  ChildCode64 code2 r2_lo <- iselExpr64 e2
  let r1_hi = getHiVRegFromLo r1_lo
      r2_hi = getHiVRegFromLo r2_lo
      cond = machOpToCond mop
      Just cond' = maybeFlipCond cond
  --TODO: Update CFG for x86
  let code = code1 `appOL` code2 `appOL` toOL [
        CMP II32 (OpReg r2_hi) (OpReg r1_hi),
        JXX cond true,
        JXX cond' false,
        CMP II32 (OpReg r2_lo) (OpReg r1_lo),
        JXX cond true] `appOL` genBranch false
  return code

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
                    ASSERT2(False, ppr "Should have been turned into >")
                    and_ordered
                  LE  ->
                    ASSERT2(False, ppr "Should have been turned into >=")
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

genCCall
    :: Bool                     -- 32 bit platform?
    -> ForeignTarget            -- function to call
    -> [CmmFormal]        -- where to put the result
    -> [CmmActual]        -- arguments (of mixed type)
    -> BlockId      -- The block we are in
    -> NatM (InstrBlock, Maybe BlockId)

-- First we deal with cases which might introduce new blocks in the stream.

genCCall is32Bit (PrimTarget (MO_AtomicRMW width amop))
                                           [dst] [addr, n] bid = do
    Amode amode addr_code <-
        if amop `elem` [AMO_Add, AMO_Sub]
        then getAmode addr
        else getSimpleAmode is32Bit addr  -- See genCCall for MO_Cmpxchg
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

genCCall is32Bit (PrimTarget (MO_Ctz width)) [dst] [src] bid
  | is32Bit, width == W64 = do
      ChildCode64 vcode rlo <- iselExpr64 src
      platform <- ncgPlatform <$> getConfig
      let rhi     = getHiVRegFromLo rlo
          dst_r   = getRegisterReg platform  (CmmLocal dst)
      lbl1 <- getBlockIdNat
      lbl2 <- getBlockIdNat
      let format = if width == W8 then II16 else intFormat width
      tmp_r <- getNewRegNat format

      -- New CFG Edges:
      --  bid -> lbl2
      --  bid -> lbl1 -> lbl2
      --  We also changes edges originating at bid to start at lbl2 instead.
      dflags <- getDynFlags
      updateCfgNat (addWeightEdge bid lbl1 110 .
                    addWeightEdge lbl1 lbl2 110 .
                    addImmediateSuccessor dflags bid lbl2)

      -- The following instruction sequence corresponds to the pseudo-code
      --
      --  if (src) {
      --    dst = src.lo32 ? BSF(src.lo32) : (BSF(src.hi32) + 32);
      --  } else {
      --    dst = 64;
      --  }
      let !instrs = vcode `appOL` toOL
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

  | otherwise = do
    code_src <- getAnyReg src
    platform <- ncgPlatform <$> getConfig
    let dst_r = getRegisterReg platform (CmmLocal dst)
    dflags <- getDynFlags
    if isBmi2Enabled dflags
    then do
        src_r <- getNewRegNat (intFormat width)
        let instrs = appOL (code_src src_r) $ case width of
                W8 -> toOL
                    [ OR    II32 (OpImm (ImmInteger 0xFFFFFF00)) (OpReg src_r)
                    , TZCNT II32 (OpReg src_r)        dst_r
                    ]
                W16 -> toOL
                    [ TZCNT  II16 (OpReg src_r) dst_r
                    , MOVZxL II16 (OpReg dst_r) (OpReg dst_r)
                    ]
                _ -> unitOL $ TZCNT (intFormat width) (OpReg src_r) dst_r
        return (instrs, Nothing)
    else do
        -- The following insn sequence makes sure 'ctz 0' has a defined value.
        -- starting with Haswell, one could use the TZCNT insn instead.
        let format = if width == W8 then II16 else intFormat width
        src_r <- getNewRegNat format
        tmp_r <- getNewRegNat format
        let !instrs = code_src src_r `appOL` toOL
                 ([ MOVZxL  II8    (OpReg src_r) (OpReg src_r) | width == W8 ] ++
                  [ BSF     format (OpReg src_r) tmp_r
                  , MOV     II32   (OpImm (ImmInt bw)) (OpReg dst_r)
                  , CMOV NE format (OpReg tmp_r) dst_r
                  ]) -- NB: We don't need to zero-extend the result for the
                     -- W8/W16 cases because the 'MOV' insn already
                     -- took care of implicitly clearing the upper bits
        return (instrs, Nothing)
  where
    bw = widthInBits width

genCCall bits mop dst args bid = do
  dflags <- getDynFlags
  instr <- genCCall' dflags bits mop dst args bid
  return (instr, Nothing)

-- genCCall' handles cases not introducing new code blocks.
genCCall'
    :: DynFlags
    -> Bool                     -- 32 bit platform?
    -> ForeignTarget            -- function to call
    -> [CmmFormal]        -- where to put the result
    -> [CmmActual]        -- arguments (of mixed type)
    -> BlockId      -- The block we are in
    -> NatM InstrBlock

-- Unroll memcpy calls if the number of bytes to copy isn't too
-- large.  Otherwise, call C's memcpy.
genCCall' dflags _ (PrimTarget (MO_Memcpy align)) _
         [dst, src, CmmLit (CmmInt n _)] _
    | fromInteger insns <= maxInlineMemcpyInsns dflags = do
        code_dst <- getAnyReg dst
        dst_r <- getNewRegNat format
        code_src <- getAnyReg src
        src_r <- getNewRegNat format
        tmp_r <- getNewRegNat format
        return $ code_dst dst_r `appOL` code_src src_r `appOL`
            go dst_r src_r tmp_r (fromInteger n)
  where
    platform = targetPlatform dflags
    -- The number of instructions we will generate (approx). We need 2
    -- instructions per move.
    insns = 2 * ((n + sizeBytes - 1) `div` sizeBytes)

    maxAlignment = wordAlignment platform -- only machine word wide MOVs are supported
    effectiveAlignment = min (alignmentOf align) maxAlignment
    format = intFormat . widthFromBytes $ alignmentBytes effectiveAlignment

    -- The size of each move, in bytes.
    sizeBytes :: Integer
    sizeBytes = fromIntegral (formatInBytes format)

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

genCCall' dflags _ (PrimTarget (MO_Memset align)) _
         [dst,
          CmmLit (CmmInt c _),
          CmmLit (CmmInt n _)]
         _
    | fromInteger insns <= maxInlineMemsetInsns dflags = do
        code_dst <- getAnyReg dst
        dst_r <- getNewRegNat format
        if format == II64 && n >= 8 then do
          code_imm8byte <- getAnyReg (CmmLit (CmmInt c8 W64))
          imm8byte_r <- getNewRegNat II64
          return $ code_dst dst_r `appOL`
                   code_imm8byte imm8byte_r `appOL`
                   go8 dst_r imm8byte_r (fromInteger n)
        else
          return $ code_dst dst_r `appOL`
                   go4 dst_r (fromInteger n)
  where
    platform = targetPlatform dflags
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

genCCall' _ _ (PrimTarget MO_ReadBarrier) _ _ _  = return nilOL
genCCall' _ _ (PrimTarget MO_WriteBarrier) _ _ _ = return nilOL
        -- barriers compile to no code on x86/x86-64;
        -- we keep it this long in order to prevent earlier optimisations.

genCCall' _ _ (PrimTarget MO_Touch) _ _ _ = return nilOL

genCCall' _ is32bit (PrimTarget (MO_Prefetch_Data n )) _  [src] _ =
        case n of
            0 -> genPrefetch src $ PREFETCH NTA  format
            1 -> genPrefetch src $ PREFETCH Lvl2 format
            2 -> genPrefetch src $ PREFETCH Lvl1 format
            3 -> genPrefetch src $ PREFETCH Lvl0 format
            l -> panic $ "unexpected prefetch level in genCCall MO_Prefetch_Data: " ++ (show l)
            -- the c / llvm prefetch convention is 0, 1, 2, and 3
            -- the x86 corresponding names are : NTA, 2 , 1, and 0
   where
        format = archWordFormat is32bit
        -- need to know what register width for pointers!
        genPrefetch inRegSrc prefetchCTor =
            do
                code_src <- getAnyReg inRegSrc
                src_r <- getNewRegNat format
                return $ code_src src_r `appOL`
                  (unitOL (prefetchCTor  (OpAddr
                              ((AddrBaseIndex (EABaseReg src_r )   EAIndexNone (ImmInt 0))))  ))
                  -- prefetch always takes an address

genCCall' _ is32Bit (PrimTarget (MO_BSwap width)) [dst] [src] _ = do
    platform <- ncgPlatform <$> getConfig
    let dst_r = getRegisterReg platform (CmmLocal dst)
    case width of
        W64 | is32Bit -> do
               ChildCode64 vcode rlo <- iselExpr64 src
               let dst_rhi = getHiVRegFromLo dst_r
                   rhi     = getHiVRegFromLo rlo
               return $ vcode `appOL`
                        toOL [ MOV II32 (OpReg rlo) (OpReg dst_rhi),
                               MOV II32 (OpReg rhi) (OpReg dst_r),
                               BSWAP II32 dst_rhi,
                               BSWAP II32 dst_r ]
        W16 -> do code_src <- getAnyReg src
                  return $ code_src dst_r `appOL`
                           unitOL (BSWAP II32 dst_r) `appOL`
                           unitOL (SHR II32 (OpImm $ ImmInt 16) (OpReg dst_r))
        _   -> do code_src <- getAnyReg src
                  return $ code_src dst_r `appOL` unitOL (BSWAP format dst_r)
  where
    format = intFormat width

genCCall' dflags is32Bit (PrimTarget (MO_PopCnt width)) dest_regs@[dst]
         args@[src] bid = do
    sse4_2 <- sse4_2Enabled
    platform <- ncgPlatform <$> getConfig
    if sse4_2
        then do code_src <- getAnyReg src
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
        else do
            targetExpr <- cmmMakeDynamicReference dflags
                          CallReference lbl
            let target = ForeignTarget targetExpr (ForeignConvention CCallConv
                                                           [NoHint] [NoHint]
                                                           CmmMayReturn)
            genCCall' dflags is32Bit target dest_regs args bid
  where
    format = intFormat width
    lbl = mkCmmCodeLabel primUnitId (fsLit (popCntLabel width))

genCCall' dflags is32Bit (PrimTarget (MO_Pdep width)) dest_regs@[dst]
         args@[src, mask] bid = do
    platform <- ncgPlatform <$> getConfig
    if isBmi2Enabled dflags
        then do code_src  <- getAnyReg src
                code_mask <- getAnyReg mask
                src_r     <- getNewRegNat format
                mask_r    <- getNewRegNat format
                let dst_r = getRegisterReg platform  (CmmLocal dst)
                return $ code_src src_r `appOL` code_mask mask_r `appOL`
                    (if width == W8 then
                         -- The PDEP instruction doesn't take a r/m8
                         unitOL (MOVZxL II8  (OpReg src_r ) (OpReg src_r )) `appOL`
                         unitOL (MOVZxL II8  (OpReg mask_r) (OpReg mask_r)) `appOL`
                         unitOL (PDEP   II16 (OpReg mask_r) (OpReg src_r ) dst_r)
                     else
                         unitOL (PDEP format (OpReg mask_r) (OpReg src_r) dst_r)) `appOL`
                    (if width == W8 || width == W16 then
                         -- We used a 16-bit destination register above,
                         -- so zero-extend
                         unitOL (MOVZxL II16 (OpReg dst_r) (OpReg dst_r))
                     else nilOL)
        else do
            targetExpr <- cmmMakeDynamicReference dflags
                          CallReference lbl
            let target = ForeignTarget targetExpr (ForeignConvention CCallConv
                                                           [NoHint] [NoHint]
                                                           CmmMayReturn)
            genCCall' dflags is32Bit target dest_regs args bid
  where
    format = intFormat width
    lbl = mkCmmCodeLabel primUnitId (fsLit (pdepLabel width))

genCCall' dflags is32Bit (PrimTarget (MO_Pext width)) dest_regs@[dst]
         args@[src, mask] bid = do
    platform <- ncgPlatform <$> getConfig
    if isBmi2Enabled dflags
        then do code_src  <- getAnyReg src
                code_mask <- getAnyReg mask
                src_r     <- getNewRegNat format
                mask_r    <- getNewRegNat format
                let dst_r = getRegisterReg platform  (CmmLocal dst)
                return $ code_src src_r `appOL` code_mask mask_r `appOL`
                    (if width == W8 then
                         -- The PEXT instruction doesn't take a r/m8
                         unitOL (MOVZxL II8 (OpReg src_r ) (OpReg src_r )) `appOL`
                         unitOL (MOVZxL II8 (OpReg mask_r) (OpReg mask_r)) `appOL`
                         unitOL (PEXT II16 (OpReg mask_r) (OpReg src_r) dst_r)
                     else
                         unitOL (PEXT format (OpReg mask_r) (OpReg src_r) dst_r)) `appOL`
                    (if width == W8 || width == W16 then
                         -- We used a 16-bit destination register above,
                         -- so zero-extend
                         unitOL (MOVZxL II16 (OpReg dst_r) (OpReg dst_r))
                     else nilOL)
        else do
            targetExpr <- cmmMakeDynamicReference dflags
                          CallReference lbl
            let target = ForeignTarget targetExpr (ForeignConvention CCallConv
                                                           [NoHint] [NoHint]
                                                           CmmMayReturn)
            genCCall' dflags is32Bit target dest_regs args bid
  where
    format = intFormat width
    lbl = mkCmmCodeLabel primUnitId (fsLit (pextLabel width))

genCCall' dflags is32Bit (PrimTarget (MO_Clz width)) dest_regs@[dst] args@[src] bid
  | is32Bit && width == W64 = do
    -- Fallback to `hs_clz64` on i386
    targetExpr <- cmmMakeDynamicReference dflags CallReference lbl
    let target = ForeignTarget targetExpr (ForeignConvention CCallConv
                                           [NoHint] [NoHint]
                                           CmmMayReturn)
    genCCall' dflags is32Bit target dest_regs args bid

  | otherwise = do
    code_src <- getAnyReg src
    platform <- ncgPlatform <$> getConfig
    let dst_r = getRegisterReg platform (CmmLocal dst)
    if isBmi2Enabled dflags
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
  where
    bw = widthInBits width
    lbl = mkCmmCodeLabel primUnitId (fsLit (clzLabel width))

genCCall' dflags is32Bit (PrimTarget (MO_UF_Conv width)) dest_regs args bid = do
    targetExpr <- cmmMakeDynamicReference dflags
                  CallReference lbl
    let target = ForeignTarget targetExpr (ForeignConvention CCallConv
                                           [NoHint] [NoHint]
                                           CmmMayReturn)
    genCCall' dflags is32Bit target dest_regs args bid
  where
    lbl = mkCmmCodeLabel primUnitId (fsLit (word2FloatLabel width))

genCCall' _ _ (PrimTarget (MO_AtomicRead width)) [dst] [addr] _ = do
  load_code <- intLoadCode (MOV (intFormat width)) addr
  platform <- ncgPlatform <$> getConfig

  return (load_code (getRegisterReg platform  (CmmLocal dst)))

genCCall' _ _ (PrimTarget (MO_AtomicWrite width)) [] [addr, val] _ = do
    code <- assignMem_IntCode (intFormat width) addr val
    return $ code `snocOL` MFENCE

genCCall' _ is32Bit (PrimTarget (MO_Cmpxchg width)) [dst] [addr, old, new] _ = do
    -- On x86 we don't have enough registers to use cmpxchg with a
    -- complicated addressing mode, so on that architecture we
    -- pre-compute the address first.
    Amode amode addr_code <- getSimpleAmode is32Bit addr
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
  where
    format = intFormat width

genCCall' _ is32Bit target dest_regs args bid = do
  platform <- ncgPlatform <$> getConfig
  case (target, dest_regs) of
    -- void return type prim op
    (PrimTarget op, []) ->
        outOfLineCmmOp bid op Nothing args
    -- we only cope with a single result for foreign calls
    (PrimTarget op, [r])  -> case op of
          MO_F32_Fabs -> case args of
            [x] -> sse2FabsCode W32 x
            _ -> panic "genCCall: Wrong number of arguments for fabs"
          MO_F64_Fabs -> case args of
            [x] -> sse2FabsCode W64 x
            _ -> panic "genCCall: Wrong number of arguments for fabs"

          MO_F32_Sqrt -> actuallyInlineSSE2Op (\fmt r -> SQRT fmt (OpReg r)) FF32 args
          MO_F64_Sqrt -> actuallyInlineSSE2Op (\fmt r -> SQRT fmt (OpReg r)) FF64 args
          _other_op -> outOfLineCmmOp bid op (Just r) args

       where
        actuallyInlineSSE2Op = actuallyInlineFloatOp'

        actuallyInlineFloatOp'  instr format [x]
              = do res <- trivialUFCode format (instr format) x
                   any <- anyReg res
                   return (any (getRegisterReg platform  (CmmLocal r)))

        actuallyInlineFloatOp' _ _ args
              = panic $ "genCCall.actuallyInlineFloatOp': bad number of arguments! ("
                      ++ show (length args) ++ ")"

        sse2FabsCode :: Width -> CmmExpr -> NatM InstrBlock
        sse2FabsCode w x = do
          let fmt = floatFormat w
          x_code <- getAnyReg x
          let
            const | FF32 <- fmt = CmmInt 0x7fffffff W32
                  | otherwise   = CmmInt 0x7fffffffffffffff W64
          Amode amode amode_code <- memConstant (mkAlignment $ widthInBytes w) const
          tmp <- getNewRegNat fmt
          let
            code dst = x_code dst `appOL` amode_code `appOL` toOL [
                MOV fmt (OpAddr amode) (OpReg tmp),
                AND fmt (OpReg tmp) (OpReg dst)
                ]

          return $ code (getRegisterReg platform (CmmLocal r))

    (PrimTarget (MO_S_QuotRem  width), _) -> divOp1 platform True  width dest_regs args
    (PrimTarget (MO_U_QuotRem  width), _) -> divOp1 platform False width dest_regs args
    (PrimTarget (MO_U_QuotRem2 width), _) -> divOp2 platform False width dest_regs args
    (PrimTarget (MO_Add2 width), [res_h, res_l]) ->
        case args of
        [arg_x, arg_y] ->
            do hCode <- getAnyReg (CmmLit (CmmInt 0 width))
               let format = intFormat width
               lCode <- anyReg =<< trivialCode width (ADD_CC format)
                                     (Just (ADD_CC format)) arg_x arg_y
               let reg_l = getRegisterReg platform (CmmLocal res_l)
                   reg_h = getRegisterReg platform (CmmLocal res_h)
                   code = hCode reg_h `appOL`
                          lCode reg_l `snocOL`
                          ADC format (OpImm (ImmInteger 0)) (OpReg reg_h)
               return code
        _ -> panic "genCCall: Wrong number of arguments/results for add2"
    (PrimTarget (MO_AddWordC width), [res_r, res_c]) ->
        addSubIntC platform ADD_CC (const Nothing) CARRY width res_r res_c args
    (PrimTarget (MO_SubWordC width), [res_r, res_c]) ->
        addSubIntC platform SUB_CC (const Nothing) CARRY width res_r res_c args
    (PrimTarget (MO_AddIntC width), [res_r, res_c]) ->
        addSubIntC platform ADD_CC (Just . ADD_CC) OFLO width res_r res_c args
    (PrimTarget (MO_SubIntC width), [res_r, res_c]) ->
        addSubIntC platform SUB_CC (const Nothing) OFLO width res_r res_c args
    (PrimTarget (MO_U_Mul2 width), [res_h, res_l]) ->
        case args of
        [arg_x, arg_y] ->
            do (y_reg, y_code) <- getRegOrMem arg_y
               x_code <- getAnyReg arg_x
               let format = intFormat width
                   reg_h = getRegisterReg platform (CmmLocal res_h)
                   reg_l = getRegisterReg platform (CmmLocal res_l)
                   code = y_code `appOL`
                          x_code rax `appOL`
                          toOL [MUL2 format y_reg,
                                MOV format (OpReg rdx) (OpReg reg_h),
                                MOV format (OpReg rax) (OpReg reg_l)]
               return code
        _ -> panic "genCCall: Wrong number of arguments/results for mul2"
    (PrimTarget (MO_S_Mul2 width), [res_c, res_h, res_l]) ->
        case args of
        [arg_x, arg_y] ->
            do (y_reg, y_code) <- getRegOrMem arg_y
               x_code <- getAnyReg arg_x
               reg_tmp <- getNewRegNat II8
               let format = intFormat width
                   reg_h = getRegisterReg platform (CmmLocal res_h)
                   reg_l = getRegisterReg platform (CmmLocal res_l)
                   reg_c = getRegisterReg platform (CmmLocal res_c)
                   code = y_code `appOL`
                          x_code rax `appOL`
                          toOL [ IMUL2 format y_reg
                               , MOV format (OpReg rdx) (OpReg reg_h)
                               , MOV format (OpReg rax) (OpReg reg_l)
                               , SETCC CARRY (OpReg reg_tmp)
                               , MOVZxL II8 (OpReg reg_tmp) (OpReg reg_c)
                               ]
               return code
        _ -> panic "genCCall: Wrong number of arguments/results for imul2"

    _ -> if is32Bit
         then genCCall32' target dest_regs args
         else genCCall64' target dest_regs args

  where divOp1 platform signed width results [arg_x, arg_y]
            = divOp platform signed width results Nothing arg_x arg_y
        divOp1 _ _ _ _ _
            = panic "genCCall: Wrong number of arguments for divOp1"
        divOp2 platform signed width results [arg_x_high, arg_x_low, arg_y]
            = divOp platform signed width results (Just arg_x_high) arg_x_low arg_y
        divOp2 _ _ _ _ _
            = panic "genCCall: Wrong number of arguments for divOp2"

        -- See Note [DIV/IDIV for bytes]
        divOp platform signed W8 [res_q, res_r] m_arg_x_high arg_x_low arg_y =
            let widen | signed = MO_SS_Conv W8 W16
                      | otherwise = MO_UU_Conv W8 W16
                arg_x_low_16 = CmmMachOp widen [arg_x_low]
                arg_y_16 = CmmMachOp widen [arg_y]
                m_arg_x_high_16 = (\p -> CmmMachOp widen [p]) <$> m_arg_x_high
            in divOp
                  platform signed W16 [res_q, res_r]
                  m_arg_x_high_16 arg_x_low_16 arg_y_16

        divOp platform signed width [res_q, res_r]
              m_arg_x_high arg_x_low arg_y
            = do let format = intFormat width
                     reg_q = getRegisterReg platform (CmmLocal res_q)
                     reg_r = getRegisterReg platform (CmmLocal res_r)
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
        divOp _ _ _ _ _ _ _
            = panic "genCCall: Wrong number of results for divOp"

        addSubIntC platform instr mrevinstr cond width
                   res_r res_c [arg_x, arg_y]
            = do let format = intFormat width
                 rCode <- anyReg =<< trivialCode width (instr format)
                                       (mrevinstr format) arg_x arg_y
                 reg_tmp <- getNewRegNat II8
                 let reg_c = getRegisterReg platform  (CmmLocal res_c)
                     reg_r = getRegisterReg platform  (CmmLocal res_r)
                     code = rCode reg_r `snocOL`
                            SETCC cond (OpReg reg_tmp) `snocOL`
                            MOVZxL II8 (OpReg reg_tmp) (OpReg reg_c)

                 return code
        addSubIntC _ _ _ _ _ _ _ _
            = panic "genCCall: Wrong number of arguments/results for addSubIntC"

-- Note [DIV/IDIV for bytes]
--
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

genCCall32' :: ForeignTarget            -- function to call
            -> [CmmFormal]        -- where to put the result
            -> [CmmActual]        -- arguments (of mixed type)
            -> NatM InstrBlock
genCCall32' target dest_regs args = do
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
                ChildCode64 code r_lo <- iselExpr64 arg
                delta <- getDeltaNat
                setDeltaNat (delta - 8)
                let r_hi = getHiVRegFromLo r_lo
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
                ASSERT((typeWidth arg_ty) <= W32) return ()
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
            -- maintiain. See Note [rts/StgCRun.c : Stack Alignment on X86]
            sizes               = map (arg_size_bytes . cmmExprType platform) (reverse args)
            raw_arg_size        = sum sizes + platformWordSizeInBytes platform
            arg_pad_size        = (roundTo 16 $ raw_arg_size) - raw_arg_size
            tot_arg_size        = raw_arg_size + arg_pad_size - platformWordSizeInBytes platform


        delta0 <- getDeltaNat
        setDeltaNat (delta0 - arg_pad_size)

        push_codes <- mapM push_arg (reverse prom_args)
        delta <- getDeltaNat
        MASSERT(delta == delta0 - tot_arg_size)

        -- deal with static vs dynamic call targets
        (callinsns,cconv) <-
          case target of
            ForeignTarget (CmmLit (CmmLabel lbl)) conv
               -> -- ToDo: stdcall arg sizes
                  return (unitOL (CALL (Left fn_imm) []), conv)
               where fn_imm = ImmCLbl lbl
            ForeignTarget expr conv
               -> do { (dyn_r, dyn_c) <- getSomeReg expr
                     ; ASSERT( isWord32 (cmmExprType platform expr) )
                       return (dyn_c `snocOL` CALL (Right dyn_r) [], conv) }
            PrimTarget _
                -> panic $ "genCCall: Can't handle PrimTarget call type here, error "
                            ++ "probably because too many return values."

        let push_code
                | arg_pad_size /= 0
                = toOL [SUB II32 (OpImm (ImmInt arg_pad_size)) (OpReg esp),
                        DELTA (delta0 - arg_pad_size)]
                  `appOL` concatOL push_codes
                | otherwise
                = concatOL push_codes

              -- Deallocate parameters after call for ccall;
              -- but not for stdcall (callee does it)
              --
              -- We have to pop any stack padding we added
              -- even if we are doing stdcall, though (#5052)
            pop_size
               | ForeignConvention StdCallConv _ _ _ <- cconv = arg_pad_size
               | otherwise = tot_arg_size

            call = callinsns `appOL`
                   toOL (
                      (if pop_size==0 then [] else
                       [ADD II32 (OpImm (ImmInt pop_size)) (OpReg esp)])
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
                                   -- revisted once GHC does more work around
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
                    r_dest    = getRegisterReg platform (CmmLocal dest)
            assign_code many = pprPanic "genCCall.assign_code - too many return values:" (ppr many)

        return (push_code `appOL`
                call `appOL`
                assign_code dest_regs)

genCCall64' :: ForeignTarget      -- function to call
            -> [CmmFormal]        -- where to put the result
            -> [CmmActual]        -- arguments (of mixed type)
            -> NatM InstrBlock
genCCall64' target dest_regs args = do
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
                | isOperand False arg = do
                    arg_code <- getAnyReg arg
                    return (code, (acode `appOL` arg_code r))
                -- The last non-operand arg can be directly assigned after its
                -- computation without going into a temporary register
                | all (isOperand False) rest = do
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
             ASSERT(width <= W64) return ()
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
           let used_regs rs as = reverse (drop (length rs) (reverse as))
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
    -- maintain. See Note [rts/StgCRun.c : Stack Alignment on X86]
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
    (callinsns,_cconv) <-
      case target of
        ForeignTarget (CmmLit (CmmLabel lbl)) conv
           -> -- ToDo: stdcall arg sizes
              return (unitOL (CALL (Left fn_imm) arg_regs), conv)
           where fn_imm = ImmCLbl lbl
        ForeignTarget expr conv
           -> do (dyn_r, dyn_c) <- getSomeReg expr
                 return (dyn_c `snocOL` CALL (Right dyn_r) arg_regs, conv)
        PrimTarget _
            -> panic $ "genCCall: Can't handle PrimTarget call type here, error "
                        ++ "probably because too many return values."

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
                    -- Deallocate parameters after call for ccall;
                    -- stdcall has callee do it, but is not supported on
                    -- x86_64 target (see #3336)
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
        assign_code _many = panic "genCCall.assign_code many"

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

outOfLineCmmOp :: BlockId -> CallishMachOp -> Maybe CmmFormal -> [CmmActual]
               -> NatM InstrBlock
outOfLineCmmOp bid mop res args
  = do
      dflags <- getDynFlags
      targetExpr <- cmmMakeDynamicReference dflags CallReference lbl
      let target = ForeignTarget targetExpr
                           (ForeignConvention CCallConv [] [] CmmMayReturn)

      -- We know foreign calls results in no new basic blocks, so we can ignore
      -- the returned block id.
      (instrs, _) <- stmtToInstrs bid (CmmUnsafeForeignCall target (catMaybes [res]) args)
      return instrs
  where
        -- Assume we can call these functions directly, and that they're not in a dynamic library.
        -- TODO: Why is this ok? Under linux this code will be in libm.so
        --       Is it because they're really implemented as a primitive instruction by the assembler??  -- BL 2009/12/31
        lbl = mkForeignLabel fn Nothing ForeignLabelInThisPackage IsFunction

        fn = case mop of
              MO_F32_Sqrt  -> fsLit "sqrtf"
              MO_F32_Fabs  -> fsLit "fabsf"
              MO_F32_Sin   -> fsLit "sinf"
              MO_F32_Cos   -> fsLit "cosf"
              MO_F32_Tan   -> fsLit "tanf"
              MO_F32_Exp   -> fsLit "expf"
              MO_F32_ExpM1 -> fsLit "expm1f"
              MO_F32_Log   -> fsLit "logf"
              MO_F32_Log1P -> fsLit "log1pf"

              MO_F32_Asin  -> fsLit "asinf"
              MO_F32_Acos  -> fsLit "acosf"
              MO_F32_Atan  -> fsLit "atanf"

              MO_F32_Sinh  -> fsLit "sinhf"
              MO_F32_Cosh  -> fsLit "coshf"
              MO_F32_Tanh  -> fsLit "tanhf"
              MO_F32_Pwr   -> fsLit "powf"

              MO_F32_Asinh -> fsLit "asinhf"
              MO_F32_Acosh -> fsLit "acoshf"
              MO_F32_Atanh -> fsLit "atanhf"

              MO_F64_Sqrt  -> fsLit "sqrt"
              MO_F64_Fabs  -> fsLit "fabs"
              MO_F64_Sin   -> fsLit "sin"
              MO_F64_Cos   -> fsLit "cos"
              MO_F64_Tan   -> fsLit "tan"
              MO_F64_Exp   -> fsLit "exp"
              MO_F64_ExpM1 -> fsLit "expm1"
              MO_F64_Log   -> fsLit "log"
              MO_F64_Log1P -> fsLit "log1p"

              MO_F64_Asin  -> fsLit "asin"
              MO_F64_Acos  -> fsLit "acos"
              MO_F64_Atan  -> fsLit "atan"

              MO_F64_Sinh  -> fsLit "sinh"
              MO_F64_Cosh  -> fsLit "cosh"
              MO_F64_Tanh  -> fsLit "tanh"
              MO_F64_Pwr   -> fsLit "pow"

              MO_F64_Asinh  -> fsLit "asinh"
              MO_F64_Acosh  -> fsLit "acosh"
              MO_F64_Atanh  -> fsLit "atanh"

              MO_Memcpy _  -> fsLit "memcpy"
              MO_Memset _  -> fsLit "memset"
              MO_Memmove _ -> fsLit "memmove"
              MO_Memcmp _  -> fsLit "memcmp"

              MO_PopCnt _  -> fsLit "popcnt"
              MO_BSwap _   -> fsLit "bswap"
              {- Here the C implementation is used as there is no x86
              instruction to reverse a word's bit order.
              -}
              MO_BRev w    -> fsLit $ bRevLabel w
              MO_Clz w     -> fsLit $ clzLabel w
              MO_Ctz _     -> unsupported

              MO_Pdep w    -> fsLit $ pdepLabel w
              MO_Pext w    -> fsLit $ pextLabel w

              MO_AtomicRMW _ _ -> fsLit "atomicrmw"
              MO_AtomicRead _  -> fsLit "atomicread"
              MO_AtomicWrite _ -> fsLit "atomicwrite"
              MO_Cmpxchg _     -> fsLit "cmpxchg"

              MO_UF_Conv _ -> unsupported

              MO_S_Mul2    {}  -> unsupported
              MO_S_QuotRem {}  -> unsupported
              MO_U_QuotRem {}  -> unsupported
              MO_U_QuotRem2 {} -> unsupported
              MO_Add2 {}       -> unsupported
              MO_AddIntC {}    -> unsupported
              MO_SubIntC {}    -> unsupported
              MO_AddWordC {}   -> unsupported
              MO_SubWordC {}   -> unsupported
              MO_U_Mul2 {}     -> unsupported
              MO_ReadBarrier   -> unsupported
              MO_WriteBarrier  -> unsupported
              MO_Touch         -> unsupported
              (MO_Prefetch_Data _ ) -> unsupported
        unsupported = panic ("outOfLineCmmOp: " ++ show mop
                          ++ " not supported here")

-- -----------------------------------------------------------------------------
-- Generating a table-branch

genSwitch :: CmmExpr -> SwitchTargets -> NatM InstrBlock

genSwitch expr targets = do
  config <- getConfig
  dflags <- getDynFlags
  let platform = ncgPlatform config
  if ncgPIC config
  then do
        (reg,e_code) <- getNonClobberedReg (cmmOffset platform expr offset)
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
        dynRef <- cmmMakeDynamicReference dflags DataReference lbl
        (tableReg,t_code) <- getSomeReg $ dynRef
        let op = OpAddr (AddrBaseIndex (EABaseReg tableReg)
                                       (EAIndex reg (platformWordSizeInBytes platform)) (ImmInt 0))

        offsetReg <- getNewRegNat (intFormat (platformWordWidth platform))
        return $ if is32bit || os == OSDarwin
                 then e_code `appOL` t_code `appOL` toOL [
                                ADD (intFormat (platformWordWidth platform)) op (OpReg tableReg),
                                JMP_TBL (OpReg tableReg) ids rosection lbl
                       ]
                 else -- HACK: On x86_64 binutils<2.17 is only able to generate
                      -- PC32 relocations, hence we only get 32-bit offsets in
                      -- the jump table. As these offsets are always negative
                      -- we need to properly sign extend them to 64-bit. This
                      -- hack should be removed in conjunction with the hack in
                      -- PprMach.hs/pprDataItem once binutils 2.17 is standard.
                      e_code `appOL` t_code `appOL` toOL [
                               MOVSxL II32 op (OpReg offsetReg),
                               ADD (intFormat (platformWordWidth platform))
                                   (OpReg offsetReg)
                                   (OpReg tableReg),
                               JMP_TBL (OpReg tableReg) ids rosection lbl
                       ]
  else do
        (reg,e_code) <- getSomeReg (cmmOffset platform expr offset)
        lbl <- getNewLabelNat
        let op = OpAddr (AddrBaseIndex EABaseNone (EAIndex reg (platformWordSizeInBytes platform)) (ImmCLbl lbl))
            code = e_code `appOL` toOL [
                    JMP_TBL op ids (Section ReadOnlyData lbl) lbl
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


-----------------------------------------------------------
---          Note [SSE Parity Checks]                   ---
-----------------------------------------------------------

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
-- Removing the jump reduces the pressure on the branch predidiction system
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
                LTT -> ASSERT2(False, ppr "Should have been turned into >")
                       and_ordered  dst
                LE  -> ASSERT2(False, ppr "Should have been turned into >=")
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
    = do is32Bit <- is32BitPlatform
         trivialCode' is32Bit width instr m a b

trivialCode' :: Bool -> Width -> (Operand -> Operand -> Instr)
             -> Maybe (Operand -> Operand -> Instr)
             -> CmmExpr -> CmmExpr -> NatM Register
trivialCode' is32Bit width _ (Just revinstr) (CmmLit lit_a) b
  | is32BitLit is32Bit lit_a = do
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
     -- We want the value of b to stay alive across the computation of a.
     -- But, we want to calculate a straight into the destination register,
     -- because the instruction only has two operands (dst := dst `op` src).
     -- The troublesome case is when the result of b is in the same register
     -- as the destination reg.  In this case, we have to save b in a
     -- new temporary across the computation of a.
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


trivialUFCode :: Format -> (Reg -> Reg -> Instr) -> CmmExpr -> NatM Register
trivialUFCode format instr x = do
  (x_reg, x_code) <- getSomeReg x
  let
     code dst =
        x_code `snocOL`
        instr x_reg dst
  return (Any format code)


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
    invert ((BasicBlock lbl1 ins@(_:_:_xs)):b2@(BasicBlock lbl2 _):bs)
      | --pprTrace "Block" (ppr lbl1) True,
        (jmp1,jmp2) <- last2 ins
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
