-----------------------------------------------------------------------------
--
-- Generating machine code (instruction selection)
--
-- (c) The University of Glasgow 1996-2013
--
-----------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
module SPARC.CodeGen (
        cmmTopCodeGen,
        generateJumpTableForInstr,
        InstrBlock
)

where

#include "HsVersions.h"
#include "nativeGen/NCG.h"
#include "../includes/MachDeps.h"

-- NCG stuff:
import SPARC.Base
import SPARC.CodeGen.Sanity
import SPARC.CodeGen.Amode
import SPARC.CodeGen.CondCode
import SPARC.CodeGen.Gen64
import SPARC.CodeGen.Gen32
import SPARC.CodeGen.Base
import SPARC.Ppr        ()
import SPARC.Instr
import SPARC.Imm
import SPARC.AddrMode
import SPARC.Regs
import SPARC.Stack
import Instruction
import Size
import NCGMonad

-- Our intermediate code:
import BlockId
import Cmm
import CmmUtils
import Hoopl
import PIC
import Reg
import CLabel
import CPrim

-- The rest:
import BasicTypes
import DynFlags
import FastString
import OrdList
import Outputable
import Platform
import Unique

import Control.Monad    ( mapAndUnzipM )

-- | Top level code generation
cmmTopCodeGen :: RawCmmDecl
              -> NatM [NatCmmDecl CmmStatics Instr]

cmmTopCodeGen (CmmProc info lab live graph)
 = do let blocks = toBlockListEntryFirst graph
      (nat_blocks,statics) <- mapAndUnzipM basicBlockCodeGen blocks

      let proc = CmmProc info lab live (ListGraph $ concat nat_blocks)
      let tops = proc : concat statics

      return tops

cmmTopCodeGen (CmmData sec dat) = do
  return [CmmData sec dat]  -- no translation, we just use CmmStatic


-- | Do code generation on a single block of CMM code.
--      code generation may introduce new basic block boundaries, which
--      are indicated by the NEWBLOCK instruction.  We must split up the
--      instruction stream into basic blocks again.  Also, we extract
--      LDATAs here too.
basicBlockCodeGen :: CmmBlock
                  -> NatM ( [NatBasicBlock Instr]
                          , [NatCmmDecl CmmStatics Instr])

basicBlockCodeGen block = do
  let (CmmEntry id, nodes, tail)  = blockSplit block
      stmts = blockToList nodes
  mid_instrs <- stmtsToInstrs stmts
  tail_instrs <- stmtToInstrs tail
  let instrs = mid_instrs `appOL` tail_instrs
  let
        (top,other_blocks,statics)
                = foldrOL mkBlocks ([],[],[]) instrs

        mkBlocks (NEWBLOCK id) (instrs,blocks,statics)
          = ([], BasicBlock id instrs : blocks, statics)

        mkBlocks (LDATA sec dat) (instrs,blocks,statics)
          = (instrs, blocks, CmmData sec dat:statics)

        mkBlocks instr (instrs,blocks,statics)
          = (instr:instrs, blocks, statics)

        -- do intra-block sanity checking
        blocksChecked
                = map (checkBlock block)
                $ BasicBlock id top : other_blocks

  return (blocksChecked, statics)


-- | Convert some Cmm statements to SPARC instructions.
stmtsToInstrs :: [CmmNode e x] -> NatM InstrBlock
stmtsToInstrs stmts
   = do instrss <- mapM stmtToInstrs stmts
        return (concatOL instrss)


stmtToInstrs :: CmmNode e x -> NatM InstrBlock
stmtToInstrs stmt = do
  dflags <- getDynFlags
  case stmt of
    CmmComment s   -> return (unitOL (COMMENT s))

    CmmAssign reg src
      | isFloatType ty  -> assignReg_FltCode size reg src
      | isWord64 ty     -> assignReg_I64Code      reg src
      | otherwise       -> assignReg_IntCode size reg src
        where ty = cmmRegType dflags reg
              size = cmmTypeSize ty

    CmmStore addr src
      | isFloatType ty  -> assignMem_FltCode size addr src
      | isWord64 ty     -> assignMem_I64Code      addr src
      | otherwise       -> assignMem_IntCode size addr src
        where ty = cmmExprType dflags src
              size = cmmTypeSize ty

    CmmUnsafeForeignCall target result_regs args
       -> genCCall target result_regs args

    CmmBranch   id              -> genBranch id
    CmmCondBranch arg true false -> do b1 <- genCondJump true arg
                                       b2 <- genBranch false
                                       return (b1 `appOL` b2)
    CmmSwitch   arg ids         -> do dflags <- getDynFlags
                                      genSwitch dflags arg ids
    CmmCall { cml_target = arg } -> genJump arg

    _
     -> panic "stmtToInstrs: statement should have been cps'd away"


{-
Now, given a tree (the argument to an CmmLoad) that references memory,
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



-- | Convert a BlockId to some CmmStatic data
jumpTableEntry :: DynFlags -> Maybe BlockId -> CmmStatic
jumpTableEntry dflags Nothing = CmmStaticLit (CmmInt 0 (wordWidth dflags))
jumpTableEntry _ (Just blockid) = CmmStaticLit (CmmLabel blockLabel)
    where blockLabel = mkAsmTempLabel (getUnique blockid)



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

assignMem_IntCode :: Size -> CmmExpr -> CmmExpr -> NatM InstrBlock
assignMem_IntCode pk addr src = do
    (srcReg, code) <- getSomeReg src
    Amode dstAddr addr_code <- getAmode addr
    return $ code `appOL` addr_code `snocOL` ST pk srcReg dstAddr


assignReg_IntCode :: Size -> CmmReg  -> CmmExpr -> NatM InstrBlock
assignReg_IntCode _ reg src = do
    dflags <- getDynFlags
    r <- getRegister src
    let dst = getRegisterReg (targetPlatform dflags) reg
    return $ case r of
        Any _ code         -> code dst
        Fixed _ freg fcode -> fcode `snocOL` OR False g0 (RIReg freg) dst



-- Floating point assignment to memory
assignMem_FltCode :: Size -> CmmExpr -> CmmExpr -> NatM InstrBlock
assignMem_FltCode pk addr src = do
    dflags <- getDynFlags
    Amode dst__2 code1 <- getAmode addr
    (src__2, code2) <- getSomeReg src
    tmp1 <- getNewRegNat pk
    let
        pk__2   = cmmExprType dflags src
        code__2 = code1 `appOL` code2 `appOL`
            if   sizeToWidth pk == typeWidth pk__2
            then unitOL (ST pk src__2 dst__2)
            else toOL   [ FxTOy (cmmTypeSize pk__2) pk src__2 tmp1
                        , ST    pk tmp1 dst__2]
    return code__2

-- Floating point assignment to a register/temporary
assignReg_FltCode :: Size -> CmmReg  -> CmmExpr -> NatM InstrBlock
assignReg_FltCode pk dstCmmReg srcCmmExpr = do
    dflags <- getDynFlags
    let platform = targetPlatform dflags
    srcRegister <- getRegister srcCmmExpr
    let dstReg  = getRegisterReg platform dstCmmReg

    return $ case srcRegister of
        Any _ code                  -> code dstReg
        Fixed _ srcFixedReg srcCode -> srcCode `snocOL` FMOV pk srcFixedReg dstReg




genJump :: CmmExpr{-the branch target-} -> NatM InstrBlock

genJump (CmmLit (CmmLabel lbl))
  = return (toOL [CALL (Left target) 0 True, NOP])
  where
    target = ImmCLbl lbl

genJump tree
  = do
        (target, code) <- getSomeReg tree
        return (code `snocOL` JMP (AddrRegReg target g0)  `snocOL` NOP)

-- -----------------------------------------------------------------------------
--  Unconditional branches

genBranch :: BlockId -> NatM InstrBlock
genBranch = return . toOL . mkJumpInstr


-- -----------------------------------------------------------------------------
--  Conditional jumps

{-
Conditional jumps are always to local labels, so we can use branch
instructions.  We peek at the arguments to decide what kind of
comparison to do.

SPARC: First, we have to ensure that the condition codes are set
according to the supplied comparison operation.  We generate slightly
different code for floating point comparisons, because a floating
point operation cannot directly precede a @BF@.  We assume the worst
and fill that slot with a @NOP@.

SPARC: Do not fill the delay slots here; you will confuse the register
allocator.
-}


genCondJump
    :: BlockId      -- the branch target
    -> CmmExpr      -- the condition on which to branch
    -> NatM InstrBlock



genCondJump bid bool = do
  CondCode is_float cond code <- getCondCode bool
  return (
       code `appOL`
       toOL (
         if   is_float
         then [NOP, BF cond False bid, NOP]
         else [BI cond False bid, NOP]
       )
    )



-- -----------------------------------------------------------------------------
-- Generating a table-branch

genSwitch :: DynFlags -> CmmExpr -> [Maybe BlockId] -> NatM InstrBlock
genSwitch dflags expr ids
        | gopt Opt_PIC dflags
        = error "MachCodeGen: sparc genSwitch PIC not finished\n"

        | otherwise
        = do    (e_reg, e_code) <- getSomeReg expr

                base_reg        <- getNewRegNat II32
                offset_reg      <- getNewRegNat II32
                dst             <- getNewRegNat II32

                label           <- getNewLabelNat

                return $ e_code `appOL`
                 toOL
                        [ -- load base of jump table
                          SETHI (HI (ImmCLbl label)) base_reg
                        , OR    False base_reg (RIImm $ LO $ ImmCLbl label) base_reg

                        -- the addrs in the table are 32 bits wide..
                        , SLL   e_reg (RIImm $ ImmInt 2) offset_reg

                        -- load and jump to the destination
                        , LD      II32 (AddrRegReg base_reg offset_reg) dst
                        , JMP_TBL (AddrRegImm dst (ImmInt 0)) ids label
                        , NOP ]

generateJumpTableForInstr :: DynFlags -> Instr
                          -> Maybe (NatCmmDecl CmmStatics Instr)
generateJumpTableForInstr dflags (JMP_TBL _ ids label) =
        let jumpTable = map (jumpTableEntry dflags) ids
        in Just (CmmData ReadOnlyData (Statics label jumpTable))
generateJumpTableForInstr _ _ = Nothing



-- -----------------------------------------------------------------------------
-- Generating C calls

{-
   Now the biggest nightmare---calls.  Most of the nastiness is buried in
   @get_arg@, which moves the arguments to the correct registers/stack
   locations.  Apart from that, the code is easy.

   The SPARC calling convention is an absolute
   nightmare.  The first 6x32 bits of arguments are mapped into
   %o0 through %o5, and the remaining arguments are dumped to the
   stack, beginning at [%sp+92].  (Note that %o6 == %sp.)

   If we have to put args on the stack, move %o6==%sp down by
   the number of words to go on the stack, to ensure there's enough space.

   According to Fraser and Hanson's lcc book, page 478, fig 17.2,
   16 words above the stack pointer is a word for the address of
   a structure return value.  I use this as a temporary location
   for moving values from float to int regs.  Certainly it isn't
   safe to put anything in the 16 words starting at %sp, since
   this area can get trashed at any time due to window overflows
   caused by signal handlers.

   A final complication (if the above isn't enough) is that
   we can't blithely calculate the arguments one by one into
   %o0 .. %o5.  Consider the following nested calls:

       fff a (fff b c)

   Naive code moves a into %o0, and (fff b c) into %o1.  Unfortunately
   the inner call will itself use %o0, which trashes the value put there
   in preparation for the outer call.  Upshot: we need to calculate the
   args into temporary regs, and move those to arg regs or onto the
   stack only immediately prior to the call proper.  Sigh.
-}

genCCall
    :: ForeignTarget            -- function to call
    -> [CmmFormal]        -- where to put the result
    -> [CmmActual]        -- arguments (of mixed type)
    -> NatM InstrBlock



-- On SPARC under TSO (Total Store Ordering), writes earlier in the instruction stream
-- are guaranteed to take place before writes afterwards (unlike on PowerPC).
-- Ref: Section 8.4 of the SPARC V9 Architecture manual.
--
-- In the SPARC case we don't need a barrier.
--
genCCall (PrimTarget MO_WriteBarrier) _ _
 = do   return nilOL

genCCall target dest_regs args0
 = do
        -- need to remove alignment information
        let args | PrimTarget mop <- target,
                            (mop == MO_Memcpy ||
                             mop == MO_Memset ||
                             mop == MO_Memmove)
                          = init args0

                          | otherwise
                          = args0

        -- work out the arguments, and assign them to integer regs
        argcode_and_vregs       <- mapM arg_to_int_vregs args
        let (argcodes, vregss)  = unzip argcode_and_vregs
        let vregs               = concat vregss

        let n_argRegs           = length allArgRegs
        let n_argRegs_used      = min (length vregs) n_argRegs


        -- deal with static vs dynamic call targets
        callinsns <- case target of
                ForeignTarget (CmmLit (CmmLabel lbl)) _ ->
                        return (unitOL (CALL (Left (litToImm (CmmLabel lbl))) n_argRegs_used False))

                ForeignTarget expr _
                 -> do  (dyn_c, [dyn_r]) <- arg_to_int_vregs expr
                        return (dyn_c `snocOL` CALL (Right dyn_r) n_argRegs_used False)

                PrimTarget mop
                 -> do  res     <- outOfLineMachOp mop
                        lblOrMopExpr <- case res of
                                Left lbl -> do
                                        return (unitOL (CALL (Left (litToImm (CmmLabel lbl))) n_argRegs_used False))

                                Right mopExpr -> do
                                        (dyn_c, [dyn_r]) <- arg_to_int_vregs mopExpr
                                        return (dyn_c `snocOL` CALL (Right dyn_r) n_argRegs_used False)

                        return lblOrMopExpr

        let argcode = concatOL argcodes

        let (move_sp_down, move_sp_up)
                   = let diff = length vregs - n_argRegs
                         nn   = if odd diff then diff + 1 else diff -- keep 8-byte alignment
                     in  if   nn <= 0
                         then (nilOL, nilOL)
                         else (unitOL (moveSp (-1*nn)), unitOL (moveSp (1*nn)))

        let transfer_code
                = toOL (move_final vregs allArgRegs extraStackArgsHere)

        dflags <- getDynFlags
        return
         $      argcode                 `appOL`
                move_sp_down            `appOL`
                transfer_code           `appOL`
                callinsns               `appOL`
                unitOL NOP              `appOL`
                move_sp_up              `appOL`
                assign_code (targetPlatform dflags) dest_regs


-- | Generate code to calculate an argument, and move it into one
--      or two integer vregs.
arg_to_int_vregs :: CmmExpr -> NatM (OrdList Instr, [Reg])
arg_to_int_vregs arg = do dflags <- getDynFlags
                          arg_to_int_vregs' dflags arg

arg_to_int_vregs' :: DynFlags -> CmmExpr -> NatM (OrdList Instr, [Reg])
arg_to_int_vregs' dflags arg

        -- If the expr produces a 64 bit int, then we can just use iselExpr64
        | isWord64 (cmmExprType dflags arg)
        = do    (ChildCode64 code r_lo) <- iselExpr64 arg
                let r_hi                = getHiVRegFromLo r_lo
                return (code, [r_hi, r_lo])

        | otherwise
        = do    (src, code)     <- getSomeReg arg
                let pk          = cmmExprType dflags arg

                case cmmTypeSize pk of

                 -- Load a 64 bit float return value into two integer regs.
                 FF64 -> do
                        v1 <- getNewRegNat II32
                        v2 <- getNewRegNat II32

                        let code2 =
                                code                            `snocOL`
                                FMOV FF64 src f0                `snocOL`
                                ST   FF32  f0 (spRel 16)        `snocOL`
                                LD   II32  (spRel 16) v1        `snocOL`
                                ST   FF32  f1 (spRel 16)        `snocOL`
                                LD   II32  (spRel 16) v2

                        return  (code2, [v1,v2])

                 -- Load a 32 bit float return value into an integer reg
                 FF32 -> do
                        v1 <- getNewRegNat II32

                        let code2 =
                                code                            `snocOL`
                                ST   FF32  src (spRel 16)       `snocOL`
                                LD   II32  (spRel 16) v1

                        return (code2, [v1])

                 -- Move an integer return value into its destination reg.
                 _ -> do
                        v1 <- getNewRegNat II32

                        let code2 =
                                code                            `snocOL`
                                OR False g0 (RIReg src) v1

                        return (code2, [v1])


-- | Move args from the integer vregs into which they have been
--      marshalled, into %o0 .. %o5, and the rest onto the stack.
--
move_final :: [Reg] -> [Reg] -> Int -> [Instr]

-- all args done
move_final [] _ _
        = []

-- out of aregs; move to stack
move_final (v:vs) [] offset
        = ST II32 v (spRel offset)
        : move_final vs [] (offset+1)

-- move into an arg (%o[0..5]) reg
move_final (v:vs) (a:az) offset
        = OR False g0 (RIReg v) a
        : move_final vs az offset


-- | Assign results returned from the call into their
--      destination regs.
--
assign_code :: Platform -> [LocalReg] -> OrdList Instr

assign_code _ [] = nilOL

assign_code platform [dest]
 = let  rep     = localRegType dest
        width   = typeWidth rep
        r_dest  = getRegisterReg platform (CmmLocal dest)

        result
                | isFloatType rep
                , W32   <- width
                = unitOL $ FMOV FF32 (regSingle $ fReg 0) r_dest

                | isFloatType rep
                , W64   <- width
                = unitOL $ FMOV FF64 (regSingle $ fReg 0) r_dest

                | not $ isFloatType rep
                , W32   <- width
                = unitOL $ mkRegRegMoveInstr platform (regSingle $ oReg 0) r_dest

                | not $ isFloatType rep
                , W64           <- width
                , r_dest_hi     <- getHiVRegFromLo r_dest
                = toOL  [ mkRegRegMoveInstr platform (regSingle $ oReg 0) r_dest_hi
                        , mkRegRegMoveInstr platform (regSingle $ oReg 1) r_dest]

                | otherwise
                = panic "SPARC.CodeGen.GenCCall: no match"

   in   result

assign_code _ _
        = panic "SPARC.CodeGen.GenCCall: no match"



-- | Generate a call to implement an out-of-line floating point operation
outOfLineMachOp
        :: CallishMachOp
        -> NatM (Either CLabel CmmExpr)

outOfLineMachOp mop
 = do   let functionName
                = outOfLineMachOp_table mop

        dflags  <- getDynFlags
        mopExpr <- cmmMakeDynamicReference dflags CallReference
                $  mkForeignLabel functionName Nothing ForeignLabelInExternalPackage IsFunction

        let mopLabelOrExpr
                = case mopExpr of
                        CmmLit (CmmLabel lbl)   -> Left lbl
                        _                       -> Right mopExpr

        return mopLabelOrExpr


-- | Decide what C function to use to implement a CallishMachOp
--
outOfLineMachOp_table
        :: CallishMachOp
        -> FastString

outOfLineMachOp_table mop
 = case mop of
        MO_F32_Exp    -> fsLit "expf"
        MO_F32_Log    -> fsLit "logf"
        MO_F32_Sqrt   -> fsLit "sqrtf"
        MO_F32_Pwr    -> fsLit "powf"

        MO_F32_Sin    -> fsLit "sinf"
        MO_F32_Cos    -> fsLit "cosf"
        MO_F32_Tan    -> fsLit "tanf"

        MO_F32_Asin   -> fsLit "asinf"
        MO_F32_Acos   -> fsLit "acosf"
        MO_F32_Atan   -> fsLit "atanf"

        MO_F32_Sinh   -> fsLit "sinhf"
        MO_F32_Cosh   -> fsLit "coshf"
        MO_F32_Tanh   -> fsLit "tanhf"

        MO_F64_Exp    -> fsLit "exp"
        MO_F64_Log    -> fsLit "log"
        MO_F64_Sqrt   -> fsLit "sqrt"
        MO_F64_Pwr    -> fsLit "pow"

        MO_F64_Sin    -> fsLit "sin"
        MO_F64_Cos    -> fsLit "cos"
        MO_F64_Tan    -> fsLit "tan"

        MO_F64_Asin   -> fsLit "asin"
        MO_F64_Acos   -> fsLit "acos"
        MO_F64_Atan   -> fsLit "atan"

        MO_F64_Sinh   -> fsLit "sinh"
        MO_F64_Cosh   -> fsLit "cosh"
        MO_F64_Tanh   -> fsLit "tanh"

        MO_UF_Conv w -> fsLit $ word2FloatLabel w

        MO_Memcpy    -> fsLit "memcpy"
        MO_Memset    -> fsLit "memset"
        MO_Memmove   -> fsLit "memmove"

        MO_PopCnt w  -> fsLit $ popCntLabel w

        MO_S_QuotRem {}  -> unsupported
        MO_U_QuotRem {}  -> unsupported
        MO_U_QuotRem2 {} -> unsupported
        MO_Add2 {}       -> unsupported
        MO_U_Mul2 {}     -> unsupported
        MO_WriteBarrier  -> unsupported
        MO_Touch         -> unsupported
        MO_Prefetch_Data -> unsupported
    where unsupported = panic ("outOfLineCmmOp: " ++ show mop
                            ++ " not supported here")

