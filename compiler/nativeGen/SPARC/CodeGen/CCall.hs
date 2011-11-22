-- | Generating C calls

module SPARC.CodeGen.CCall (
        genCCall
)

where

import SPARC.CodeGen.Gen64
import SPARC.CodeGen.Gen32
import SPARC.CodeGen.Base
import SPARC.Stack
import SPARC.Instr
import SPARC.Imm
import SPARC.Regs
import SPARC.Base
import CPrim
import NCGMonad
import PIC
import Instruction
import Size
import Reg

import OldCmm
import CLabel
import BasicTypes

import OrdList
import DynFlags
import FastString
import Outputable
import Platform

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
    :: CmmCallTarget            -- function to call
    -> [HintedCmmFormal]        -- where to put the result
    -> [HintedCmmActual]        -- arguments (of mixed type)
    -> NatM InstrBlock



-- On SPARC under TSO (Total Store Ordering), writes earlier in the instruction stream
-- are guaranteed to take place before writes afterwards (unlike on PowerPC). 
-- Ref: Section 8.4 of the SPARC V9 Architecture manual.
--
-- In the SPARC case we don't need a barrier.
--
genCCall (CmmPrim (MO_WriteBarrier)) _ _
 = do   return nilOL

genCCall target dest_regs argsAndHints 
 = do           
        -- need to remove alignment information
        let argsAndHints' | (CmmPrim mop) <- target,
                            (mop == MO_Memcpy ||
                             mop == MO_Memset ||
                             mop == MO_Memmove)
                          = init argsAndHints

                          | otherwise
                          = argsAndHints
                
        -- strip hints from the arg regs
        let args :: [CmmExpr]
            args  = map hintlessCmm argsAndHints'


        -- work out the arguments, and assign them to integer regs
        argcode_and_vregs       <- mapM arg_to_int_vregs args
        let (argcodes, vregss)  = unzip argcode_and_vregs
        let vregs               = concat vregss

        let n_argRegs           = length allArgRegs
        let n_argRegs_used      = min (length vregs) n_argRegs


        -- deal with static vs dynamic call targets
        callinsns <- case target of
                CmmCallee (CmmLit (CmmLabel lbl)) _ -> 
                        return (unitOL (CALL (Left (litToImm (CmmLabel lbl))) n_argRegs_used False))

                CmmCallee expr _
                 -> do  (dyn_c, [dyn_r]) <- arg_to_int_vregs expr
                        return (dyn_c `snocOL` CALL (Right dyn_r) n_argRegs_used False)

                CmmPrim mop 
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
                                
        dflags <- getDynFlagsNat
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
arg_to_int_vregs arg

        -- If the expr produces a 64 bit int, then we can just use iselExpr64
        | isWord64 (cmmExprType arg)
        = do    (ChildCode64 code r_lo) <- iselExpr64 arg
                let r_hi                = getHiVRegFromLo r_lo
                return (code, [r_hi, r_lo])

        | otherwise
        = do    (src, code)     <- getSomeReg arg
                let pk          = cmmExprType arg

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
--      desination regs.
--
assign_code :: Platform -> [CmmHinted LocalReg] -> OrdList Instr

assign_code _ [] = nilOL

assign_code platform [CmmHinted dest _hint]
 = let  rep     = localRegType dest
        width   = typeWidth rep
        r_dest  = getRegisterReg (CmmLocal dest)

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
        
        dflags  <- getDynFlagsNat
        mopExpr <- cmmMakeDynamicReference dflags addImportNat CallReference 
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

        MO_Memcpy    -> fsLit "memcpy"
        MO_Memset    -> fsLit "memset"
        MO_Memmove   -> fsLit "memmove"

        MO_PopCnt w  -> fsLit $ popCntLabel w

        MO_WriteBarrier ->
            panic $ "outOfLineCmmOp: MO_WriteBarrier not supported here"
        MO_Touch ->
            panic $ "outOfLineCmmOp: MO_Touch not supported here"

