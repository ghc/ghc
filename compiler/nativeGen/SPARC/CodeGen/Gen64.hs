-- | Evaluation of 64 bit values on 32 bit platforms.
module SPARC.CodeGen.Gen64 (
        assignMem_I64Code,
        assignReg_I64Code,
        iselExpr64
)

where

import {-# SOURCE #-} SPARC.CodeGen.Gen32
import SPARC.CodeGen.Base
import SPARC.CodeGen.Amode
import SPARC.Regs
import SPARC.AddrMode
import SPARC.Imm
import SPARC.Instr
import SPARC.Ppr()
import NCGMonad
import Instruction
import Size
import Reg

import Cmm

import DynFlags
import OrdList
import Outputable

-- | Code to assign a 64 bit value to memory.
assignMem_I64Code 
        :: CmmExpr              -- ^ expr producing the destination address
        -> CmmExpr              -- ^ expr producing the source value.
        -> NatM InstrBlock

assignMem_I64Code addrTree valueTree 
 = do
     ChildCode64 vcode rlo      <- iselExpr64 valueTree  

     (src, acode) <- getSomeReg addrTree
     let 
         rhi = getHiVRegFromLo rlo
 
         -- Big-endian store
         mov_hi = ST II32 rhi (AddrRegImm src (ImmInt 0))
         mov_lo = ST II32 rlo (AddrRegImm src (ImmInt 4))
         
         code   = vcode `appOL` acode `snocOL` mov_hi `snocOL` mov_lo

{-     pprTrace "assignMem_I64Code" 
        (vcat   [ text "addrTree:  " <+> ppr addrTree
                , text "valueTree: " <+> ppr valueTree
                , text "vcode:"
                , vcat $ map ppr $ fromOL vcode 
                , text ""
                , text "acode:"
                , vcat $ map ppr $ fromOL acode ])
       $ -}
     return code


-- | Code to assign a 64 bit value to a register.
assignReg_I64Code 
        :: CmmReg               -- ^ the destination register
        -> CmmExpr              -- ^ expr producing the source value
        -> NatM InstrBlock

assignReg_I64Code (CmmLocal (LocalReg u_dst pk)) valueTree 
 = do
     ChildCode64 vcode r_src_lo <- iselExpr64 valueTree    
     let 
         r_dst_lo = RegVirtual $ mkVirtualReg u_dst (cmmTypeSize pk)
         r_dst_hi = getHiVRegFromLo r_dst_lo
         r_src_hi = getHiVRegFromLo r_src_lo
         mov_lo = mkMOV r_src_lo r_dst_lo
         mov_hi = mkMOV r_src_hi r_dst_hi
         mkMOV sreg dreg = OR False g0 (RIReg sreg) dreg

     return (vcode `snocOL` mov_hi `snocOL` mov_lo)

assignReg_I64Code _ _
   = panic "assignReg_I64Code(sparc): invalid lvalue"




-- | Get the value of an expression into a 64 bit register.

iselExpr64 :: CmmExpr -> NatM ChildCode64

-- Load a 64 bit word
iselExpr64 (CmmLoad addrTree ty) 
 | isWord64 ty
 = do   Amode amode addr_code   <- getAmode addrTree
        let result

                | AddrRegReg r1 r2      <- amode
                = do    rlo     <- getNewRegNat II32
                        tmp     <- getNewRegNat II32
                        let rhi = getHiVRegFromLo rlo

                        return  $ ChildCode64 
                                (        addr_code 
                                `appOL`  toOL
                                         [ ADD False False r1 (RIReg r2) tmp
                                         , LD II32 (AddrRegImm tmp (ImmInt 0)) rhi
                                         , LD II32 (AddrRegImm tmp (ImmInt 4)) rlo ])
                                rlo

                | AddrRegImm r1 (ImmInt i) <- amode
                = do    rlo     <- getNewRegNat II32
                        let rhi = getHiVRegFromLo rlo
                        
                        return  $ ChildCode64 
                                (        addr_code 
                                `appOL`  toOL
                                         [ LD II32 (AddrRegImm r1 (ImmInt $ 0 + i)) rhi
                                         , LD II32 (AddrRegImm r1 (ImmInt $ 4 + i)) rlo ])
                                rlo

                | otherwise
                = panic "SPARC.CodeGen.Gen64: no match"
                
        result


-- Add a literal to a 64 bit integer
iselExpr64 (CmmMachOp (MO_Add _) [e1, CmmLit (CmmInt i _)]) 
 = do   ChildCode64 code1 r1_lo <- iselExpr64 e1
        let r1_hi       = getHiVRegFromLo r1_lo
        
        r_dst_lo        <- getNewRegNat II32
        let r_dst_hi    =  getHiVRegFromLo r_dst_lo 
        
        let code =      code1
                `appOL` toOL
                        [ ADD False True  r1_lo (RIImm (ImmInteger i)) r_dst_lo
                        , ADD True  False r1_hi (RIReg g0)         r_dst_hi ]
                        
        return  $ ChildCode64 code r_dst_lo


-- Addition of II64
iselExpr64 (CmmMachOp (MO_Add _) [e1, e2])
 = do   ChildCode64 code1 r1_lo <- iselExpr64 e1
        let r1_hi       = getHiVRegFromLo r1_lo

        ChildCode64 code2 r2_lo <- iselExpr64 e2
        let r2_hi       = getHiVRegFromLo r2_lo
        
        r_dst_lo        <- getNewRegNat II32
        let r_dst_hi    = getHiVRegFromLo r_dst_lo
        
        let code =      code1
                `appOL` code2
                `appOL` toOL
                        [ ADD False True  r1_lo (RIReg r2_lo) r_dst_lo
                        , ADD True  False r1_hi (RIReg r2_hi) r_dst_hi ]
        
        return  $ ChildCode64 code r_dst_lo


iselExpr64 (CmmReg (CmmLocal (LocalReg uq ty))) 
 | isWord64 ty 
 = do
     r_dst_lo <-  getNewRegNat II32
     let r_dst_hi = getHiVRegFromLo r_dst_lo
         r_src_lo = RegVirtual $ mkVirtualReg uq II32
         r_src_hi = getHiVRegFromLo r_src_lo
         mov_lo = mkMOV r_src_lo r_dst_lo
         mov_hi = mkMOV r_src_hi r_dst_hi
         mkMOV sreg dreg = OR False g0 (RIReg sreg) dreg
     return (
            ChildCode64 (toOL [mov_hi, mov_lo]) r_dst_lo
         )

-- Convert something into II64
iselExpr64 (CmmMachOp (MO_UU_Conv _ W64) [expr]) 
 = do
        r_dst_lo        <- getNewRegNat II32
        let r_dst_hi    = getHiVRegFromLo r_dst_lo

        -- compute expr and load it into r_dst_lo
        (a_reg, a_code) <- getSomeReg expr

        dflags <- getDynFlags
        let platform = targetPlatform dflags
            code        = a_code
                `appOL` toOL
                        [ mkRegRegMoveInstr platform g0    r_dst_hi     -- clear high 32 bits
                        , mkRegRegMoveInstr platform a_reg r_dst_lo ]
                        
        return  $ ChildCode64 code r_dst_lo


iselExpr64 expr
   = pprPanic "iselExpr64(sparc)" (ppr expr)



