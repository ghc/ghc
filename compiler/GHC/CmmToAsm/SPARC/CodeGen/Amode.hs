module GHC.CmmToAsm.SPARC.CodeGen.Amode (
        getAmode
)

where

import GHC.Prelude

import {-# SOURCE #-} GHC.CmmToAsm.SPARC.CodeGen.Gen32
import GHC.CmmToAsm.SPARC.CodeGen.Base
import GHC.CmmToAsm.SPARC.AddrMode
import GHC.CmmToAsm.SPARC.Imm
import GHC.CmmToAsm.SPARC.Instr
import GHC.CmmToAsm.SPARC.Regs
import GHC.CmmToAsm.SPARC.Base
import GHC.CmmToAsm.Monad
import GHC.CmmToAsm.Format

import GHC.Cmm

import GHC.Data.OrdList


-- | Generate code to reference a memory address.
getAmode
        :: CmmExpr      -- ^ expr producing an address
        -> NatM Amode

getAmode tree@(CmmRegOff _ _)
    = do platform <- getPlatform
         getAmode (mangleIndexTree platform tree)

getAmode (CmmMachOp (MO_Sub _) [x, CmmLit (CmmInt i _)])
  | fits13Bits (-i)
  = do
       (reg, code) <- getSomeReg x
       let
         off  = ImmInt (-(fromInteger i))
       return (Amode (AddrRegImm reg off) code)


getAmode (CmmMachOp (MO_Add _) [x, CmmLit (CmmInt i _)])
  | fits13Bits i
  = do
       (reg, code) <- getSomeReg x
       let
         off  = ImmInt (fromInteger i)
       return (Amode (AddrRegImm reg off) code)

getAmode (CmmMachOp (MO_Add _) [x, y])
  = do
    (regX, codeX) <- getSomeReg x
    (regY, codeY) <- getSomeReg y
    let
        code = codeX `appOL` codeY
    return (Amode (AddrRegReg regX regY) code)

getAmode (CmmLit lit)
  = do
        let imm__2      = litToImm lit
        tmp1    <- getNewRegNat II32
        tmp2    <- getNewRegNat II32

        let code = toOL [ SETHI (HI imm__2) tmp1
                        , OR    False tmp1 (RIImm (LO imm__2)) tmp2]

        return (Amode (AddrRegReg tmp2 g0) code)

getAmode other
  = do
       (reg, code) <- getSomeReg other
       let
            off  = ImmInt 0
       return (Amode (AddrRegImm reg off) code)
