--------------------------------------------------------------------------------
-- | Deal with Cmm registers
--

module LlvmCodeGen.Regs (
        lmGlobalRegArg, lmGlobalRegVar
    ) where

#include "HsVersions.h"

import Llvm

import CmmExpr
import Outputable ( panic )
import FastString

-- | Get the LlvmVar function variable storing the real register
lmGlobalRegVar :: GlobalReg -> LlvmVar
lmGlobalRegVar = (pVarLift . lmGlobalReg "_Var")

-- | Get the LlvmVar function argument storing the real register
lmGlobalRegArg :: GlobalReg -> LlvmVar
lmGlobalRegArg = lmGlobalReg "_Arg"

{- Need to make sure the names here can't conflict with the unique generated
   names. Uniques generated names containing only base62 chars. So using say
    the '_' char guarantees this.
-}
lmGlobalReg :: String -> GlobalReg -> LlvmVar
lmGlobalReg suf reg
  = case reg of
        BaseReg        -> ptrGlobal $ "Base" ++ suf
        Sp             -> ptrGlobal $ "Sp" ++ suf
        Hp             -> ptrGlobal $ "Hp" ++ suf
        VanillaReg 1 _ -> wordGlobal $ "R1" ++ suf
        VanillaReg 2 _ -> wordGlobal $ "R2" ++ suf
        VanillaReg 3 _ -> wordGlobal $ "R3" ++ suf
        VanillaReg 4 _ -> wordGlobal $ "R4" ++ suf
        VanillaReg 5 _ -> wordGlobal $ "R5" ++ suf
        VanillaReg 6 _ -> wordGlobal $ "R6" ++ suf
        VanillaReg 7 _ -> wordGlobal $ "R7" ++ suf
        VanillaReg 8 _ -> wordGlobal $ "R8" ++ suf
        SpLim          -> wordGlobal $ "SpLim" ++ suf
        FloatReg 1     -> floatGlobal $"F1" ++ suf
        FloatReg 2     -> floatGlobal $"F2" ++ suf
        FloatReg 3     -> floatGlobal $"F3" ++ suf
        FloatReg 4     -> floatGlobal $"F4" ++ suf
        DoubleReg 1    -> doubleGlobal $ "D1" ++ suf
        DoubleReg 2    -> doubleGlobal $ "D2" ++ suf
        _other         -> panic $ "LlvmCodeGen.Reg: GlobalReg (" ++ (show reg)
                                ++ ") not supported!"
    where
        wordGlobal   name = LMNLocalVar (fsLit name) llvmWord
        ptrGlobal    name = LMNLocalVar (fsLit name) llvmWordPtr
        floatGlobal  name = LMNLocalVar (fsLit name) LMFloat
        doubleGlobal name = LMNLocalVar (fsLit name) LMDouble

