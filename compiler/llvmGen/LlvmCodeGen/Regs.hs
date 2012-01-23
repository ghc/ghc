--------------------------------------------------------------------------------
-- | Deal with Cmm registers
--

module LlvmCodeGen.Regs (
        lmGlobalRegArg, lmGlobalRegVar, alwaysLive,
        stgTBAA, top, base, stack, heap, rx, tbaa, getTBAA
    ) where

#include "HsVersions.h"

import Llvm

import CmmExpr
import FastString
import Outputable ( panic )

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
        -- LongReg, HpLim, CCSS, CurrentTSO, CurrentNusery, HpAlloc
        -- EagerBlackholeInfo, GCEnter1, GCFun, BaseReg, PicBaseReg
    where
        wordGlobal   name = LMNLocalVar (fsLit name) llvmWord
        ptrGlobal    name = LMNLocalVar (fsLit name) llvmWordPtr
        floatGlobal  name = LMNLocalVar (fsLit name) LMFloat
        doubleGlobal name = LMNLocalVar (fsLit name) LMDouble

-- | A list of STG Registers that should always be considered alive
alwaysLive :: [GlobalReg]
alwaysLive = [BaseReg, Sp, Hp, SpLim, HpLim, node]

-- | STG Type Based Alias Analysis metadata
stgTBAA :: [LlvmMeta]
stgTBAA
  = [ MetaUnamed topN   [MetaStr (fsLit "top")]
    , MetaUnamed stackN [MetaStr (fsLit "stack"), MetaNode topN]
    , MetaUnamed heapN  [MetaStr (fsLit "heap"),  MetaNode topN]
    , MetaUnamed rxN    [MetaStr (fsLit "rx"),    MetaNode heapN]
    , MetaUnamed baseN  [MetaStr (fsLit "base"),  MetaNode topN]
    ]

-- | Id values
topN, stackN, heapN, rxN, baseN :: LlvmMetaUnamed
topN   = LMMetaUnamed 0
stackN = LMMetaUnamed 1
heapN  = LMMetaUnamed 2
rxN    = LMMetaUnamed 3
baseN  = LMMetaUnamed 4

-- | The various TBAA types
top, heap, stack, rx, base :: MetaData
top   = (tbaa, topN)
heap  = (tbaa, heapN)
stack = (tbaa, stackN)
rx    = (tbaa, rxN)
base  = (tbaa, baseN)

-- | The TBAA metadata identifier
tbaa :: LMString
tbaa = fsLit "tbaa"

-- | Get the correct TBAA metadata information for this register type
getTBAA :: GlobalReg -> MetaData
getTBAA BaseReg          = base
getTBAA Sp               = stack
getTBAA Hp               = heap
getTBAA (VanillaReg _ _) = rx
getTBAA _                = top

