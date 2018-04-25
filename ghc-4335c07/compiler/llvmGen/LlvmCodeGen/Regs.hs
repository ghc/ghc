{-# LANGUAGE CPP #-}

--------------------------------------------------------------------------------
-- | Deal with Cmm registers
--

module LlvmCodeGen.Regs (
        lmGlobalRegArg, lmGlobalRegVar, alwaysLive,
        stgTBAA, baseN, stackN, heapN, rxN, topN, tbaa, getTBAA
    ) where

#include "HsVersions.h"

import GhcPrelude

import Llvm

import CmmExpr
import DynFlags
import FastString
import Outputable ( panic )
import Unique

-- | Get the LlvmVar function variable storing the real register
lmGlobalRegVar :: DynFlags -> GlobalReg -> LlvmVar
lmGlobalRegVar dflags = pVarLift . lmGlobalReg dflags "_Var"

-- | Get the LlvmVar function argument storing the real register
lmGlobalRegArg :: DynFlags -> GlobalReg -> LlvmVar
lmGlobalRegArg dflags = lmGlobalReg dflags "_Arg"

{- Need to make sure the names here can't conflict with the unique generated
   names. Uniques generated names containing only base62 chars. So using say
   the '_' char guarantees this.
-}
lmGlobalReg :: DynFlags -> String -> GlobalReg -> LlvmVar
lmGlobalReg dflags suf reg
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
        FloatReg 5     -> floatGlobal $"F5" ++ suf
        FloatReg 6     -> floatGlobal $"F6" ++ suf
        DoubleReg 1    -> doubleGlobal $ "D1" ++ suf
        DoubleReg 2    -> doubleGlobal $ "D2" ++ suf
        DoubleReg 3    -> doubleGlobal $ "D3" ++ suf
        DoubleReg 4    -> doubleGlobal $ "D4" ++ suf
        DoubleReg 5    -> doubleGlobal $ "D5" ++ suf
        DoubleReg 6    -> doubleGlobal $ "D6" ++ suf
        XmmReg 1       -> xmmGlobal $ "XMM1" ++ suf
        XmmReg 2       -> xmmGlobal $ "XMM2" ++ suf
        XmmReg 3       -> xmmGlobal $ "XMM3" ++ suf
        XmmReg 4       -> xmmGlobal $ "XMM4" ++ suf
        XmmReg 5       -> xmmGlobal $ "XMM5" ++ suf
        XmmReg 6       -> xmmGlobal $ "XMM6" ++ suf
        YmmReg 1       -> ymmGlobal $ "YMM1" ++ suf
        YmmReg 2       -> ymmGlobal $ "YMM2" ++ suf
        YmmReg 3       -> ymmGlobal $ "YMM3" ++ suf
        YmmReg 4       -> ymmGlobal $ "YMM4" ++ suf
        YmmReg 5       -> ymmGlobal $ "YMM5" ++ suf
        YmmReg 6       -> ymmGlobal $ "YMM6" ++ suf
        ZmmReg 1       -> zmmGlobal $ "ZMM1" ++ suf
        ZmmReg 2       -> zmmGlobal $ "ZMM2" ++ suf
        ZmmReg 3       -> zmmGlobal $ "ZMM3" ++ suf
        ZmmReg 4       -> zmmGlobal $ "ZMM4" ++ suf
        ZmmReg 5       -> zmmGlobal $ "ZMM5" ++ suf
        ZmmReg 6       -> zmmGlobal $ "ZMM6" ++ suf
        MachSp         -> wordGlobal $ "MachSp" ++ suf
        _other         -> panic $ "LlvmCodeGen.Reg: GlobalReg (" ++ (show reg)
                                ++ ") not supported!"
        -- LongReg, HpLim, CCSS, CurrentTSO, CurrentNusery, HpAlloc
        -- EagerBlackholeInfo, GCEnter1, GCFun, BaseReg, PicBaseReg
    where
        wordGlobal   name = LMNLocalVar (fsLit name) (llvmWord dflags)
        ptrGlobal    name = LMNLocalVar (fsLit name) (llvmWordPtr dflags)
        floatGlobal  name = LMNLocalVar (fsLit name) LMFloat
        doubleGlobal name = LMNLocalVar (fsLit name) LMDouble
        xmmGlobal    name = LMNLocalVar (fsLit name) (LMVector 4 (LMInt 32))
        ymmGlobal    name = LMNLocalVar (fsLit name) (LMVector 8 (LMInt 32))
        zmmGlobal    name = LMNLocalVar (fsLit name) (LMVector 16 (LMInt 32))

-- | A list of STG Registers that should always be considered alive
alwaysLive :: [GlobalReg]
alwaysLive = [BaseReg, Sp, Hp, SpLim, HpLim, node]

-- | STG Type Based Alias Analysis hierarchy
stgTBAA :: [(Unique, LMString, Maybe Unique)]
stgTBAA
  = [ (rootN,  fsLit "root",   Nothing)
    , (topN,   fsLit "top",   Just rootN)
    , (stackN, fsLit "stack", Just topN)
    , (heapN,  fsLit "heap",  Just topN)
    , (rxN,    fsLit "rx",    Just heapN)
    , (baseN,  fsLit "base",  Just topN)
    -- FIX: Not 100% sure if this hierarchy is complete.  I think the big thing
    -- is Sp is never aliased, so might want to change the hierarchy to have Sp
    -- on its own branch that is never aliased (e.g never use top as a TBAA
    -- node).
    ]

-- | Id values
-- The `rootN` node is the root (there can be more than one) of the TBAA
-- hierarchy and as of LLVM 4.0 should *only* be referenced by other nodes. It
-- should never occur in any LLVM instruction statement.
rootN, topN, stackN, heapN, rxN, baseN :: Unique
rootN  = getUnique (fsLit "LlvmCodeGen.Regs.rootN")
topN   = getUnique (fsLit "LlvmCodeGen.Regs.topN")
stackN = getUnique (fsLit "LlvmCodeGen.Regs.stackN")
heapN  = getUnique (fsLit "LlvmCodeGen.Regs.heapN")
rxN    = getUnique (fsLit "LlvmCodeGen.Regs.rxN")
baseN  = getUnique (fsLit "LlvmCodeGen.Regs.baseN")

-- | The TBAA metadata identifier
tbaa :: LMString
tbaa = fsLit "tbaa"

-- | Get the correct TBAA metadata information for this register type
getTBAA :: GlobalReg -> Unique
getTBAA BaseReg          = baseN
getTBAA Sp               = stackN
getTBAA Hp               = heapN
getTBAA (VanillaReg _ _) = rxN
getTBAA _                = topN
