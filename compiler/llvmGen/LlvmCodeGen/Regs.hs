--------------------------------------------------------------------------------
-- | Deal with Cmm registers
--

module LlvmCodeGen.Regs (
        lmGlobalRegArg, lmGlobalRegVar, alwaysLive,
        stgTBAA, top, base, stack, heap, rx, other, tbaa, getTBAA
    ) where

#include "HsVersions.h"

import Llvm

import CmmExpr
import DynFlags
import FastString
import Outputable ( panic )

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

-- | A list of STG Registers that should always be considered alive
alwaysLive :: [GlobalReg]
alwaysLive = [BaseReg, Sp, Hp, SpLim, HpLim, node]

-- | STG Type Based Alias Analysis metadata
stgTBAA :: [MetaDecl]
stgTBAA
  = [ MetaUnamed topN   $ MetaStr (fsLit "top")
    , MetaUnamed stackN $ MetaExpr [MetaStr (fsLit "stack"), MetaNode topN]
    , MetaUnamed heapN  $ MetaExpr [MetaStr (fsLit "heap"),  MetaNode topN]
    , MetaUnamed rxN    $ MetaExpr [MetaStr (fsLit "rx"),    MetaNode heapN]
    , MetaUnamed baseN  $ MetaExpr [MetaStr (fsLit "base"),  MetaNode topN]
    -- FIX: Not 100% sure about 'others' place. Might need to be under 'heap'.
    -- OR I think the big thing is Sp is never aliased, so might want
    -- to change the hieracy to have Sp on its own branch that is never
    -- aliased (e.g never use top as a TBAA node).
    , MetaUnamed otherN $ MetaExpr [MetaStr (fsLit "other"), MetaNode topN]
    ]

-- | Id values
topN, stackN, heapN, rxN, baseN, otherN:: Int
topN   = 0
stackN = 1
heapN  = 2
rxN    = 3
baseN  = 4
otherN = 5

-- | The various TBAA types
top, heap, stack, rx, base, other :: MetaData
top   = (tbaa, MetaValNode topN)
heap  = (tbaa, MetaValNode heapN)
stack = (tbaa, MetaValNode stackN)
rx    = (tbaa, MetaValNode rxN)
base  = (tbaa, MetaValNode baseN)
other = (tbaa, MetaValNode otherN)

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

