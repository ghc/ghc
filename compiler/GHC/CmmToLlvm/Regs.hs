

--------------------------------------------------------------------------------
-- | Deal with Cmm registers
--

module GHC.CmmToLlvm.Regs (
        lmGlobalRegArg, lmGlobalRegVar, alwaysLive,
        stgTBAA, baseN, stackN, heapN, rxN, topN, tbaa, getTBAA
    ) where

import GHC.Prelude

import GHC.Llvm

import GHC.Cmm.Expr
import GHC.CmmToAsm.Format
import GHC.Platform
import GHC.Data.FastString
import GHC.Utils.Panic ( panic )
import GHC.Types.Unique


-- | Get the LlvmVar function variable storing the real register
lmGlobalRegVar :: Platform -> GlobalRegUse -> LlvmVar
lmGlobalRegVar platform = pVarLift . lmGlobalReg platform "_Var"

-- | Get the LlvmVar function argument storing the real register
lmGlobalRegArg :: Platform -> GlobalRegUse -> LlvmVar
lmGlobalRegArg platform = lmGlobalReg platform "_Arg"

{- Need to make sure the names here can't conflict with the unique generated
   names. Uniques generated names containing only base62 chars. So using say
   the '_' char guarantees this.
-}
lmGlobalReg :: Platform -> String -> GlobalRegUse -> LlvmVar
lmGlobalReg platform suf (GlobalRegUse reg ty)
  = case reg of
        BaseReg        -> ptrGlobal $ "Base" ++ suf
        Sp             -> ptrGlobal $ "Sp" ++ suf
        Hp             -> ptrGlobal $ "Hp" ++ suf
        VanillaReg 1   -> wordGlobal $ "R1" ++ suf
        VanillaReg 2   -> wordGlobal $ "R2" ++ suf
        VanillaReg 3   -> wordGlobal $ "R3" ++ suf
        VanillaReg 4   -> wordGlobal $ "R4" ++ suf
        VanillaReg 5   -> wordGlobal $ "R5" ++ suf
        VanillaReg 6   -> wordGlobal $ "R6" ++ suf
        VanillaReg 7   -> wordGlobal $ "R7" ++ suf
        VanillaReg 8   -> wordGlobal $ "R8" ++ suf
        VanillaReg 9   -> wordGlobal $ "R9" ++ suf
        VanillaReg 10  -> wordGlobal $ "R10" ++ suf
        SpLim          -> wordGlobal $ "SpLim" ++ suf
        FloatReg 1     -> floatGlobal $ "F1" ++ suf
        FloatReg 2     -> floatGlobal $ "F2" ++ suf
        FloatReg 3     -> floatGlobal $ "F3" ++ suf
        FloatReg 4     -> floatGlobal $ "F4" ++ suf
        FloatReg 5     -> floatGlobal $ "F5" ++ suf
        FloatReg 6     -> floatGlobal $ "F6" ++ suf
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
        _other         -> panic $ "GHC.CmmToLlvm.Reg: GlobalReg (" ++ (show reg)
                                ++ ") not supported!"
        -- LongReg, HpLim, CCSS, CurrentTSO, CurrentNusery, HpAlloc
        -- EagerBlackholeInfo, GCEnter1, GCFun, BaseReg, PicBaseReg
    where
        wordGlobal   name = LMNLocalVar (fsLit name) (llvmWord platform)
        ptrGlobal    name = LMNLocalVar (fsLit name) (llvmWordPtr platform)
        floatGlobal  name = LMNLocalVar (fsLit name) LMFloat
        doubleGlobal name = LMNLocalVar (fsLit name) LMDouble
        fmt = cmmTypeFormat ty
        xmmGlobal    name = LMNLocalVar (fsLit name) (formatLlvmType fmt)
        ymmGlobal    name = LMNLocalVar (fsLit name) (formatLlvmType fmt)
        zmmGlobal    name = LMNLocalVar (fsLit name) (formatLlvmType fmt)

formatLlvmType :: Format -> LlvmType
formatLlvmType II8 = LMInt 8
formatLlvmType II16 = LMInt 16
formatLlvmType II32 = LMInt 32
formatLlvmType II64 = LMInt 64
formatLlvmType FF32 = LMFloat
formatLlvmType FF64 = LMDouble
formatLlvmType (VecFormat l sFmt) = LMVector l (formatLlvmType $ scalarFormatFormat sFmt)

-- | A list of STG Registers that should always be considered alive
alwaysLive :: Platform -> [GlobalRegUse]
alwaysLive platform =
  [ GlobalRegUse r (globalRegSpillType platform r)
  | r <- [BaseReg, Sp, Hp, SpLim, HpLim, node]
  ]

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
rootN  = getUnique (fsLit "GHC.CmmToLlvm.Regs.rootN")
topN   = getUnique (fsLit "GHC.CmmToLlvm.Regs.topN")
stackN = getUnique (fsLit "GHC.CmmToLlvm.Regs.stackN")
heapN  = getUnique (fsLit "GHC.CmmToLlvm.Regs.heapN")
rxN    = getUnique (fsLit "GHC.CmmToLlvm.Regs.rxN")
baseN  = getUnique (fsLit "GHC.CmmToLlvm.Regs.baseN")

-- | The TBAA metadata identifier
tbaa :: LMString
tbaa = fsLit "tbaa"

-- | Get the correct TBAA metadata information for this register type
getTBAA :: GlobalReg -> Unique
getTBAA BaseReg        = baseN
getTBAA Sp             = stackN
getTBAA Hp             = heapN
getTBAA (VanillaReg _) = rxN
getTBAA _              = topN
