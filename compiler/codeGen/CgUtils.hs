-----------------------------------------------------------------------------
--
-- Code generator utilities; mostly monadic
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module CgUtils ( fixStgRegisters ) where

#include "HsVersions.h"

import CodeGen.Platform
import OldCmm
import CLabel
import DynFlags
import Outputable

-- -----------------------------------------------------------------------------
-- Information about global registers

baseRegOffset :: DynFlags -> GlobalReg -> Int

baseRegOffset dflags (VanillaReg 1 _)    = oFFSET_StgRegTable_rR1 dflags
baseRegOffset dflags (VanillaReg 2 _)    = oFFSET_StgRegTable_rR2 dflags
baseRegOffset dflags (VanillaReg 3 _)    = oFFSET_StgRegTable_rR3 dflags
baseRegOffset dflags (VanillaReg 4 _)    = oFFSET_StgRegTable_rR4 dflags
baseRegOffset dflags (VanillaReg 5 _)    = oFFSET_StgRegTable_rR5 dflags
baseRegOffset dflags (VanillaReg 6 _)    = oFFSET_StgRegTable_rR6 dflags
baseRegOffset dflags (VanillaReg 7 _)    = oFFSET_StgRegTable_rR7 dflags
baseRegOffset dflags (VanillaReg 8 _)    = oFFSET_StgRegTable_rR8 dflags
baseRegOffset dflags (VanillaReg 9 _)    = oFFSET_StgRegTable_rR9 dflags
baseRegOffset dflags (VanillaReg 10 _)   = oFFSET_StgRegTable_rR10 dflags
baseRegOffset _      (VanillaReg n _)    = panic ("Registers above R10 are not supported (tried to use R" ++ show n ++ ")")
baseRegOffset dflags (FloatReg  1)       = oFFSET_StgRegTable_rF1 dflags
baseRegOffset dflags (FloatReg  2)       = oFFSET_StgRegTable_rF2 dflags
baseRegOffset dflags (FloatReg  3)       = oFFSET_StgRegTable_rF3 dflags
baseRegOffset dflags (FloatReg  4)       = oFFSET_StgRegTable_rF4 dflags
baseRegOffset _      (FloatReg  n)       = panic ("Registers above F4 are not supported (tried to use F" ++ show n ++ ")")
baseRegOffset dflags (DoubleReg 1)       = oFFSET_StgRegTable_rD1 dflags
baseRegOffset dflags (DoubleReg 2)       = oFFSET_StgRegTable_rD2 dflags
baseRegOffset _      (DoubleReg n)       = panic ("Registers above D2 are not supported (tried to use D" ++ show n ++ ")")
baseRegOffset dflags Sp                  = oFFSET_StgRegTable_rSp dflags
baseRegOffset dflags SpLim               = oFFSET_StgRegTable_rSpLim dflags
baseRegOffset dflags (LongReg 1)         = oFFSET_StgRegTable_rL1 dflags
baseRegOffset _      (LongReg n)         = panic ("Registers above L1 are not supported (tried to use L" ++ show n ++ ")")
baseRegOffset dflags Hp                  = oFFSET_StgRegTable_rHp dflags
baseRegOffset dflags HpLim               = oFFSET_StgRegTable_rHpLim dflags
baseRegOffset dflags CCCS                = oFFSET_StgRegTable_rCCCS dflags
baseRegOffset dflags CurrentTSO          = oFFSET_StgRegTable_rCurrentTSO dflags
baseRegOffset dflags CurrentNursery      = oFFSET_StgRegTable_rCurrentNursery dflags
baseRegOffset dflags HpAlloc             = oFFSET_StgRegTable_rHpAlloc dflags
baseRegOffset dflags EagerBlackholeInfo  = oFFSET_stgEagerBlackholeInfo dflags
baseRegOffset dflags GCEnter1            = oFFSET_stgGCEnter1 dflags
baseRegOffset dflags GCFun               = oFFSET_stgGCFun dflags
baseRegOffset _      BaseReg             = panic "baseRegOffset:BaseReg"
baseRegOffset _      PicBaseReg          = panic "baseRegOffset:PicBaseReg"


-- -----------------------------------------------------------------------------
--
-- STG/Cmm GlobalReg
--
-- -----------------------------------------------------------------------------

-- | We map STG registers onto appropriate CmmExprs.  Either they map
-- to real machine registers or stored as offsets from BaseReg.  Given
-- a GlobalReg, get_GlobalReg_addr always produces the
-- register table address for it.
get_GlobalReg_addr :: DynFlags -> GlobalReg -> CmmExpr
get_GlobalReg_addr dflags BaseReg = regTableOffset dflags 0
get_GlobalReg_addr dflags mid
    = get_Regtable_addr_from_offset dflags
                                    (globalRegType dflags mid) (baseRegOffset dflags mid)

-- Calculate a literal representing an offset into the register table.
-- Used when we don't have an actual BaseReg to offset from.
regTableOffset :: DynFlags -> Int -> CmmExpr
regTableOffset dflags n =
  CmmLit (CmmLabelOff mkMainCapabilityLabel (oFFSET_Capability_r dflags + n))

get_Regtable_addr_from_offset :: DynFlags -> CmmType -> Int -> CmmExpr
get_Regtable_addr_from_offset dflags _ offset =
    if haveRegBase (targetPlatform dflags)
    then CmmRegOff (CmmGlobal BaseReg) offset
    else regTableOffset dflags offset

-- | Fixup global registers so that they assign to locations within the
-- RegTable if they aren't pinned for the current target.
fixStgRegisters :: DynFlags -> RawCmmDecl -> RawCmmDecl
fixStgRegisters _ top@(CmmData _ _) = top

fixStgRegisters dflags (CmmProc info lbl (ListGraph blocks)) =
  let blocks' = map (fixStgRegBlock dflags) blocks
  in CmmProc info lbl $ ListGraph blocks'

fixStgRegBlock :: DynFlags -> CmmBasicBlock -> CmmBasicBlock
fixStgRegBlock dflags (BasicBlock id stmts) =
  let stmts' = map (fixStgRegStmt dflags) stmts
  in BasicBlock id stmts'

fixStgRegStmt :: DynFlags -> CmmStmt -> CmmStmt
fixStgRegStmt dflags stmt
  = case stmt of
        CmmAssign (CmmGlobal reg) src ->
            let src' = fixStgRegExpr dflags src
                baseAddr = get_GlobalReg_addr dflags reg
            in case reg `elem` activeStgRegs platform of
                True  -> CmmAssign (CmmGlobal reg) src'
                False -> CmmStore baseAddr src'

        CmmAssign reg src ->
            let src' = fixStgRegExpr dflags src
            in CmmAssign reg src'

        CmmStore addr src -> CmmStore (fixStgRegExpr dflags addr) (fixStgRegExpr dflags src)

        CmmCall target regs args returns ->
            let target' = case target of
                    CmmCallee e conv -> CmmCallee (fixStgRegExpr dflags e) conv
                    CmmPrim op mStmts ->
                        CmmPrim op (fmap (map (fixStgRegStmt dflags)) mStmts)
                args' = map (\(CmmHinted arg hint) ->
                                (CmmHinted (fixStgRegExpr dflags arg) hint)) args
            in CmmCall target' regs args' returns

        CmmCondBranch test dest -> CmmCondBranch (fixStgRegExpr dflags test) dest

        CmmSwitch expr ids -> CmmSwitch (fixStgRegExpr dflags expr) ids

        CmmJump addr live -> CmmJump (fixStgRegExpr dflags addr) live

        -- CmmNop, CmmComment, CmmBranch, CmmReturn
        _other -> stmt
    where platform = targetPlatform dflags


fixStgRegExpr :: DynFlags -> CmmExpr -> CmmExpr
fixStgRegExpr dflags expr
  = case expr of
        CmmLoad addr ty -> CmmLoad (fixStgRegExpr dflags addr) ty

        CmmMachOp mop args -> CmmMachOp mop args'
            where args' = map (fixStgRegExpr dflags) args

        CmmReg (CmmGlobal reg) ->
            -- Replace register leaves with appropriate StixTrees for
            -- the given target.  MagicIds which map to a reg on this
            -- arch are left unchanged.  For the rest, BaseReg is taken
            -- to mean the address of the reg table in MainCapability,
            -- and for all others we generate an indirection to its
            -- location in the register table.
            case reg `elem` activeStgRegs platform of
                True  -> expr
                False ->
                    let baseAddr = get_GlobalReg_addr dflags reg
                    in case reg of
                        BaseReg -> fixStgRegExpr dflags baseAddr
                        _other  -> fixStgRegExpr dflags
                                    (CmmLoad baseAddr (globalRegType dflags reg))

        CmmRegOff (CmmGlobal reg) offset ->
            -- RegOf leaves are just a shorthand form. If the reg maps
            -- to a real reg, we keep the shorthand, otherwise, we just
            -- expand it and defer to the above code.
            case reg `elem` activeStgRegs platform of
                True  -> expr
                False -> fixStgRegExpr dflags (CmmMachOp (MO_Add (wordWidth dflags)) [
                                    CmmReg (CmmGlobal reg),
                                    CmmLit (CmmInt (fromIntegral offset)
                                                (wordWidth dflags))])

        -- CmmLit, CmmReg (CmmLocal), CmmStackSlot
        _other -> expr
    where platform = targetPlatform dflags

