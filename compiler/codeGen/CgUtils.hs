-----------------------------------------------------------------------------
--
-- Code generator utilities; mostly monadic
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
module CgUtils ( fixStgRegisters ) where

#include "HsVersions.h"

import CodeGen.Platform
import Cmm
import Hoopl
import CmmUtils
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
baseRegOffset dflags (FloatReg  5)       = oFFSET_StgRegTable_rF5 dflags
baseRegOffset dflags (FloatReg  6)       = oFFSET_StgRegTable_rF6 dflags
baseRegOffset _      (FloatReg  n)       = panic ("Registers above F6 are not supported (tried to use F" ++ show n ++ ")")
baseRegOffset dflags (DoubleReg 1)       = oFFSET_StgRegTable_rD1 dflags
baseRegOffset dflags (DoubleReg 2)       = oFFSET_StgRegTable_rD2 dflags
baseRegOffset dflags (DoubleReg 3)       = oFFSET_StgRegTable_rD3 dflags
baseRegOffset dflags (DoubleReg 4)       = oFFSET_StgRegTable_rD4 dflags
baseRegOffset dflags (DoubleReg 5)       = oFFSET_StgRegTable_rD5 dflags
baseRegOffset dflags (DoubleReg 6)       = oFFSET_StgRegTable_rD6 dflags
baseRegOffset _      (DoubleReg n)       = panic ("Registers above D6 are not supported (tried to use D" ++ show n ++ ")")
baseRegOffset dflags (XmmReg 1)          = oFFSET_StgRegTable_rXMM1 dflags
baseRegOffset dflags (XmmReg 2)          = oFFSET_StgRegTable_rXMM2 dflags
baseRegOffset dflags (XmmReg 3)          = oFFSET_StgRegTable_rXMM3 dflags
baseRegOffset dflags (XmmReg 4)          = oFFSET_StgRegTable_rXMM4 dflags
baseRegOffset dflags (XmmReg 5)          = oFFSET_StgRegTable_rXMM5 dflags
baseRegOffset dflags (XmmReg 6)          = oFFSET_StgRegTable_rXMM6 dflags
baseRegOffset _      (XmmReg n)          = panic ("Registers above XMM6 are not supported (tried to use XMM" ++ show n ++ ")")
baseRegOffset dflags (YmmReg 1)          = oFFSET_StgRegTable_rYMM1 dflags
baseRegOffset dflags (YmmReg 2)          = oFFSET_StgRegTable_rYMM2 dflags
baseRegOffset dflags (YmmReg 3)          = oFFSET_StgRegTable_rYMM3 dflags
baseRegOffset dflags (YmmReg 4)          = oFFSET_StgRegTable_rYMM4 dflags
baseRegOffset dflags (YmmReg 5)          = oFFSET_StgRegTable_rYMM5 dflags
baseRegOffset dflags (YmmReg 6)          = oFFSET_StgRegTable_rYMM6 dflags
baseRegOffset _      (YmmReg n)          = panic ("Registers above YMM6 are not supported (tried to use YMM" ++ show n ++ ")")
baseRegOffset dflags (ZmmReg 1)          = oFFSET_StgRegTable_rZMM1 dflags
baseRegOffset dflags (ZmmReg 2)          = oFFSET_StgRegTable_rZMM2 dflags
baseRegOffset dflags (ZmmReg 3)          = oFFSET_StgRegTable_rZMM3 dflags
baseRegOffset dflags (ZmmReg 4)          = oFFSET_StgRegTable_rZMM4 dflags
baseRegOffset dflags (ZmmReg 5)          = oFFSET_StgRegTable_rZMM5 dflags
baseRegOffset dflags (ZmmReg 6)          = oFFSET_StgRegTable_rZMM6 dflags
baseRegOffset _      (ZmmReg n)          = panic ("Registers above ZMM6 are not supported (tried to use ZMM" ++ show n ++ ")")
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

fixStgRegisters dflags (CmmProc info lbl live graph) =
  let graph' = modifyGraph (mapGraphBlocks (fixStgRegBlock dflags)) graph
  in CmmProc info lbl live graph'

fixStgRegBlock :: DynFlags -> Block CmmNode e x -> Block CmmNode e x
fixStgRegBlock dflags block = mapBlock (fixStgRegStmt dflags) block

fixStgRegStmt :: DynFlags -> CmmNode e x -> CmmNode e x
fixStgRegStmt dflags stmt = fixAssign $ mapExpDeep fixExpr stmt
  where
    platform = targetPlatform dflags

    fixAssign stmt =
      case stmt of
        CmmAssign (CmmGlobal reg) src ->
            let baseAddr = get_GlobalReg_addr dflags reg
            in case reg `elem` activeStgRegs (targetPlatform dflags) of
                True  -> CmmAssign (CmmGlobal reg) src
                False -> CmmStore baseAddr src
        other_stmt -> other_stmt

    fixExpr expr = case expr of
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
                        BaseReg -> baseAddr
                        _other  -> CmmLoad baseAddr (globalRegType dflags reg)

        CmmRegOff (CmmGlobal reg) offset ->
            -- RegOf leaves are just a shorthand form. If the reg maps
            -- to a real reg, we keep the shorthand, otherwise, we just
            -- expand it and defer to the above code.
            case reg `elem` activeStgRegs platform of
                True  -> expr
                False -> CmmMachOp (MO_Add (wordWidth dflags)) [
                                    fixExpr (CmmReg (CmmGlobal reg)),
                                    CmmLit (CmmInt (fromIntegral offset)
                                                   (wordWidth dflags))]

        other_expr -> other_expr

