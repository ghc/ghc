{-# LANGUAGE GADTs #-}

-----------------------------------------------------------------------------
--
-- Code generator utilities; mostly monadic
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module GHC.StgToCmm.CgUtils (
        fixStgRegisters,
        baseRegOffset,
        get_Regtable_addr_from_offset,
        regTableOffset,
        get_GlobalReg_addr,

        -- * Streaming for CG
        CgStream
  ) where

import GHC.Prelude

import GHC.Platform.Regs
import GHC.Platform
import GHC.Cmm
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Utils
import GHC.Cmm.CLabel
import GHC.Utils.Panic

import GHC.Data.Stream (Stream)
import GHC.Types.Unique.DSM (UniqDSMT)

-- -----------------------------------------------------------------------------
-- Streaming

-- | The Stream instantiation used for code generation.
-- Note the underlying monad is @UniqDSMT IO@, where @UniqDSMT@ is a transformer
-- that propagates a deterministic unique supply (essentially an incrementing
-- counter) from which new uniques are deterministically created during the
-- code generation stages following StgToCmm.
-- See Note [Object determinism].
type CgStream = Stream (UniqDSMT IO)


-- -----------------------------------------------------------------------------
-- Information about global registers

baseRegOffset :: Platform -> GlobalReg -> Int
baseRegOffset platform reg = case reg of
   VanillaReg 1         -> pc_OFFSET_StgRegTable_rR1  constants
   VanillaReg 2         -> pc_OFFSET_StgRegTable_rR2  constants
   VanillaReg 3         -> pc_OFFSET_StgRegTable_rR3  constants
   VanillaReg 4         -> pc_OFFSET_StgRegTable_rR4  constants
   VanillaReg 5         -> pc_OFFSET_StgRegTable_rR5  constants
   VanillaReg 6         -> pc_OFFSET_StgRegTable_rR6  constants
   VanillaReg 7         -> pc_OFFSET_StgRegTable_rR7  constants
   VanillaReg 8         -> pc_OFFSET_StgRegTable_rR8  constants
   VanillaReg 9         -> pc_OFFSET_StgRegTable_rR9  constants
   VanillaReg 10        -> pc_OFFSET_StgRegTable_rR10 constants
   VanillaReg n         -> panic ("Registers above R10 are not supported (tried to use R" ++ show n ++ ")")
   FloatReg  1          -> pc_OFFSET_StgRegTable_rF1 constants
   FloatReg  2          -> pc_OFFSET_StgRegTable_rF2 constants
   FloatReg  3          -> pc_OFFSET_StgRegTable_rF3 constants
   FloatReg  4          -> pc_OFFSET_StgRegTable_rF4 constants
   FloatReg  5          -> pc_OFFSET_StgRegTable_rF5 constants
   FloatReg  6          -> pc_OFFSET_StgRegTable_rF6 constants
   FloatReg  n          -> panic ("Registers above F6 are not supported (tried to use F" ++ show n ++ ")")
   DoubleReg 1          -> pc_OFFSET_StgRegTable_rD1 constants
   DoubleReg 2          -> pc_OFFSET_StgRegTable_rD2 constants
   DoubleReg 3          -> pc_OFFSET_StgRegTable_rD3 constants
   DoubleReg 4          -> pc_OFFSET_StgRegTable_rD4 constants
   DoubleReg 5          -> pc_OFFSET_StgRegTable_rD5 constants
   DoubleReg 6          -> pc_OFFSET_StgRegTable_rD6 constants
   DoubleReg n          -> panic ("Registers above D6 are not supported (tried to use D" ++ show n ++ ")")
   XmmReg 1             -> pc_OFFSET_StgRegTable_rXMM1 constants
   XmmReg 2             -> pc_OFFSET_StgRegTable_rXMM2 constants
   XmmReg 3             -> pc_OFFSET_StgRegTable_rXMM3 constants
   XmmReg 4             -> pc_OFFSET_StgRegTable_rXMM4 constants
   XmmReg 5             -> pc_OFFSET_StgRegTable_rXMM5 constants
   XmmReg 6             -> pc_OFFSET_StgRegTable_rXMM6 constants
   XmmReg n             -> panic ("Registers above XMM6 are not supported (tried to use XMM" ++ show n ++ ")")
   YmmReg 1             -> pc_OFFSET_StgRegTable_rYMM1 constants
   YmmReg 2             -> pc_OFFSET_StgRegTable_rYMM2 constants
   YmmReg 3             -> pc_OFFSET_StgRegTable_rYMM3 constants
   YmmReg 4             -> pc_OFFSET_StgRegTable_rYMM4 constants
   YmmReg 5             -> pc_OFFSET_StgRegTable_rYMM5 constants
   YmmReg 6             -> pc_OFFSET_StgRegTable_rYMM6 constants
   YmmReg n             -> panic ("Registers above YMM6 are not supported (tried to use YMM" ++ show n ++ ")")
   ZmmReg 1             -> pc_OFFSET_StgRegTable_rZMM1 constants
   ZmmReg 2             -> pc_OFFSET_StgRegTable_rZMM2 constants
   ZmmReg 3             -> pc_OFFSET_StgRegTable_rZMM3 constants
   ZmmReg 4             -> pc_OFFSET_StgRegTable_rZMM4 constants
   ZmmReg 5             -> pc_OFFSET_StgRegTable_rZMM5 constants
   ZmmReg 6             -> pc_OFFSET_StgRegTable_rZMM6 constants
   ZmmReg n             -> panic ("Registers above ZMM6 are not supported (tried to use ZMM" ++ show n ++ ")")
   Sp                   -> pc_OFFSET_StgRegTable_rSp    constants
   SpLim                -> pc_OFFSET_StgRegTable_rSpLim constants
   LongReg 1            -> pc_OFFSET_StgRegTable_rL1    constants
   LongReg n            -> panic ("Registers above L1 are not supported (tried to use L" ++ show n ++ ")")
   Hp                   -> pc_OFFSET_StgRegTable_rHp             constants
   HpLim                -> pc_OFFSET_StgRegTable_rHpLim          constants
   CCCS                 -> pc_OFFSET_StgRegTable_rCCCS           constants
   CurrentTSO           -> pc_OFFSET_StgRegTable_rCurrentTSO     constants
   CurrentNursery       -> pc_OFFSET_StgRegTable_rCurrentNursery constants
   HpAlloc              -> pc_OFFSET_StgRegTable_rHpAlloc        constants
   EagerBlackholeInfo   -> pc_OFFSET_stgEagerBlackholeInfo       constants
   GCEnter1             -> pc_OFFSET_stgGCEnter1                 constants
   GCFun                -> pc_OFFSET_stgGCFun                    constants
   BaseReg              -> panic "GHC.StgToCmm.CgUtils.baseRegOffset:BaseReg"
   PicBaseReg           -> panic "GHC.StgToCmm.CgUtils.baseRegOffset:PicBaseReg"
   MachSp               -> panic "GHC.StgToCmm.CgUtils.baseRegOffset:MachSp"
   UnwindReturnReg      -> panic "GHC.StgToCmm.CgUtils.baseRegOffset:UnwindReturnReg"
 where
   !constants = platformConstants platform


-- -----------------------------------------------------------------------------
--
-- STG/Cmm GlobalReg
--
-- -----------------------------------------------------------------------------

-- | We map STG registers onto appropriate CmmExprs.  Either they map
-- to real machine registers or stored as offsets from BaseReg.  Given
-- a GlobalReg, get_GlobalReg_addr always produces the
-- register table address for it.
get_GlobalReg_addr :: Platform -> GlobalReg -> CmmExpr
get_GlobalReg_addr platform BaseReg = regTableOffset platform 0
get_GlobalReg_addr platform mid
    = get_Regtable_addr_from_offset platform (baseRegOffset platform mid)

-- Calculate a literal representing an offset into the register table.
-- Used when we don't have an actual BaseReg to offset from.
regTableOffset :: Platform -> Int -> CmmExpr
regTableOffset platform n =
  CmmLit (CmmLabelOff mkMainCapabilityLabel (pc_OFFSET_Capability_r (platformConstants platform) + n))

get_Regtable_addr_from_offset :: Platform -> Int -> CmmExpr
get_Regtable_addr_from_offset platform offset =
    if haveRegBase platform
    then cmmRegOff (baseReg platform) offset
    else regTableOffset platform offset

-- | Fixup global registers so that they assign to locations within the
-- RegTable if they aren't pinned for the current target.
fixStgRegisters :: Platform -> RawCmmDecl -> RawCmmDecl
fixStgRegisters _ top@(CmmData _ _) = top

fixStgRegisters platform (CmmProc info lbl live graph) =
  let graph' = modifyGraph (mapGraphBlocks (fixStgRegBlock platform)) graph
  in CmmProc info lbl live graph'

fixStgRegBlock :: Platform -> Block CmmNode e x -> Block CmmNode e x
fixStgRegBlock platform block = mapBlock (fixStgRegStmt platform) block

fixStgRegStmt :: Platform -> CmmNode e x -> CmmNode e x
fixStgRegStmt platform stmt = fixAssign $ mapExpDeep fixExpr stmt
  where
    fixAssign stmt =
      case stmt of
        CmmAssign (CmmGlobal reg_use) src
          -- MachSp isn't an STG register; it's merely here for tracking unwind
          -- information
          | reg == MachSp -> stmt
          | otherwise ->
            let baseAddr = get_GlobalReg_addr platform reg
            in case reg `elem` activeStgRegs platform of
                True  -> CmmAssign (CmmGlobal reg_use) src
                False -> CmmStore baseAddr src NaturallyAligned
          where reg = globalRegUseGlobalReg reg_use
        other_stmt -> other_stmt

    fixExpr expr = case expr of
        -- MachSp isn't an STG; it's merely here for tracking unwind information
        CmmReg (CmmGlobal (GlobalRegUse MachSp _)) -> expr
        CmmReg (CmmGlobal reg_use) ->
            -- Replace register leaves with appropriate StixTrees for
            -- the given target.  MagicIds which map to a reg on this
            -- arch are left unchanged.  For the rest, BaseReg is taken
            -- to mean the address of the reg table in MainCapability,
            -- and for all others we generate an indirection to its
            -- location in the register table.
            let reg = globalRegUseGlobalReg reg_use in
            case reg `elem` activeStgRegs platform of
                True  -> expr
                False ->
                    let baseAddr = get_GlobalReg_addr platform reg
                    in case reg of
                        BaseReg -> baseAddr
                        _other  -> CmmLoad baseAddr
                                     (globalRegSpillType platform reg)
                                     NaturallyAligned

        CmmRegOff greg@(CmmGlobal reg) offset ->
            -- RegOf leaves are just a shorthand form. If the reg maps
            -- to a real reg, we keep the shorthand, otherwise, we just
            -- expand it and defer to the above code.
            -- NB: to ensure type correctness we need to ensure the Add
            --     as well as the Int need to be of the same size as the
            --     register.
            case globalRegUseGlobalReg reg `elem` activeStgRegs platform of
                True  -> expr
                False -> CmmMachOp (MO_Add (cmmRegWidth greg)) [
                                    fixExpr (CmmReg greg),
                                    CmmLit (CmmInt (fromIntegral offset)
                                                   (cmmRegWidth greg))]

        other_expr -> other_expr
