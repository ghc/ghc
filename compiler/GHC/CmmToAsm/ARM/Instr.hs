module GHC.CmmToAsm.ARM.Instr where

import GHC.Cmm
import GHC.Cmm.BlockId
import GHC.Cmm.CLabel
import GHC.Cmm.Dataflow.Label
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Instr
import GHC.Data.FastString
import GHC.Platform
import GHC.Platform.Reg
import GHC.Prelude
import GHC.Types.Unique.Supply
import GHC.Utils.Outputable

instance Outputable Instr where
  ppr = undefined

data Imm
  = ImmInt        Int
  | ImmInteger    Integer     -- Sigh.
  | ImmCLbl       CLabel      -- AbstractC Label (with baggage)
  | ImmLit        SDoc        -- Simple string
  | ImmIndex    CLabel Int
  | ImmFloat      Rational
  | ImmDouble     Rational
  | ImmConstantSum Imm Imm
  | ImmConstantDiff Imm Imm
  | LO Imm
  | HI Imm
  | HA Imm        {- high halfword adjusted -}
  | HIGHERA Imm
  | HIGHESTA Imm

data RI
    = RIReg Reg
    | RIImm Imm

data Instr
  -- comment pseudo-op
  = COMMENT FastString

  -- location pseudo-op (file, line, col, name)
  | LOCATION Int Int Int String

  -- some static data spat out during code
  -- generation.  Will be extracted before
  -- pretty-printing.
  | LDATA   Section RawCmmStatics

  -- start a new basic block.  Useful during
  -- codegen, removed later.  Preceding
  -- instruction should be a jump, as per the
  -- invariants for a BasicBlock (see Cmm).
  | NEWBLOCK BlockId

  -- specify current stack offset for
  -- benefit of subsequent passes
  | DELTA   Int


instance Instruction Instr where
  regUsageOfInstr         = arm_regUsageOfInstr
  patchRegsOfInstr        = arm_patchRegsOfInstr
  isJumpishInstr          = arm_isJumpishInstr
  jumpDestsOfInstr        = arm_jumpDestsOfInstr
  patchJumpInstr          = arm_patchJumpInstr
  mkSpillInstr            = arm_mkSpillInstr
  mkLoadInstr             = arm_mkLoadInstr
  takeDeltaInstr          = arm_takeDeltaInstr
  isMetaInstr             = arm_isMetaInstr
  mkRegRegMoveInstr _     = arm_mkRegRegMoveInstr
  takeRegRegMoveInstr     = arm_takeRegRegMoveInstr
  mkJumpInstr             = arm_mkJumpInstr
  mkStackAllocInstr       = arm_mkStackAllocInstr
  mkStackDeallocInstr     = arm_mkStackDeallocInstr

arm_regUsageOfInstr :: Platform -> Instr -> RegUsage
arm_regUsageOfInstr = undefined

arm_patchRegsOfInstr :: Instr -> (Reg -> Reg) -> Instr
arm_patchRegsOfInstr = undefined

arm_isJumpishInstr :: Instr -> Bool
arm_isJumpishInstr = undefined

arm_jumpDestsOfInstr :: Instr -> [BlockId]
arm_jumpDestsOfInstr = undefined

arm_patchJumpInstr :: Instr -> (BlockId -> BlockId) -> Instr
arm_patchJumpInstr = undefined

arm_mkSpillInstr :: NCGConfig -> Reg -> Int -> Int -> Instr
arm_mkSpillInstr = undefined

arm_mkLoadInstr :: NCGConfig -> Reg -> Int -> Int -> Instr
arm_mkLoadInstr = undefined

arm_takeDeltaInstr :: Instr -> Maybe Int
arm_takeDeltaInstr = undefined

arm_isMetaInstr :: Instr -> Bool
arm_isMetaInstr instr =
 case instr of
   COMMENT{}   -> True
   LOCATION{}  -> True
   LDATA{}     -> True
   NEWBLOCK{}  -> True
   DELTA{}     -> True
   _           -> False

arm_mkRegRegMoveInstr :: Reg -> Reg -> Instr
arm_mkRegRegMoveInstr = undefined

arm_takeRegRegMoveInstr :: Instr -> Maybe (Reg, Reg)
arm_takeRegRegMoveInstr = undefined

arm_mkJumpInstr :: BlockId -> [Instr]
arm_mkJumpInstr = undefined

arm_mkStackAllocInstr :: Platform -> Int -> [Instr]
arm_mkStackAllocInstr = undefined

arm_mkStackDeallocInstr :: Platform -> Int -> [Instr]
arm_mkStackDeallocInstr = undefined

maxSpillSlots :: NCGConfig -> Int
maxSpillSlots = undefined

allocMoreStack
  :: Platform
  -> Int
  -> NatCmmDecl RawCmmStatics Instr
  -> UniqSM (NatCmmDecl RawCmmStatics Instr, [(BlockId, BlockId)])
allocMoreStack = undefined

makeFarBranches
  :: LabelMap RawCmmStatics
  -> [NatBasicBlock Instr]
  -> [NatBasicBlock Instr]
makeFarBranches = undefined


