-- | Native code generator configuration
module GHC.CmmToAsm.Config
   ( NCGConfig(..)
   , ncgX86Features
   , LoopAlignMode(..)
   , ncgWordWidth
   , ncgSpillPreallocSize
   , platformWordWidth
   )
where

import GHC.Prelude
import GHC.Platform
import GHC.Platform.CpuFeatures
import GHC.Cmm.Type (Width(..))
import GHC.CmmToAsm.CFG.Weight
import GHC.Unit.Module (Module)
import GHC.Utils.Outputable
import GHC.Utils.Panic

-- | Which loops the @-falign-loops@ padding applies to.
-- See Note [Aligning loop headers] in GHC.CmmToAsm.BlockLayout.
data LoopAlignMode
   = AlignAllLoops    -- ^ every qualifying loop-top block
   | AlignInnerLoops  -- ^ only blocks belonging to innermost loops
                      --   (loops that contain no nested loop)
   deriving (Eq, Show)

-- | Native code generator configuration
data NCGConfig = NCGConfig
   { ncgPlatform              :: !Platform        -- ^ Target platform
   , ncgAsmContext            :: !SDocContext     -- ^ Context for ASM code generation
   , ncgThisModule            :: !Module          -- ^ The name of the module we are currently compiling
   , ncgProcAlignment         :: !(Maybe Int)     -- ^ Mandatory proc alignment
   , ncgLoopAlignment         :: !(Maybe Int)     -- ^ Align loop-top blocks at this boundary (bytes)
                                                  -- See Note [Aligning loop headers] in GHC.CmmToAsm.BlockLayout
   , ncgLoopAlignmentMode     :: !LoopAlignMode   -- ^ Which loops -falign-loops applies to
   , ncgLoopAlignPadFallthrough :: !Bool          -- ^ Also align fallthrough-entered loop blocks;
                                                  --   the entering path then executes the nop padding
   , ncgExternalDynamicRefs   :: !Bool            -- ^ Generate code to link against dynamic libraries
   , ncgPIC                   :: !Bool            -- ^ Enable Position-Independent Code
   , ncgInlineThresholdMemcpy :: !Word            -- ^ If inlining `memcpy` produces less than this threshold (in pseudo-instruction unit), do it
   , ncgInlineThresholdMemset :: !Word            -- ^ Ditto for `memset`
   , ncgSplitSections         :: !Bool            -- ^ Split sections
   , ncgRegsIterative         :: !Bool
   , ncgRegsGraph             :: !Bool
   , ncgAsmLinting            :: !Bool            -- ^ Perform ASM linting pass
   , ncgDoConstantFolding     :: !Bool            -- ^ Perform CMM constant folding
   , ncgCpuFeatures           :: !CpuFeatures     -- ^ Enabled CPU features
   , ncgDumpRegAllocStages    :: !Bool
   , ncgDumpAsmStats          :: !Bool
   , ncgDumpAsmConflicts      :: !Bool
   , ncgCfgWeights            :: !Weights         -- ^ CFG edge weights
   , ncgCfgBlockLayout        :: !Bool            -- ^ Use CFG based block layout algorithm
   , ncgCfgWeightlessLayout   :: !Bool            -- ^ Layout based on last instruction per block.
   , ncgDwarfEnabled          :: !Bool            -- ^ Enable Dwarf generation
   , ncgDwarfUnwindings       :: !Bool            -- ^ Enable unwindings
   , ncgDwarfStripBlockInfo   :: !Bool            -- ^ Strip out block information from generated Dwarf
   , ncgExposeInternalSymbols :: !Bool            -- ^ Expose symbol table entries for internal symbols
   , ncgDwarfSourceNotes      :: !Bool            -- ^ Enable GHC-specific source note DIEs
   , ncgCmmStaticPred         :: !Bool            -- ^ Enable static control-flow prediction
   , ncgEnableShortcutting    :: !Bool            -- ^ Enable shortcutting (don't jump to blocks only containing a jump)
   , ncgEnableInterModuleFarJumps:: !Bool            -- ^ Use far-jumps for cross-module jumps.
   , ncgComputeUnwinding      :: !Bool            -- ^ Compute block unwinding tables
   , ncgEnableDeadCodeElimination :: !Bool        -- ^ Whether to enable the dead-code elimination
   , ncgLa664Enabled          :: !Bool            -- ^ la664 is equal to isav1.1
   }

-- | X86 only: enabled x86 CPU features.
ncgX86Features :: NCGConfig -> X86Features
ncgX86Features config =
  case ncgCpuFeatures config of
    X86CpuFeatures feats -> feats
    _ -> panic "ncgX86Features: not an x86 target"

-- | Return Word size
ncgWordWidth :: NCGConfig -> Width
ncgWordWidth config = platformWordWidth (ncgPlatform config)

-- | Size in bytes of the pre-allocated spill space on the C stack
ncgSpillPreallocSize :: NCGConfig -> Int
ncgSpillPreallocSize config = pc_RESERVED_C_STACK_BYTES (platformConstants (ncgPlatform config))

-- | Return Word size
platformWordWidth :: Platform -> Width
platformWordWidth platform = case platformWordSize platform of
   PW4 -> W32
   PW8 -> W64
