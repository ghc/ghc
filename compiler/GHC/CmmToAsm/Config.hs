-- | Native code generator configuration
module GHC.CmmToAsm.Config
   ( NCGConfig(..)
   , ncgWordWidth
   , ncgSpillPreallocSize
   , platformWordWidth
   )
where

import GHC.Prelude
import GHC.Platform
import GHC.Cmm.Type (Width(..))
import GHC.CmmToAsm.CFG.Weight
import GHC.Unit.Module (Module)
import GHC.Utils.Outputable

-- | Native code generator configuration
data NCGConfig = NCGConfig
   { ncgPlatform              :: !Platform        -- ^ Target platform
   , ncgAsmContext            :: !SDocContext     -- ^ Context for ASM code generation
   , ncgThisModule            :: !Module          -- ^ The name of the module we are currently compiling
   , ncgProcAlignment         :: !(Maybe Int)     -- ^ Mandatory proc alignment
   , ncgExternalDynamicRefs   :: !Bool            -- ^ Generate code to link against dynamic libraries
   , ncgPIC                   :: !Bool            -- ^ Enable Position-Independent Code
   , ncgInlineThresholdMemcpy :: !Word            -- ^ If inlining `memcpy` produces less than this threshold (in pseudo-instruction unit), do it
   , ncgInlineThresholdMemset :: !Word            -- ^ Ditto for `memset`
   , ncgSplitSections         :: !Bool            -- ^ Split sections
   , ncgRegsIterative         :: !Bool
   , ncgAsmLinting            :: !Bool            -- ^ Perform ASM linting pass
   , ncgDoConstantFolding     :: !Bool            -- ^ Perform CMM constant folding
   , ncgSseVersion            :: Maybe SseVersion -- ^ (x86) SSE instructions
   , ncgBmiVersion            :: Maybe BmiVersion -- ^ (x86) BMI instructions
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
   }

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
