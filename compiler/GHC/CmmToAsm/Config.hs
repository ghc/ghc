-- | Native code generator configuration
module GHC.CmmToAsm.Config
   ( NCGConfig(..)
   , ncgWordWidth
   , platformWordWidth
   )
where

import GHC.Prelude
import GHC.Platform
import GHC.Cmm.Type (Width(..))

-- | Native code generator configuration
data NCGConfig = NCGConfig
   { ncgPlatform              :: !Platform        -- ^ Target platform
   , ncgProcAlignment         :: !(Maybe Int)     -- ^ Mandatory proc alignment
   , ncgDebugLevel            :: !Int             -- ^ Debug level
   , ncgExternalDynamicRefs   :: !Bool            -- ^ Generate code to link against dynamic libraries
   , ncgPIC                   :: !Bool            -- ^ Enable Position-Independent Code
   , ncgInlineThresholdMemcpy :: !Word            -- ^ If inlining `memcpy` produces less than this threshold (in pseudo-instruction unit), do it
   , ncgInlineThresholdMemset :: !Word            -- ^ Ditto for `memset`
   , ncgSplitSections         :: !Bool            -- ^ Split sections
   , ncgSpillPreallocSize     :: !Int             -- ^ Size in bytes of the pre-allocated spill space on the C stack
   , ncgRegsIterative         :: !Bool
   , ncgAsmLinting            :: !Bool            -- ^ Perform ASM linting pass
   , ncgDoConstantFolding     :: !Bool            -- ^ Perform CMM constant folding
   , ncgSseVersion            :: Maybe SseVersion -- ^ (x86) SSE instructions
   , ncgBmiVersion            :: Maybe BmiVersion -- ^ (x86) BMI instructions
   , ncgDumpRegAllocStages    :: !Bool
   , ncgDumpAsmStats          :: !Bool
   , ncgDumpAsmConflicts      :: !Bool
   }

-- | Return Word size
ncgWordWidth :: NCGConfig -> Width
ncgWordWidth config = platformWordWidth (ncgPlatform config)

-- | Return Word size
platformWordWidth :: Platform -> Width
platformWordWidth platform = case platformWordSize platform of
   PW4 -> W32
   PW8 -> W64
