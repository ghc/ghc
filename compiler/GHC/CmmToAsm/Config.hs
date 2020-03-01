-- | Native code generator configuration
module GHC.CmmToAsm.Config
   ( NCGConfig(..)
   , ncgWordWidth
   )
where

import GhcPrelude
import GHC.Platform
import GHC.Cmm.Type (Width(..))

-- | Native code generator configuration
data NCGConfig = NCGConfig
   { ncgPlatform            :: !Platform    -- ^ Target platform
   , ncgProcAlignment       :: !(Maybe Int) -- ^ Mandatory proc alignment
   , ncgDebugLevel          :: !Int         -- ^ Debug level
   , ncgExternalDynamicRefs :: !Bool        -- ^ Generate code to link against dynamic libraries
   , ncgPIC                 :: !Bool        -- ^ Enable Position-Independent Code
   , ncgSplitSections       :: !Bool        -- ^ Split sections
   , ncgSpillPreallocSize   :: !Int         -- ^ Size in bytes of the pre-allocated spill space on the C stack
   , ncgRegsIterative       :: !Bool
   , ncgAsmLinting          :: !Bool        -- ^ Perform ASM linting pass
   , ncgDumpRegAllocStages  :: !Bool
   , ncgDumpAsmStats        :: !Bool
   , ncgDumpAsmConflicts    :: !Bool
   }

-- | Return Word size
ncgWordWidth :: NCGConfig -> Width
ncgWordWidth config = case platformWordSize (ncgPlatform config) of
   PW4 -> W32
   PW8 -> W64
