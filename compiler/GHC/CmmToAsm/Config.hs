-- | Native code generator configuration
module GHC.CmmToAsm.Config
   ( Config(..)
   , configWordWidth
   , platformWordWidth
   )
where

import GhcPrelude
import GHC.Platform
import GHC.Cmm.Type (Width(..))

-- | Native code generator configuration
data Config = Config
   { configPlatform            :: !Platform    -- ^ Target platform
   , configProcAlignment       :: !(Maybe Int) -- ^ Mandatory proc alignment
   , configDebugLevel          :: !Int         -- ^ Debug level
   , configExternalDynamicRefs :: !Bool        -- ^ Generate code to link against dynamic libraries
   , configPIC                 :: !Bool        -- ^ Enable Position-Independent Code
   , configSplitSections       :: !Bool        -- ^ Split sections
   , configSpillPreallocSize   :: !Int         -- ^ Size in bytes of the pre-allocated spill space on the C stack
   , configRegsIterative       :: !Bool
   , configAsmLinting          :: !Bool        -- ^ Perform ASM linting pass
   , configDumpRegAllocStages  :: !Bool
   , configDumpAsmStats        :: !Bool
   , configDumpAsmConflicts    :: !Bool
   }

-- | Return Word size
configWordWidth :: Config -> Width
configWordWidth config = platformWordWidth (configPlatform config)

-- | Return Word size
platformWordWidth :: Platform -> Width
platformWordWidth platform = case platformWordSize platform of
   PW4 -> W32
   PW8 -> W64
