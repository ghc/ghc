-- | WebAssembly code-generator configuration
module GHC.CmmToWasm.Config
   ( WasmGenConfig(..)
   )
where

import GHC.Prelude

import GHC.Cmm
import GHC.Platform
import GHC.Unit.Module (Module)
import GHC.Utils.Outputable

-- | Native code generator configuration
data WasmGenConfig = WasmGenConfig
   { wcgPlatform              :: !Platform        -- ^ Target platform
   , wcgAsmContext            :: !SDocContext     -- ^ Context for ASM code generation
   , wcgThisModule            :: !Module          -- ^ The name of the module we are currently compiling
   , wcgPinnedRegs            :: ![GlobalReg]     -- ^ GHC global registers to be passed as
                                                   -- parameters and returned as results

   , wcgDoConstantFolding     :: !Bool            -- ^ rewrite Cmm
   , wcgComputeUnwinding      :: !Bool            -- XXX
   , wcgDwarfEnabled          :: !Bool            -- XXX
   }
