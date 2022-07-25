-- | WebAssembly code-generator configuration
module GHC.CmmToWasm.Config
   ( WasmGenConfig(..)
   )
where

import GHC.Prelude
import GHC.Platform
import GHC.Unit.Module (Module)
import GHC.Utils.Outputable

-- | Native code generator configuration
data WasmGenConfig = WasmGenConfig
   { wasmPlatform              :: !Platform        -- ^ Target platform
   , wasmAsmContext            :: !SDocContext     -- ^ Context for ASM code generation
   , wasmThisModule            :: !Module          -- ^ The name of the module we are currently compiling
   }
