-- | LLVM config cache
module GHC.Driver.LlvmConfigCache
  ( LlvmConfigCache
  , initLlvmConfigCache
  , readLlvmConfigCache
  )
where

import GHC.Prelude
import GHC.CmmToLlvm.Config

import System.IO.Unsafe

-- | Cache LLVM configuration read from files in top_dir
--
-- See Note [LLVM configuration] in GHC.CmmToLlvm.Config
--
-- Currently implemented with unsafe lazy IO. But it could be implemented with
-- an IORef as the exposed interface is in IO.
data LlvmConfigCache = LlvmConfigCache LlvmConfig

initLlvmConfigCache :: FilePath -> IO LlvmConfigCache
initLlvmConfigCache top_dir = pure $ LlvmConfigCache (unsafePerformIO $ initLlvmConfig top_dir)

readLlvmConfigCache :: LlvmConfigCache -> IO LlvmConfig
readLlvmConfigCache (LlvmConfigCache !config) = pure config
