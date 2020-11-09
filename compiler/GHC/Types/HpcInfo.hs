-- | Haskell Program Coverage (HPC) support
module GHC.Types.HpcInfo
   ( HpcInfo (..)
   , AnyHpcUsage
   , emptyHpcInfo
   , isHpcUsed
   )
where

import GHC.Prelude

-- | Information about a modules use of Haskell Program Coverage
data HpcInfo
  = HpcInfo
     { hpcInfoTickCount :: Int
     , hpcInfoHash      :: Int
     }
  | NoHpcInfo
     { hpcUsed          :: AnyHpcUsage  -- ^ Is hpc used anywhere on the module \*tree\*?
     }

-- | This is used to signal if one of my imports used HPC instrumentation
-- even if there is no module-local HPC usage
type AnyHpcUsage = Bool

emptyHpcInfo :: AnyHpcUsage -> HpcInfo
emptyHpcInfo = NoHpcInfo

-- | Find out if HPC is used by this module or any of the modules
-- it depends upon
isHpcUsed :: HpcInfo -> AnyHpcUsage
isHpcUsed (HpcInfo {})                   = True
isHpcUsed (NoHpcInfo { hpcUsed = used }) = used

