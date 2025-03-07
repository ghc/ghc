-- | Haskell Program Coverage (HPC) support
module GHC.Types.HpcInfo
   ( HpcInfo (..)
   , emptyHpcInfo
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


emptyHpcInfo :: HpcInfo
emptyHpcInfo = NoHpcInfo

