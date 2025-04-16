module Language.Haskell.Syntax.ImpExp.IsBoot ( IsBootInterface(..) ) where

import Prelude (Eq, Ord, Show)
import Data.Data (Data)
import Control.DeepSeq (NFData(..), rwhnf)

-- | Indicates whether a module name is referring to a boot interface (hs-boot
-- file) or regular module (hs file). We need to treat boot modules specially
-- when building compilation graphs, since they break cycles. Regular source
-- files and signature files are treated equivalently.
data IsBootInterface = NotBoot | IsBoot
    deriving (Eq, Ord, Show, Data)

instance NFData IsBootInterface where
  rnf = rwhnf