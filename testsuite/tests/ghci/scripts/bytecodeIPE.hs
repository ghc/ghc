module BytecodeIPE where

import Data.Maybe (isJust)
import GHC.InfoProv (whereFrom)

data Marker = Marker

-- `whereFrom` only succeeds if the module's IPE initializer ran.
probe :: IO Bool
probe = isJust <$> whereFrom Marker
