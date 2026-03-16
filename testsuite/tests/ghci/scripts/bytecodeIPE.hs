module bytecodeIPE where

import Data.Maybe (isJust)
import GHC.InfoProv (whereFrom)

marker :: String
marker = id "bytecode-stub-init"
{-# NOINLINE marker #-}

-- `whereFrom` only succeeds if the module's IPE initializer ran.
probe :: IO Bool
probe = isJust <$> whereFrom marker
