{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeSourceCallIface where

import ClosedTableLibrary (flipBit)
import Data.Proxy (Proxy(..))

tag :: foreach (b :: Bool) -> Proxy b
tag _b = Proxy

yes :: Proxy True
yes = tag (flipBit False)
