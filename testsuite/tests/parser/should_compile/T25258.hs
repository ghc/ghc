{-# LANGUAGE MagicHash, ExtendedLiterals, RequiredTypeArguments #-}

module T25258 where

import GHC.Exts
import GHC.Word

f :: Int# -> forall t -> ()
f _ _ = ()

x4 :: forall t -> ()
x4 = f 0x08#Int

x5 :: ()
x5 = f 0x08# Int

x6 :: Word8
x6 = W8# (-10#Word8) -- we now allow negative unsigned extended literals (with a warning)
