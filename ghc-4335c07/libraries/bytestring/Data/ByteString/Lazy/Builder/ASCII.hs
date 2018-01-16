
-- | We decided to rename the Builder modules. Sorry about that.
--
-- In additon, the ASCII module has been merged into the main
-- "Data.ByteString.Builder" module.
--
-- The old names will hang about for at least once release cycle before we
-- deprecate them and then later remove them.
--
module Data.ByteString.Lazy.Builder.ASCII (
  module Data.ByteString.Builder
, byteStringHexFixed
, lazyByteStringHexFixed
) where

import Data.ByteString.Builder
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L

byteStringHexFixed :: S.ByteString -> Builder
byteStringHexFixed = byteStringHex

lazyByteStringHexFixed :: L.ByteString -> Builder
lazyByteStringHexFixed = lazyByteStringHex
