
-- | JS symbol generation
module GHC.StgToJS.Symbols
  ( moduleGlobalSymbol
  , moduleExportsSymbol
  , mkJsSymbol
  , mkJsSymbolBS
  , mkFreshJsSymbol
  , mkRawSymbol
  , intBS
  , word64BS
  ) where

import GHC.Prelude

import GHC.Data.FastString
import GHC.Unit.Module
import GHC.Utils.Word64 (intToWord64)
import Data.ByteString (ByteString)
import Data.Word (Word64)
import qualified Data.ByteString.Char8   as BSC
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy    as BSL

-- | Hexadecimal representation of an int
--
-- Used for the sub indices.
intBS :: Int -> ByteString
intBS = word64BS . intToWord64

-- | Hexadecimal representation of a 64-bit word
--
-- Used for uniques. We could use base-62 as GHC usually does but this is likely
-- faster.
word64BS :: Word64 -> ByteString
word64BS = BSL.toStrict . BSB.toLazyByteString . BSB.word64Hex

-- | Return z-encoded unit:module
unitModuleStringZ :: Module -> ByteString
unitModuleStringZ mod = mconcat
  [ fastZStringToByteString (zEncodeFS (unitIdFS (moduleUnitId mod)))
  , BSC.pack "ZC" -- z-encoding for ":"
  , fastZStringToByteString (zEncodeFS (moduleNameFS (moduleName mod)))
  ]

-- | the global linkable unit of a module exports this symbol, depend on it to
--   include that unit (used for cost centres)
moduleGlobalSymbol :: Module -> FastString
moduleGlobalSymbol m = mkFastStringByteString $ mconcat
  [ hd
  , unitModuleStringZ m
  , BSC.pack "_<global>"
  ]

moduleExportsSymbol :: Module -> FastString
moduleExportsSymbol m = mkFastStringByteString $ mconcat
  [ hd
  , unitModuleStringZ m
  , BSC.pack "_<exports>"
  ]

-- | Make JS symbol corresponding to the given Haskell symbol in the given
-- module
mkJsSymbolBS :: Bool -> Module -> FastString -> ByteString
mkJsSymbolBS exported mod s = mconcat
  [ if exported then hd else hdd
  , unitModuleStringZ mod
  , BSC.pack "zi" -- z-encoding of "."
  , fastZStringToByteString (zEncodeFS s)
  ]

-- | Make JS symbol corresponding to the given Haskell symbol in the given
-- module
mkJsSymbol :: Bool -> Module -> FastString -> FastString
mkJsSymbol exported mod s = mkFastStringByteString (mkJsSymbolBS exported mod s)

-- | Make JS symbol for given module and unique.
mkFreshJsSymbol :: Module -> Int -> FastString
mkFreshJsSymbol mod i = mkFastStringByteString $ mconcat
  [ hdd
  , unitModuleStringZ mod
  , BSC.pack "_"
  , intBS i
  ]

-- | Make symbol "h$XYZ" or "h$$XYZ"
mkRawSymbol :: Bool -> FastString -> FastString
mkRawSymbol exported fs
  | exported  = mkFastStringByteString $ mconcat [ hd,  bytesFS fs ]
  | otherwise = mkFastStringByteString $ mconcat [ hdd, bytesFS fs ]

-- | "h$$" constant string
hdd :: ByteString
hdd = BSC.pack "h$$"

-- | "h$" constant string
hd :: ByteString
hd = BSC.take 2 hdd
