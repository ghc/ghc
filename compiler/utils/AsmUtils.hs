-- | Various utilities used in generating assembler.
--
-- These are used not only by the native code generator, but also by the
-- "DriverPipeline".
module AsmUtils
    ( sectionType
    ) where

import GhcPrelude

import Platform
import Outputable

-- | Generate a section type (e.g. @\@progbits@). See #13937.
sectionType :: String -- ^ section type
            -> SDoc   -- ^ pretty assembler fragment
sectionType ty = sdocWithPlatform $ \platform ->
    case platformArch platform of
      ArchARM{} -> char '%' <> text ty
      _         -> char '@' <> text ty
