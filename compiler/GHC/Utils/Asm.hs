-- | Various utilities used in generating assembler.
--
-- These are used not only by the native code generator, but also by the
-- GHC.Driver.Pipeline
module GHC.Utils.Asm
    ( sectionType
    ) where

import GHC.Prelude

import GHC.Platform
import GHC.Utils.Outputable

-- | Generate a section type (e.g. @\@progbits@). See #13937.
sectionType :: Platform -- ^ Target platform
            -> String   -- ^ section type
            -> SDoc     -- ^ pretty assembler fragment
sectionType platform ty =
    case platformArch platform of
      ArchARM{} -> char '%' <> text ty
      _         -> char '@' <> text ty
