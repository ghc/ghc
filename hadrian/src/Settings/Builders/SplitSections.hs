-- | Settings required when split-sections is enabled.
module Settings.Builders.SplitSections where

import Expression
import Packages
import Settings
import Flavour.Type

import Oracles.Setting

-- | Does it make sense to enable or disable split sections?
splitSectionsArgs :: Args
splitSectionsArgs = do
  pkg <- getPackage
  osx <- expr isOsxTarget
  notSt0 <- notStage0
  flav <- expr flavour
  if ( ghcSplitSections flav
         -- Flavour enables split-sections
    && not osx
         -- OS X doesn't support split sections
    && notSt0
         -- Disable for stage 0 because we aren't going to ship
         -- the resulting binaries and consequently there is no
         -- reason to minimize size.
    && (pkg /= ghc)
         -- Disable section splitting for the GHC library.
         -- It takes too long and there is little benefit.
    ) then
    ( mconcat
        [ builder (Ghc CompileHs) ? arg "-fsplit-sections"
        , builder (Ghc CompileCWithGhc) ? arg "-fsplit-sections"
        , builder (Ghc CompileCppWithGhc) ? arg "-fsplit-sections"
        , builder (Cc CompileC) ? arg "-ffunction-sections" <> arg "-fdata-sections"
        , builder MergeObjects ? ifM (expr isWinTarget)
            (pure ["-T", "driver/utils/merge_sections_pe.ld"])
            (pure ["-T", "driver/utils/merge_sections.ld"])
        ]
    ) else mempty
