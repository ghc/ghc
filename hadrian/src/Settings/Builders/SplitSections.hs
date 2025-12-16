-- | Settings required when split-sections is enabled.
module Settings.Builders.SplitSections where

import Expression
import Packages
import Settings
import Settings.Builders.Common
import Flavour.Type


-- | Does it make sense to enable or disable split sections?
splitSectionsArgs :: Args
splitSectionsArgs = do
  pkg <- getPackage
  osx <- expr isOsxTarget
  cross <- expr $ flag CrossCompiling
  notSt0 <- notStage0
  flav <- expr flavour
  if ( ghcSplitSections flav
         -- Flavour enables split-sections
    && not osx
         -- OS X doesn't support split sections
    && (cross || notSt0)
         -- Disable for stage 0 because we aren't going to ship
         -- the resulting binaries and consequently there is no
         -- reason to minimize size. Unless cross compiling.
    && (pkg /= ghc)
         -- Disable section splitting for the GHC library.
         -- It takes too long and there is little benefit.
    ) then
    ( mconcat
        [ builder (Ghc CompileHs) ? arg "-fsplit-sections"
        , builder (Ghc CompileCWithGhc) ? arg "-fsplit-sections"
        , builder (Ghc CompileCppWithGhc) ? arg "-fsplit-sections"
        , builder (Cc CompileC) ? arg "-ffunction-sections" <> arg "-fdata-sections"
        ]
    ) else mempty
