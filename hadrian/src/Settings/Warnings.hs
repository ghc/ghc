module Settings.Warnings (defaultGhcWarningsArgs, ghcWarningsArgs) where

import Expression
import Oracles.Flag
import Packages
import Settings

-- See @mk/warnings.mk@ for warning-related arguments in the Make build system.

-- | Default Haskell warning-related arguments.
defaultGhcWarningsArgs :: Args
defaultGhcWarningsArgs = mconcat
    [ notStage0 ? arg "-Wnoncanonical-monad-instances"
    , notM (flag CcLlvmBackend) ? arg "-optc-Wno-error=inline"
    , flag CcLlvmBackend ? arg "-optc-Wno-unknown-pragmas" ]

-- | Package-specific warnings-related arguments, mostly suppressing various warnings.
ghcWarningsArgs :: Args
ghcWarningsArgs = do
    isIntegerSimple <- (== integerSimple) <$> getIntegerPackage
    mconcat
        [ stage0 ? mconcat
        [ libraryPackage       ? pure [ "-fno-warn-deprecated-flags" ]
        , package terminfo     ? pure [ "-fno-warn-unused-imports" ]
        , package transformers ? pure [ "-fno-warn-unused-matches"
                                      , "-fno-warn-unused-imports" ] ]
        , notStage0 ? mconcat
        [ libraryPackage       ? pure [ "-Wno-deprecated-flags" ]
        , package base         ? pure [ "-Wno-trustworthy-safe" ]
        , package binary       ? pure [ "-Wno-deprecations" ]
        , package bytestring   ? pure [ "-Wno-inline-rule-shadowing" ]
        , package compiler     ? pure [ "-Wcpp-undef" ]
        , package directory    ? pure [ "-Wno-unused-imports" ]
        , package ghc          ? pure [ "-Wcpp-undef"
                                      , "-Wincomplete-uni-patterns"
                                      , "-Wincomplete-record-updates"
                                      ]
        , package ghcPrim      ? pure [ "-Wno-trustworthy-safe" ]
        , package haddock      ? pure [ "-Wno-unused-imports"
                                      , "-Wno-deprecations" ]
        , package haskeline    ? pure [ "-Wno-deprecations"
                                      , "-Wno-unused-imports"
                                      , "-Wno-redundant-constraints"
                                      , "-Wno-simplifiable-class-constraints" ]
        , package pretty       ? pure [ "-Wno-unused-imports" ]
        , package primitive    ? pure [ "-Wno-unused-imports"
                                      , "-Wno-deprecations" ]
        , package rts          ? pure [ "-Wcpp-undef" ]
        , package terminfo     ? pure [ "-Wno-unused-imports" ]
        , isIntegerSimple      ?
          package text         ? pure [ "-Wno-unused-imports" ]
        , package transformers ? pure [ "-Wno-unused-matches"
                                      , "-Wno-unused-imports"
                                      , "-Wno-redundant-constraints"
                                      , "-Wno-orphans" ]
        , package win32        ? pure [ "-Wno-trustworthy-safe" ]
        , package xhtml        ? pure [ "-Wno-unused-imports" ] ] ]
