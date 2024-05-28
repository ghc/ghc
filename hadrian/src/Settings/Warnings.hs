module Settings.Warnings (defaultGhcWarningsArgs, ghcWarningsArgs) where

import Expression
import Oracles.Flag
import Oracles.Setting (isOsxTarget, isWinTarget)
import Packages

-- See @mk/warnings.mk@ for warning-related arguments in the Make build system.

-- | Default Haskell warning-related arguments.
defaultGhcWarningsArgs :: Args
defaultGhcWarningsArgs = mconcat
    [ notStage0 ? arg "-Wnoncanonical-monad-instances"
    , notM (flag CcLlvmBackend) ? arg "-optc-Wno-error=inline"
    , flag CcLlvmBackend ? arg "-optc-Wno-unknown-pragmas"
      -- Cabal can seemingly produce filepaths with incorrect case on filesystems
      -- with case-insensitive names. Ignore such issues for now as they seem benign.
      -- See #17798.
    , isOsxTarget ? arg "-optP-Wno-nonportable-include-path"
    , isWinTarget ? arg "-optP-Wno-nonportable-include-path"
    ]

-- | Package-specific warnings-related arguments, mostly suppressing various warnings.
ghcWarningsArgs :: Args
ghcWarningsArgs = do
    mconcat
        [ stage0 ? mconcat
        [ libraryPackage       ? pure [ "-fno-warn-deprecated-flags" ]
        , package terminfo     ? pure [ "-fno-warn-unused-imports", "-Wno-deriving-typeable"]
        , package transformers ? pure [ "-fno-warn-unused-matches"
                                      , "-fno-warn-unused-imports" ]
        , package stm          ? pure [ "-Wno-deriving-typeable" ]
        , package osString     ? pure [ "-Wno-deriving-typeable" ]
        , package parsec       ? pure [ "-Wno-deriving-typeable" ]
        , package cabal        ? pure [ "-Wno-deriving-typeable" ]
        , package cabalSyntax  ? pure [ "-Wno-deriving-typeable" ]
        , package time         ? pure [ "-Wno-deriving-typeable" ]
        , package unix         ? pure [ "-Wno-deriving-typeable" ]
          ]
        , notStage0 ? mconcat
        [ libraryPackage       ? pure [ "-Wno-deprecated-flags" ]
        , package ghcInternal  ? pure [ "-Wno-trustworthy-safe" ]
        , package base         ? pure [ "-Wno-trustworthy-safe" ]
        , package binary       ? pure [ "-Wno-deprecations" ]
        , package bytestring   ? pure [ "-Wno-inline-rule-shadowing" ]
        , package compiler     ? pure [ "-Wcpp-undef" ]
        , package directory    ? pure [ "-Wno-unused-imports"
                                      , "-Wno-deprecations" -- https://gitlab.haskell.org/ghc/ghc/-/issues/24240
                                      ]
        , package ghc          ? pure [ "-Wcpp-undef"
                                      , "-Wincomplete-uni-patterns"
                                      , "-Wincomplete-record-updates"
                                      ]
        , package ghcPrim      ? pure [ "-Wno-trustworthy-safe" ]
        , package haddockLibrary ? pure [ "-Wno-unused-imports" ]
        , package haddockApi     ? pure [ "-Wno-unused-imports"
                                        , "-Wno-deprecations"
                                        , "-Wno-x-partial" ]
        , package haskeline    ? pure [ "-Wno-deprecations"
                                      , "-Wno-x-partial"
                                      , "-Wno-unused-imports"
                                      , "-Wno-redundant-constraints"
                                      , "-Wno-simplifiable-class-constraints"
                                      , "-Wno-deriving-typeable" ]
        , package pretty       ? pure [ "-Wno-unused-imports" ]
        , package primitive    ? pure [ "-Wno-unused-imports"
                                      , "-Wno-deprecations" ]
        , package rts          ? pure [ "-Wcpp-undef" ]
        , package text         ? pure [ "-Wno-deprecations", "-Wno-deriving-typeable" ]
        , package terminfo     ? pure [ "-Wno-unused-imports", "-Wno-deriving-typeable" ]
        , package stm          ? pure [ "-Wno-deriving-typeable" ]
        , package osString     ? pure [ "-Wno-deriving-typeable" ]
        , package parsec       ? pure [ "-Wno-deriving-typeable" ]
        , package cabal        ? pure [ "-Wno-deriving-typeable" ]
        , package cabalSyntax  ? pure [ "-Wno-deriving-typeable" ]
        , package time         ? pure [ "-Wno-deriving-typeable" ]
        , package transformers ? pure [ "-Wno-unused-matches"
                                      , "-Wno-unused-imports"
                                      , "-Wno-redundant-constraints"
                                      , "-Wno-orphans" ]
        , package unix         ? pure [ "-Wno-deprecations", "-Wno-deriving-typeable" ]
        , package win32        ? pure [ "-Wno-trustworthy-safe"
                                      , "-Wno-deprecations" -- https://gitlab.haskell.org/ghc/ghc/-/issues/24240
                                      , "-Wno-deriving-typeable"
                                      ]
        , package xhtml        ? pure [ "-Wno-unused-imports" ] ] ]
