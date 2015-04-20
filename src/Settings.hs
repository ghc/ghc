{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}

module Settings (
    buildSettings
    ) where

import Base hiding (arg, args, Args)
import Rules.Data
import Switches
import Oracles.Builder
import Expression.Base

buildSettings :: Settings
buildSettings = msum
    [ builder       GhcCabal ? cabalSettings
    , stagedBuilder GhcPkg   ? ghcPkgSettings ]

packageSettings :: Settings
packageSettings = msum
    [ args ["-hide-all-packages", "-no-user-package-db", "-include-pkg-deps"]
    , stage Stage0 ?
      (arg "-package-db" |> argPath "libraries/bootstrapping.conf")
    , supportsPackageKey && notStage Stage0 ??
      ( argPairs "-this-package-key" argPackageKey <|>
        argPairs "-package-key"      argPackageDepKeys
      , argPairs "-package-name"     argPackageKey <|>
        argPairs "-package"          argPackageDeps )]
