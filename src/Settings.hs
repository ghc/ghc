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
