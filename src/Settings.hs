{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}

module Settings (
    buildSettings
    ) where

import Base hiding (arg, args, Args)
import Rules.Data
import Oracles.Builder
import Expression
import Expression.Settings

buildSettings :: Settings
buildSettings = do
    stage <- asks getStage
    mconcat [ builder GhcCabal ? cabalSettings
            , builder (GhcPkg stage) ? ghcPkgSettings ]
