{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}

module Settings (
    settings
    ) where

import Base hiding (arg, args, Args)
import Rules.Data
import Oracles.Builder
import Expression
import Expression.Settings

settings :: Settings
settings = do
    stage <- asks getStage
    mconcat [ builder GhcCabal ? cabalSettings
            , builder (GhcPkg stage) ? ghcPkgSettings ]
