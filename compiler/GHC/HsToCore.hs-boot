{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


The Desugarer: turning HsSyn into Core.
-}

module GHC.HsToCore (
    -- * Desugaring operations
    deSugar, deSugarExpr
    ) where

import GHC.Prelude

import GHC.Driver.Env

import GHC.Hs

import GHC.HsToCore.Errors.Types

import GHC.Tc.Types

import GHC.Core

import GHC.Utils.Error

import GHC.Unit
import GHC.Unit.Module.ModGuts

deSugar :: HscEnv -> ModLocation -> TcGblEnv -> IO (Messages DsMessage, Maybe ModGuts)

deSugarExpr :: HscEnv -> LHsExpr GhcTc -> IO (Messages DsMessage, Maybe CoreExpr)
