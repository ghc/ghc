
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
--
-- Stg to C-- code generation
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module GHC.StgToCmm ( cgTopRhs ) where

import GHC.Prelude as Prelude

import GHC.StgToCmm.Prof (initCostCentres, ldvEnter)
import GHC.StgToCmm.Monad
import GHC.StgToCmm.Env
import GHC.StgToCmm.Bind
import GHC.StgToCmm.Layout
import GHC.StgToCmm.Utils
import GHC.StgToCmm.Closure
import GHC.StgToCmm.Config
import GHC.StgToCmm.Hpc
import GHC.StgToCmm.Ticky
import GHC.StgToCmm.Types (ModuleLFInfos)

import GHC.Cmm
import GHC.Cmm.Utils
import GHC.Cmm.CLabel
import GHC.Cmm.Graph

import GHC.Stg.Syntax

import GHC.Types.CostCentre
import GHC.Types.IPE
import GHC.Types.HpcInfo
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.RepType
import GHC.Types.Basic
import GHC.Types.Var.Set ( isEmptyDVarSet )
import GHC.Types.Unique.FM
import GHC.Types.Name.Env

import GHC.Core.DataCon
import GHC.Core.TyCon
import GHC.Core.Multiplicity

import GHC.Unit.Module

import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Panic.Plain
import GHC.Utils.Logger

import GHC.Utils.TmpFs

import GHC.Data.Stream
import GHC.Data.OrdList
import GHC.Types.Unique.Map

import Control.Monad (when,void, forM_)
import GHC.Utils.Misc
import System.IO.Unsafe
import qualified Data.ByteString as BS
import Data.IORef
import GHC.Utils.Panic (assertPpr)

cgTopRhs :: StgToCmmConfig -> RecFlag -> Id -> CgStgRhs -> (CgIdInfo, FCode ())
        -- The Id is passed along for setting up a binding...
