
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

import GHC.Prelude as Prelude ()

import GHC.StgToCmm.Monad

import GHC.Stg.Syntax

import GHC.Types.Id
import GHC.Types.Basic

cgTopRhs :: StgToCmmConfig -> RecFlag -> Id -> CgStgRhs -> (CgIdInfo, FCode ())
        -- The Id is passed along for setting up a binding...
