-- | Cmm compilation configuration

{-# LANGUAGE DerivingStrategies         #-}

module GHC.Cmm.Config
  ( CmmConfig(..)
  , cmmPlatform
  ) where

import GHC.Prelude

import GHC.Platform
import GHC.Platform.Profile


data CmmConfig = CmmConfig
  { cmmProfile             :: !Profile -- ^ Target Profile
  , cmmOptControlFlow      :: !Bool    -- ^ Optimize Cmm Control Flow or not
  , cmmDoLinting           :: !Bool    -- ^ Do Cmm Linting Optimization or not
  , cmmOptElimCommonBlks   :: !Bool    -- ^ Eliminate common blocks or not
  , cmmOptSink             :: !Bool    -- ^ Perform sink after stack layout or not
  , cmmOptThreadSanitizer  :: !Bool    -- ^ Instrument memory accesses for ThreadSanitizer
  , cmmGenStackUnwindInstr :: !Bool    -- ^ Generate stack unwinding instructions (for debugging)
  , cmmExternalDynamicRefs :: !Bool    -- ^ Generate code to link against dynamic libraries
  , cmmDoCmmSwitchPlans    :: !Bool    -- ^ Should the Cmm pass replace Stg switch statements
  , cmmSplitProcPoints     :: !Bool    -- ^ Should Cmm split proc points or not
  , cmmAllowMul2           :: !Bool    -- ^ Does this platform support mul2
  , cmmOptConstDivision    :: !Bool    -- ^ Should we optimize constant divisors
  }

-- | retrieve the target Cmm platform
cmmPlatform :: CmmConfig -> Platform
cmmPlatform = profilePlatform . cmmProfile

