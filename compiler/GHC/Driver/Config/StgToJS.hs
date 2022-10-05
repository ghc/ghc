module GHC.Driver.Config.StgToJS
  ( initStgToJSConfig
  )
where

import GHC.StgToJS.Types

import GHC.Driver.Session
import GHC.Platform.Ways
import GHC.Utils.Outputable

import GHC.Prelude

-- | Initialize StgToJS settings from DynFlags
initStgToJSConfig :: DynFlags -> StgToJSConfig
initStgToJSConfig dflags = StgToJSConfig
  -- flags
  { csInlinePush      = False
  , csInlineBlackhole = False
  , csInlineLoadRegs  = False
  , csInlineEnter     = False
  , csInlineAlloc     = False
  , csTraceRts        = False
  , csAssertRts       = False
  , csBoundsCheck     = gopt Opt_DoBoundsChecking dflags
  , csDebugAlloc      = False
  , csTraceForeign    = False
  , csProf            = ways dflags `hasWay` WayProf
  , csRuntimeAssert   = False
  -- settings
  , csContext         = initSDocContext dflags defaultDumpStyle
  }
