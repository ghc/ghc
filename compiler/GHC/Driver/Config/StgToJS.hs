module GHC.Driver.Config.StgToJS
  ( initStgToJSConfig
  )
where

import GHC.Prelude
import GHC.Driver.Session
import GHC.Platform.Ways
import GHC.StgToJS.Types

-- | Initialize StgToJS settings from DynFlags
initStgToJSConfig :: DynFlags -> StgToJSConfig
initStgToJSConfig dflags = StgToJSConfig
  { csInlinePush      = False
  , csInlineBlackhole = False
  , csInlineLoadRegs  = False
  , csInlineEnter     = False
  , csInlineAlloc     = False
  , csTraceRts        = False
  , csAssertRts       = False
  , csDebugAlloc      = False
  , csTraceForeign    = False
  , csProf            = ways dflags `hasWay` WayProf
  , csRuntimeAssert   = False
  }
