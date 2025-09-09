module GHC.Driver.Config.StgToJS
  ( initStgToJSConfig
  , initJSLinkConfig
  )
where

import GHC.StgToJS.Types
import GHC.StgToJS.Linker.Types

import GHC.Driver.DynFlags
import GHC.Driver.Config.Linker

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
  , csPrettyRender    = gopt Opt_DisableJsMinifier dflags
  , csTraceRts        = False
  , csAssertRts       = False
  , csBoundsCheck     = gopt Opt_DoBoundsChecking dflags
  , csDebugAlloc      = False
  , csTraceForeign    = False
  , csProf            = ways dflags `hasWay` WayProf
  , csRuntimeAssert   = False
  -- settings
  , csContext         = initSDocContext dflags defaultDumpStyle
  , csLinkerConfig    = initLinkerConfig dflags False -- no C++ linking
  }

-- | Default linker configuration
initJSLinkConfig :: DynFlags -> JSLinkConfig
initJSLinkConfig dflags = JSLinkConfig
  { lcNoJSExecutables = False
  , lcNoHsMain        = False
  , lcNoRts           = False
  , lcNoStats         = False
  , lcCombineAll      = True
  , lcForeignRefs     = True
  , lcForceEmccRts    = False
  , lcLinkCsources    = not (gopt Opt_DisableJsCsources dflags)
  }

