module GHC.Driver.Config.HsToCore.Ticks
  ( initTicksConfig
  , breakpointsAllowed
  )
where

import GHC.Prelude

import Data.Maybe (catMaybes)

import GHC.Driver.Backend
import GHC.Driver.Session
import GHC.HsToCore.Ticks

initTicksConfig :: DynFlags -> TicksConfig
initTicksConfig dflags = TicksConfig
  { ticks_passes       = coveragePasses dflags
  , ticks_profAuto     = profAuto dflags
  , ticks_countEntries = gopt Opt_ProfCountEntries dflags
  }

breakpointsAllowed :: DynFlags -> Bool
breakpointsAllowed dflags =
  gopt Opt_InsertBreakpoints dflags &&
  backendSupportsBreakpoints (backend dflags)

coveragePasses :: DynFlags -> [TickishType]
coveragePasses dflags = catMaybes
  [ ifA Breakpoints $ breakpointsAllowed dflags
  , ifA HpcTicks $ gopt Opt_Hpc dflags
  , ifA ProfNotes $ sccProfilingEnabled dflags && profAuto dflags /= NoProfAuto
  , ifA SourceNotes $ needSourceNotes dflags
  ]
  where ifA x cond = if cond then Just x else Nothing
