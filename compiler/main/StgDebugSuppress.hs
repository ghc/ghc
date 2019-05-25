module StgDebugSuppress where

import GhcPrelude

data StgDebugSuppress = StgDebugSuppress
  { stgDebugSuppress_suppressStgExts :: Bool
  , stgDebugSuppress_sccProfilingOn :: Bool
  }

class HasStgDebugSuppress c where
  getStgDebugSuppress :: c -> StgDebugSuppress
