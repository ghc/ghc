module CoreDebugSuppress where

import GhcPrelude

data CoreDebugSuppress = CoreDebugSuppress
  { coreDebugSuppress_suppressIdInfo :: Bool
  , coreDebugSuppress_suppressTicks :: Bool
  , coreDebugSuppress_pprCaseAsLet :: Bool
  }

class HasCoreDebugSuppress c where
  getCoreDebugSuppress :: c -> CoreDebugSuppress
