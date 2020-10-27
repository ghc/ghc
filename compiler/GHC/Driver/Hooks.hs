-- | Retrieving hooks from the session state
module GHC.Driver.Hooks
   ( lookupHook
   , getHooked
   , module GHC.Types.Hook
   )
where

import GHC.Prelude
import GHC.Driver.Session
import GHC.Types.Hook

import Data.Maybe

getHooked :: (Functor f, HasDynFlags f, IsHook h) => h -> f h
getHooked def = fmap (lookupHook def) getDynFlags

lookupHook :: IsHook h => h -> DynFlags -> h
lookupHook def = fromMaybe def . getHook . hooks
