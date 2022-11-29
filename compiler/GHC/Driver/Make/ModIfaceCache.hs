module GHC.Driver.Make.ModIfaceCache where

import GHC.Prelude

import GHC.Unit.Module.ModIface
import GHC.Unit.Home.ModInfo


import Data.IORef


-- Abstract interface to a cache of HomeModInfo
-- See Note [Caching HomeModInfo]
data ModIfaceCache = ModIfaceCache { iface_clearCache :: IO [CachedIface]
                                   , iface_addToCache :: CachedIface -> IO () }

addHmiToCache :: ModIfaceCache -> HomeModInfo -> IO ()
addHmiToCache c (HomeModInfo i _ l) = iface_addToCache c (CachedIface i l)

data CachedIface = CachedIface { cached_modiface :: !ModIface
                               , cached_linkable :: !HomeModLinkable }

noIfaceCache :: Maybe ModIfaceCache
noIfaceCache = Nothing

newIfaceCache :: IO ModIfaceCache
newIfaceCache = do
  ioref <- newIORef []
  return $
    ModIfaceCache
      { iface_clearCache = atomicModifyIORef' ioref (\c -> ([], c))
      , iface_addToCache = \hmi -> atomicModifyIORef' ioref (\c -> (hmi:c, ()))
      }