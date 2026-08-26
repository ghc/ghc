-- | Caches of module search results.
--
-- The caches for homes modules and external modules are kept separately,
-- as explained in Note [The finder caches].
module GHC.Unit.Finder.Cache
  ( InstalledFindResult (..)

    -- * Home module finder cache
  , HomeFinderCache
  , newHomeFinderCache
  , lookupHomeFinderCache
  , insertHomeFinderCache
  , clearHomeFinderCache

    -- * External module finder cache
  , ExternalFinderCache
  , newExternalFinderCache
  , noExternalFinderCache
  , lookupExternalFinderCache
  , insertExternalFinderCache
  )
where

import GHC.Prelude

import GHC.Data.OsPath
import GHC.Unit.Module.Env
import GHC.Unit.Module.Location
import GHC.Unit.Types

import Data.IORef

--------------------------------------------------------------------------------

{- Note [The finder caches]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Finding the source code for a Haskell module is an expensive operation that
searches the file system, so we want to cache the result of the search to avoid
repeatedly hammering the file system.

There are two separate queries, which we both cache using 'withCacheOrElse':

  - 'findInstalledHomeModule' for home modules,
  - 'findPackageModule' for modules in external units.

The usual downside of caching is that it introduces the risk of a stale cache.
The validity of the cache depends very much on which of the two different
queries we are performing:

  - Home modules can change freely during a session: source files appear, move
    or vanish between two :load operations, and compilation itself writes
    interface files.

    We keep this cache fresh by clearing it at appropriate points, using
    'clearHomeFinderCache'.

  - Modules of external units, by contrast, cannot change in this way. The cache
    belongs to the 'UnitState', which is replaced wholesale at very specific
    points: whenever package flags or unit databases change ('setSessionDynFlags',
    GHCi's `:set -package`), or when Backpack registers a new unit after
    instantiation.
-}

--------------------------------------------------------------------------------
-- InstalledFindResult

-- | The result of searching for an 'InstalledModule'.
data InstalledFindResult
  = InstalledFound ModLocation
  | InstalledNoPackage UnitId
  | InstalledNotFound [OsPath] (Maybe UnitId)

--------------------------------------------------------------------------------
-- HomeFinderCache

-- | Cached results of searching for home modules within a search path.
--
-- Home modules may appear or move around during a session (e.g. between two
-- @:load@s), so this cache can be cleared (see 'clearHomeFinderCache').
--
-- See Note [The finder caches].
newtype HomeFinderCache = HomeFinderCache FinderSearchCache

-- | A new, empty cache.
newHomeFinderCache :: IO HomeFinderCache
newHomeFinderCache = HomeFinderCache <$> newFinderSearchCache

lookupHomeFinderCache :: HomeFinderCache -> InstalledModule -> IO (Maybe InstalledFindResult)
lookupHomeFinderCache (HomeFinderCache table) = lookupFinderSearchCache table

-- | Record a search result in the cache.
-- See Note [Monotonic search caches].
insertHomeFinderCache :: HomeFinderCache -> InstalledModule -> InstalledFindResult -> IO ()
insertHomeFinderCache (HomeFinderCache table) = insertFinderSearchCache table

-- | Remove all entries from the cache.
clearHomeFinderCache :: HomeFinderCache -> IO ()
clearHomeFinderCache (HomeFinderCache table) =
  atomicWriteIORef table emptyInstalledModuleEnv

--------------------------------------------------------------------------------
-- ExternalFinderCache

-- | Cached results of searching for interface files of external units.
--
-- See Note [The finder caches].
data ExternalFinderCache
  = ExternalFinderCache !FinderSearchCache
  | NoExternalFinderCache

newExternalFinderCache :: IO ExternalFinderCache
newExternalFinderCache = ExternalFinderCache <$> newFinderSearchCache

noExternalFinderCache :: ExternalFinderCache
noExternalFinderCache = NoExternalFinderCache

lookupExternalFinderCache :: ExternalFinderCache -> InstalledModule -> IO (Maybe InstalledFindResult)
lookupExternalFinderCache (ExternalFinderCache table) key = lookupFinderSearchCache table key
lookupExternalFinderCache NoExternalFinderCache _ = return Nothing

-- | Record a search result in the cache.
insertExternalFinderCache :: ExternalFinderCache -> InstalledModule -> InstalledFindResult -> IO ()
insertExternalFinderCache (ExternalFinderCache table) key val = insertFinderSearchCache table key val
insertExternalFinderCache NoExternalFinderCache _ _ = return ()

--------------------------------------------------------------------------------
-- Common (internal) implementation

{- Note [Monotonic search caches]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Every insertion into a search cache records the result of a search that just
ran. Two threads may race to insert the result for the same module, and,
because the filesystem may change between their two searches (for example, a
concurrent compilation creating an interface file), the two results may
disagree. In that case we keep the 'InstalledFound' result rather than letting
the loser of the race clobber it with 'InstalledNotFound'.
-}

type FinderSearchCache = IORef (InstalledModuleEnv InstalledFindResult)

newFinderSearchCache :: IO FinderSearchCache
newFinderSearchCache = newIORef emptyInstalledModuleEnv

lookupFinderSearchCache :: FinderSearchCache -> InstalledModule -> IO (Maybe InstalledFindResult)
lookupFinderSearchCache table key = do
  c <- readIORef table
  return $! lookupInstalledModuleEnv c key

insertFinderSearchCache :: FinderSearchCache -> InstalledModule -> InstalledFindResult -> IO ()
insertFinderSearchCache table key val =
  atomicModifyIORef' table $ \c ->
    case (lookupInstalledModuleEnv c key, val) of
      -- See Note [Monotonic search caches]
      (Just InstalledFound{}, InstalledNotFound{}) -> (c, ())
      _ -> (extendInstalledModuleEnv c key val, ())

--------------------------------------------------------------------------------
