module Distribution.Types.Library.Lens (
    Library,
    module Distribution.Types.Library.Lens,
    ) where

import Prelude ()
import Distribution.Compat.Prelude
import Distribution.Compat.Lens

import Distribution.ModuleName                (ModuleName)
import Distribution.Types.BuildInfo           (BuildInfo)
import Distribution.Types.Library             (Library)
import Distribution.Types.ModuleReexport      (ModuleReexport)
import Distribution.Types.UnqualComponentName (UnqualComponentName)

import qualified Distribution.Types.Library as T

libName :: Lens' Library (Maybe UnqualComponentName)
libName f s = fmap (\x -> s { T.libName = x }) (f (T.libName s))
{-# INLINE libName #-}

exposedModules :: Lens' Library [ModuleName]
exposedModules f s = fmap (\x -> s { T.exposedModules = x }) (f (T.exposedModules s))
{-# INLINE exposedModules #-}

reexportedModules :: Lens' Library [ModuleReexport]
reexportedModules f s = fmap (\x -> s { T.reexportedModules = x }) (f (T.reexportedModules s))
{-# INLINE reexportedModules #-}

signatures :: Lens' Library [ModuleName]
signatures f s = fmap (\x -> s { T.signatures = x }) (f (T.signatures s))
{-# INLINE signatures #-}

libExposed :: Lens' Library Bool
libExposed f s = fmap (\x -> s { T.libExposed = x }) (f (T.libExposed s))
{-# INLINE libExposed #-}

libBuildInfo :: Lens' Library BuildInfo
libBuildInfo f s = fmap (\x -> s { T.libBuildInfo = x }) (f (T.libBuildInfo s))
{-# INLINE libBuildInfo #-}
