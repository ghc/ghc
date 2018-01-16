{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.Library (
    Library(..),
    emptyLibrary,
    explicitLibModules,
    libModulesAutogen,
    libModules,
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.BuildInfo
import Distribution.Types.ModuleReexport
import Distribution.Types.UnqualComponentName
import Distribution.ModuleName

import qualified Distribution.Types.BuildInfo.Lens as L

data Library = Library
    { libName           :: Maybe UnqualComponentName
    , exposedModules    :: [ModuleName]
    , reexportedModules :: [ModuleReexport]
    , signatures        :: [ModuleName]   -- ^ What sigs need implementations?
    , libExposed        :: Bool           -- ^ Is the lib to be exposed by default?
    , libBuildInfo      :: BuildInfo
    }
    deriving (Generic, Show, Eq, Read, Typeable, Data)

instance L.HasBuildInfo Library where
    buildInfo f l = (\x -> l { libBuildInfo = x }) <$> f (libBuildInfo l)

instance Binary Library

instance Monoid Library where
  mempty = Library {
    libName = mempty,
    exposedModules = mempty,
    reexportedModules = mempty,
    signatures = mempty,
    libExposed     = True,
    libBuildInfo   = mempty
  }
  mappend = (<>)

instance Semigroup Library where
  a <> b = Library {
    libName = combine libName,
    exposedModules = combine exposedModules,
    reexportedModules = combine reexportedModules,
    signatures = combine signatures,
    libExposed     = libExposed a && libExposed b, -- so False propagates
    libBuildInfo   = combine libBuildInfo
  }
    where combine field = field a `mappend` field b

emptyLibrary :: Library
emptyLibrary = mempty

-- | Get all the module names from the library (exposed and internal modules)
-- which are explicitly listed in the package description which would
-- need to be compiled.  (This does not include reexports, which
-- do not need to be compiled.)  This may not include all modules for which
-- GHC generated interface files (i.e., implicit modules.)
explicitLibModules :: Library -> [ModuleName]
explicitLibModules lib = exposedModules lib
              ++ otherModules (libBuildInfo lib)
              ++ signatures lib

-- | Get all the auto generated module names from the library, exposed or not.
-- This are a subset of 'libModules'.
libModulesAutogen :: Library -> [ModuleName]
libModulesAutogen lib = autogenModules (libBuildInfo lib)

-- | Backwards-compatibility shim for 'explicitLibModules'.  In most cases,
-- you actually want 'allLibModules', which returns all modules that will
-- actually be compiled, as opposed to those which are explicitly listed
-- in the package description ('explicitLibModules'); unfortunately, the
-- type signature for 'allLibModules' is incompatible since we need a
-- 'ComponentLocalBuildInfo'.
{-# DEPRECATED libModules "If you want all modules that are built with a library, use 'allLibModules'.  Otherwise, use 'explicitLibModules' for ONLY the modules explicitly mentioned in the package description." #-}
libModules :: Library -> [ModuleName]
libModules = explicitLibModules
