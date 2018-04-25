{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.Executable (
    Executable(..),
    emptyExecutable,
    exeModules,
    exeModulesAutogen
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.BuildInfo
import Distribution.Types.UnqualComponentName
import Distribution.Types.ExecutableScope
import Distribution.ModuleName

import qualified Distribution.Types.BuildInfo.Lens as L

data Executable = Executable {
        exeName    :: UnqualComponentName,
        modulePath :: FilePath,
        exeScope   :: ExecutableScope,
        buildInfo  :: BuildInfo
    }
    deriving (Generic, Show, Read, Eq, Typeable, Data)

instance L.HasBuildInfo Executable where
    buildInfo f l = (\x -> l { buildInfo = x }) <$> f (buildInfo l)

instance Binary Executable

instance Monoid Executable where
  mempty = gmempty
  mappend = (<>)

instance Semigroup Executable where
  a <> b = Executable{
    exeName    = combine' exeName,
    modulePath = combine modulePath,
    exeScope   = combine exeScope,
    buildInfo  = combine buildInfo
  }
    where combine field = field a `mappend` field b
          combine' field = case ( unUnqualComponentName $ field a
                                , unUnqualComponentName $ field b) of
                      ("", _) -> field b
                      (_, "") -> field a
                      (x, y) -> error $ "Ambiguous values for executable field: '"
                                  ++ x ++ "' and '" ++ y ++ "'"

emptyExecutable :: Executable
emptyExecutable = mempty

-- | Get all the module names from an exe
exeModules :: Executable -> [ModuleName]
exeModules exe = otherModules (buildInfo exe)

-- | Get all the auto generated module names from an exe
-- This are a subset of 'exeModules'.
exeModulesAutogen :: Executable -> [ModuleName]
exeModulesAutogen exe = autogenModules (buildInfo exe)
