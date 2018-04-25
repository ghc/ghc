module Distribution.Types.ForeignLib.Lens (
    ForeignLib,
    module Distribution.Types.ForeignLib.Lens,
    ) where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.BuildInfo           (BuildInfo)
import Distribution.Types.ForeignLib          (ForeignLib, LibVersionInfo)
import Distribution.Types.ForeignLibOption    (ForeignLibOption)
import Distribution.Types.ForeignLibType      (ForeignLibType)
import Distribution.Types.UnqualComponentName (UnqualComponentName)
import Distribution.Version                   (Version)

import qualified Distribution.Types.ForeignLib as T

foreignLibName :: Lens' ForeignLib UnqualComponentName
foreignLibName f s = fmap (\x -> s { T.foreignLibName = x }) (f (T.foreignLibName s))
{-# INLINE foreignLibName #-}

foreignLibType :: Lens' ForeignLib ForeignLibType
foreignLibType f s = fmap (\x -> s { T.foreignLibType = x }) (f (T.foreignLibType s))
{-# INLINE foreignLibType #-}

foreignLibOptions :: Lens' ForeignLib [ForeignLibOption]
foreignLibOptions f s = fmap (\x -> s { T.foreignLibOptions = x }) (f (T.foreignLibOptions s))
{-# INLINE foreignLibOptions #-}

foreignLibBuildInfo :: Lens' ForeignLib BuildInfo
foreignLibBuildInfo f s = fmap (\x -> s { T.foreignLibBuildInfo = x }) (f (T.foreignLibBuildInfo s))
{-# INLINE foreignLibBuildInfo #-}

foreignLibVersionInfo :: Lens' ForeignLib (Maybe LibVersionInfo)
foreignLibVersionInfo f s = fmap (\x -> s { T.foreignLibVersionInfo = x }) (f (T.foreignLibVersionInfo s))
{-# INLINE foreignLibVersionInfo #-}

foreignLibVersionLinux :: Lens' ForeignLib (Maybe Version)
foreignLibVersionLinux f s = fmap (\x -> s { T.foreignLibVersionLinux = x }) (f (T.foreignLibVersionLinux s))
{-# INLINE foreignLibVersionLinux #-}

foreignLibModDefFile :: Lens' ForeignLib [FilePath]
foreignLibModDefFile f s = fmap (\x -> s { T.foreignLibModDefFile = x }) (f (T.foreignLibModDefFile s))
{-# INLINE foreignLibModDefFile #-}
