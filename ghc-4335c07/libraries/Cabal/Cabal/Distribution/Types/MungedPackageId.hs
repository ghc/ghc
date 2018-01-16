{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.Types.MungedPackageId
  ( MungedPackageId(..)
  , computeCompatPackageId
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Version
         ( Version, nullVersion )

import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp
import Distribution.Compat.ReadP
import Distribution.Text
import Distribution.Types.PackageId
import Distribution.Types.UnqualComponentName
import Distribution.Types.MungedPackageName

-- | A simple pair of a 'MungedPackageName' and 'Version'. 'MungedPackageName' is to
-- 'MungedPackageId' as 'PackageName' is to 'PackageId'. See 'MungedPackageName' for more
-- info.
data MungedPackageId
    = MungedPackageId {
        -- | The combined package and component name. see documentation for
        -- 'MungedPackageName'.
        mungedName    :: MungedPackageName,
        -- | The version of this package / component, eg 1.2
        mungedVersion :: Version
     }
     deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

instance Binary MungedPackageId

instance Text MungedPackageId where
  disp (MungedPackageId n v)
    | v == nullVersion = disp n -- if no version, don't show version.
    | otherwise        = disp n <<>> Disp.char '-' <<>> disp v

  parse = do
    n <- parse
    v <- (Parse.char '-' >> parse) <++ return nullVersion
    return (MungedPackageId n v)

instance NFData MungedPackageId where
    rnf (MungedPackageId name version) = rnf name `seq` rnf version

-- | See docs for 'Distribution.Types.MungedPackageName.computeCompatPackageId'. this
-- is a thin wrapper around that.
computeCompatPackageId :: PackageId -> Maybe UnqualComponentName -> MungedPackageId
computeCompatPackageId (PackageIdentifier pn vr) mb_uqn = MungedPackageId pn' vr
  where pn' = computeCompatPackageName pn mb_uqn
