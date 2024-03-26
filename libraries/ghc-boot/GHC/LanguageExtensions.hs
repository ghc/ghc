{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module re-exports the 'Extension' type along with an orphan 'Binary'
-- instance for it.
--
-- Note that the @ghc-boot@ package has a large set of dependencies; for this
-- reason the 'Extension' type itself is defined in the
-- "GHC.LanguageExtensions.Type" module provided by the @ghc-boot-th@ package,
-- which has no dependencies outside of @base@. For this reason
-- @template-haskell@ depends upon @ghc-boot-th@, not @ghc-boot@.
--
-- When building the stage1 GHC, we have a potentially old version of
-- @ghc-boot-th@, which comes with an old list of extensions.
-- In that case, we define the current list of extensions here, and export that.
module GHC.LanguageExtensions
  ( Extension(..)
  , migrateExt
  , unmigrateExt
  )
  where

import Prelude -- See note [Why do we import Prelude here?]
import Data.Binary
import qualified "ghc-boot-th" GHC.LanguageExtensions.Type as TH
#if MIN_VERSION_ghc_boot_th(9,11,0)
import "ghc-boot-th" GHC.LanguageExtensions.Type (Extension(..))
#else
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import GHC.LanguageExtensions.Type (Extension(..))
#endif

instance Binary Extension
deriving instance Read Extension

migrateExt :: TH.Extension -> Extension
unmigrateExt :: Extension -> TH.Extension
#if MIN_VERSION_ghc_boot_th(9,11,0)
migrateExt = id
unmigrateExt = id
#else
errStr :: String
errStr = "unsupported language extension encountered in TH while bootstrapping"
deriving instance Read TH.Extension
migrateExt = fromMaybe (error errStr) . readMaybe . show
unmigrateExt = fromMaybe (error errStr) . readMaybe. show
#endif
