module GHC.Unit.External.Visibility (
  VisibilityMap,
  UnitVisibility(..),
) where

import GHC.Prelude

import GHC.Data.FastString
import GHC.Driver.DynFlags
import GHC.Types.Unique.Map
import GHC.Unit.Module
import GHC.Utils.Outputable as Outputable

import Control.Applicative
import Data.Monoid (First (..))
import Data.Semigroup qualified as Semigroup
import Data.Set (Set)
import Data.Set qualified as Set

-- | 'UniqFM' map from 'Unit' to a 'UnitVisibility'.
type VisibilityMap = UniqMap Unit UnitVisibility

-- | 'UnitVisibility' records the various aspects of visibility of a particular
-- 'Unit'.
data UnitVisibility = UnitVisibility
    { uv_expose_all :: Bool
      --  ^ Should all modules in exposed-modules should be dumped into scope?
    , uv_renamings :: [(ModuleName, ModuleName)]
      -- ^ Any custom renamings that should bring extra 'ModuleName's into
      -- scope.
    , uv_package_name :: First FastString
      -- ^ The package name associated with the 'Unit'.  This is used
      -- to implement legacy behavior where @-package foo-0.1@ implicitly
      -- hides any packages named @foo@
    , uv_requirements :: UniqMap ModuleName (Set InstantiatedModule)
      -- ^ The signatures which are contributed to the requirements context
      -- from this unit ID.
    , uv_explicit :: Maybe PackageArg
      -- ^ Whether or not this unit was explicitly brought into scope,
      -- as opposed to implicitly via the 'exposed' fields in the
      -- package database (when @-hide-all-packages@ is not passed.)
    }

instance Outputable UnitVisibility where
    ppr (UnitVisibility {
        uv_expose_all = b,
        uv_renamings = rns,
        uv_package_name = First mb_pn,
        uv_requirements = reqs,
        uv_explicit = explicit
    }) = ppr (b, rns, mb_pn, reqs, explicit)

instance Semigroup UnitVisibility where
    uv1 <> uv2
        = UnitVisibility
          { uv_expose_all = uv_expose_all uv1 || uv_expose_all uv2
          , uv_renamings = uv_renamings uv1 ++ uv_renamings uv2
          , uv_package_name = mappend (uv_package_name uv1) (uv_package_name uv2)
          , uv_requirements = plusUniqMap_C Set.union (uv_requirements uv2) (uv_requirements uv1)
          , uv_explicit = uv_explicit uv1 <|> uv_explicit uv2
          }

instance Monoid UnitVisibility where
    mempty = UnitVisibility
             { uv_expose_all = False
             , uv_renamings = []
             , uv_package_name = First Nothing
             , uv_requirements = emptyUniqMap
             , uv_explicit = Nothing
             }
    mappend = (Semigroup.<>)

