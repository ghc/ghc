module Settings.Packages.IntegerGmp (integerGmpPackageArgs, gmpBuildPath) where

import Base
import Expression
import GHC (integerGmp)
import Predicates (builder, builderGcc, package)
import Settings.User

-- TODO: move elsewhere
gmpBuildPath :: FilePath
gmpBuildPath = buildRootPath -/- "stage0/gmp"

-- TODO: move build artefacts to buildRootPath, see #113
-- TODO: Is this needed?
-- ifeq "$(GMP_PREFER_FRAMEWORK)" "YES"
-- libraries/integer-gmp_CONFIGURE_OPTS += --with-gmp-framework-preferred
-- endif
integerGmpPackageArgs :: Args
integerGmpPackageArgs = package integerGmp ? do
    let includeGmp = "-I" ++ gmpBuildPath -/- "include"
    mconcat [ builder GhcCabal ? mconcat
              [ arg "--configure-option=--with-intree-gmp"
              , appendSub "--configure-option=CFLAGS" [includeGmp]
              , appendSub "--gcc-options"             [includeGmp] ]

            , builderGcc ? arg includeGmp ]
  where

