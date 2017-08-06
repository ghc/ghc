module Settings.Packages.IntegerGmp (integerGmpPackageArgs, gmpBuildPath) where

import Base
import Expression
import GHC
import Oracles.Config.Setting
import Settings.Path

-- TODO: Is this needed?
-- ifeq "$(GMP_PREFER_FRAMEWORK)" "YES"
-- libraries/integer-gmp_CONFIGURE_OPTS += --with-gmp-framework-preferred
-- endif
integerGmpPackageArgs :: Args
integerGmpPackageArgs = package integerGmp ? do
    let includeGmp = "-I" ++ gmpBuildPath -/- "include"
    gmpIncludeDir <- getSetting GmpIncludeDir
    gmpLibDir     <- getSetting GmpLibDir
    mconcat [ builder Cc ? arg includeGmp

            , builder GhcCabal ? mconcat
              [ (null gmpIncludeDir && null gmpLibDir) ?
                arg "--configure-option=--with-intree-gmp"
              , arg ("--configure-option=CFLAGS=" ++ includeGmp)
              , arg ("--gcc-options="             ++ includeGmp) ] ]
