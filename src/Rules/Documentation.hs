module Rules.Documentation (buildPackageDocumentation) where

import Way
import Base
import Stage
import Builder
import Package
import Expression
import Oracles.PackageData
import qualified Target
import Settings.TargetDirectory
import Rules.Actions
import Rules.Resources
import Settings.Util
import Settings.User
import Settings.Packages
import Control.Monad.Extra

-- Note: this build rule creates plenty of files, not just the .haddock one.
-- All of them go into the 'doc' subdirectory. Pedantically tracking all built
-- files in the Shake databases seems fragile and unnecesarry.
buildPackageDocumentation :: Resources -> StagePackageTarget -> Rules ()
buildPackageDocumentation _ target =
    let stage   = Target.stage target
        pkg     = Target.package target
        name    = pkgName pkg
        cabal   = pkgCabalPath pkg
        haddock = pkgHaddockPath pkg
    in when (stage == Stage1) $ do

        haddock %> \file -> do
            whenM (specified HsColour) $ do
                need [cabal]
                build $ fullTarget target GhcCabalHsColour [cabal] []
            srcs <- interpret target getPackageSources
            deps <- interpret target $ getPkgDataList DepNames
            let haddocks = [ pkgHaddockPath depPkg
                           | Just depPkg <- map findKnownPackage deps ]
            need $ srcs ++ haddocks
            let haddockWay = if dynamicGhcPrograms then dynamic else vanilla
            build $ fullTargetWithWay target Haddock haddockWay srcs [file]

-- $$($1_PACKAGE)-$$($1_$2_VERSION)_HADDOCK_DEPS =
--    $$(foreach n,$$($1_$2_DEPS)
--        ,$$($$n_HADDOCK_FILE) $$($$n_dist-install_$$(HADDOCK_WAY)_LIB))

-- $$($$($1_PACKAGE)-$$($1_$2_VERSION)_HADDOCK_FILE) :
--     $$$$($$($1_PACKAGE)-$$($1_$2_VERSION)_HADDOCK_DEPS) | $$$$(dir $$$$@)/.

-- # Make the haddocking depend on the library .a file, to ensure
-- # that we wait until the library is fully built before we haddock it
-- $$($$($1_PACKAGE)-$$($1_$2_VERSION)_HADDOCK_FILE) : $$($1_$2_$$(HADDOCK_WAY)_LIB)
-- endif
