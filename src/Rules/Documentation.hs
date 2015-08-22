module Rules.Documentation (buildPackageDocumentation) where

import Expression
import Oracles
import Rules.Actions
import Rules.Resources
import Settings

-- Note: this build rule creates plenty of files, not just the .haddock one.
-- All of them go into the 'doc' subdirectory. Pedantically tracking all built
-- files in the Shake databases seems fragile and unnecesarry.
buildPackageDocumentation :: Resources -> PartialTarget -> Rules ()
buildPackageDocumentation _ target @ (PartialTarget _ pkg) =
    let cabalFile   = pkgCabalFile pkg
        haddockFile = pkgHaddockFile pkg
    in do
        haddockFile %> \file -> do
            whenM (specified HsColour) $ do
                need [cabalFile]
                build $ fullTarget target GhcCabalHsColour [cabalFile] []
            srcs <- interpretPartial target getPackageSources
            deps <- interpretPartial target $ getPkgDataList DepNames
            let haddocks = [ pkgHaddockFile depPkg
                           | Just depPkg <- map findKnownPackage deps ]
            need $ srcs ++ haddocks
            let haddockWay = if dynamicGhcPrograms then dynamic else vanilla
            build $ fullTargetWithWay target Haddock haddockWay srcs [file]

-- # Make the haddocking depend on the library .a file, to ensure
-- # that we wait until the library is fully built before we haddock it
-- $$($$($1_PACKAGE)-$$($1_$2_VERSION)_HADDOCK_FILE) : $$($1_$2_$$(HADDOCK_WAY)_LIB)
-- endif
