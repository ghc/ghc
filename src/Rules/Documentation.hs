module Rules.Documentation (buildPackageDocumentation, haddockDependencies) where

import Base
import Context
import Flavour
import GHC
import Oracles.ModuleFiles
import Oracles.PackageData
import Settings
import Target
import Utilities

haddockHtmlLib :: FilePath
haddockHtmlLib = "inplace/lib/html/haddock-util.js"

haddockDependencies :: Context -> Action [FilePath]
haddockDependencies context = do
    path     <- buildPath context
    depNames <- pkgDataList $ DepNames path
    sequence [ pkgHaddockFile $ vanillaContext Stage1 depPkg
             | Just depPkg <- map findPackageByName depNames, depPkg /= rts ]

-- Note: this build rule creates plenty of files, not just the .haddock one.
-- All of them go into the 'doc' subdirectory. Pedantically tracking all built
-- files in the Shake database seems fragile and unnecessary.
buildPackageDocumentation :: Context -> Rules ()
buildPackageDocumentation context@Context {..} = when (stage == Stage1) $ do
    "//" ++ contextDir context ++ "//*.haddock" %> \file -> do
        srcs     <- hsSources context
        haddocks <- haddockDependencies context
        need $ srcs ++ haddocks ++ [haddockHtmlLib]

        -- Build Haddock documentation
        -- TODO: pass the correct way from Rules via Context
        dynamicPrograms <- dynamicGhcPrograms <$> flavour
        let haddockWay = if dynamicPrograms then dynamic else vanilla
        build $ target (context {way = haddockWay}) Haddock srcs [file]

    when (package == haddock) $ haddockHtmlLib %> \_ -> do
        let dir = takeDirectory haddockHtmlLib
        liftIO $ removeFiles dir ["//*"]
        copyDirectory "utils/haddock/haddock-api/resources/html" dir

-- # Make the haddocking depend on the library .a file, to ensure
-- # that we wait until the library is fully built before we haddock it
-- $$($$($1_PACKAGE)-$$($1_$2_VERSION)_HADDOCK_FILE) : $$($1_$2_$$(HADDOCK_WAY)_LIB)
-- endif
