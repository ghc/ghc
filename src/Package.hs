module Package (packageRules) where

import Package.Base
import Package.Data
import Package.Compile
import Package.Library
import Package.Dependencies

-- See Package.Base for definitions of basic types

-- These are the packages we build:
packages :: [Package]
packages = [libraryPackage "array"          Stage1 defaultSettings,
            libraryPackage "deepseq"        Stage1 defaultSettings,
            libraryPackage "bin-package-db" Stage1 defaultSettings]

-- Rule buildPackageX is defined in module Package.X
buildPackage :: Package -> TodoItem -> Rules ()
buildPackage = buildPackageData
            <> buildPackageDependencies
            <> buildPackageCompile
            <> buildPackageLibrary

packageRules :: Rules ()
packageRules = do
    -- TODO: control targets from commang line arguments
    want [ "libraries/deepseq/dist-install/build/libHSdeeps_FT5iVCELxOr62eHY0nbvnU.a"
         , "libraries/deepseq/dist-install/build/libHSdeeps_FT5iVCELxOr62eHY0nbvnU.p_a"
         , "libraries/deepseq/dist-install/build/HSdeeps_FT5iVCELxOr62eHY0nbvnU.o"
         , "libraries/array/dist-install/build/libHSarray_3w0nMK0JfaFJPpLFn2yWAJ.a"
         , "libraries/array/dist-install/build/libHSarray_3w0nMK0JfaFJPpLFn2yWAJ.p_a"
         , "libraries/array/dist-install/build/HSarray_3w0nMK0JfaFJPpLFn2yWAJ.o"
         , "libraries/bin-package-db/dist-install/build/libHSbinpa_9qPPbdABQ6HK3eua2jBtib.a"
         , "libraries/bin-package-db/dist-install/build/libHSbinpa_9qPPbdABQ6HK3eua2jBtib.p_a"
         , "libraries/bin-package-db/dist-install/build/HSbinpa_9qPPbdABQ6HK3eua2jBtib.o" ]
    forM_ packages $ \pkg -> do
        forM_ (pkgTodo pkg) $ \todoItem -> do
            buildPackage pkg todoItem
