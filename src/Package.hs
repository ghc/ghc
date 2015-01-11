module Package (packageRules) where

import Package.Base
import Package.Data
import Package.Compile
import Package.Dependencies

-- See Package.Base for definitions of basic types

-- These are the packages we build:
packages :: [Package]
packages = [libraryPackage "deepseq" Stage1 defaultSettings]

-- Rule buildPackageX is defined in module Package.X
buildPackage :: Package -> TodoItem -> Rules ()
buildPackage = buildPackageData
            <> buildPackageDependencies
            <> buildPackageCompile

packageRules :: Rules ()
packageRules = do
    -- TODO: control targets from commang line arguments
    want [ "libraries/deepseq/dist-install/build/Control/DeepSeq.o"
         , "libraries/deepseq/dist-install/build/Control/DeepSeq.p_o" ]
    forM_ packages $ \pkg -> do
        forM_ (pkgTodo pkg) $ \todoItem -> do
            buildPackage pkg todoItem
