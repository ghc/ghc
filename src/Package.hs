module Package (packageRules) where

import Package.Base
import Package.Data
import Package.Compile
import Package.Dependencies

-- See Package.Base for definitions of basic types

-- These are the packages we build:
packages :: [Package]
packages = [libraryPackage "deepseq" Stage1 defaultSettings]

-- Rule buildXY is defined in module X.Y
buildPackage :: Package -> TodoItem -> Rules ()
buildPackage pkg todoItem = do
    buildPackageData         pkg todoItem
    buildPackageDependencies pkg todoItem
    buildPackageCompile      pkg todoItem

packageRules :: Rules ()
packageRules = do
    -- TODO: control targets from commang line arguments
    want [ "libraries/deepseq/dist-install/build/Control/DeepSeq.o"
         , "libraries/deepseq/dist-install/build/Control/DeepSeq.p_o" ]
    forM_ packages $ \pkg -> do
        forM_ (pkgTodo pkg) $ \todoItem -> do
            buildPackage pkg todoItem
