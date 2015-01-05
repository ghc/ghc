{-# LANGUAGE NoImplicitPrelude #-}
module Package (
    packageRules
    ) where

import Package.Base
import Package.Data
import Package.Dependencies

-- These are the packages we build
packages :: [Package]
packages = [libraryPackage "deepseq" Stage1 defaultSettings]

buildPackage :: Package -> TodoItem -> Rules ()
buildPackage pkg todoItem = do
    buildPackageData         pkg todoItem
    buildPackageDependencies pkg todoItem

packageRules :: Rules ()
packageRules = do

    want ["libraries/deepseq/dist-install/build/deepseq.m"]
    forM_ packages $ \pkg -> do
        forM_ (pkgTodo pkg) $ \todoItem -> do
            buildPackage pkg todoItem
